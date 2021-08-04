
#include <clang/Basic/AttrKinds.h>

#include <vector>
#include <string>

#include <clang/Driver/Options.h>
#include <clang/AST/AST.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/Mangle.h>
#include <clang/AST/TemplateBase.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Frontend/ASTConsumers.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Rewrite/Core/Rewriter.h>

namespace {
	llvm::cl::OptionCategory oakReflectCategory{ "oak_reflect options" };

	llvm::cl::extrahelp commonHelp{ clang::tooling::CommonOptionsParser::HelpMessage };

	llvm::cl::opt<std::string> outputPathOption{
		"reflectionOutputPath",
		llvm::cl::desc{"Specify reflection output file path"},
		llvm::cl::value_desc{"reflection output path"},
		llvm::cl::Required,
	};

	llvm::cl::opt<std::string> includePrefixOption{
		"reflectionIncludePrefix",
		llvm::cl::desc{"Specify an include prefix to be added to the generated include statements"},
		llvm::cl::value_desc{"reflection include prefix"},
	};

	constexpr uint64_t hash_combine(uint64_t const a, uint64_t const b) {
		// Combine the two hash values using a bunch of random large primes
		return 262147 + a * 131101 + b * 65599;
	}

}

std::string get_annotation_string(clang::Decl const *decl) {
	std::string result;
	for (auto const& attr : decl->attrs()) {
		if (attr->getKind() == clang::attr::Annotate) {
			llvm::SmallString<32> str;
			llvm::raw_svector_ostream os{ str };
			clang::LangOptions langOpts;
			clang::PrintingPolicy policy{ langOpts };
			attr->printPretty(os, policy);
			result = str.slice(26, str.size() - 6).str();
			break;
		}
	}
	return result;
}

bool should_reflect_decl(clang::Decl const *decl) {
	return get_annotation_string(decl).find("reflect") == 0;
}

bool decl_always_comes_before(clang::TagDecl const *lhs, clang::TagDecl const *rhs) {
	if (rhs->isRecord()) {
		auto recordDecl = static_cast<clang::CXXRecordDecl const*>(rhs);
		if (recordDecl->getTemplateSpecializationKind() != 0) {
			auto specialization = static_cast<clang::ClassTemplateSpecializationDecl const*>(recordDecl);
			for (auto const& arg : specialization->getTemplateInstantiationArgs().asArray()) {
				auto kind = arg.getKind();
				switch (kind) {
					case clang::TemplateArgument::Type:
						{
							auto type = arg.getAsType().getTypePtr();
							if (type->isRecordType() || type->isEnumeralType()) {
								if (lhs == type->getAsTagDecl()) {
									return true;
								}
							}
							if (type->isPointerType()) {
								auto pointeeType = static_cast<clang::PointerType const*>(type)
									->getPointeeType().getTypePtr();
								if (pointeeType->isRecordType()) {
									if (lhs == pointeeType->getAsTagDecl()) {
										return true;
									}
								}
							}
						} break;
					default:
						break;
				}
			}
		}
		for (auto field : recordDecl->fields()) {
			auto ft = field->getType().getTypePtr()->getUnqualifiedDesugaredType();
			auto fpt = ft->isPointerType() ? ft->getPointeeType()->getUnqualifiedDesugaredType() : nullptr;

			auto dt = lhs->getTypeForDecl()->getUnqualifiedDesugaredType();

			if (ft == dt || fpt == dt) {
				return true;
			}
		}
		for (auto const& base : recordDecl->bases()) {
			auto bt = base.getType().getTypePtr()->getUnqualifiedDesugaredType();

			auto dt = lhs->getTypeForDecl()->getUnqualifiedDesugaredType();

			if (bt == dt) {
				return true;
			}
		}
	}

	return false;
}

struct DeclFinder;

struct DeclSerializer {
	virtual void serialize(DeclFinder *finder) = 0;
	virtual ~DeclSerializer() {};
};

struct DeclFinder : public clang::ast_matchers::MatchFinder::MatchCallback {

	clang::ASTContext *context = nullptr;
	clang::SourceManager *sourceManager = nullptr;

	DeclSerializer *serializer = nullptr;
	llvm::DenseSet<clang::TagDecl const*> decls;

	DeclFinder(DeclSerializer *serializer_)
		: serializer{ serializer_ } {}

	void parse_record(clang::CXXRecordDecl const *record);

	void parse_enum(clang::EnumDecl const *enumeration);

	void add_decl(clang::TagDecl const *declToAdd);

	std::vector<clang::TagDecl const*> get_sorted_decls();

	void run(clang::ast_matchers::MatchFinder::MatchResult const& result) override;

	void onStartOfTranslationUnit() override;

	void onEndOfTranslationUnit() override;

};

void DeclFinder::parse_record(clang::CXXRecordDecl const *record) {
	if (record->isDependentType())
		return;

	// Only reflect template type whose template type parameters are also being reflected
	if (record->getTemplateSpecializationKind() != 0) {
		auto specialization = static_cast<clang::ClassTemplateSpecializationDecl const*>(record);
		for (auto const& arg : specialization->getTemplateInstantiationArgs().asArray()) {
			auto kind = arg.getKind();
			switch (kind) {
				case clang::TemplateArgument::Type:
					{
						auto type = arg.getAsType().getTypePtr();
						if (type->isBuiltinType()) {
							break;
						}

						if (type->isRecordType() && !should_reflect_decl(type->getAsTagDecl())) {
							return;
						} if (type->isPointerType()) {
							auto pType = type->getPointeeType().getTypePtr();
							if (pType->isRecordType() && !should_reflect_decl(pType->getAsTagDecl())) {
								return;
							}
						}
					} break;
				default:
					break;
			}
		}
	}

	add_decl(record);
}

void DeclFinder::parse_enum(clang::EnumDecl const *enumeration) {
	auto sourceLoc = enumeration->getOuterLocStart();
	auto isInMainFile = sourceManager->isWrittenInMainFile(sourceLoc);

	if (!isInMainFile)
		return;

	add_decl(enumeration);
}

void DeclFinder::add_decl(clang::TagDecl const *declToAdd) {
	decls.insert(declToAdd);
}

std::vector<clang::TagDecl const*> DeclFinder::get_sorted_decls() {
	std::vector<clang::TagDecl const*> sortedDecls;
	std::vector<clang::TagDecl const*> tmpDecls;
	sortedDecls.reserve(decls.size());
	tmpDecls.reserve(decls.size());

	for (auto decl : decls) {
		sortedDecls.push_back(decl);
	}

	bool sort = true;
	while (sort) {
		sort = false;
		for (auto decl : sortedDecls) {
			size_t i = 0;
			for (; i < tmpDecls.size(); ++i) {
				if (decl_always_comes_before(decl, tmpDecls[i])) {
					sort = true;
					break;
				}
			}
			tmpDecls.insert(std::begin(tmpDecls) + i, decl);
		}
		std::swap(sortedDecls, tmpDecls);
		tmpDecls.clear();
	}

	return sortedDecls;
}

void DeclFinder::run(clang::ast_matchers::MatchFinder::MatchResult const& result) {
	context = result.Context;
	sourceManager = result.SourceManager;

	auto record = result.Nodes.getNodeAs<clang::CXXRecordDecl>("id");
	if (record) {
		parse_record(record);
	}

	auto enumeration = result.Nodes.getNodeAs<clang::EnumDecl>("id");
	if (enumeration) {
		parse_enum(enumeration);
	}
}

void DeclFinder::onStartOfTranslationUnit() {
}

void DeclFinder::onEndOfTranslationUnit() {
	serializer->serialize(this);
	decls.clear();
}

struct DeclConstexprSerializerCreateInfo {
	std::string outputPath;
	std::string includePrefix;
	std::vector<std::string> sourcePaths;
};

struct DeclConstexprSerializer : DeclSerializer {

	std::string outputPath;
	llvm::raw_fd_ostream fs;

	llvm::SmallString<64> typeListString;
	llvm::StringSet<> writtenDeclNames;

	std::error_code ec;

	DeclConstexprSerializer(std::string const& outputPath, std::string const& includePrefix, std::vector<std::string> const& sourcePaths);
	~DeclConstexprSerializer();

	void init(DeclConstexprSerializerCreateInfo const& createInfo);
	void destroy();

	std::string get_mangled_name(clang::Decl const *decl);

	std::string get_specialization_name(clang::CXXRecordDecl const *decl);

	std::string get_enum_name(clang::EnumDecl const* decl);

	std::string get_namespace_name(clang::TagDecl const* decl);

	void write_start_type_list();

	void write_end_type_list();

	void write_record_type_list_item(clang::CXXRecordDecl const* decl);

	void write_enum_type_list_item(clang::EnumDecl const* decl);

	void write_type_list();

	void write_header(std::string const& includePrefix, std::vector<std::string> const& filenames);

	void write_footer();

	void write_parsed_record(clang::CXXRecordDecl const *decl);

	void write_parsed_enum(clang::EnumDecl const *decl);

	void serialize(DeclFinder *finder) override;
};

DeclConstexprSerializer::DeclConstexprSerializer(std::string const& outputPath_, std::string const& includePrefix_, std::vector<std::string> const& sourcePaths_)
		: outputPath{ outputPath_ }, fs{ outputPath_, ec } {

	write_header(includePrefix_, sourcePaths_);

	write_start_type_list();
}

DeclConstexprSerializer::~DeclConstexprSerializer() {
	write_end_type_list();
	write_type_list();
	write_footer();
}

std::string DeclConstexprSerializer::get_mangled_name(clang::Decl const *decl) {
	clang::ASTNameGenerator nameGen{ decl->getASTContext() };
	return nameGen.getName(decl);
}

std::string DeclConstexprSerializer::get_specialization_name(clang::CXXRecordDecl const *decl) {
	llvm::SmallString<64> result;
	llvm::raw_svector_ostream ss{ result };

	ss << decl->getName();

	if (decl->getTemplateSpecializationKind() != 0) {
		auto specialization = static_cast<clang::ClassTemplateSpecializationDecl const*>(decl);
		ss << "<";
		bool first = true;
		for (auto const& arg : specialization->getTemplateInstantiationArgs().asArray()) {
			if (!first) {
				// Add comma's to the template argument list
				ss << ", ";
			}
			first = false;
			auto kind = arg.getKind();
			switch (kind) {
				case clang::TemplateArgument::Type:
					{
						auto const& qualType = arg.getAsType();
						auto type = qualType.getTypePtr();
						if (type->isRecordType()) {
							auto recordDecl = type->getAsCXXRecordDecl();
							ss << get_namespace_name(recordDecl) << get_specialization_name(recordDecl);
						} else if (type->isEnumeralType()) {
							auto enumDecl = static_cast<clang::EnumType const*>(type)->getDecl();
							ss << get_namespace_name(enumDecl) << get_enum_name(enumDecl);
						} else if (type->isPointerType()) {
							auto pointeeQualType = static_cast<clang::PointerType const*>(type)->getPointeeType();
							auto pointeeType = pointeeQualType.getTypePtr();
							if (pointeeType->isRecordType()) {
								auto recordDecl = pointeeType->getAsCXXRecordDecl();
								ss << get_namespace_name(recordDecl) << get_specialization_name(recordDecl) << "*";
							} else if (pointeeType->isEnumeralType()) {
								auto enumDecl = static_cast<clang::EnumType const*>(type)->getDecl();
								ss << get_namespace_name(enumDecl) << get_enum_name(enumDecl) << "*";
							} else {
								ss << qualType.getAsString() << "*";
							}
						} else {
							ss << qualType.getAsString();
						}

					} break;
				case clang::TemplateArgument::Integral:
					{
						auto const& integral = arg.getAsIntegral();
						ss << integral.toString(10);
					} break;
				default:
					assert(false && "Template argument type unsupported");
					break;
			}
		}
		ss << ">";
	}

	return result.str().str();
}

std::string DeclConstexprSerializer::get_enum_name(clang::EnumDecl const* decl) {
	return decl->getName().str();
}

std::string DeclConstexprSerializer::get_namespace_name(clang::TagDecl const* decl) {
	std::string result;

	auto nsContext = decl->getParent();
	while (nsContext) {
		if (nsContext->isNamespace()) {
			result = (static_cast<clang::NamespaceDecl const*>(nsContext)->getName() + "::" + result).str();
		} else if (nsContext->isRecord()) {
			result = (static_cast<clang::RecordDecl const*>(nsContext)->getName() + "::" + result).str();
		}
		nsContext = nsContext->getLexicalParent();
	}

	return result;
}

void DeclConstexprSerializer::write_start_type_list() {
	auto start = outputPath.find_last_of("/") + 1;
	auto end = outputPath.find_first_of(".", start);
	auto listName = outputPath.substr(start, end - start);

	llvm::raw_svector_ostream ss{ typeListString };
	ss << "constexpr TypeInfo const* typeList_" << listName << "[] = {\n";
}

void DeclConstexprSerializer::write_end_type_list() {
	llvm::raw_svector_ostream ss{ typeListString };
	ss << "};\n";
}

void DeclConstexprSerializer::write_record_type_list_item(clang::CXXRecordDecl const* decl) {
	llvm::raw_svector_ostream ss{ typeListString };
	ss << "&Reflect<" << get_namespace_name(decl) << get_specialization_name(decl) << ">::typeInfo,\n";
}

void DeclConstexprSerializer::write_enum_type_list_item(clang::EnumDecl const* decl) {
	llvm::raw_svector_ostream ss{ typeListString };
	ss << "&Reflect<" << get_namespace_name(decl) << get_enum_name(decl) << ">::typeInfo,\n";
}

void DeclConstexprSerializer::write_type_list() {
	fs << typeListString;
}

void DeclConstexprSerializer::write_header(std::string const& includePrefix, std::vector<std::string> const& filenames) {
	fs << "#pragma once\n\n";

	fs << "#include <oak_reflect/type_info.h>\n";

	for (auto const& fn : filenames) {
		fs << "#include \"" << includePrefix << fn << "\"\n";
	}

	fs << "\nnamespace oak {\n";
}

void DeclConstexprSerializer::write_footer() {
	fs << "}\n";
}

void DeclConstexprSerializer::write_parsed_record(clang::CXXRecordDecl const *decl) {
	auto annotation = get_annotation_string(decl);

	auto namespaceName = get_namespace_name(decl);
	auto specializationName = get_specialization_name(decl);

	fs << "template<> struct Reflect<" << namespaceName << specializationName << "> {\n";
	fs <<  "\tusing T = " << namespaceName << specializationName << ";\n";

	bool hasFields = false;
	bool hasMethods = false;
	for (auto const& field : decl->fields()) {
		if (should_reflect_decl(field)) {
			hasFields = true;
			break;
		}
	}
	for (auto const& func : decl->methods()) {
		if (should_reflect_decl(func)) {
			hasMethods = true;
			break;
		}
	}
	for (auto const& base : decl->bases()) {
		auto bDecl = base.getType().getTypePtr()->getUnqualifiedDesugaredType()->getAsCXXRecordDecl();
		for (auto const& field : bDecl->fields()) {
			if (should_reflect_decl(field)) {
				hasFields = true;
				break;
			}
		}
		for (auto const& func : bDecl->methods()) {
			if (should_reflect_decl(func)) {
				hasMethods = true;
				break;
			}
		}
	}
	if (hasFields) {
		fs << "\tstatic constexpr FieldInfo fields[] = {\n";
		for (auto const& base : decl->bases()) {
			auto bDecl = base.getType().getTypePtr()->getUnqualifiedDesugaredType()->getAsCXXRecordDecl();
			for (auto const field : bDecl->fields()) {
				if (!should_reflect_decl(field))
					continue;
				auto const& fname = field->getDeclName().getAsString();
				auto fanno = get_annotation_string(field);
				fs << "\t\t{ \"" << fname
					<< "\", \"" << fanno
					<< "\", &Reflect<decltype(T::" << fname
					<< ")>::typeInfo, offsetof(T, " << fname << ")},\n";
			}
		}
		for (auto const field : decl->fields()) {
			if (!should_reflect_decl(field))
				continue;
			auto const& fname = field->getDeclName().getAsString();
			auto fanno = get_annotation_string(field);
			fs << "\t\t{ \"" << fname
				<< "\", \"" << fanno
				<< "\", &Reflect<decltype(T::" << fname
				<< ")>::typeInfo, offsetof(T, " << fname << ")},\n";
		}
		fs << "\t};\n";
	}
	if (hasMethods) {
		fs << "\tstatic constexpr MethodInfo methods[] = {\n";
		for (auto const& base : decl->bases()) {
			auto bDecl = base.getType().getTypePtr()->getUnqualifiedDesugaredType()->getAsCXXRecordDecl();
			for (auto const func : bDecl->methods()) {
				if (!should_reflect_decl(func))
					continue;
				auto fname = func->getNameInfo().getAsString();
				auto fmangledName = get_mangled_name(func);
				auto fanno = get_annotation_string(func);
				fs << "\t\t{ \"" << fname
					<< "\", \"" << fmangledName
					<< "\", \"" << fanno
					<< "\", &Reflect<decltype(&T::" << fname << ")>::typeInfo, 0},\n";
			}
		}
		for (auto const func : decl->methods()) {
			if (!should_reflect_decl(func))
				continue;
			auto fname = func->getNameInfo().getAsString();
			auto fmangledName = get_mangled_name(func);
			auto fanno = get_annotation_string(func);
			fs << "\t\t{ \"" << fname
				<< "\", \"" << fmangledName
				<< "\", \"" << fanno
				<< "\", &Reflect<decltype(&T::" << fname << ")>::typeInfo, 0},\n";
		}
		fs << "\t};\n";
	}

	auto typeId = hash_combine(
			std::hash<std::string>{}(namespaceName),
			std::hash<std::string>{}(specializationName));

	if (decl->isUnion()) {
		fs << "\tstatic constexpr UnionTypeInfo typeInfo{ { " << typeId
			<< "ul, TypeInfoKind::UNION }, \"" << specializationName
			<< "\", \"" << annotation
			<< "\", sizeof(T), alignof(T), " << (hasFields ? "fields" : "{}")
			<< ", &detail::generic_construct<T> };\n";
	} else if (decl->isClass() || decl->isStruct()) {
		// Get the first base
		if (decl->getNumBases()) {
			auto base = decl->bases_begin()->getType().getTypePtr()->getAsCXXRecordDecl();
			auto baseNamespaceName = get_namespace_name(base);
			auto baseName = get_specialization_name(base);
			fs << "\tstatic constexpr StructTypeInfo typeInfo{ { " << typeId
				<< "ul, TypeInfoKind::STRUCT }, \"" << specializationName
				<< "\", \"" << annotation
				<< "\", sizeof(T), alignof(T), &Reflect<" << baseNamespaceName << baseName
				<< ">::typeInfo, " << (hasFields ? "fields" : "{}")
				<< ", " << (hasMethods ? "methods" : "{}")
				<< ", &detail::generic_construct<T> };\n";
		} else {
			fs << "\tstatic constexpr StructTypeInfo typeInfo{ { " << typeId
				<< "ul, TypeInfoKind::STRUCT }, \"" << specializationName
				<< "\", \"" << annotation
				<< "\", sizeof(T), alignof(T), &Reflect<NoType>::typeInfo, " << (hasFields ? "fields" : "{}")
				<< ", " << (hasMethods ? "methods" : "{}")
				<< ", &detail::generic_construct<T> };\n";
		}
	}
	fs << "};\n";
}

void DeclConstexprSerializer::write_parsed_enum(clang::EnumDecl const *decl) {
	auto annotation = get_annotation_string(decl);

	auto namespaceName = get_namespace_name(decl);
	auto enumName = get_enum_name(decl);

	fs << "template<> struct Reflect<" << namespaceName << enumName << "> {\n";
	fs << "\tusing T = " << namespaceName << enumName << ";\n";

	bool hasConstants = decl->enumerator_begin() != decl->enumerator_end();
	if (hasConstants) {
		fs << "\tstatic constexpr EnumConstantInfo enumConstants[] = {\n";
		for (auto const& enumConstant : decl->enumerators()) {
			auto const& ename = enumConstant->getDeclName().getAsString();
			auto evalue = static_cast<uint64_t>(enumConstant->getInitVal().getExtValue());
			fs << "\t\t{ \"" << ename << "\", " << evalue << " },\n";
		}
		fs << "\t};\n";
	}

	auto const& underlyingTypeName = decl->getIntegerType().getAsString();

	auto typeId = hash_combine(std::hash<std::string>{}(namespaceName), std::hash<std::string>{}(enumName));

	fs << "\tstatic constexpr EnumTypeInfo typeInfo{ { " << typeId
		<< "ul, TypeInfoKind::ENUM }, \"" << enumName
		<< "\", \"" << annotation
		<< "\", &Reflect<" << underlyingTypeName
		<< ">::typeInfo, " << (hasConstants ? "enumConstants" : "{}")
		<< " };\n";

	fs << "};\n";
}

void DeclConstexprSerializer::serialize(DeclFinder *finder) {
	auto sortedDecls = finder->get_sorted_decls();
	for (auto decl : sortedDecls) {
		if (decl->isEnum()) {
			auto enumDecl = static_cast<clang::EnumDecl const*>(decl);
			auto declName = get_namespace_name(enumDecl) + get_enum_name(enumDecl);
			if (writtenDeclNames.find(declName) != std::end(writtenDeclNames))
				continue;
			write_parsed_enum(enumDecl);
			write_enum_type_list_item(enumDecl);
			writtenDeclNames.insert(declName);
		} else {
			auto recordDecl = static_cast<clang::CXXRecordDecl const*>(decl);
			auto declName = get_namespace_name(recordDecl) + get_specialization_name(recordDecl);
			if (writtenDeclNames.find(declName) != std::end(writtenDeclNames))
				continue;
			write_parsed_record(recordDecl);
			write_record_type_list_item(recordDecl);
			writtenDeclNames.insert(declName);
		}
	}
}

int main(int argc, char const **argv) {

	clang::tooling::CommonOptionsParser parser{ argc, argv, oakReflectCategory };
	clang::tooling::ClangTool tool{ parser.getCompilations(), parser.getSourcePathList() };

	DeclConstexprSerializer serializer{ outputPathOption, includePrefixOption, parser.getSourcePathList() };
	DeclFinder declFinder{ &serializer };

	clang::ast_matchers::MatchFinder finder;

	clang::ast_matchers::DeclarationMatcher classMatcher
		= clang::ast_matchers::cxxRecordDecl(
				clang::ast_matchers::decl().bind("id"),
				clang::ast_matchers::hasAttr(clang::attr::Annotate));
	clang::ast_matchers::DeclarationMatcher enumMatcher
		= clang::ast_matchers::enumDecl(
				clang::ast_matchers::decl().bind("id"),
				clang::ast_matchers::hasAttr(clang::attr::Annotate));

	finder.addMatcher(classMatcher, &declFinder);
	finder.addMatcher(enumMatcher, &declFinder);

	tool.run(clang::tooling::newFrontendActionFactory(&finder).get());

	return 0;
}


