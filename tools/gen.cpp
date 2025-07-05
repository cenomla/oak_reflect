
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
#include <clang/Frontend/LogDiagnosticPrinter.h>
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

	llvm::cl::opt<bool> onlyMainOption{
		"onlyIncludeMain",
		llvm::cl::desc{"Set to true to only generate info for types that are in the main input file"},
		llvm::cl::value_desc{"only generate types defined in the main file"},
		llvm::cl::init(false),
	};

}

std::string get_mangled_name(clang::Decl const *decl);
std::string get_type_name(clang::TypeDecl const *decl);

std::string get_mangled_name(clang::Decl const *decl) {
	clang::ASTNameGenerator nameGen{ decl->getASTContext() };
	return nameGen.getName(decl);
}

std::string get_type_name(clang::TypeDecl const *decl) {
	auto qt = clang::QualType{ decl->getTypeForDecl(), 0 };
	return qt.getAsString(decl->getASTContext().getPrintingPolicy());
}

void get_annotation_string(llvm::SmallVectorImpl<char>& out, clang::Decl const *decl) {
	for (auto const& attr : decl->attrs()) {
		if (attr->getKind() == clang::attr::Annotate) {
			auto annoAttr = static_cast<clang::AnnotateAttr*>(attr);
			auto str = annoAttr->getAnnotation();
			out.append(str.begin(), str.end());
			break;
		}
	}
}

bool should_reflect_decl(clang::Decl const *decl) {
	llvm::SmallString<64> annotationString;
	get_annotation_string(annotationString, decl);
	return annotationString.find("reflect") == 0;
}

bool is_property(clang::Decl const *decl) {
	return decl->getKind() == clang::Decl::Kind::Var
			&& static_cast<clang::VarDecl const*>(decl)->isStaticDataMember();
}

bool decl_always_comes_before(clang::TagDecl const *lhs, clang::TagDecl const *rhs) {
	// TODO: Needs to take arrays into consideration (ie. Vec2 points[2])
	if (rhs->isRecord()) {
		auto recordDecl = static_cast<clang::CXXRecordDecl const*>(rhs);
		if (isTemplateInstantiation(recordDecl->getTemplateSpecializationKind())) {
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
	llvm::SmallVector<clang::TagDecl const*, 4> decls;

	DeclFinder(DeclSerializer *serializer_)
		: serializer{ serializer_ } {}

	void parse_record(clang::CXXRecordDecl const *record);

	void parse_enum(clang::EnumDecl const *enumeration);

	void add_decl(clang::TagDecl const *declToAdd);

	void get_sorted_decls(llvm::SmallVectorImpl<clang::TagDecl const*> &sortedDecls);

	void run(clang::ast_matchers::MatchFinder::MatchResult const& result) override;

	void onStartOfTranslationUnit() override;

	void onEndOfTranslationUnit() override;

};

void DeclFinder::parse_record(clang::CXXRecordDecl const *record) {
	if (record->isDependentType()
			|| !record->isThisDeclarationADefinition()
			|| !record->isCompleteDefinition())
		return;
	if (onlyMainOption == true) {
		auto& context = record->getASTContext();
		if (!context.getSourceManager().isInMainFile(record->getLocation())) {
			return;
		}
	}
	if (clang::isTemplateInstantiation(record->getTemplateSpecializationKind())) {
		// Only reflect template type whose template type parameters are also being reflected
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

						if (type->isEnumeralType() && !should_reflect_decl(type->getAsTagDecl()))
							return;

						if (type->isRecordType() && !should_reflect_decl(type->getAsTagDecl()))
							return;

						if (type->isPointerType()) {
							auto pType = type->getPointeeType().getTypePtr();
							if (pType->isEnumeralType() && !should_reflect_decl(pType->getAsTagDecl()))
								return;

							if (pType->isRecordType() && !should_reflect_decl(pType->getAsTagDecl()))
								return;
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
	if (enumeration->isDependentType()
			|| !enumeration->isThisDeclarationADefinition()
			|| !enumeration->isCompleteDefinition())
		return;
	if (onlyMainOption == true) {
		auto& context = enumeration->getASTContext();
		if (!context.getSourceManager().isInMainFile(enumeration->getLocation())) {
			return;
		}
	}
	add_decl(enumeration);
}

void DeclFinder::add_decl(clang::TagDecl const *declToAdd) {
	decls.push_back(declToAdd);
}

void DeclFinder::get_sorted_decls(llvm::SmallVectorImpl<clang::TagDecl const*> &sortedDecls) {
	sortedDecls.append(decls.begin(), decls.end());

	llvm::SmallVector<clang::TagDecl const*, 4> tmpDecls;
	tmpDecls.reserve(decls.size());

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

struct DeclConstexprSerializer : DeclSerializer {

	std::string outputPath;
	llvm::raw_fd_ostream fs;

	std::string typeListString;

	std::error_code ec;

	DeclConstexprSerializer(
			std::string const& outputPath,
			std::string const& includePrefix,
			std::vector<std::string> const& sourcePaths);
	~DeclConstexprSerializer();

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

DeclConstexprSerializer::DeclConstexprSerializer(
		std::string const& outputPath_,
		std::string const& includePrefix_,
		std::vector<std::string> const& sourcePaths_)
			: outputPath{ outputPath_ }, fs{ outputPath_, ec } {

	write_header(includePrefix_, sourcePaths_);

	write_start_type_list();
}

DeclConstexprSerializer::~DeclConstexprSerializer() {
	write_end_type_list();
	write_type_list();
	write_footer();
}

void DeclConstexprSerializer::write_start_type_list() {
	auto start = outputPath.find_last_of("/") + 1;
	auto end = outputPath.find_first_of(".", start);
	auto listName = outputPath.substr(start, end - start);

	llvm::raw_string_ostream ss{ typeListString };
	ss << "constexpr TypeInfo const* typeList_" << listName << "[] = {\n";
}

void DeclConstexprSerializer::write_end_type_list() {
	llvm::raw_string_ostream ss{ typeListString };
	ss << "};\n";
}

void DeclConstexprSerializer::write_record_type_list_item(clang::CXXRecordDecl const* decl) {
	llvm::raw_string_ostream ss{ typeListString };
	ss << "&Reflect<" << get_type_name(decl) << ">::typeInfo,\n";
}

void DeclConstexprSerializer::write_enum_type_list_item(clang::EnumDecl const* decl) {
	llvm::raw_string_ostream ss{ typeListString };
	ss << "&Reflect<" << get_type_name(decl) << ">::typeInfo,\n";
}

void DeclConstexprSerializer::write_type_list() {
	fs << typeListString;
}

void DeclConstexprSerializer::write_header(
		std::string const& includePrefix, std::vector<std::string> const& filenames) {
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
	llvm::SmallString<64> annotation;
	get_annotation_string(annotation, decl);

	auto typeName = get_type_name(decl);

	fs << "template<> struct Reflect<" << typeName << "> {\n";
	fs <<  "\tusing T = " << typeName << ";\n";

	bool hasFields = false;
	bool hasMethods = false;
	bool hasProperties = false;
	for (auto field : decl->fields()) {
		if (should_reflect_decl(field)) {
			hasFields = true;
			break;
		}
	}
	for (auto func : decl->methods()) {
		if (should_reflect_decl(func)) {
			hasMethods = true;
			break;
		}
	}

	for (auto prop : decl->decls()) {
		if (is_property(prop) && should_reflect_decl(prop)) {
			hasProperties = true;
			break;
		}
	}

	for (auto const& base : decl->bases()) {
		auto bDecl = base.getType().getTypePtr()->getUnqualifiedDesugaredType()->getAsCXXRecordDecl();
		for (auto field : bDecl->fields()) {
			if (should_reflect_decl(field)) {
				hasFields = true;
				break;
			}
		}
		for (auto func : bDecl->methods()) {
			if (should_reflect_decl(func)) {
				hasMethods = true;
				break;
			}
		}
		for (auto prop : decl->decls()) {
			if (is_property(prop) && should_reflect_decl(prop)) {
				hasProperties = true;
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
				llvm::SmallString<64> fanno;
				get_annotation_string(fanno, field);
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
			llvm::SmallString<64> fanno;
			get_annotation_string(fanno, field);
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
				llvm::SmallString<64> fanno;
				get_annotation_string(fanno, func);
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
			llvm::SmallString<64> fanno;
			get_annotation_string(fanno, func);
			fs << "\t\t{ \"" << fname
				<< "\", \"" << fmangledName
				<< "\", \"" << fanno
				<< "\", &Reflect<decltype(&T::" << fname << ")>::typeInfo, 0},\n";
		}
		fs << "\t};\n";
	}
	if (hasProperties) {
		fs << "\tstatic constexpr PropertyInfo properties[] = {\n";
		for (auto const& base : decl->bases()) {
			auto bDecl = base.getType().getTypePtr()->getUnqualifiedDesugaredType()->getAsCXXRecordDecl();
			for (auto prop : bDecl->decls()) {
				if (!is_property(prop) || !should_reflect_decl(prop))
					continue;
				auto const& fname = static_cast<clang::VarDecl*>(prop)->getDeclName().getAsString();
				llvm::SmallString<64> fanno;
				get_annotation_string(fanno, prop);
				fs << "\t\t{ \"" << fname
					<< "\", \"" << fanno
					<< "\", &Reflect<std::remove_cv_t<decltype(T::" << fname
					<< ")>>::typeInfo, &T::" << fname << "},\n";
			}
		}
		for (auto prop : decl->decls()) {
			if (!is_property(prop) || !should_reflect_decl(prop))
				continue;
			auto const& fname = static_cast<clang::VarDecl*>(prop)->getDeclName().getAsString();
			llvm::SmallString<64> fanno;
			get_annotation_string(fanno, prop);
			fs << "\t\t{ \"" << fname
				<< "\", \"" << fanno
				<< "\", &Reflect<std::remove_cv_t<decltype(T::" << fname
				<< ")>>::typeInfo, &T::" << fname << "},\n";
		}
		fs << "\t};\n";
	}

	if (decl->isUnion()) {
		fs << "\tstatic constexpr UnionTypeInfo typeInfo{ { OAK_TYPE_UID(" << typeName
			<< "), TypeInfoKind::UNION }, \"" << typeName
			<< "\", \"" << annotation
			<< "\", sizeof(T), alignof(T), " << (hasFields ? "fields" : "{}")
			<< ", &detail::generic_construct<T> };\n";
	} else if (decl->isClass() || decl->isStruct()) {
		// Get the first base
		clang::CXXRecordDecl *baseDecl = nullptr;
		if (decl->getNumBases()) {
			auto base = decl->bases_begin()->getType().getTypePtr()->getAsCXXRecordDecl();
			if (should_reflect_decl(base))
				baseDecl = base;
		}

		if (baseDecl) {
			auto baseName = get_type_name(baseDecl);
			fs << "\tstatic constexpr StructTypeInfo typeInfo{ { OAK_TYPE_UID(" << typeName
				<< "), TypeInfoKind::STRUCT }, \"" << typeName
				<< "\", \"" << annotation
				<< "\", sizeof(T), alignof(T), &Reflect<" << baseName
				<< ">::typeInfo, " << (hasFields ? "fields" : "{}")
				<< ", " << (hasMethods ? "methods" : "{}")
				<< ", " << (hasProperties ? "properties" : "{}")
				<< ", &detail::generic_construct<T> };\n";
		} else {
			fs << "\tstatic constexpr StructTypeInfo typeInfo{ { OAK_TYPE_UID(" << typeName
				<< "), TypeInfoKind::STRUCT }, \"" << typeName
				<< "\", \"" << annotation
				<< "\", sizeof(T), alignof(T), &Reflect<NoType>::typeInfo, " << (hasFields ? "fields" : "{}")
				<< ", " << (hasMethods ? "methods" : "{}")
				<< ", " << (hasProperties ? "properties" : "{}")
				<< ", &detail::generic_construct<T> };\n";
		}
	}
	fs << "};\n";
}

void DeclConstexprSerializer::write_parsed_enum(clang::EnumDecl const *decl) {
	llvm::SmallString<64> annotation;
	get_annotation_string(annotation, decl);

	auto typeName = get_type_name(decl);

	fs << "template<> struct Reflect<" << typeName << "> {\n";
	fs << "\tusing T = " << typeName << ";\n";

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

	fs << "\tstatic constexpr EnumTypeInfo typeInfo{ { OAK_TYPE_UID(" << typeName
		<< "), TypeInfoKind::ENUM }, \"" << typeName
		<< "\", \"" << annotation
		<< "\", &Reflect<" << underlyingTypeName
		<< ">::typeInfo, " << (hasConstants ? "enumConstants" : "{}")
		<< " };\n";

	fs << "};\n";
}

void DeclConstexprSerializer::serialize(DeclFinder *finder) {
	llvm::SmallVector<clang::TagDecl const*, 4> sortedDecls;
	finder->get_sorted_decls(sortedDecls);
	for (auto decl : sortedDecls) {
		if (decl->isEnum()) {
			auto enumDecl = static_cast<clang::EnumDecl const*>(decl);
			write_parsed_enum(enumDecl);
			write_enum_type_list_item(enumDecl);
		} else {
			auto recordDecl = static_cast<clang::CXXRecordDecl const*>(decl);
			write_parsed_record(recordDecl);
			write_record_type_list_item(recordDecl);
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

	auto result = tool.run(clang::tooling::newFrontendActionFactory(&finder).get());

	return result;
}


