
#include <clang/Basic/AttrKinds.h>
#include <cstdio>
#include <sstream>

#include <clang/Driver/Options.h>
#include <clang/AST/AST.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/ASTMatchers/ASTMatchers.h>
#include <clang/ASTMatchers/ASTMatchFinder.h>
#include <clang/Frontend/ASTConsumers.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Rewrite/Core/Rewriter.h>

#include <oak_util/types.h>
#include <oak_util/containers.h>
#include <oak_util/memory.h>
#include <oak_util/fmt.h>

namespace {
	llvm::cl::OptionCategory oakRttiCategory{ "oak_rtti options" };

	llvm::cl::extrahelp commonHelp{ clang::tooling::CommonOptionsParser::HelpMessage };

	llvm::cl::extrahelp oakRttiHelp{ "\nOak RTTI help\n" };
}

namespace cltool {
	using namespace clang::ast_matchers;
	using namespace clang::tooling;
	using namespace clang::attr;
	using namespace clang;
	using namespace llvm;
}

namespace oak {
	String to_str(Allocator *, std::string const& str, FmtKind) {
		return { str.data(), static_cast<i64>(str.size()) };
	}

	String to_str(Allocator *, cltool::StringRef const& str, FmtKind) {
		return { str.data(), static_cast<i64>(str.size()) };
	}

}

oak::String get_annotation_string(cltool::Decl const *decl) {
	oak::Slice<char> annotation;
	oak::StringBuffer sbA{ &oak::temporaryMemory, &annotation, 0 };
	for (auto const& attr : decl->attrs()) {
		if (attr->getKind() == clang::attr::Annotate) {
			cltool::SmallString<32> str;
			cltool::raw_svector_ostream os{ str };
			cltool::LangOptions langOpts;
			cltool::PrintingPolicy policy{ langOpts };
			attr->printPretty(os, policy);
			oak::buffer_fmt(sbA, "%g", oak::sub_slice(oak::String{ str.c_str() }, 26, str.size() - 4));
			break;
		}
	}
	return annotation;
}

struct DeclFinder;

struct DeclSerializer {
	virtual void serialize(DeclFinder *finder, oak::String inputFn, oak::String outputFn) = 0;
	virtual ~DeclSerializer() {};
};

struct DeclFinder : public cltool::MatchFinder::MatchCallback {

	oak::Allocator *allocator = nullptr;

	cltool::ASTContext *context = nullptr;
	cltool::SourceManager *sourceManager = nullptr;

	DeclSerializer *serializer = nullptr;
	oak::String currentTranslationUnit;
	oak::Vector<cltool::TagDecl const*> decls;

	DeclFinder(oak::Allocator *allocator_, DeclSerializer *serializer_) : allocator{ allocator_ }, serializer{ serializer_ } {}

	void parse_record(cltool::CXXRecordDecl const *record) {
		oak::print_fmt("\n\nRecord: \n");
		record->dump();

		// Always insert records before their topmost enclosing declaration
		i64 insertIndex = decls.count;
		for (i64 i = 0; i < decls.count; ++i) {
			auto const& decl = decls[i];
			if (decl->Encloses(record)) {
				insertIndex = i;
				oak::print_fmt("\n\nEnclosed by: %g\n", decl->getDeclName().getAsString());
				break;
			}
		}
		decls.insert(allocator, record, insertIndex);

		auto fn = sourceManager->getFilename(record->getLocation());
		currentTranslationUnit = { fn.data(), static_cast<i64>(fn.size()) };
		oak::print_fmt("End Record\n\n");
	}

	void parse_enum(cltool::EnumDecl const *enumeration) {
		decls.push(allocator, enumeration);
		auto fn = sourceManager->getFilename(enumeration->getLocation());
		currentTranslationUnit = { fn.data(), static_cast<i64>(fn.size()) };
	}

	void run(const cltool::MatchFinder::MatchResult &result) override {
		context = result.Context;
		sourceManager = result.SourceManager;

		auto record = result.Nodes.getNodeAs<cltool::CXXRecordDecl>("id");
		if (record) {
			parse_record(record);
		}

		auto enumeration = result.Nodes.getNodeAs<cltool::EnumDecl>("id");
		if (enumeration) {
			parse_enum(enumeration);
		}

	}

	void onStartOfTranslationUnit() override {
	}

	void onEndOfTranslationUnit() override {
		oak::Slice<char> filename;
		oak::StringBuffer sb{ &oak::temporaryMemory, &filename, 0 };
		oak::buffer_fmt(sb, "%g",
				oak::sub_slice(
					currentTranslationUnit,
					0,
					oak::find_last_of(currentTranslationUnit, oak::String{"."})));
		oak::buffer_fmt(sb, ".reflect.h");

		serializer->serialize(this, currentTranslationUnit, filename);

		decls.clear();
	}

};

struct DeclConstexprSerializer : DeclSerializer {

	FILE *file = nullptr;

	oak::String get_specialization_name(
			cltool::RecordDecl const* decl,
			cltool::ClassTemplateSpecializationDecl const* specialization) {

		oak::Slice<char> specializationName;
		oak::StringBuffer sb{ &oak::temporaryMemory, &specializationName, 0 };
		oak::buffer_fmt(sb, "%g", decl->getName());

		if (specialization) {
			oak::buffer_fmt(sb, "<");
			for (auto const& arg : specialization->getTemplateInstantiationArgs().asArray()) {
				auto kind = arg.getKind();
				switch (kind) {
					case clang::TemplateArgument::Type:
						{
							auto const& type = arg.getAsType();
							oak::buffer_fmt(sb, "%g", type.getAsString());

						} break;
					case clang::TemplateArgument::Integral:
						{
							auto const& integral = arg.getAsIntegral();
							oak::buffer_fmt(sb, "%g", integral.toString(10));
						} break;
					default:
						assert(false && "Template argument type unsupported");
						break;
				}
			}
			oak::buffer_fmt(sb, ">");
		}

		return specializationName;
	}

	oak::String get_enum_name(cltool::EnumDecl const* decl) {
		oak::Slice<char> enumName;
		oak::StringBuffer sb{ &oak::temporaryMemory, &enumName, 0 };
		oak::buffer_fmt(sb, "%g", decl->getName());

		return enumName;
	}

	oak::String get_namespace_name(cltool::TagDecl const* decl) {
		oak::Slice<char> namespaceName;
		oak::StringBuffer sb{ &oak::temporaryMemory, &namespaceName, 0 };

		auto nsContext = decl->getParent();
		while (nsContext) {
			if (nsContext->isNamespace()) {
				oak::buffer_fmt(sb, "::%g", static_cast<cltool::NamespaceDecl const*>(nsContext)->getNameAsString());
			} else if (nsContext->isRecord()) {
				oak::buffer_fmt(sb, "::%g", static_cast<cltool::RecordDecl const*>(nsContext)->getNameAsString());
			}
			nsContext = nsContext->getLexicalParent();
		}

		return namespaceName;
	}

	void write_header(DeclFinder *, oak::String filename) {
		oak::FileBuffer fb{ file };
		oak::buffer_fmt(fb, "#pragma once\n\n");

		oak::buffer_fmt(fb, "#include <oak_reflect/type_info.h>\n");
		oak::buffer_fmt(fb, "#include <%g>\n", filename);

		oak::buffer_fmt(fb, "\nnamespace oak {\n");
	}

	void write_footer(DeclFinder *) {
		oak::buffer_fmt(oak::FileBuffer{ file }, "}\n");
	}

	void write_forward_decl(
			cltool::CXXRecordDecl const *decl,
			cltool::ClassTemplateSpecializationDecl* specialization
			) {

		oak::FileBuffer fb{ file };
		auto specializationName = get_specialization_name(decl, specialization);
		auto namespaceName = get_namespace_name(decl);

		oak::buffer_fmt(fb, "template<> struct Reflect<%g::%g>;\n", namespaceName, specializationName);
	}

	void write_parsed_record(
			cltool::CXXRecordDecl const *decl,
			cltool::ClassTemplateSpecializationDecl* specialization) {
		auto annotation = get_annotation_string(decl);

		oak::FileBuffer fb{ file };
		auto specializationName = get_specialization_name(decl, specialization);
		auto namespaceName = get_namespace_name(decl);

		oak::buffer_fmt(fb, "template<> struct Reflect<%g::%g> {\n", namespaceName, specializationName);
		oak::buffer_fmt(fb, "\tusing T = %g::%g;\n", namespaceName, specializationName);

		bool hasFields = !decl->field_empty();
		if (hasFields) {
			oak::buffer_fmt(fb, "\tstatic constexpr FieldInfo fields[] = {\n");
			for (auto const field : decl->fields()) {
				auto const& fname = field->getDeclName().getAsString();
				auto fanno = get_annotation_string(field);
				if (oak::find_slice(fanno, oak::String{ "reflect" }) != 0)
					continue;
				oak::buffer_fmt(
						oak::FileBuffer{ file },
						"\t\t{ \"%g\", \"%g\", &Reflect<decltype(T::%g)>::typeInfo, offsetof(T, %g)},\n",
						fname, fanno, fname, fname);
			}
			for (auto const func : decl->methods()) {
				auto const& fname = func->getNameInfo().getAsString();
				auto fanno = get_annotation_string(func);
				if (oak::find_slice(fanno, oak::String{ "reflect" }) != 0)
					continue;
				oak::buffer_fmt(
						oak::FileBuffer{ file },
						"\t\t{ \"%g\", \"%g\", &Reflect<decltype(&T::%g)>::typeInfo, 0},\n",
						fname, fanno, fname);
			}
			oak::buffer_fmt(fb, "\t};\n");
		}

		auto typeId = oak::HashFn<oak::String>{}(specializationName);

		if (decl->isUnion()) {
			oak::buffer_fmt(fb,
					"\tstatic constexpr UnionTypeInfo typeInfo{ { %gul, TypeInfoKind::UNION }"
					", \"%g\", \"%g\", sizeof(T), alignof(T), %g };\n",
					typeId,
					specializationName,
					annotation,
					hasFields ? "fields" : "{}");
		} else if (decl->isClass() || decl->isStruct()) {
			oak::buffer_fmt(fb,
					"\tstatic constexpr StructTypeInfo typeInfo{ { %gul, TypeInfoKind::STRUCT }"
					", \"%g\", \"%g\", sizeof(T), alignof(T), %g };\n",
					typeId,
					specializationName,
					annotation,
					hasFields ? "fields" : "{}");
		}
		oak::buffer_fmt(fb, "};\n");
	}

	void write_parsed_enum(cltool::EnumDecl const *decl) {
		auto annotation = get_annotation_string(decl);

		oak::FileBuffer fb{ file };

		auto enumName = get_enum_name(decl);
		auto namespaceName = get_namespace_name(decl);

		oak::buffer_fmt(fb, "template<> struct Reflect<%g::%g> {\n", namespaceName, enumName);
		oak::buffer_fmt(fb, "\tusing T = %g::%g;\n", namespaceName, enumName);

		bool hasConstants;
		{
			auto begin = decl->enumerator_begin();
			auto end = decl->enumerator_end();
			hasConstants = begin != end;
		}
		if (hasConstants) {
			oak::buffer_fmt(fb, "\tstatic constexpr EnumConstantInfo enumConstants[] = {\n");
			for (auto const& enumConstant : decl->enumerators()) {
				auto const& ename = enumConstant->getDeclName().getAsString();
				auto evalue = static_cast<u64>(enumConstant->getInitVal().getExtValue());
				oak::buffer_fmt(
						fb,
						"\t\t{ \"%g\", %g },\n",
						ename, evalue
						);
			}
			oak::buffer_fmt(fb, "\t};\n");
		}

		auto const& underlyingTypeName = decl->getIntegerType().getAsString();

		auto typeId = oak::HashFn<oak::String>{}(enumName);

		oak::buffer_fmt(fb,
				"\tstatic constexpr EnumTypeInfo typeInfo{ { %gul, TypeInfoKind::ENUM }"
				", \"%g\", \"%g\", &Reflect<%g>::typeInfo, %g };\n",
				typeId,
				enumName,
				annotation,
				underlyingTypeName,
				hasConstants ? "enumConstants" : "{}");


		// End of struct
		oak::buffer_fmt(fb, "};\n");

	}

	void serialize(DeclFinder *finder, oak::String inputFn, oak::String outputFn) override {
		file = std::fopen(oak::as_c_str(outputFn), "wb");
		write_header(finder, inputFn);

		for (auto const& decl : finder->decls) {
			if (decl->isEnum()) {
				write_parsed_enum(static_cast<cltool::EnumDecl const*>(decl));
			} else {
				auto record = static_cast<cltool::CXXRecordDecl const*>(decl);
				if (record->getTemplateSpecializationKind() != 0) {
					continue;
				}
				auto dct = record->getDescribedClassTemplate();
				if (dct) {
					for (auto const& specialization : dct->specializations()) {
						write_parsed_record(record, specialization);
					}
				} else {
					write_parsed_record(record, nullptr);
				}
			}
		}
		write_footer(finder);
		std::fclose(file);

	}
};

void ast_matcher(cltool::ClangTool &tool) {
	DeclConstexprSerializer serializer;
	DeclFinder declFinder{ &oak::temporaryMemory, &serializer };
	cltool::MatchFinder finder;

	cltool::DeclarationMatcher classMatcher
		= cltool::cxxRecordDecl(cltool::decl().bind("id"), cltool::hasAttr(cltool::Annotate));
	cltool::DeclarationMatcher enumMatcher
		= cltool::enumDecl(cltool::decl().bind("id"), cltool::hasAttr(cltool::Annotate));

	finder.addMatcher(classMatcher, &declFinder);
	finder.addMatcher(enumMatcher, &declFinder);

	tool.run(cltool::newFrontendActionFactory(&finder).get());
}


cltool::CommandLineArguments reflectDefineAdjuster(cltool::CommandLineArguments const& args, cltool::StringRef filename) {
	auto result = args;
	result.emplace_back("-D__OSIG__");
	return result;
}

int main(int argc, char const **argv) {

	oak::MemoryArena tempArena;
	oak::init_linear_arena(&tempArena, &oak::globalAllocator, 16 * 1024 * 1024);

	oak::temporaryMemory = { &tempArena, oak::allocate_from_linear_arena, nullptr };

	cltool::CommonOptionsParser op{ argc, argv, oakRttiCategory };

	auto sourceList = op.getSourcePathList();

	for (auto const& source : sourceList) {
		oak::print_fmt("source: %g\n", source);
	}

	cltool::ClangTool tool{ op.getCompilations(), op.getSourcePathList() };
	tool.appendArgumentsAdjuster(reflectDefineAdjuster);

	//auto result = tool.run(cltool::newFrontendActionFactory<OakRttiFrontendAction>().get());
	ast_matcher(tool);

	return 0;
}
