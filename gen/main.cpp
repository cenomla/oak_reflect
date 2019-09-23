
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
}

namespace oak {
	String to_str(Allocator *, std::string const& str, FmtKind) {
		return { str.data(), static_cast<i64>(str.size()) };
	}

	String to_str(Allocator *, cltool::StringRef const& str, FmtKind) {
		return { str.data(), static_cast<i64>(str.size()) };
	}

}

struct ParsedRecord {
	cltool::CXXRecordDecl const *record = nullptr;
	oak::Vector<cltool::FieldDecl const*> fields;
	oak::Vector<cltool::FunctionDecl const*> functions;
};

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
	oak::Vector<ParsedRecord> records;

	DeclFinder(oak::Allocator *allocator_, DeclSerializer *serializer_) : allocator{ allocator_ }, serializer{ serializer_ } {}

	void parse_record(cltool::CXXRecordDecl const *record) {
		oak::print_fmt("\n\nRecord: \n");
		record->dump();
		records.push(allocator, ParsedRecord{ record, {}, {} });
		auto fn = sourceManager->getFilename(record->getLocation());
		currentTranslationUnit = { fn.data(), static_cast<i64>(fn.size()) };
		oak::print_fmt("End Record\n\n");
	}

	void parse_field(cltool::FieldDecl const *field) {
		if (!records.count)
			return;
		records[records.count - 1].fields.push(allocator, field);
		field->dump();
	}

	void parse_function(cltool::FunctionDecl const *function) {
		if (!records.count)
			return;
		records[records.count - 1].functions.push(allocator, function);
		function->dump();
	}

	void run(const cltool::MatchFinder::MatchResult &result) override {
		context = result.Context;
		sourceManager = result.SourceManager;

		auto record = result.Nodes.getNodeAs<cltool::CXXRecordDecl>("id");
		if (record) {
			parse_record(record);
		}

		auto field = result.Nodes.getNodeAs<cltool::FieldDecl>("id");
		if (field) {
			parse_field(field);
		}

		auto function = result.Nodes.getNodeAs<cltool::FunctionDecl>("id");
		if (function) {
			parse_function(function);
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
		oak::buffer_fmt(sb, ".gen.h");
		serializer->serialize(this, currentTranslationUnit, filename);
	}

};

struct DeclConstexprSerializer : DeclSerializer {

	FILE *file = nullptr;

	void write_header(DeclFinder *, oak::String filename) {
		oak::FileBuffer fb{ file };
		oak::buffer_fmt(fb, "#pragma once\n\n");

		oak::buffer_fmt(fb, "#include <%g>\n", filename);

		oak::buffer_fmt(fb, "\nnamespace {\n");
	}

	void write_footer(DeclFinder *) {
		oak::buffer_fmt(oak::FileBuffer{ file }, "}\n");
	}

	void write_parsed_record(
			ParsedRecord const *parsedRecord,
			cltool::ClassTemplateSpecializationDecl* specialization) {
		if (parsedRecord->record->getTemplateSpecializationKind() != 0) {
			return;
		}

		oak::FileBuffer fb{ file };
		oak::Slice<char> specializationName;
		oak::StringBuffer sb{ &oak::temporaryMemory, &specializationName, 0 };

		oak::buffer_fmt(sb, "%g", parsedRecord->record->getName());
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
				}
			}
			oak::buffer_fmt(sb, ">");
		}

		oak::buffer_fmt(fb, "template<typename F>\n");
		oak::buffer_fmt(fb,
				"void reflect_field_visit(%g const& record, F&& functor) {\n",
				specializationName);

		for (auto const field : parsedRecord->fields) {
			auto const& fname = field->getDeclName().getAsString();
			oak::buffer_fmt(oak::FileBuffer{ file }, "\tfunctor(record, record.%g, \"%g\");\n", fname, fname);
		}
		oak::buffer_fmt(fb, "}\n");

		oak::buffer_fmt(fb, "\ntemplate<typename F>\n");
		oak::buffer_fmt(fb,
				"void reflect_function_visit(%g const& record, F&& functor) {\n",
				specializationName);
		for (auto const function : parsedRecord->functions) {
			auto const& fname = function->getNameInfo().getAsString();
			oak::buffer_fmt(fb, "\tfunctor(record, &%g::%g, \"%g\");\n", specializationName, fname, fname);
		}
		oak::buffer_fmt(fb, "}\n");
	}

	void serialize(DeclFinder *finder, oak::String inputFn, oak::String outputFn) override {
		file = std::fopen(oak::as_c_str(outputFn), "wb");
		write_header(finder, inputFn);
		for (auto const& parsedRecord : finder->records) {
			auto dct = parsedRecord.record->getDescribedClassTemplate();
			if (dct) {
				for (auto const& specialization : dct->specializations()) {
					write_parsed_record(&parsedRecord, specialization);
				}
			} else {
				write_parsed_record(&parsedRecord, nullptr);
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
	cltool::DeclarationMatcher propertyMatcher
		= cltool::fieldDecl(cltool::decl().bind("id"), cltool::hasAttr(cltool::Annotate));
	cltool::DeclarationMatcher functionMatcher
		= cltool::functionDecl(cltool::decl().bind("id"), cltool::hasAttr(cltool::Annotate));

	finder.addMatcher(classMatcher, &declFinder);
	finder.addMatcher(propertyMatcher, &declFinder);
	finder.addMatcher(functionMatcher, &declFinder);

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

