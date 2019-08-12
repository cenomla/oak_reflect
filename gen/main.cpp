#include <clang/Driver/Options.h>
#include <clang/AST/AST.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/Frontend/ASTConsumers.h>
#include <clang/Frontend/FrontendActions.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/CommonOptionsParser.h>
#include <clang/Tooling/Tooling.h>
#include <clang/Rewrite/Core/Rewriter.h>

#include <oak_util/types.h>
#include <oak_util/memory.h>
#include <oak_util/fmt.h>

namespace {
	llvm::cl::OptionCategory oakRttiCategory{ "oak_rtti options" };

	llvm::cl::extrahelp commonHelp{ clang::tooling::CommonOptionsParser::HelpMessage };

	llvm::cl::extrahelp oakRttiHelp{ "\nOak RTTI help\n" };
}

class OakRttiVisitor : public clang::RecursiveASTVisitor<OakRttiVisitor> {
	public:
		bool VisitCXXRecordDecl(clang::CXXRecordDecl *declaration) {
			declaration->dump();
			oak::print_fmt("decl has move assignment operator: %\n", declaration->hasMoveAssignment() ? "yaw" : "neah");

			// Continue with visitation?
			return true;
		}
};

class OakRttiConsumer : public clang::ASTConsumer {
	public:
		virtual void HandleTranslationUnit(clang::ASTContext &context) override {
			oak::print_fmt("handle translation unit\n");
			visitor.TraverseDecl(context.getTranslationUnitDecl());
		}
	private:
		OakRttiVisitor visitor;
};

class OakRttiFrontendAction : public clang::ASTFrontendAction {
	public:
		virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
				clang::CompilerInstance &/*compiler*/, llvm::StringRef /*inFile*/) override {
			oak::print_fmt("frontend action start\n");
			return std::make_unique<OakRttiConsumer>();
		}
};

namespace oak {
	String to_str(std::string const& str, FmtKind) {
		return { str.data(), static_cast<i64>(str.size()) };
	}

}

int main(int argc, char const **argv) {

	oak::MemoryArena tempArena;
	oak::init_linear_arena(&tempArena, &oak::globalAllocator, 16 * 1024 * 1024);

	oak::temporaryMemory = { &tempArena, oak::allocate_from_linear_arena, nullptr };

	clang::tooling::CommonOptionsParser op{ argc, argv, oakRttiCategory };

	auto sourceList = op.getSourcePathList();

	for (auto const& source : sourceList) {
		oak::print_fmt("source: %\n", source);
	}

	clang::tooling::ClangTool tool{ op.getCompilations(), op.getSourcePathList() };

	auto result = tool.run(clang::tooling::newFrontendActionFactory<OakRttiFrontendAction>().get());

	return result;
}
