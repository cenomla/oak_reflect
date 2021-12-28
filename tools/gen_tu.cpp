#include <string>

#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/CommandLine.h>
#include <clang/Driver/Options.h>

namespace {
	llvm::cl::OptionCategory genTuCategory{ "gen_tu options" };

	llvm::cl::opt<std::string> outputPathOption{
		"o",
		llvm::cl::desc{"output path"},
		llvm::cl::Required,
	};

	llvm::cl::opt<std::string> includePathPrefixOption{
		"includePrefix",
		llvm::cl::desc{"include path prefix"},
		llvm::cl::Required,
	};

	llvm::cl::list<std::string> inputPathsOption{
		llvm::cl::Positional,
		llvm::cl::desc{"input files"},
		llvm::cl::OneOrMore,
	};
}

int main(int argc, char const **argv) {
	llvm::cl::ParseCommandLineOptions(argc, argv);

	std::error_code EC;
	llvm::raw_fd_ostream out{ outputPathOption, EC, llvm::sys::fs::F_None };
	for (auto& input : inputPathsOption) {
		out << "#include \"" << includePathPrefixOption << input << "\"\n";
	}
}
