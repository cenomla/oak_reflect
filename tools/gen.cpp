
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

namespace cltool {
	using namespace clang::ast_matchers;
	using namespace clang::tooling;
	using namespace clang::attr;
	using namespace clang;
	using namespace llvm;
}

namespace {
	llvm::cl::OptionCategory oakRttiCategory{ "oak_rtti options" };

	llvm::cl::extrahelp commonHelp{ clang::tooling::CommonOptionsParser::HelpMessage };

	llvm::cl::extrahelp oakRttiHelp{ "\nOak RTTI help\n" };

	struct CLIArgOutput {
		oak::String buildDir;
		oak::String outputFilename;
		oak::Vector<oak::String> inputFilenames;
	};

	struct CLIArgs {
		oak::Vector<CLIArgOutput> outputs;
	};

	CLIArgs parse_args(oak::Allocator *allocator, int argc, char const *const *argv) {
		CLIArgs result;
		oak::String buildDir;

		for (int i = 0; i < argc; ++i) {
			if (oak::find_slice(oak::String{ argv[i] }, oak::String{ "--build-dir" }) == 0) {
				++i;

				for (;i < argc && oak::find_slice(oak::String{ argv[i] }, oak::String{ "-" }) != 0;) {
					buildDir = argv[i];
					++i;
					break;
				}
			}
			if (oak::find_slice(oak::String{ argv[i] }, oak::String{ "--output" }) == 0) {
				CLIArgOutput output;
				output.buildDir = buildDir;
				++i;
				// First arg after this one is the output, after that a list of inputs
				for (; i < argc && oak::find_slice(oak::String{ argv[i] }, oak::String{ "-" }) != 0;) {
					output.outputFilename = argv[i];
					++i;
					break;
				}
				for (; i < argc && oak::find_slice(oak::String{ argv[i] }, oak::String{ "-" }) != 0; ++i) {
					output.inputFilenames.push(allocator, argv[i]);
				}
				result.outputs.push(allocator, output);
			}
		}

		return result;
	}

	cltool::CommonOptionsParser build_options_parser(oak::Allocator *allocator, CLIArgs *args, char const *programName) {
		// Pass in a list of all the inputs
		oak::Vector<char const*> inputs;
		inputs.push(allocator, programName);
		for (auto const& output : args->outputs) {
			for (auto const& ifn : output.inputFilenames) {
				inputs.push(allocator, ifn.data);
			}
		}
		oak::print_fmt("args: ");
		for (auto input : inputs) {
			oak::print_fmt("%g, ", input);
		}
		oak::print_fmt("\n");
		auto argc = static_cast<int>(inputs.count);
		return cltool::CommonOptionsParser{ argc, inputs.data, oakRttiCategory };
	}

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
	virtual void serialize(DeclFinder *finder) = 0;
	virtual ~DeclSerializer() {};
};

struct DeclFinder : public cltool::MatchFinder::MatchCallback {

	oak::Allocator *allocator = nullptr;

	cltool::ASTContext *context = nullptr;
	cltool::SourceManager *sourceManager = nullptr;

	DeclSerializer *serializer = nullptr;
	oak::Vector<cltool::TagDecl const*> decls;

	DeclFinder(oak::Allocator *allocator_, DeclSerializer *serializer_)
		: allocator{ allocator_ }, serializer{ serializer_ } {}

	void parse_record(cltool::CXXRecordDecl const *record) {
		auto isSpecialization = record->getTemplateSpecializationKind() != 0;
		auto sourceLoc = isSpecialization
			? static_cast<cltool::ClassTemplateSpecializationDecl const*>(record)->getPointOfInstantiation()
			: record->getOuterLocStart();
		auto isInMainFile = sourceManager->isWrittenInMainFile(sourceLoc);
		oak::print_fmt("decl: %g, dependent type: %g, specialization kind: %g\nmain file: %g, file: %g\n",
				record->getNameAsString(),
				record->isDependentType() ? "true" : "false",
				record->getTemplateSpecializationKind(),
				sourceManager->getFileEntryForID(sourceManager->getMainFileID())->getName(),
				sourceManager->getFileEntryForID(sourceManager->getFileID(sourceLoc))->getName()
				);

		if (!isInMainFile || record->isDependentType())
			return;

		add_decl(record);
	}

	void parse_enum(cltool::EnumDecl const *enumeration) {
		auto sourceLoc = enumeration->getOuterLocStart();
		auto isInMainFile = sourceManager->isWrittenInMainFile(sourceLoc);

		if (!isInMainFile)
			return;

		add_decl(enumeration);
	}

	void add_decl(cltool::TagDecl const *declToAdd) {
		decls.push(allocator, declToAdd);
	}

	void sort_decls() {
		decltype(decls) tmpDecls;

		bool swap;

		do {
			swap = false;
			tmpDecls.resize(&oak::temporaryMemory, decls.count);
			std::memcpy(tmpDecls.data, decls.data, sizeof(*decls.data) * decls.count);
			decls.count = 0;
			for (auto const& declToAdd : tmpDecls) {
				// Always insert decls before their topmost enclosing declaration and any structs whose fields reference this type or who inherit from this type
				i64 insertIndex = decls.count;
				for (i64 i = 0; i < decls.count; ++i) {
					auto const& decl = decls[i];
					if (decl->isRecord()) {
						auto recordDecl = static_cast<cltool::CXXRecordDecl const*>(decl);
						if (recordDecl->getTemplateSpecializationKind() != 0) {
							auto specialization = static_cast<cltool::ClassTemplateSpecializationDecl const*>(recordDecl);
							for (auto const& arg : specialization->getTemplateInstantiationArgs().asArray()) {
								auto kind = arg.getKind();
								switch (kind) {
									case clang::TemplateArgument::Type:
										{
											auto type = arg.getAsType().getTypePtr();
											if (type->isRecordType()) {
												if (declToAdd == type->getAsTagDecl()) {
													insertIndex = i;
												}
											} if (type->isPointerType()) {
												auto pointeeType = static_cast<clang::PointerType const*>(type)
													->getPointeeType().getTypePtr();
												if (pointeeType->isRecordType()) {
													if (declToAdd == pointeeType->getAsTagDecl()) {
														insertIndex = i;
													}
												}
											}
										} break;
									default:
										break;
								}
							}
						}
						for (auto const& field : recordDecl->fields()) {
							auto ft = field->getType().getTypePtr()->getUnqualifiedDesugaredType();
							auto fpt = ft->isPointerType() ? ft->getPointeeType()->getUnqualifiedDesugaredType() : nullptr;
							auto dt = declToAdd->getTypeForDecl()->getUnqualifiedDesugaredType();
							if (ft == dt || fpt == dt) {
								insertIndex = i;
								break;
							}
						}
						for (auto const& base : recordDecl->bases()) {
							auto bt = base.getType().getTypePtr()->getUnqualifiedDesugaredType();
							auto dt = declToAdd->getTypeForDecl()->getUnqualifiedDesugaredType();
							if (bt == dt) {
								insertIndex = i;
								break;
							}
						}
					}
					if (decl->Encloses(declToAdd)) {
						insertIndex = i;
					}
					if (insertIndex != decls.count) {
						break;
					}
				}
				if (insertIndex != decls.count) {
					swap = true;
				}
				decls.insert(allocator, declToAdd, insertIndex);
			}
		} while(swap);
	}

	void run(cltool::MatchFinder::MatchResult const& result) override {
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
		sort_decls();
		serializer->serialize(this);
		decls.clear();
	}

};

struct DeclConstexprSerializer : DeclSerializer {

	CLIArgOutput *output = nullptr;
	FILE *file = nullptr;

	oak::Slice<char> typeListString;
	oak::StringBuffer typeListStringBuffer;

	void init(CLIArgOutput *output_) {
		output = output_;
		if (!file) {
			file = std::fopen(oak::as_c_str(output->outputFilename), "wb");
		}
		write_header(output->buildDir, output->inputFilenames);

		typeListStringBuffer = oak::StringBuffer{ &oak::temporaryMemory, &typeListString, 0 };
		write_start_type_list();
	}

	void destroy() {
		write_end_type_list();
		write_type_list();
		write_footer();
		std::fclose(file);
	}

	oak::String get_specialization_name(cltool::CXXRecordDecl const* decl) {

		oak::Slice<char> specializationName;
		oak::StringBuffer sb{ &oak::temporaryMemory, &specializationName, 0 };
		oak::buffer_fmt(sb, "%g", decl->getName());

		if (decl->getTemplateSpecializationKind() != 0) {
			auto specialization = static_cast<cltool::ClassTemplateSpecializationDecl const*>(decl);
			oak::buffer_fmt(sb, "<");
			bool first = true;
			for (auto const& arg : specialization->getTemplateInstantiationArgs().asArray()) {
				if (!first) {
					// Add comma's to the template argument list
					oak::buffer_fmt(sb, ", ");
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
								oak::buffer_fmt(sb, "%g%g", get_namespace_name(recordDecl), get_specialization_name(recordDecl));
							} else if (type->isEnumeralType()) {
								auto enumDecl = static_cast<cltool::EnumType const*>(type)->getDecl();
								oak::buffer_fmt(sb, "%g%g", get_namespace_name(enumDecl), get_enum_name(enumDecl));
							} else if (type->isPointerType()) {
								auto pointeeQualType = static_cast<clang::PointerType const*>(type)->getPointeeType();
								auto pointeeType = pointeeQualType.getTypePtr();
								if (pointeeType->isRecordType()) {
									auto recordDecl = pointeeType->getAsCXXRecordDecl();
									oak::buffer_fmt(sb, "%g%g*", get_namespace_name(recordDecl), get_specialization_name(recordDecl));
								} else if (pointeeType->isEnumeralType()) {
									auto enumDecl = static_cast<cltool::EnumType const*>(type)->getDecl();
									oak::buffer_fmt(sb, "%g%g*", get_namespace_name(enumDecl), get_enum_name(enumDecl));
								} else {
									oak::buffer_fmt(sb, "%g*", qualType.getAsString());
								}
							} else {
								oak::buffer_fmt(sb, "%g", qualType.getAsString());
							}

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
		oak::String namespaceName;

		auto nsContext = decl->getParent();
		while (nsContext) {
			if (nsContext->isNamespace()) {
				namespaceName = oak::fmt(&oak::temporaryMemory, "%g::%g",
						static_cast<cltool::NamespaceDecl const*>(nsContext)->getNameAsString(),
						namespaceName);
			} else if (nsContext->isRecord()) {
				namespaceName = oak::fmt(&oak::temporaryMemory, "%g::%g",
						static_cast<cltool::RecordDecl const*>(nsContext)->getNameAsString(),
						namespaceName);
			}
			nsContext = nsContext->getLexicalParent();
		}

		return namespaceName;
	}

	void write_start_type_list() {
		auto listName = oak::sub_slice(
				output->outputFilename,
				oak::find_last_of(output->outputFilename, oak::String{ "/" }) + 1,
				oak::find_first_of(output->outputFilename, oak::String{ "." }));
		oak::buffer_fmt(typeListStringBuffer, "constexpr TypeInfo const* typeList_%g[] = {\n", listName);
	}

	void write_end_type_list() {
		oak::buffer_fmt(typeListStringBuffer, "};\n");
	}

	void write_record_type_list_item(cltool::CXXRecordDecl const* decl) {
		oak::buffer_fmt(typeListStringBuffer,
				"&Reflect<%g%g>::typeInfo,\n",
				get_namespace_name(decl),
				get_specialization_name(decl));
	}

	void write_enum_type_list_item(cltool::EnumDecl const* decl) {
		oak::buffer_fmt(typeListStringBuffer,
				"&Reflect<%g%g>::typeInfo,\n",
				get_namespace_name(decl),
				get_enum_name(decl));
	}

	void write_type_list() {
		oak::FileBuffer fb{ file };
		oak::buffer_fmt(fb, typeListString);
	}

	void write_header(oak::String buildDir, oak::Vector<oak::String> const& filenames) {
		oak::FileBuffer fb{ file };
		oak::buffer_fmt(fb, "#pragma once\n\n");

		oak::buffer_fmt(fb, "#include <oak_reflect/type_info.h>\n");

		for (auto const& fn : filenames) {
			auto separatorStr = oak::find_last_of(buildDir, oak::String{ "/" }) != buildDir.count - 1 ? "/" : "";
			oak::buffer_fmt(fb, "#include <%g%g%g>\n", buildDir, separatorStr, fn);
		}

		oak::buffer_fmt(fb, "\nnamespace oak {\n");
	}

	void write_footer() {
		oak::buffer_fmt(oak::FileBuffer{ file }, "}\n");
	}

	void write_forward_decl( cltool::CXXRecordDecl const *decl) {

		oak::FileBuffer fb{ file };
		auto specializationName = get_specialization_name(decl);
		auto namespaceName = get_namespace_name(decl);

		oak::buffer_fmt(fb, "template<> struct Reflect<%g%g>;\n", namespaceName, specializationName);
	}

	void write_parsed_record(cltool::CXXRecordDecl const *decl) {
		auto annotation = get_annotation_string(decl);

		oak::FileBuffer fb{ file };
		auto specializationName = get_specialization_name(decl);
		auto namespaceName = get_namespace_name(decl);

		oak::buffer_fmt(fb, "template<> struct Reflect<%g%g> {\n", namespaceName, specializationName);
		oak::buffer_fmt(fb, "\tusing T = %g%g;\n", namespaceName, specializationName);

		bool hasFields = false;
		for (auto const& field : decl->fields()) {
			auto fanno = get_annotation_string(field);
			if (oak::find_slice(fanno, oak::String{ "reflect" }) == 0) {
				hasFields = true;
				break;
			}
		}
		for (auto const& func : decl->methods()) {
			auto fanno = get_annotation_string(func);
			if (oak::find_slice(fanno, oak::String{ "reflect" }) == 0) {
				hasFields = true;
				break;
			}
		}
		for (auto const& base : decl->bases()) {
			auto bDecl = base.getType().getTypePtr()->getUnqualifiedDesugaredType()->getAsCXXRecordDecl();
			for (auto const& field : bDecl->fields()) {
				auto fanno = get_annotation_string(field);
				if (oak::find_slice(fanno, oak::String{ "reflect" }) == 0) {
					hasFields = true;
					break;
				}
			}
			for (auto const& func : bDecl->methods()) {
				auto fanno = get_annotation_string(func);
				if (oak::find_slice(fanno, oak::String{ "reflect" }) == 0) {
					hasFields = true;
					break;
				}
			}
		}
		if (hasFields) {
			oak::buffer_fmt(fb, "\tstatic constexpr FieldInfo fields[] = {\n");
			for (auto const& base : decl->bases()) {
				auto bDecl = base.getType().getTypePtr()->getUnqualifiedDesugaredType()->getAsCXXRecordDecl();
				for (auto const field : bDecl->fields()) {
					auto const& fname = field->getDeclName().getAsString();
					auto fanno = get_annotation_string(field);
					if (oak::find_slice(fanno, oak::String{ "reflect" }) != 0)
						continue;
					oak::buffer_fmt(
							oak::FileBuffer{ file },
							"\t\t{ \"%g\", \"%g\", &Reflect<decltype(T::%g)>::typeInfo, offsetof(T, %g)},\n",
							fname, fanno, fname, fname);
				}
				for (auto const func : bDecl->methods()) {
					auto const& fname = func->getNameInfo().getAsString();
					auto fanno = get_annotation_string(func);
					if (oak::find_slice(fanno, oak::String{ "reflect" }) != 0)
						continue;
					oak::buffer_fmt(
							oak::FileBuffer{ file },
							"\t\t{ \"%g\", \"%g\", &Reflect<decltype(&T::%g)>::typeInfo, 0},\n",
							fname, fanno, fname);
				}
			}
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
					", \"%g\", \"%g\", sizeof(T), alignof(T), %g, &detail::generic_construct<T> };\n",
					typeId,
					specializationName,
					annotation,
					hasFields ? "fields" : "{}");
		} else if (decl->isClass() || decl->isStruct()) {
			// Get the first base
			if (decl->getNumBases()) {
				auto base = decl->bases_begin()->getType().getTypePtr()->getAsCXXRecordDecl();
				auto baseName = get_specialization_name(base);
				auto baseNamespaceName = get_namespace_name(base);
				oak::buffer_fmt(fb,
						"\tstatic constexpr StructTypeInfo typeInfo{ { %gul, TypeInfoKind::STRUCT }"
						", \"%g\", \"%g\", sizeof(T), alignof(T), &Reflect<%g%g>::typeInfo, %g, &detail::generic_construct<T> };\n",
						typeId,
						specializationName,
						annotation,
						baseNamespaceName,
						baseName,
						hasFields ? "fields" : "{}");
			} else {
				oak::buffer_fmt(fb,
						"\tstatic constexpr StructTypeInfo typeInfo{ { %gul, TypeInfoKind::STRUCT }"
						", \"%g\", \"%g\", sizeof(T), alignof(T), &Reflect<NoType>::typeInfo, %g, &detail::generic_construct<T> };\n",
						typeId,
						specializationName,
						annotation,
						hasFields ? "fields" : "{}");
			}
		}
		oak::buffer_fmt(fb, "};\n");
	}

	void write_parsed_enum(cltool::EnumDecl const *decl) {
		auto annotation = get_annotation_string(decl);

		oak::FileBuffer fb{ file };

		auto enumName = get_enum_name(decl);
		auto namespaceName = get_namespace_name(decl);

		oak::buffer_fmt(fb, "template<> struct Reflect<%g%g> {\n", namespaceName, enumName);
		oak::buffer_fmt(fb, "\tusing T = %g%g;\n", namespaceName, enumName);

		bool hasConstants = decl->enumerator_begin() != decl->enumerator_end();
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

	void serialize(DeclFinder *finder) override {
		for (auto const& decl : finder->decls) {
			if (decl->isEnum()) {
				auto enumDecl = static_cast<cltool::EnumDecl const*>(decl);
				write_parsed_enum(enumDecl);
				write_enum_type_list_item(enumDecl);
			} else {
				auto recordDecl = static_cast<cltool::CXXRecordDecl const*>(decl);
				write_parsed_record(recordDecl);
				write_record_type_list_item(recordDecl);
			}
		}
	}
};

void ast_matcher(CLIArgs *args, cltool::ClangTool &tool) {
	DeclConstexprSerializer serializer;
	serializer.init(&args->outputs[0]);

	DeclFinder declFinder{ &oak::temporaryMemory, &serializer };
	cltool::MatchFinder finder;

	cltool::DeclarationMatcher classMatcher
		= cltool::cxxRecordDecl(cltool::decl().bind("id"), cltool::hasAttr(cltool::Annotate));
	cltool::DeclarationMatcher enumMatcher
		= cltool::enumDecl(cltool::decl().bind("id"), cltool::hasAttr(cltool::Annotate));

	finder.addMatcher(classMatcher, &declFinder);
	finder.addMatcher(enumMatcher, &declFinder);

	tool.run(cltool::newFrontendActionFactory(&finder).get());

	serializer.destroy();
}


cltool::CommandLineArguments reflectDefineAdjuster(cltool::CommandLineArguments const& args, cltool::StringRef filename) {
	auto result = args;
	result.emplace_back("-D__OSIG__");
	// TODO: This breaks on every compiler update and is not portable, fix it lulz, I have probably spent 8+ hours debugging
	// this line of code over the past two years
	result.emplace_back("-I/usr/lib/gcc/x86_64-pc-linux-gnu/9.3.0/include");
	return result;
}

int main(int argc, char const *const *argv) {

	oak::temporaryMemory = oak::globalAllocator;

	auto args = parse_args(&oak::temporaryMemory, argc, argv);
	auto op = build_options_parser(&oak::temporaryMemory, &args, argv[0]);

	auto sourceList = op.getSourcePathList();
	for (auto const& source : sourceList) {
		oak::print_fmt("source: %g\n", source);
	}

	cltool::ClangTool tool{ op.getCompilations(), op.getSourcePathList() };
	tool.appendArgumentsAdjuster(reflectDefineAdjuster);

	ast_matcher(&args, tool);

	return 0;
}
