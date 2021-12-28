project('oak_rtti', ['cpp'],
  default_options : ['cpp_eh=none', 'cpp_rtti=false', 'cpp_std=c++17', 'warning_level=3'])

cxx = meson.get_compiler('cpp')

oak_util = subproject('oak_util')
oak_util_dep = oak_util.get_variable('oak_util_dep')

llvm_include_dir = join_paths(get_option('llvm_install_path'), 'include')
llvm_lib_dir = join_paths(get_option('llvm_install_path'), 'lib')

clang_includes = [ include_directories(llvm_include_dir, is_system: true) ]

if host_machine.system() == 'windows'
  sys_deps = [
	cxx.find_library('Version'),
  ]
else
  sys_deps = [
	cxx.find_library('pthread'),
	cxx.find_library('z'),
	cxx.find_library('ncurses'),
  ]
endif

llvm_deps = sys_deps + [
  cxx.find_library('LLVMBinaryFormat', dirs: llvm_lib_dir),
  cxx.find_library('LLVMBitstreamReader', dirs: llvm_lib_dir),
  cxx.find_library('LLVMCore', dirs: llvm_lib_dir),
  cxx.find_library('LLVMDemangle', dirs: llvm_lib_dir),
  cxx.find_library('LLVMFrontendOpenMP', dirs: llvm_lib_dir),
  cxx.find_library('LLVMMC', dirs: llvm_lib_dir),
  cxx.find_library('LLVMMCParser', dirs: llvm_lib_dir),
  cxx.find_library('LLVMOption', dirs: llvm_lib_dir),
  cxx.find_library('LLVMProfileData', dirs: llvm_lib_dir),
  cxx.find_library('LLVMRemarks', dirs: llvm_lib_dir),
  cxx.find_library('LLVMSupport', dirs: llvm_lib_dir),
]
clang_deps = [
  cxx.find_library('clangAnalysis', dirs: llvm_lib_dir),
  cxx.find_library('clangAST', dirs: llvm_lib_dir),
  cxx.find_library('clangASTMatchers', dirs: llvm_lib_dir),
  cxx.find_library('clangBasic', dirs: llvm_lib_dir),
  cxx.find_library('clangDriver', dirs: llvm_lib_dir),
  cxx.find_library('clangEdit', dirs: llvm_lib_dir),
  cxx.find_library('clangFrontend', dirs: llvm_lib_dir),
  cxx.find_library('clangLex', dirs: llvm_lib_dir),
  cxx.find_library('clangParse', dirs: llvm_lib_dir),
  cxx.find_library('clangSema', dirs: llvm_lib_dir),
  cxx.find_library('clangSerialization', dirs: llvm_lib_dir),
  cxx.find_library('clangTooling', dirs: llvm_lib_dir),
] + llvm_deps

core_includes = [ include_directories('include') ]

subdir('tools')
subdir('runtime')

if get_option('build_example')
  subdir('example')
endif

reflection_sources = files([
  'include/oak_reflect/any.h',
])
