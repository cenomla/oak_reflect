project('oak_rtti', ['cpp'],
  default_options : ['cpp_eh=none', 'cpp_rtti=false', 'cpp_std=c++17', 'warning_level=3'])

cxx = meson.get_compiler('cpp')

oak_util = subproject('oak_util')

oak_util_dep = oak_util.get_variable('oak_util_dep')

if host_machine.system() == 'windows'
  llvm_include_dir = join_paths(get_option('llvm_install_dir'), 'include')
  llvm_lib_dir = join_paths(get_option('llvm_install_dir'), 'lib')

  clang_includes = [ include_directories(llvm_include_dir, is_system: true) ]
  clang_deps = [
    cxx.find_library('clangEdit', dirs: llvm_lib_dir),
    cxx.find_library('clangAnalysis', dirs: llvm_lib_dir),
    cxx.find_library('clangAST', dirs: llvm_lib_dir),
    cxx.find_library('clangASTMatchers', dirs: llvm_lib_dir),
    cxx.find_library('clangBasic', dirs: llvm_lib_dir),
    cxx.find_library('clangDriver', dirs: llvm_lib_dir),
    cxx.find_library('clangFrontend', dirs: llvm_lib_dir),
    cxx.find_library('clangLex', dirs: llvm_lib_dir),
    cxx.find_library('clangParse', dirs: llvm_lib_dir),
    cxx.find_library('clangSema', dirs: llvm_lib_dir),
    cxx.find_library('clangSerialization', dirs: llvm_lib_dir),
    cxx.find_library('clangTooling', dirs: llvm_lib_dir),
    cxx.find_library('LLVMSupport', dirs: llvm_lib_dir),
    cxx.find_library('LLVMCore', dirs: llvm_lib_dir),
    cxx.find_library('LLVMRemarks', dirs: llvm_lib_dir),
    cxx.find_library('LLVMOption', dirs: llvm_lib_dir),
    cxx.find_library('LLVMBinaryFormat', dirs: llvm_lib_dir),
    cxx.find_library('LLVMBitstreamReader', dirs: llvm_lib_dir),
    cxx.find_library('LLVMMC', dirs: llvm_lib_dir),
    cxx.find_library('LLVMMCParser', dirs: llvm_lib_dir),
    cxx.find_library('LLVMProfileData', dirs: llvm_lib_dir),
    cxx.find_library('LLVMFrontendOpenMP', dirs: llvm_lib_dir),
    cxx.find_library('LLVMDemangle', dirs: llvm_lib_dir),
    cxx.find_library('Version'),
  ]
else
  clang_includes = []
  clang_deps = [
    cxx.find_library('clang-cpp'),
    cxx.find_library('LLVM'),
  ]
endif


core_includes = [ include_directories('include') ]

subdir('tools')
subdir('runtime')

if get_option('build_example')
  subdir('example')
endif

reflection_sources = files([
  'include/oak_reflect/any.h',
])
