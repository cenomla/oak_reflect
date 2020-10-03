project('oak_rtti', ['cpp'],
  default_options : ['cpp_eh=none', 'cpp_rtti=false', 'cpp_std=c++17', 'warning_level=3'])

cxx = meson.get_compiler('cpp')

oak_util = subproject('oak_util')

oak_util_dep = oak_util.get_variable('oak_util_dep')

if host_machine.system() == 'windows'
  clang_includes = [ include_directories('C:\Program Files (x86)\LLVM\include', is_system: true) ]
  clang_deps = [
    cxx.find_library('clangEdit', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangAnalysis', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangAST', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangASTMatchers', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangBasic', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangDriver', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangFrontend', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangLex', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangParse', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangSema', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangSerialization', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('clangTooling', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMSupport', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMCore', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMRemarks', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMOption', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMBinaryFormat', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMBitstreamReader', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMMC', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMMCParser', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMProfileData', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMFrontendOpenMP', dirs: 'C:\Program Files (x86)\LLVM\lib'),
    cxx.find_library('LLVMDemangle', dirs: 'C:\Program Files (x86)\LLVM\lib'),
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
