project('oak_rtti', ['cpp'],
  default_options : ['cpp_args=-fno-exceptions -fno-rtti', 'cpp_std=c++17', 'warning_level=3'])

cxx = meson.get_compiler('cpp')

oak_util = subproject('oak_util')

oak_util_dep = oak_util.get_variable('oak_util_dep')

clang_deps = [
  cxx.find_library('clangBasic'),
  cxx.find_library('clangAST'),
  cxx.find_library('clangASTMatchers'),
  cxx.find_library('clangFrontend'),
  cxx.find_library('clangTooling'),
  cxx.find_library('clangSerialization'),
  cxx.find_library('LLVM'),
]

core_includes = [ include_directories('include') ]

subdir('tools')
subdir('runtime')

if get_option('build_example')
  subdir('example')
endif

reflection_sources = files([
  'include/oak_reflect/any.h',
])
