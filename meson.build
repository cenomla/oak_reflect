project('oak_rtti', ['cpp'],
  default_options : ['cpp_eh=none', 'cpp_rtti=false', 'cpp_std=c++17', 'warning_level=3'])

cxx = meson.get_compiler('cpp')

oak_util = subproject('oak_util')

oak_util_dep = oak_util.get_variable('oak_util_dep')

if cxx.get_id() == 'msvc'
  clang_deps = [
    cxx.find_library('clang', dirs: 'C:\Program Files\LLVM\lib')
  ]
else
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
