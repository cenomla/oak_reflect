

gen_sources = [
  'gen.cpp',
]

optimize_args = []
if get_option('optimize_tools')
  optimize_args = [ '-O3', '-march=native' ]
endif

gen = executable(
  'gen',
  sources : gen_sources,
  include_directories : core_includes,
  dependencies : clang_deps + [ oak_util_dep ],
  cpp_args : optimize_args,
  cpp_pch : 'pch/clang.h')
