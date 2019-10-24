

gen_sources = [
  'gen.cpp',
]

gen = executable(
  'gen',
  sources : gen_sources,
  include_directories : core_includes,
  dependencies : clang_deps + [ oak_util_dep ],
  cpp_pch : 'pch/clang.h')
