

gen_sources = [
  'gen.cpp',
]

gen_tu_sources = [
  'gen_tu.cpp',
]

optimize_args = []
if get_option('optimize_tools')
  if host_machine.system() == 'windows'
    optimize_args = [ '/O2' ]
  else
    optimize_args = [ '-O3', '-march=native', '-flto' ]
  endif
endif

gen = executable(
  'gen',
  sources : gen_sources,
  include_directories : clang_includes,
  dependencies : clang_deps,
  cpp_args : optimize_args)

gen_tu = executable(
  'gen_tu',
  sources : gen_tu_sources,
  include_directories : clang_includes,
  dependencies : llvm_deps,
  cpp_args : optimize_args)

