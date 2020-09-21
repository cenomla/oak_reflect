

gen_sources = [
  'gen.cpp',
]

optimize_args = []
if get_option('optimize_tools')
  if host_machine.system() == 'windows'
    optimize_args = [ '/O2' ]
  else 
    optimize_args = [ '-O3', '-march=native' ]
  endif
endif

gen = executable(
  'gen',
  sources : gen_sources,
  include_directories : core_includes + clang_includes,
  dependencies : clang_deps + [ oak_util_dep ],
  cpp_args : optimize_args,
  cpp_pch : 'pch/clang.h')
