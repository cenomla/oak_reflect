
runtime_sources = [
  'type_info.cpp',
  'type_info_list.cpp',
]

runtime = library(
    'reflect_runtime',
    runtime_sources,
    gnu_symbol_visibility: 'hidden',
    include_directories: core_includes,
    dependencies: [ oak_util_dep ],
    install : true)

runtime_dep = declare_dependency(link_with: runtime, include_directories: core_includes)
