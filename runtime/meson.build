
runtime_sources = [
  'type_info_list.cpp',
]

runtime = library('reflect_runtime', runtime_sources, include_directories: core_includes, dependencies: [ oak_util_dep ])

runtime_dep = declare_dependency(link_with: runtime, include_directories: core_includes)
