### Design constraints
 - Needs to be optimized for runtime above all else
 - Data compactness is a high priority aswell (compact data = less cache usage = better performance impact on surrounding code)
 - Ease of build integration
 - Don't over-bloat compile times
 - Robust data integration
  - Pointer or integer identity
  - Compile time availability (templates? and constexpr)
  - Type info database files for runtime loading of information
  - API's that don't rely on method of integration
  - Zero heap allocation for constexpr available type info
 - Ability to represent the object model of c++ to a high degree without going overboard on stupid c++isms 

### TODO:
 - [x] Generation of struct type info
 - [x] Generation of union type info
 - [x] Generation of enum type info
 - [x] Generation of templated struct type info
 - [x] Generation of function type info
 - [x] Generation of member function type info
 - [ ] Generation of type lists
 - [ ] Generation of function lists
 - [x] Serialize type info to constexpr code
 - [x] Optimized runtime library
