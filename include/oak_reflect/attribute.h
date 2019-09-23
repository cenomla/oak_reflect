#pragma once

#ifdef __OSIG__
#define REFLECT(...) __attribute__((annotate("reflect;" #__VA_ARGS__)))
#else
#define REFLECT(...)
#endif
