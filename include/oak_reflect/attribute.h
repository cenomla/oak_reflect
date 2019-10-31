#pragma once

#ifdef __OSIG__
#define _reflect(...) __attribute__((annotate("reflect;" #__VA_ARGS__)))
#else
#define _reflect(...)
#endif
