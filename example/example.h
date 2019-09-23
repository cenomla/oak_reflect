#pragma once

#include <oak_reflect/attribute.h>


template<typename T>
struct REFLECT() TClassDef {
	REFLECT() T *data;
	REFLECT() long count;
};

template<int I>
struct REFLECT() IClassDef {
	REFLECT() float data[I];
	REFLECT() long count;
};


struct REFLECT(common, mark) ClassDef {
	REFLECT() int a;
	REFLECT() TClassDef<float> b;
	REFLECT() IClassDef<4> c;
	REFLECT() void func(int) {
	}
};

