#pragma once

#include <oak_reflect/attribute.h>

namespace test {

template<typename T>
struct REFLECT() TClassDef {
	REFLECT(data) T *data;
	REFLECT() long count;
};

struct Vec4{};

struct Vertex {
	REFLECT(color, editable, volatile) Vec4 color;
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

}

enum class REFLECT() Things {
	THING0,
	THING1,
	THING2,
	THING_ALPHA,
};

union REFLECT() UnionDef {
	REFLECT() int a;
	REFLECT() float b;
};
