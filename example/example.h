#pragma once

#include <oak_reflect/attribute.h>

namespace test {

template<typename T>
struct _reflect() TClassDef {
	_reflect(data) T *data;
	_reflect() long count;
};

struct Vec4{};

struct Vertex {
	_reflect(color, editable, volatile) Vec4 color;
};

template<int I>
struct _reflect() IClassDef {
	_reflect() float data[I];
	_reflect() long count;
};


struct _reflect() ClassBase {
	_reflect() int baseA;
};

struct _reflect() ClassSub : ClassBase {
	_reflect() int subA;
};


struct _reflect(common, mark) ClassDef {
	_reflect() int a;
	_reflect() TClassDef<float> b;
	_reflect() IClassDef<4> c;
	_reflect() void func(int) {
	}
};

}

enum class _reflect() Things {
	THING0,
	THING1,
	THING2,
	THING_ALPHA,
};

union _reflect() UnionDef {
	_reflect() int a;
	_reflect() float b;
};

struct _reflect() TaggedUnionDef {
	_reflect(tag) int tag;
	_reflect() union _reflect() Name {
		_reflect(active0) int a;
		_reflect(active1) float b;
		_reflect(active2) long c;
	} members;
};

struct _reflect() DeclWithPointer {
	_reflect() int *other;
};

