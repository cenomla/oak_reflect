#pragma once

#include <oak_reflect/attribute.h>

template<typename T>
struct _reflect() TClassDef {
	_reflect(data) T *data;
	_reflect() long count;
};

