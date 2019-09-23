#pragma once

#include </home/cenomla/data/workspace/cpp/oak_reflect/example/example.h>

namespace {
template<typename F>
void reflect_field_visit(TClassDef<float> const& record, F&& functor) {
	functor(record, record.data, "data");
	functor(record, record.count, "count");
}

template<typename F>
void reflect_function_visit(TClassDef<float> const& record, F&& functor) {
}
template<typename F>
void reflect_field_visit(IClassDef<4> const& record, F&& functor) {
	functor(record, record.data, "data");
	functor(record, record.count, "count");
}

template<typename F>
void reflect_function_visit(IClassDef<4> const& record, F&& functor) {
}
template<typename F>
void reflect_field_visit(ClassDef const& record, F&& functor) {
	functor(record, record.a, "a");
	functor(record, record.b, "b");
	functor(record, record.c, "c");
}

template<typename F>
void reflect_function_visit(ClassDef const& record, F&& functor) {
	functor(record, &ClassDef::func, "func");
}
}
