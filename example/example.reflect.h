#pragma once

#include <oak_reflect/type_info.h>
#include </home/cenomla/data/workspace/cpp/oak_reflect/example/example.h>

namespace oak {
template<> struct Reflect<::test::TClassDef<float>> {
	using T = ::test::TClassDef<float>;
	static constexpr FieldInfo fields[] = {
		{ "data", "reflect;data", &Reflect<decltype(T::data)>::typeInfo, offsetof(T, data)},
		{ "count", "reflect;", &Reflect<decltype(T::count)>::typeInfo, offsetof(T, count)},
	};
	static constexpr StructTypeInfo typeInfo{ { 13920250912624137118ul, TypeInfoKind::STRUCT }, "TClassDef<float>", "reflect;", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::test::IClassDef<4>> {
	using T = ::test::IClassDef<4>;
	static constexpr FieldInfo fields[] = {
		{ "data", "reflect;", &Reflect<decltype(T::data)>::typeInfo, offsetof(T, data)},
		{ "count", "reflect;", &Reflect<decltype(T::count)>::typeInfo, offsetof(T, count)},
	};
	static constexpr StructTypeInfo typeInfo{ { 13041824888433916529ul, TypeInfoKind::STRUCT }, "IClassDef<4>", "reflect;", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::test::ClassBase> {
	using T = ::test::ClassBase;
	static constexpr FieldInfo fields[] = {
		{ "baseA", "reflect;", &Reflect<decltype(T::baseA)>::typeInfo, offsetof(T, baseA)},
	};
	static constexpr StructTypeInfo typeInfo{ { 15664491875355541578ul, TypeInfoKind::STRUCT }, "ClassBase", "reflect;", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::test::ClassSub> {
	using T = ::test::ClassSub;
	static constexpr FieldInfo fields[] = {
		{ "subA", "reflect;", &Reflect<decltype(T::subA)>::typeInfo, offsetof(T, subA)},
	};
	static constexpr StructTypeInfo typeInfo{ { 17624178121959904137ul, TypeInfoKind::STRUCT }, "ClassSub", "reflect;", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::test::ClassDef> {
	using T = ::test::ClassDef;
	static constexpr FieldInfo fields[] = {
		{ "a", "reflect;", &Reflect<decltype(T::a)>::typeInfo, offsetof(T, a)},
		{ "b", "reflect;", &Reflect<decltype(T::b)>::typeInfo, offsetof(T, b)},
		{ "c", "reflect;", &Reflect<decltype(T::c)>::typeInfo, offsetof(T, c)},
		{ "func", "reflect;", &Reflect<decltype(&T::func)>::typeInfo, 0},
	};
	static constexpr StructTypeInfo typeInfo{ { 17624178057410422542ul, TypeInfoKind::STRUCT }, "ClassDef", "reflect;common, mark", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::Things> {
	using T = ::Things;
	static constexpr EnumConstantInfo enumConstants[] = {
		{ "THING0", 0 },
		{ "THING1", 1 },
		{ "THING2", 2 },
		{ "THING_ALPHA", 3 },
	};
	static constexpr EnumTypeInfo typeInfo{ { 5962092804371112614ul, TypeInfoKind::ENUM }, "Things", "reflect;", &Reflect<int>::typeInfo, enumConstants };
};
template<> struct Reflect<::UnionDef> {
	using T = ::UnionDef;
	static constexpr FieldInfo fields[] = {
		{ "a", "reflect;", &Reflect<decltype(T::a)>::typeInfo, offsetof(T, a)},
		{ "b", "reflect;", &Reflect<decltype(T::b)>::typeInfo, offsetof(T, b)},
	};
	static constexpr UnionTypeInfo typeInfo{ { 12676661488884284855ul, TypeInfoKind::UNION }, "UnionDef", "reflect;", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::TaggedUnionDef::Name> {
	using T = ::TaggedUnionDef::Name;
	static constexpr FieldInfo fields[] = {
		{ "a", "reflect;active0", &Reflect<decltype(T::a)>::typeInfo, offsetof(T, a)},
		{ "b", "reflect;active1", &Reflect<decltype(T::b)>::typeInfo, offsetof(T, b)},
		{ "c", "reflect;active2", &Reflect<decltype(T::c)>::typeInfo, offsetof(T, c)},
	};
	static constexpr UnionTypeInfo typeInfo{ { 22018842897454412ul, TypeInfoKind::UNION }, "Name", "reflect;", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::TaggedUnionDef> {
	using T = ::TaggedUnionDef;
	static constexpr FieldInfo fields[] = {
		{ "tag", "reflect;tag", &Reflect<decltype(T::tag)>::typeInfo, offsetof(T, tag)},
		{ "members", "reflect;", &Reflect<decltype(T::members)>::typeInfo, offsetof(T, members)},
	};
	static constexpr StructTypeInfo typeInfo{ { 12210655553202725059ul, TypeInfoKind::STRUCT }, "TaggedUnionDef", "reflect;", sizeof(T), alignof(T), fields };
};
template<> struct Reflect<::DeclWithPointer> {
	using T = ::DeclWithPointer;
	static constexpr FieldInfo fields[] = {
		{ "other", "reflect;", &Reflect<decltype(T::other)>::typeInfo, offsetof(T, other)},
	};
	static constexpr StructTypeInfo typeInfo{ { 7251760669828589230ul, TypeInfoKind::STRUCT }, "DeclWithPointer", "reflect;", sizeof(T), alignof(T), fields };
};
}
