#pragma once

#include <type_traits>

#include <oak_util/types.h>

namespace oak {

	enum class TypeInfoKind {
		NONE,
		PRIMITIVE,
		FUNCTION,
		POINTER,
		ARRAY,
		STRUCT,
		ENUM,
		UNION,
	};

	struct TypeInfo {
		u64 uid;
		TypeInfoKind kind;
	};

	struct PrimitiveTypeInfo : TypeInfo {
		String name;
		i64 size;
		i64 align;
	};

	struct PointerTypeInfo : TypeInfo {
		TypeInfo const *to;
	};

	struct ArrayTypeInfo : TypeInfo {
		TypeInfo const *of;
		i64 count;
	};

	struct FieldInfo {
		String name;
		String annotation;
		TypeInfo const *typeInfo;
		i64 offset;
	};

	struct StructTypeInfo : TypeInfo {
		String name;
		String annotation;
		i64 size;
		i64 align;
		Slice<FieldInfo const> fields;
	};

	struct EnumConstantInfo {
		String name;
		u64 value;
	};

	struct EnumTypeInfo : TypeInfo {
		String name;
		String annotation;
		TypeInfo const *underlyingType = nullptr;
		Slice<EnumConstantInfo const> constants;
	};

	struct UnionTypeInfo : TypeInfo {
		String name;
		String annotation;
		i64 size;
		i64 align;
		Slice<FieldInfo const> fields;
	};

	struct FunctionTypeInfo : TypeInfo {
		String name;
		String annotation;
		Slice<TypeInfo const*> argTypeInfos;
		TypeInfo const* returnTypeInfo;
	};

	constexpr TypeInfo noTypeInfo{ 0, TypeInfoKind::NONE };

	template<typename T, typename Enable = std::true_type>
	struct Reflect {
		static constexpr TypeInfo typeInfo = noTypeInfo;
	};

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_pointer_v<T>>> {
		static constexpr PointerTypeInfo typeInfo{ { 1, TypeInfoKind::POINTER }, &Reflect<std::remove_pointer<T>>::typeInfo };
	};

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_array_v<T>>> {
		static constexpr ArrayTypeInfo typeInfo{ { 1, TypeInfoKind::ARRAY }, &Reflect<std::remove_extent_t<T>>::typeInfo };
	};

	template<> struct Reflect<i8> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "int8", sizeof(i8), alignof(i8) };
	};

	template<> struct Reflect<i16> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "int16", sizeof(i16), alignof(i16) };
	};

	template<> struct Reflect<i32> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "int32", sizeof(i32), alignof(i32) };
	};

	template<> struct Reflect<i64> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "int64", sizeof(i64), alignof(i64) };
	};

	template<> struct Reflect<u8> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "uint8", sizeof(i8), alignof(i8) };
	};

	template<> struct Reflect<u16> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "uint16", sizeof(i16), alignof(i16) };
	};

	template<> struct Reflect<u32> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "uint32", sizeof(i32), alignof(i32) };
	};

	template<> struct Reflect<u64> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "uint64", sizeof(i64), alignof(i64) };
	};

	template<> struct Reflect<f32> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "float32", sizeof(f32), alignof(f32) };
	};

	template<> struct Reflect<f64> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 1, TypeInfoKind::PRIMITIVE }, "float64", sizeof(f64), alignof(f64) };
	};

}
