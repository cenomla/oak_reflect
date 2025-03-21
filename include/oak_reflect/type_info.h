#pragma once

#include <type_traits>

#include <oak_util/types.h>
#include <oak_util/hash.h>
#include <oak_util/containers.h>
#include <oak_util/algorithm.h>

#include "type_uid.h"

#ifdef _MSC_VER

#ifdef OAK_REFLECT_DYNAMIC_LIB

#ifdef OAK_REFLECT_EXPORT_SYMBOLS
#define OAK_REFLECT_API __declspec(dllexport)
#else
#define OAK_REFLECT_API __declspec(dllimport)
#endif // OAK_REFLECT_EXPORT_SYMBOLS

#else

#define OAK_REFLECT_API

#endif // OAK_REFLECT_DYNAMIC_LIB

#else

#define OAK_REFLECT_API __attribute__((visibility("default")))

#endif // _MSC_VER


namespace oak {

	namespace detail {

		template<typename T>
		void generic_construct(void *obj) {
			if constexpr(std::is_default_constructible_v<T>) {
				new (obj) T{};
			}
		}
	}

	enum class TypeInfoKind {
		NONE,
		VOID,
		PRIMITIVE,
		FUNCTION,
		MEMBER_FUNCTION,
		POINTER,
		ARRAY,
		STRUCT,
		ENUM,
		UNION,
	};

	struct TypeInfo {
		/// Unique type id, zero means intangible type (non instantiable)
		u64 uid;
		TypeInfoKind kind;
	};

	struct PrimitiveTypeInfo : TypeInfo {
		String name;
		u64 size;
		u64 align;
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

	struct MethodInfo {
		String name;
		String symbolName;
		String annotation;
		TypeInfo const *typeInfo;
		i64 virtualOffset;
	};

	struct PropertyInfo {
		String name;
		String annotation;
		TypeInfo const *typeInfo;
		void const* data;
	};

	struct StructTypeInfo : TypeInfo {
		String name;
		String annotation;
		u64 size;
		u64 align;
		TypeInfo const *base;
		Slice<FieldInfo const> fields;
		Slice<MethodInfo const> methods;
		Slice<PropertyInfo const> properties;
		void (*defaultConstructFn)(void*);
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
		u64 size;
		u64 align;
		Slice<FieldInfo const> fields;
		void (*defaultConstructFn)(void*);
	};

	struct FunctionTypeInfo : TypeInfo {
		Slice<TypeInfo const* const> argTypeInfos;
		TypeInfo const* returnTypeInfo;
	};

	struct MemberFunctionTypeInfo : TypeInfo {
		Slice<TypeInfo const* const> argTypeInfos;
		TypeInfo const* returnTypeInfo;
		TypeInfo const* classTypeInfo;
	};

	constexpr PointerTypeInfo make_pointer_type_info(TypeInfo const* pointeeTypeInfo) {
		return {
			{ hash_combine(pointeeTypeInfo->uid, OAK_TYPE_UID(*)), TypeInfoKind::POINTER },
			pointeeTypeInfo,
		};
	}

	constexpr ArrayTypeInfo make_array_type_info(TypeInfo const* elementTypeInfo, i64 count) {
		return {
			{ hash_combine(elementTypeInfo->uid, hash_int(count)), TypeInfoKind::ARRAY },
			elementTypeInfo, count,
		};
	}

	// Default type:

	// TODO: Do not specialize `Reflect` based on this type
	struct NoType {};

	template<typename T, typename Enable = std::true_type>
	struct Reflect {
		static constexpr TypeInfo typeInfo{ 0, TypeInfoKind::NONE };
	};

	// Built in types:

	template<> struct Reflect<void> {
		static constexpr TypeInfo typeInfo{ OAK_TYPE_UID(void), TypeInfoKind::VOID };
	};

	template<> struct Reflect<char> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(char), TypeInfoKind::PRIMITIVE }, "char", sizeof(char), alignof(char) };
	};

	template<> struct Reflect<i8> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(i8), TypeInfoKind::PRIMITIVE }, "i8", sizeof(i8), alignof(i8) };
	};

	template<> struct Reflect<i16> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(i16), TypeInfoKind::PRIMITIVE }, "i16", sizeof(i16), alignof(i16) };
	};

	template<> struct Reflect<i32> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(i32), TypeInfoKind::PRIMITIVE }, "i32", sizeof(i32), alignof(i32) };
	};

	template<> struct Reflect<i64> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(i64), TypeInfoKind::PRIMITIVE }, "i64", sizeof(i64), alignof(i64) };
	};

	template<> struct Reflect<u8> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(u8), TypeInfoKind::PRIMITIVE }, "u8", sizeof(i8), alignof(i8) };
	};

	template<> struct Reflect<u16> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(u16), TypeInfoKind::PRIMITIVE }, "u16", sizeof(i16), alignof(i16) };
	};

	template<> struct Reflect<u32> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(u32), TypeInfoKind::PRIMITIVE }, "u32", sizeof(i32), alignof(i32) };
	};

	template<> struct Reflect<u64> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(u64), TypeInfoKind::PRIMITIVE }, "u64", sizeof(i64), alignof(i64) };
	};

	template<> struct Reflect<f32> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(f32), TypeInfoKind::PRIMITIVE }, "f32", sizeof(f32), alignof(f32) };
	};

	template<> struct Reflect<f64> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(f64), TypeInfoKind::PRIMITIVE }, "f64", sizeof(f64), alignof(f64) };
	};

	template<> struct Reflect<bool> {
		static constexpr PrimitiveTypeInfo typeInfo{
			{ OAK_TYPE_UID(bool), TypeInfoKind::PRIMITIVE }, "bool", sizeof(bool), alignof(bool) };
	};

	// Meta-generated types:

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_pointer_v<std::remove_cv_t<std::remove_reference_t<T>>>, std::true_type>> {
		using PointeeType = std::remove_cv_t<std::remove_pointer_t<std::remove_cv_t<std::remove_reference_t<T>>>>;
		static constexpr PointerTypeInfo typeInfo = make_pointer_type_info(&Reflect<PointeeType>::typeInfo);
	};

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_array_v<std::remove_cv_t<std::remove_reference_t<T>>>, std::true_type>> {
		using PointeeType = std::remove_extent_t<std::remove_cv_t<std::remove_reference_t<T>>>;
		static constexpr ArrayTypeInfo typeInfo = make_array_type_info(
				&Reflect<PointeeType>::typeInfo,
				std::extent_v<std::remove_cv_t<std::remove_reference_t<T>>>);
	};

	namespace detail {

		template<typename T>
		struct FunctionReflectionData;

		template<typename Out>
		struct FunctionReflectionData<Out()> {
			static constexpr TypeInfo const* returnTypeInfo = &Reflect<Out>::typeInfo;

			static constexpr Slice<TypeInfo const* const> argTypeInfos{};
		};

		template<typename Out, typename... In>
		struct FunctionReflectionData<Out(In...)> {
			static constexpr TypeInfo const* returnTypeInfo = &Reflect<Out>::typeInfo;

			static constexpr TypeInfo const* argTypeInfos[] = {
				&Reflect<In>::typeInfo...,
			};
		};

		template<typename T>
		struct MemberFunctionReflectionData;

		template<typename T, typename Out>
		struct MemberFunctionReflectionData<Out (T::*)()> {
			static constexpr TypeInfo const* returnTypeInfo = &Reflect<Out>::typeInfo;

			static constexpr Slice<TypeInfo const* const> argTypeInfos{};

			//static constexpr TypeInfo const* classTypeInfo = &Reflect<T>::typeInfo;
		};

		template<typename T, typename Out, typename... In>
		struct MemberFunctionReflectionData<Out (T::*)(In...)> {
			static constexpr TypeInfo const* returnTypeInfo = &Reflect<Out>::typeInfo;

			static constexpr TypeInfo const* argTypeInfos[] = {
				&Reflect<In>::typeInfo...,
			};

			//static constexpr TypeInfo const* classTypeInfo = &Reflect<T>::typeInfo;
		};

	}

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_function_v<std::remove_pointer_t<std::decay_t<T>>>, std::true_type>> {
		// Function pointer partial specialization

		using Data = detail::FunctionReflectionData<std::remove_pointer_t<std::decay_t<T>>>;

		static constexpr FunctionTypeInfo typeInfo{ { 0, TypeInfoKind::FUNCTION }, Data::argTypeInfos, Data::returnTypeInfo };
	};

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_member_function_pointer_v<std::decay_t<T>>, std::true_type>> {
		// Member function pointer partial specialization

		using Data = detail::MemberFunctionReflectionData<std::decay_t<T>>;

		static constexpr MemberFunctionTypeInfo typeInfo{ { 0, TypeInfoKind::MEMBER_FUNCTION }, Data::argTypeInfos, Data::returnTypeInfo, &Reflect<void>::typeInfo };
	};

	constexpr u64 type_size(TypeInfo const *typeInfo) {
		switch (typeInfo->kind) {
		case TypeInfoKind::PRIMITIVE: return static_cast<PrimitiveTypeInfo const*>(typeInfo)->size;
		case TypeInfoKind::STRUCT: return static_cast<StructTypeInfo const*>(typeInfo)->size;
		case TypeInfoKind::UNION: return static_cast<UnionTypeInfo const*>(typeInfo)->size;
		case TypeInfoKind::ENUM: return type_size(static_cast<EnumTypeInfo const*>(typeInfo)->underlyingType);
		case TypeInfoKind::POINTER: return sizeof(void*);
		case TypeInfoKind::ARRAY: return type_size(static_cast<ArrayTypeInfo const*>(typeInfo)->of)
								  * static_cast<ArrayTypeInfo const*>(typeInfo)->count;
		default: return 0;
		}
	}

	constexpr u64 type_align(TypeInfo const *typeInfo) {
		switch (typeInfo->kind) {
		case TypeInfoKind::PRIMITIVE: return static_cast<PrimitiveTypeInfo const*>(typeInfo)->align;
		case TypeInfoKind::STRUCT: return static_cast<StructTypeInfo const*>(typeInfo)->align;
		case TypeInfoKind::UNION: return static_cast<UnionTypeInfo const*>(typeInfo)->align;
		case TypeInfoKind::ENUM: return type_align(static_cast<EnumTypeInfo const*>(typeInfo)->underlyingType);
		case TypeInfoKind::POINTER: return alignof(void*);
		case TypeInfoKind::ARRAY: return type_align(static_cast<ArrayTypeInfo const*>(typeInfo)->of);
		default: return 0;
		}
	}

	constexpr String type_name(TypeInfo const *typeInfo) {
		switch (typeInfo->kind) {
		case TypeInfoKind::PRIMITIVE: return static_cast<PrimitiveTypeInfo const*>(typeInfo)->name;
		case TypeInfoKind::STRUCT: return static_cast<StructTypeInfo const*>(typeInfo)->name;
		case TypeInfoKind::UNION: return static_cast<UnionTypeInfo const*>(typeInfo)->name;
		case TypeInfoKind::ENUM: return static_cast<EnumTypeInfo const*>(typeInfo)->name;
		default: return "";
		}
	}

	constexpr String type_annotation(TypeInfo const *typeInfo) {
		switch (typeInfo->kind) {
		case TypeInfoKind::STRUCT: return static_cast<StructTypeInfo const*>(typeInfo)->annotation;
		case TypeInfoKind::UNION: return static_cast<UnionTypeInfo const*>(typeInfo)->annotation;
		case TypeInfoKind::ENUM: return static_cast<EnumTypeInfo const*>(typeInfo)->annotation;
		default: return "";
		}
	}

	OAK_REFLECT_API bool attribute_value_in_annotation(String annotation, String attribute, String *value = nullptr);
	OAK_REFLECT_API Vector<String> attributes_in_annotation(String annotation);

	constexpr bool attribute_in_annotation(String annotation, String attribute) {
		if (annotation.count <= attribute.count)
			return false;

		// TODO: Optimize this function using SIMD
		i64 end = annotation.count;
		for (i64 i = 0; i < end; ++i) {
			if (annotation[i] == ';' || annotation[i] == ',') {
				i64 j = i + 1;
				for (; j <= end; ++j) {
					if (j == end || annotation[j] == ',') {
						auto str = String{ annotation.data + i + 1, j - i - 1 };
						if (str == attribute) {
							return true;
						}
						break;
					}
				}
				i = j - 1;
			}
		}

		return false;
	}

	constexpr bool value_attribute_decompose(String attribute, String *name = nullptr, String *value = nullptr) {
		auto pos = find(slc(attribute), '=');

		if (pos == -1)
			return false;

		if (name)
			*name = sub_slice(slc(attribute), 0, pos);

		if (value)
			*value = sub_slice(slc(attribute), pos + 1);

		return true;
	}

	constexpr bool has_attribute(TypeInfo const *typeInfo, String attribute) {
		return attribute_in_annotation(type_annotation(typeInfo), attribute);
	}

	constexpr bool has_attribute(FieldInfo const *fieldInfo, String attribute) {
		return attribute_in_annotation(fieldInfo->annotation, attribute);
	}

	template<typename T>
	constexpr String enum_name(T enumValue, TypeInfo const *typeInfo) {
		if (typeInfo->kind == TypeInfoKind::ENUM) {
			auto ei = static_cast<EnumTypeInfo const*>(typeInfo);
			for (auto constant : ei->constants) {
				if (constant.value == static_cast<u64>(eni(enumValue))) {
					return constant.name;
				}
			}
		}
		return "";
	}

}
