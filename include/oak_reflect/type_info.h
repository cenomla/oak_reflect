#pragma once

#include <type_traits>

#include <oak_util/types.h>
#include <oak_util/fmt.h>

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

	struct StructTypeInfo : TypeInfo {
		String name;
		String annotation;
		u64 size;
		u64 align;
		Slice<FieldInfo const> fields;
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

	// Default type:

	// TODO: Do not specialize `Reflect` based on this type
	struct NoType {};

	template<typename T, typename Enable = std::true_type>
	struct Reflect {
		static constexpr TypeInfo typeInfo{ 0, TypeInfoKind::NONE };
	};

	// Built in types:

	template<> struct Reflect<void> {
		static constexpr TypeInfo typeInfo{ 1ul, TypeInfoKind::VOID };
	};

	template<> struct Reflect<i8> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 2ul, TypeInfoKind::PRIMITIVE }, "int8", sizeof(i8), alignof(i8) };
	};

	template<> struct Reflect<i16> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 3ul, TypeInfoKind::PRIMITIVE }, "int16", sizeof(i16), alignof(i16) };
	};

	template<> struct Reflect<i32> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 4ul, TypeInfoKind::PRIMITIVE }, "int32", sizeof(i32), alignof(i32) };
	};

	template<> struct Reflect<i64> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 5ul, TypeInfoKind::PRIMITIVE }, "int64", sizeof(i64), alignof(i64) };
	};

	template<> struct Reflect<u8> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 6ul, TypeInfoKind::PRIMITIVE }, "uint8", sizeof(i8), alignof(i8) };
	};

	template<> struct Reflect<u16> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 7ul, TypeInfoKind::PRIMITIVE }, "uint16", sizeof(i16), alignof(i16) };
	};

	template<> struct Reflect<u32> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 8ul, TypeInfoKind::PRIMITIVE }, "uint32", sizeof(i32), alignof(i32) };
	};

	template<> struct Reflect<u64> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 9ul, TypeInfoKind::PRIMITIVE }, "uint64", sizeof(i64), alignof(i64) };
	};

	template<> struct Reflect<f32> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 10ul, TypeInfoKind::PRIMITIVE }, "float32", sizeof(f32), alignof(f32) };
	};

	template<> struct Reflect<f64> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 11ul, TypeInfoKind::PRIMITIVE }, "float64", sizeof(f64), alignof(f64) };
	};

	template<> struct Reflect<bool> {
		static constexpr PrimitiveTypeInfo typeInfo{ { 12ul, TypeInfoKind::PRIMITIVE }, "bool", sizeof(bool), alignof(bool) };
	};

	// Meta-generated types:

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_pointer_v<std::decay_t<T>>, std::true_type>> {
		static constexpr PointerTypeInfo typeInfo{ { 0, TypeInfoKind::POINTER }, &Reflect<std::remove_pointer_t<std::decay_t<T>>>::typeInfo };
	};

	template<typename T>
	struct Reflect<T, std::enable_if_t<std::is_array_v<std::decay_t<T>>, std::true_type>> {
		static constexpr ArrayTypeInfo typeInfo{ { 0, TypeInfoKind::ARRAY }, &Reflect<std::remove_extent_t<std::decay_t<T>>>::typeInfo, std::extent_v<std::decay_t<T>> };
	};

	namespace detail {

		template<typename T>
		struct FunctionReflectionData;

		template<typename Out, typename... In>
		struct FunctionReflectionData<Out(In...)> {
			static constexpr TypeInfo const* returnTypeInfo = &Reflect<Out>::typeInfo;

			static constexpr TypeInfo const* argTypeInfos[] = {
				&Reflect<In>::typeInfo...,
			};
		};

		template<typename T>
		struct MemberFunctionReflectionData;

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
			default: return 0;
		}
	}

	constexpr String type_name(TypeInfo const *typeInfo) {
		switch (typeInfo->kind) {
			case TypeInfoKind::PRIMITIVE: return static_cast<PrimitiveTypeInfo const*>(typeInfo)->name;
			case TypeInfoKind::STRUCT: return static_cast<StructTypeInfo const*>(typeInfo)->name;
			case TypeInfoKind::UNION: return static_cast<UnionTypeInfo const*>(typeInfo)->name;
			case TypeInfoKind::ENUM: return type_name(static_cast<EnumTypeInfo const*>(typeInfo)->underlyingType);
			default: return "";
		}
	}

	constexpr bool attribute_in_annotation(String annotation, String attribute) {
		if (annotation.count <= attribute.count)
			return false;

		// TODO(SIMD): Make this simd
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
					}
				}
				i = j;
			}
		}

		return false;
	}

	constexpr bool has_attribute(TypeInfo const *typeInfo, String attribute) {
		String annotation;
		switch (typeInfo->kind) {
			case TypeInfoKind::STRUCT: annotation = static_cast<StructTypeInfo const*>(typeInfo)->annotation; break;
			case TypeInfoKind::UNION: annotation = static_cast<UnionTypeInfo const*>(typeInfo)->annotation; break;
			case TypeInfoKind::ENUM: annotation = static_cast<EnumTypeInfo const*>(typeInfo)->annotation; break;
			default: break;
		}

		return attribute_in_annotation(annotation, attribute);
	}

	constexpr bool has_attribute(FieldInfo const *fieldInfo, String attribute) {
		return attribute_in_annotation(fieldInfo->annotation, attribute);
	}

	template<typename T>
	constexpr String enum_name(T e, TypeInfo const *typeInfo) {
		if (typeInfo->kind == TypeInfoKind::ENUM) {
			auto ei = static_cast<EnumTypeInfo const*>(typeInfo);
			for (auto constant : ei->constants) {
				if (constant.value == static_cast<u64>(enum_int(e))) {
					return constant.name;
				}
			}
		}
		return "";
	}

}

