#pragma once

#include <oak_util/types.h>

namespace oak::rtti {

	enum class TypeKind {
		PRIMITIVE,
		POINTER,
		ARRAY,
		ENUM,
		STRUCT,
		UNION,
		FUNCTION,
	};

	enum class TypeQualifier : u32 {
		CONSTEXPR,
		STATIC,
		NOEXCEPT,
		CONST,
		VOLATILE,
		VIRTUAL,
		OVERRIDE,
		FINAL,
	};

	struct TypeInfo {
		String name;
		u64 uid;
		u64 size;
		u64 align;
		TypeKind kind;
		u32 qualifierFlags;
	};

	struct PtrInfo : TypeInfo {
		TypeInfo const *to;
	};

	struct ArrayInfo : TypeInfo {
		TypeInfo const *of;
		i64 count;
	};

	struct EnumConstant {
		String name;
		u64 value;
	};

	struct EnumInfo : TypeInfo {
		TypeInfo const *underlyingTypeInfo;
		Slice<EnumConstant const*> constants;
	};

	struct FieldInfo {
		String name;
		TypeInfo const *typeInfo;

		/// Byte offset info function
		u64 offset;

		/// User defined annotation flags
		u32 annotationFlags;
	};

	struct FunctionInfo : TypeInfo {
		Slice<TypeInfo const*> argInfos;
		TypeInfo const *retInfo;
	};

	struct UnionInfo : TypeInfo {
		Slice<FieldInfo const*> members;
	};

	struct StructInfo : TypeInfo {
		Slice<TypeInfo const*> bases;
		Slice<FieldInfo const*> members;
	};

}

