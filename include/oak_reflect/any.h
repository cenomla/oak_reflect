#pragma once

#include <oak_util/ptr.h>

#include "type_info.h"
#include "attribute.h"

namespace oak {

	struct _reflect() OAK_REFLECT_API CAny {
		void const *ptr = nullptr;
		TypeInfo const *type = nullptr;

		template<typename T>
		T const& to_value() const noexcept {
			assert(Reflect<T>::typeInfo.uid == type->uid);
			return *static_cast<T const*>(ptr);
		}
	};

	struct _reflect() OAK_REFLECT_API Any {
		void *ptr = nullptr;
		TypeInfo const *type = nullptr;

		template<typename T>
		static constexpr Any from_value(T *value) {
			static_assert(Reflect<T>::typeInfo.uid != 0);
			return { value, &Reflect<T>::typeInfo };
		}

		static inline Any alloc_with_type(Allocator *allocator, TypeInfo const *typeInfo) {
			return { allocator->allocate(type_size(typeInfo), type_align(typeInfo)), typeInfo };
		}

		// Member functions
		Any get_member(String name, FieldInfo const ** info = nullptr) const noexcept;
		Any get_member(FieldInfo const *info) const noexcept;
		CAny get_property(String name, PropertyInfo const ** info = nullptr) const noexcept;
		CAny get_property(PropertyInfo const *info) const noexcept;
		CAny get_member_or_property(String name) const noexcept;

		// Array functions
		i64 get_array_count() const noexcept;
		Any get_element(i64 index) const noexcept;

		// Variant functions
		Any get_variant_value(Any *outVarType = nullptr) const noexcept;

		void construct() noexcept;

		template<typename T>
		T const& to_value() const noexcept {
			assert(Reflect<T>::typeInfo.uid == type->uid);
			return *static_cast<T const*>(ptr);
		}

		template<typename T>
		T& to_value() noexcept {
			assert(Reflect<T>::typeInfo.uid == type->uid);
			return *static_cast<T*>(ptr);
		}

		void*& ptr_value() noexcept {
			assert(TypeInfoKind::POINTER == type->kind);
			return *static_cast<void**>(ptr);
		}

		void set_enum_value(u64 ev) noexcept;
		u64 get_enum_value() const noexcept;

		Any shallow_copy(Allocator *allocator) const;
		Any deep_copy(Allocator *allocator) const;
	};

	bool operator==(Any lhs, Any rhs) noexcept;

	inline bool operator!=(Any lhs, Any rhs) noexcept {
		return !(lhs == rhs);
	}

	void copy_fields(Any dst, Any src);
	void copy_deep(Allocator *allocator, Any dst, Any src);

namespace {

	inline i64 get_any_struct_array_count(Any any) {
		if (auto count = any.get_member("count"); count.type->kind != TypeInfoKind::NONE)
			return count.to_value<i64>();

		if (auto capacity = any.get_member_or_property("capacity"); capacity.type->kind != TypeInfoKind::NONE)
			return capacity.to_value<i64>();

		return 0;
	}

}

	inline Any Any::get_member(String name, FieldInfo const ** info) const noexcept {
		if (info)
			*info = nullptr;
		if (type->kind == TypeInfoKind::STRUCT) {
			auto si = static_cast<StructTypeInfo const*>(type);
			for (auto& field : si->fields) {
				if (field.name == name) {
					if (info) {
						*info = &field;
					}
					return { add_ptr(ptr, field.offset), field.typeInfo };
				}
			}
		} else if (type->kind == TypeInfoKind::UNION) {
			auto si = static_cast<UnionTypeInfo const*>(type);
			for (auto& field : si->fields) {
				if (field.name == name) {
					if (info) {
						*info = &field;
					}
					return { add_ptr(ptr, field.offset), field.typeInfo };
				}
			}
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	inline Any Any::get_member(FieldInfo const *field) const noexcept {
		return { add_ptr(ptr, field->offset), field->typeInfo };
	}

	inline CAny Any::get_property(String name, PropertyInfo const ** info) const noexcept {
		if (type->kind == TypeInfoKind::STRUCT) {
			auto si = static_cast<StructTypeInfo const*>(type);
			for (auto& prop : si->properties) {
				if (prop.name == name) {
					if (info) {
						*info = &prop;
					}
					return { prop.data, prop.typeInfo };
				}
			}
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	inline CAny Any::get_property(PropertyInfo const *info) const noexcept {
		return { info->data, info->typeInfo };
	}

	inline CAny Any::get_member_or_property(String name) const noexcept {
		if (type->kind == TypeInfoKind::STRUCT) {
			auto si = static_cast<StructTypeInfo const*>(type);
			for (auto& prop : si->properties) {
				if (prop.name == name) {
					return { prop.data, prop.typeInfo };
				}
			}
			for (auto& field : si->fields) {
				if (field.name == name) {
					return { add_ptr(ptr, field.offset), field.typeInfo };
				}
			}
		} else if (type->kind == TypeInfoKind::UNION) {
			auto ui = static_cast<UnionTypeInfo const*>(type);
			for (auto& field : ui->fields) {
				if (field.name == name) {
					return { add_ptr(ptr, field.offset), field.typeInfo };
				}
			}
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	inline i64 Any::get_array_count() const noexcept {
		if (type->kind == TypeInfoKind::ARRAY) {
			return static_cast<ArrayTypeInfo const*>(type)->count;
		} else if (type->kind == TypeInfoKind::STRUCT) {
			return get_any_struct_array_count(*this);
		}
		return 0;
	}

	inline Any Any::get_element(i64 index) const noexcept {
		if (type->kind == TypeInfoKind::ARRAY) {
			auto ai = static_cast<ArrayTypeInfo const*>(type);
			if (index < ai->count) {
				return { add_ptr(ptr, index * type_size(ai->of)), ai->of };
			}
		} else if (has_attribute(type, "array")) {
			auto data = get_member("data");
			auto count = get_any_struct_array_count(*this);

			void *dataPtr;
			TypeInfo const *elemType;
			switch (data.type->kind) {
				case TypeInfoKind::ARRAY:
					dataPtr = data.ptr;
					elemType = static_cast<ArrayTypeInfo const*>(data.type)->of;
					break;
				case TypeInfoKind::POINTER:
					dataPtr = data.ptr_value();
					elemType = static_cast<PointerTypeInfo const*>(data.type)->to;
					break;
				default:
					assert(false && "Invalid element type");
			}

			if (index < count) {
				return { add_ptr(dataPtr, index * type_size(elemType)), elemType };
			} else {
				return { nullptr, elemType };
			}
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	inline Any Any::get_variant_value(Any *outVarType) const noexcept {
		assert(has_attribute(type, "variant"));

		if (outVarType)
			*outVarType = { nullptr, &Reflect<NoType>::typeInfo };

		auto varType = get_member("type");
		auto varValue = get_member("value");

		// Type must be an enum type, value must be a union
		if (varType.type->kind != TypeInfoKind::ENUM || varValue.type->kind != TypeInfoKind::UNION)
			return { nullptr, &Reflect<NoType>::typeInfo };

		auto unionInfo = static_cast<UnionTypeInfo const*>(varValue.type);

		auto fieldIdx = static_cast<i64>(varType.get_enum_value());
		if (fieldIdx < 0 || fieldIdx >= unionInfo->fields.count)
			return { nullptr, &Reflect<NoType>::typeInfo };

		if (outVarType)
			*outVarType = varType;

		auto activeField = &unionInfo->fields[fieldIdx];
		return varValue.get_member(activeField);
	}

	inline void Any::construct() noexcept {
		switch (type->kind) {
			case TypeInfoKind::PRIMITIVE:
			case TypeInfoKind::POINTER:
			case TypeInfoKind::ENUM:
				memset(ptr, 0, type_size(type));
				break;
			case TypeInfoKind::STRUCT:
				{
					auto si = static_cast<StructTypeInfo const*>(type);
					if (si->defaultConstructFn)
						si->defaultConstructFn(ptr);
				} break;
			case TypeInfoKind::UNION:
				{
					auto ui = static_cast<UnionTypeInfo const*>(type);
					if (ui->defaultConstructFn)
						ui->defaultConstructFn(ptr);
				} break;
			default: break;
		}
	}

	inline void Any::set_enum_value(u64 ev) noexcept {
		assert(type->kind == TypeInfoKind::ENUM);
		auto ei = static_cast<EnumTypeInfo const*>(type);
		switch (ei->underlyingType->uid) {
			case Reflect<i8>::typeInfo.uid: *static_cast<i8*>(ptr) = static_cast<i8>(ev); break;
			case Reflect<i16>::typeInfo.uid: *static_cast<i16*>(ptr) = static_cast<i16>(ev); break;
			case Reflect<i32>::typeInfo.uid: *static_cast<i32*>(ptr) = static_cast<i32>(ev); break;
			case Reflect<i64>::typeInfo.uid: *static_cast<i64*>(ptr) = static_cast<i64>(ev); break;
			case Reflect<u8>::typeInfo.uid: *static_cast<u8*>(ptr) = static_cast<u8>(ev); break;
			case Reflect<u16>::typeInfo.uid: *static_cast<u16*>(ptr) = static_cast<u16>(ev); break;
			case Reflect<u32>::typeInfo.uid: *static_cast<u32*>(ptr) = static_cast<u32>(ev); break;
			case Reflect<u64>::typeInfo.uid: *static_cast<u64*>(ptr) = static_cast<u64>(ev); break;
			default: break;
		}
	}

	inline u64 Any::get_enum_value() const noexcept {
		assert(type->kind == TypeInfoKind::ENUM);
		auto ei = static_cast<EnumTypeInfo const*>(type);
		u64 ev = 0;
		switch (ei->underlyingType->uid) {
			case Reflect<i8>::typeInfo.uid: ev = static_cast<u64>(*static_cast<i8*>(ptr)); break;
			case Reflect<i16>::typeInfo.uid: ev = static_cast<u64>(*static_cast<i16*>(ptr)); break;
			case Reflect<i32>::typeInfo.uid: ev = static_cast<u64>(*static_cast<i32*>(ptr)); break;
			case Reflect<i64>::typeInfo.uid: ev = static_cast<u64>(*static_cast<i64*>(ptr)); break;
			case Reflect<u8>::typeInfo.uid: ev = *static_cast<u8*>(ptr); break;
			case Reflect<u16>::typeInfo.uid: ev = *static_cast<u16*>(ptr); break;
			case Reflect<u32>::typeInfo.uid: ev = *static_cast<u32*>(ptr); break;
			case Reflect<u64>::typeInfo.uid: ev = *static_cast<u64*>(ptr); break;
			default: break;
		}
		return ev;
	}

	inline Any Any::shallow_copy(Allocator *allocator) const {
		assert(ptr);
		assert(type);
		assert(type_size(type));
		assert(type_align(type));

		Any result;
		result.type = type;
		result.ptr = allocator->allocate(type_size(type), type_align(type));

		memcpy(result.ptr, ptr, type_size(type));

		return result;
	}

	inline Any Any::deep_copy(Allocator *allocator) const {
		assert(ptr);
		assert(type);
		assert(type_size(type));
		assert(type_align(type));

		Any result;
		result.type = type;
		result.ptr = allocator->allocate(type_size(type), type_align(type));

		copy_deep(allocator, result, *this);

		return result;
	}

	inline bool operator==(Any lhs, Any rhs) noexcept {
		assert(lhs.type && rhs.type);
		if (lhs.type->uid != rhs.type->uid)
			return false;

		if (lhs.ptr == rhs.ptr)
			return true;

		if (!lhs.ptr || !rhs.ptr)
			return false;

		switch (lhs.type->kind) {
			case TypeInfoKind::NONE: return true;
			case TypeInfoKind::VOID: return true;
			case TypeInfoKind::PRIMITIVE: {
				switch (lhs.type->uid) {
#define PRIM_EQUAL_CASE(x) case Reflect<x>::typeInfo.uid:\
					return lhs.to_value<x>() == rhs.to_value<x>();
					PRIM_EQUAL_CASE(char)
					PRIM_EQUAL_CASE(bool)
					PRIM_EQUAL_CASE(i8)
					PRIM_EQUAL_CASE(i16)
					PRIM_EQUAL_CASE(i32)
					PRIM_EQUAL_CASE(i64)
					PRIM_EQUAL_CASE(u8)
					PRIM_EQUAL_CASE(u16)
					PRIM_EQUAL_CASE(u32)
					PRIM_EQUAL_CASE(u64)
					PRIM_EQUAL_CASE(f32)
					PRIM_EQUAL_CASE(f64)
#undef PRIM_EQUAL_CASE
					default: return false;
				}
			};
			case TypeInfoKind::POINTER: return lhs.ptr_value() == rhs.ptr_value();
			case TypeInfoKind::ARRAY:
			{
				bool ret = true;
				auto ai = static_cast<ArrayTypeInfo const*>(lhs.type);
				for (i32 i = 0; i < ai->count; ++i) {
					if (lhs.get_element(i) != rhs.get_element(i)) {
						ret = false;
						break;
					}
				}
				return ret;
			}
			case TypeInfoKind::STRUCT:
			{
				if (has_attribute(lhs.type, "array")) {
					auto lhsCount = get_any_struct_array_count(lhs);
					auto rhsCount = get_any_struct_array_count(rhs);
					if (lhsCount != rhsCount)
						return false;

					bool ret = true;
					for (i32 i = 0; i < lhsCount; ++i) {
						if (lhs.get_element(i) != rhs.get_element(i)) {
							ret = false;
							break;
						}
					}
					return ret;
				} else {
					bool ret = true;
					auto si = static_cast<StructTypeInfo const*>(lhs.type);
					for (auto& field : si->fields) {
						if (lhs.get_member(&field) != rhs.get_member(&field)) {
							ret = false;
							break;
						}
					}
					return ret;
				}
			}
			case TypeInfoKind::UNION:
			{
				auto ui = static_cast<UnionTypeInfo const*>(lhs.type);
				return memcmp(lhs.ptr, rhs.ptr, ui->size) == 0;
			};
			case TypeInfoKind::ENUM:
				return lhs.get_enum_value() == rhs.get_enum_value();
			default:
				return false;
		}
	}

	inline void copy_fields(Any dst, Any src) {
		assert(dst.type && src.type);

		if (dst.type->kind != TypeInfoKind::STRUCT
				|| src.type->kind != TypeInfoKind::STRUCT) {
			return;
		}

		auto si0 = static_cast<StructTypeInfo const*>(dst.type);
		auto si1 = static_cast<StructTypeInfo const*>(src.type);
		for (i64 i = 0, j = 0; i < si0->fields.count; ++i, ++j) {
			auto field0 = si0->fields[i];
			for (i64 k = 0; k < si1->fields.count; ++k) {
				auto field1 = si1->fields[j + k - (j + k >= si1->fields.count ? si1->fields.count : 0)];
				if (field0.name == field1.name) {
					if (field0.typeInfo->uid == field1.typeInfo->uid) {
						auto size = type_size(field0.typeInfo);
						if (size)
							memcpy(
									add_ptr(dst.ptr, field0.offset),
									add_ptr(src.ptr, field1.offset),
									size);
						break;
					} else if (field0.typeInfo->kind == TypeInfoKind::STRUCT
							&& field1.typeInfo->kind == TypeInfoKind::STRUCT) {
						copy_fields(
								{ add_ptr(dst.ptr, field0.offset), field0.typeInfo },
								{ add_ptr(src.ptr, field1.offset), field1.typeInfo });
						break;
					}
				}
			}
			if (j >= si1->fields.count)
				j = 0;
		}
	}

	inline void copy_deep(Allocator *allocator, Any dst, Any src) {
		assert(dst.type && src.type);
		assert(dst.type->uid == src.type->uid);

		if (dst.type->kind == TypeInfoKind::STRUCT) {
			if (dst.type->uid == OAK_TYPE_UID(oak::Any)) {
				if (src.to_value<Any>().ptr)
					dst.to_value<Any>() = src.to_value<Any>().deep_copy(allocator);
				else
					// Don't copy empty any values
					dst.to_value<Any>() = src.to_value<Any>();
			} else if (dst.type->uid == OAK_TYPE_UID(oak::String)) {
				dst.to_value<String>() = copy_str(allocator, src.to_value<String>());
			} else if (has_attribute(dst.type, "variant")) {
				Any srcVarType;
				auto srcVarValue = src.get_variant_value(&srcVarType);
				if (srcVarValue.type->kind != TypeInfoKind::NONE) {
					dst.get_member("type").set_enum_value(srcVarType.get_enum_value());
					copy_deep(allocator, dst.get_variant_value(), srcVarValue);
				}
			} else if (has_attribute(dst.type, "array")) {
				if (auto capacity = src.get_member("capacity"); capacity.type->kind != TypeInfoKind::NONE) {
					// Allocate dynamic array data
					if (capacity.to_value<i64>()) {
						auto elem0 = src.get_element(0);
						dst.get_member("data").ptr_value() = allocator->allocate(
								type_size(elem0.type)*capacity.to_value<i64>(), type_align(elem0.type));
					}
					// Set array count/capacity
					dst.get_member("capacity").to_value<i64>() = capacity.to_value<i64>();
					if (auto dstCount = dst.get_member("count"); dstCount.type->kind != TypeInfoKind::NONE)
						dstCount.to_value<i64>() = src.get_member("count").to_value<i64>();
					// Copy array elements
					for (i64 i = 0; i < src.get_array_count(); ++i) {
						copy_deep(allocator, dst.get_element(i), src.get_element(i));
					}
				} else {
					assert(src.get_property("capacity").type->kind != TypeInfoKind::NONE);
					if (auto dstCount = dst.get_member("count"); dstCount.type->kind != TypeInfoKind::NONE)
						dstCount.to_value<i64>() = src.get_member("count").to_value<i64>();
					// Copy array elements
					for (i64 i = 0; i < src.get_array_count(); ++i) {
						copy_deep(allocator, dst.get_element(i), src.get_element(i));
					}
				}
			} else {
				auto si = static_cast<StructTypeInfo const*>(dst.type);
				for (auto field : si->fields) {
					copy_deep(allocator, dst.get_member(&field), src.get_member(&field));
				}
			}
		} else {
			memcpy(dst.ptr, src.ptr, type_size(dst.type));
		}
	}

	inline void deallocate_deep(Allocator *allocator, Any value) {
		assert(value.type);

		if (value.type->kind == TypeInfoKind::STRUCT) {
			if (value.type->uid == OAK_TYPE_UID(oak::Any)) {
				auto any = value.to_value<Any>();
				deallocate_deep(allocator, any);
				allocator->deallocate(any.ptr, type_size(any.type));
			} else if (value.type->uid == OAK_TYPE_UID(oak::String)) {
				auto str = value.to_value<String>();
				deallocate(allocator, str.data, str.count);
			} else if (has_attribute(value.type, "variant")) {
				Any varType;
				auto varValue = value.get_variant_value(&varType);
				if (varValue.type->kind != TypeInfoKind::NONE) {
					// Type is guaranteed to be an enum so no need to deallocate anything there
					deallocate_deep(allocator, varValue);
				}
			} else if (has_attribute(value.type, "array")) {
				for (i64 i = value.get_array_count(); i > 0; --i)
					deallocate_deep(allocator, value.get_element(i - 1));

				auto capacity = value.get_member("capacity");
				if (capacity.type->kind != TypeInfoKind::NONE && capacity.to_value<i64>()) {
					auto elem0 = value.get_element(0);
					allocator->deallocate(
							value.get_member("data").ptr_value(),
							type_size(elem0.type)*capacity.to_value<i64>());
				}
			} else {
				auto si = static_cast<StructTypeInfo const*>(value.type);
				for (auto it = si->fields.end() - 1; it != si->fields.begin() - 1; --it)
					deallocate_deep(allocator, value.get_member(it));
			}
		}
	}

}

