#define OAK_REFLECT_EXPORT_SYMBOLS
#include <oak_reflect/any.h>

#include <string.h>

#include <oak_util/ptr.h>


namespace oak {

namespace {

	i64 get_any_struct_array_count(Any any) {
		if (auto count = any.get_member("count"); count.type->kind != TypeInfoKind::NONE)
			return count.to_value<i64>();

		if (auto capacity = any.get_member_or_property("capacity"); capacity.type->kind != TypeInfoKind::NONE)
			return capacity.to_value<i64>();

		return 0;
	}

}

	Any Any::get_member(String name, FieldInfo const ** info) const noexcept {
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
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	Any Any::get_member(FieldInfo const *field) const noexcept {
		return { add_ptr(ptr, field->offset), field->typeInfo };
	}

	CAny Any::get_property(String name, PropertyInfo const ** info) const noexcept {
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

	CAny Any::get_property(PropertyInfo const *info) const noexcept {
		return { info->data, info->typeInfo };
	}

	CAny Any::get_member_or_property(String name) const noexcept {
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
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	i64 Any::get_array_count() const noexcept {
		if (type->kind == TypeInfoKind::ARRAY) {
			return static_cast<ArrayTypeInfo const*>(type)->count;
		} else if (type->kind == TypeInfoKind::STRUCT) {
			return get_any_struct_array_count(*this);
		}
		return 0;
	}

	Any Any::get_element(i64 index) const noexcept {
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

	void Any::construct() noexcept {
		switch (type->kind) {
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

	void Any::set_enum_value(u64 ev) noexcept {
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

	u64 Any::get_enum_value() const noexcept {
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

	Any Any::shallow_copy(Allocator *allocator) const {
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

	bool operator==(Any lhs, Any rhs) noexcept {
		if (lhs.type != rhs.type)
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
				// TODO: Union compare
				return false;
			};
			case TypeInfoKind::ENUM:
				return lhs.get_enum_value() == rhs.get_enum_value();
			default:
				return false;
		}
	}

	void copy_fields(Any dst, Any src) {
		assert(dst.type && src.type);

		if (dst.type->kind != TypeInfoKind::STRUCT
				|| src.type->kind != TypeInfoKind::STRUCT) {
			return;
		}

		auto si0 = static_cast<StructTypeInfo const*>(dst.type);
		auto si1 = static_cast<StructTypeInfo const*>(src.type);
		for (auto field0 : si0->fields) {
			for (auto field1 : si1->fields) {
				if (field0.name == field1.name && !has_attribute(&field0, "volatile")) {
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
		}
	}

	/*
	void copy_deep(Any dst, Any src, Allocator *allocator) {
		assert(dst.type && src.type);

		if (dst.type->kind == TypeInfoKind::STRUCT) {
			if (has_attribute(dst.type, "array")) {
				auto count = src.get_member("count");
				if (count.to_value<i64>()) {
					dst.get_member("count").to_value<i64>() = count.to_value<i64>();
					auto elem0 = src.get_element(0);
					dst.get_member("data").ptr_value() = allocator->allocate(
							type_size(elem0.type) * count.to_value<i64>(), type_align(elem0.type));
					for (i64 i = 0; i < count.to_value<i64>(); ++i) {
						copy_deep(dst.get_element(i), src.get_element(i), allocator);
					}
				}
			} else {
				auto si = static_cast<StructTypeInfo const*>(dst.type);
				for (auto field : si->fields) {
					copy_deep(dst.get_member(&field), src.get_member(&field), allocator);
				}
			}
		} else {
			memcpy(dst.ptr, src.ptr, type_size(dst.type));
		}
	}
	*/

}
