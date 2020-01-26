#include <oak_reflect/any.h>

#include <cstring>

#include <oak_util/ptr.h>


namespace oak {

	Any Any::get_member(String name) noexcept {
		if (type->kind == TypeInfoKind::STRUCT) {
			auto si = static_cast<StructTypeInfo const*>(type);
			for (auto field : si->fields) {
				if (field.name == name) {
					return { add_ptr(ptr, field.offset), field.typeInfo };
				}
			}
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	Any Any::get_member(String name) const noexcept {
		if (type->kind == TypeInfoKind::STRUCT) {
			auto si = static_cast<StructTypeInfo const*>(type);
			for (auto field : si->fields) {
				if (field.name == name) {
					return { add_ptr(ptr, field.offset), field.typeInfo };
				}
			}
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	Any Any::get_member(FieldInfo const *field) noexcept {
		return { add_ptr(ptr, field->offset), field->typeInfo };
	}

	Any Any::get_member(FieldInfo const *field) const noexcept {
		return { add_ptr(ptr, field->offset), field->typeInfo };
	}

	Any Any::get_element(i64 index) noexcept {
		if (type->kind == TypeInfoKind::ARRAY) {
			auto ai = static_cast<ArrayTypeInfo const*>(type);
			if (index < ai->count) {
				return { add_ptr(ptr, index * type_size(ai->of)), ai->of };
			}
		} else if (has_attribute(type, "array")) {
			auto data = get_member("data");
			auto count = get_member("count");
			if (index < count.to_value<i64>()) {
				auto pi = static_cast<PointerTypeInfo const*>(data.type);
				return { add_ptr(data.to_value<void*>(), index * type_size(pi->to)), pi->to };
			}
		}
		return { nullptr, &Reflect<NoType>::typeInfo };
	}

	void Any::construct() noexcept {
		switch (type->kind) {
			case TypeInfoKind::STRUCT:
				{
					auto si = static_cast<StructTypeInfo const*>(type);
					if (si->defaultConstructFn) {
						si->defaultConstructFn(ptr);
					}
				} break;
			case TypeInfoKind::UNION:
				{
					auto ui = static_cast<UnionTypeInfo const*>(type);
					if (ui->defaultConstructFn) {
						ui->defaultConstructFn(ptr);
					}
				} break;
			default: break;
		}
	}

	void Any::set_enum_value(u64 ev) noexcept {
		assert(type->kind == TypeInfoKind::ENUM);
		auto ei = static_cast<EnumTypeInfo const*>(type);
		switch (ei->underlyingType->uid) {
			case Reflect<i8>::typeInfo.uid: to_value<i8>() = static_cast<i8>(ev); break;
			case Reflect<i16>::typeInfo.uid: to_value<i16>() = static_cast<i16>(ev); break;
			case Reflect<i32>::typeInfo.uid: to_value<i32>() = static_cast<i32>(ev); break;
			case Reflect<i64>::typeInfo.uid: to_value<i64>() = static_cast<i64>(ev); break;
			case Reflect<u8>::typeInfo.uid: to_value<u8>() = static_cast<u8>(ev); break;
			case Reflect<u16>::typeInfo.uid: to_value<u16>() = static_cast<u16>(ev); break;
			case Reflect<u32>::typeInfo.uid: to_value<u32>() = static_cast<u32>(ev); break;
			case Reflect<u64>::typeInfo.uid: to_value<u64>() = static_cast<u64>(ev); break;
			default: break;
		}
	}

	u64 Any::get_enum_value() const noexcept {
		assert(type->kind == TypeInfoKind::ENUM);
		auto ei = static_cast<EnumTypeInfo const*>(type);
		u64 ev = 0;
		switch (ei->underlyingType->uid) {
			case Reflect<i8>::typeInfo.uid: ev = static_cast<u64>(to_value<i8>()); break;
			case Reflect<i16>::typeInfo.uid: ev = static_cast<u64>(to_value<i16>()); break;
			case Reflect<i32>::typeInfo.uid: ev = static_cast<u64>(to_value<i32>()); break;
			case Reflect<i64>::typeInfo.uid: ev = static_cast<u64>(to_value<i64>()); break;
			case Reflect<u8>::typeInfo.uid: ev = to_value<u8>(); break;
			case Reflect<u16>::typeInfo.uid: ev = to_value<u16>(); break;
			case Reflect<u32>::typeInfo.uid: ev = to_value<u32>(); break;
			case Reflect<u64>::typeInfo.uid: ev = to_value<u64>(); break;
			default: break;
		}
		return ev;
	}

	Any Any::shallow_copy(Allocator *allocator) const {
		Any result;
		result.type = type;
		result.ptr = allocator->allocate(type_size(type), type_align(type));

		std::memcpy(result.ptr, ptr, type_size(type));

		return result;
	}

	Any Any::deep_copy(Allocator *allocator) const {
		Any result;
		result.type = type;
		result.ptr = allocator->allocate(type_size(type), type_align(type));

		copy_deep(result, *this, allocator);

		return result;
	}

	bool operator==(Any const& lhs, Any const& rhs) noexcept {
		if (lhs.type != rhs.type) {
			return false;
		}

		switch (lhs.type->kind) {
			case TypeInfoKind::NONE: return true;
			case TypeInfoKind::VOID: return true;
			case TypeInfoKind::PRIMITIVE: {
				switch (lhs.type->uid) {
					case Reflect<bool>::typeInfo.uid: return lhs.to_value<bool>() == rhs.to_value<bool>();
					case Reflect<i8>::typeInfo.uid: return lhs.to_value<i8>() == rhs.to_value<i8>();
					case Reflect<i16>::typeInfo.uid: return lhs.to_value<i16>() == rhs.to_value<i16>();
					case Reflect<i32>::typeInfo.uid: return lhs.to_value<i32>() == rhs.to_value<i32>();
					case Reflect<i64>::typeInfo.uid: return lhs.to_value<i64>() == rhs.to_value<i64>();
					case Reflect<u8>::typeInfo.uid: return lhs.to_value<u8>() == rhs.to_value<u8>();
					case Reflect<u16>::typeInfo.uid: return lhs.to_value<u16>() == rhs.to_value<u16>();
					case Reflect<u32>::typeInfo.uid: return lhs.to_value<u32>() == rhs.to_value<u32>();
					case Reflect<u64>::typeInfo.uid: return lhs.to_value<u64>() == rhs.to_value<u64>();
					case Reflect<f32>::typeInfo.uid: return lhs.to_value<f32>() == rhs.to_value<f32>();
					case Reflect<f64>::typeInfo.uid: return lhs.to_value<f64>() == rhs.to_value<f64>();
					default: return false;
				}
			};
			case TypeInfoKind::POINTER: return lhs.to_value<void*>() == rhs.to_value<void*>();
			case TypeInfoKind::ARRAY:
			{
				bool ret = true;
				auto ai = static_cast<ArrayTypeInfo const*>(lhs.type);
				for (i32 i = 0; i < ai->count; ++i) {
					if (Any{ add_ptr(lhs.ptr, i * type_size(ai->of)), ai->of }
							!= Any{ add_ptr(rhs.ptr, i * type_size(ai->of)), ai->of }) {
						ret = false;
						break;
					}
				}
				return ret;
			}
			case TypeInfoKind::STRUCT:
			{
				if (has_attribute(lhs.type, "array")) {
					bool ret = true;
					auto pi = static_cast<PointerTypeInfo const*>(lhs.get_member("data").type);
					auto& data0 = lhs.get_member("data").to_value<void*>();
					auto& count0 = lhs.get_member("count").to_value<i64>();
					auto& data1 = rhs.get_member("data").to_value<void*>();
					auto& count1 = rhs.get_member("count").to_value<i64>();
					if (count0 == count1) {
						for (i32 i = 0; i < count0; ++i) {
							if (Any{ add_ptr(data0, i * type_size(pi->to)), pi->to }
									!= Any{ add_ptr(data1, i * type_size(pi->to)), pi->to }) {
								ret = false;
								break;
							}
						}
					} else {
						ret = false;
					}
					return ret;
				} else {
					bool ret = true;
					auto si = static_cast<StructTypeInfo const*>(lhs.type);
					for (auto& field : si->fields) {
						if (Any{ add_ptr(lhs.ptr, field.offset), field.typeInfo }
								!= Any{ add_ptr(rhs.ptr, field.offset), field.typeInfo }) {
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
			case TypeInfoKind::ENUM: return lhs.get_enum_value() == rhs.get_enum_value();
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
					if (field0.typeInfo->kind == TypeInfoKind::STRUCT
							&& field1.typeInfo->kind == TypeInfoKind::STRUCT) {
						copy_fields(
								{ add_ptr(dst.ptr, field0.offset), field0.typeInfo },
								{ add_ptr(src.ptr, field1.offset), field1.typeInfo });
						break;
					} else if (field0.typeInfo->uid == field1.typeInfo->uid) {
						std::memcpy(add_ptr(dst.ptr, field0.offset), add_ptr(src.ptr, field1.offset), type_size(field0.typeInfo));
						break;
					}
				}
			}
		}
	}

	void copy_deep(Any dst, Any src, Allocator *allocator) {
		assert(dst.type && src.type);

		if (dst.type->kind == TypeInfoKind::STRUCT && !has_attribute(dst.type, "primitive")) {
			if (has_attribute(dst.type, "array")) {
				auto count = src.get_member("count");
				if (count.to_value<i64>()) {
					dst.get_member("count").to_value<i64>() = count.to_value<i64>();
					auto elem0 = src.get_element(0);
					dst.get_member("data").to_value<void*>() = allocator->allocate(
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
			std::memcpy(dst.ptr, src.ptr, type_size(dst.type));
		}
	}

}

