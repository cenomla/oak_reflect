#pragma once

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

		Any get_member(String name, FieldInfo const ** info = nullptr) const noexcept;
		Any get_member(FieldInfo const *info) const noexcept;
		CAny get_property(String name, PropertyInfo const ** info = nullptr) const noexcept;
		CAny get_property(PropertyInfo const *info) const noexcept;
		CAny get_member_or_property(String name) const noexcept;
		Any get_element(i64 index) const noexcept;

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

	OAK_REFLECT_API bool operator==(Any lhs, Any rhs) noexcept;

	inline bool operator!=(Any lhs, Any rhs) noexcept {
		return !(lhs == rhs);
	}

	OAK_REFLECT_API void copy_fields(Any dst, Any src);
	// TODO: Fix copy deep
	OAK_REFLECT_API void copy_deep(Any dst, Any src, Allocator *allocator);
}

