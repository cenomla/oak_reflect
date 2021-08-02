#pragma once

#include "type_info.h"
#include "attribute.h"

namespace oak {

	struct _reflect() OAK_REFLECT_API Any {
		void *ptr = nullptr;
		TypeInfo const *type = nullptr;

		Any() = default;
		Any(void *ptr_, TypeInfo const *type_) : ptr{ ptr_ }, type{ type_ } {}

		Any get_member(String name, FieldInfo const ** info = nullptr) const noexcept;
		Any get_member(FieldInfo const *info) const noexcept;
		Any get_element(i64 index) const noexcept;

		void construct() noexcept;

		template<typename T>
		T const& to_value() const noexcept {
			return *static_cast<T const*>(ptr);
		}

		template<typename T>
		T& to_value() noexcept {
			return *static_cast<T*>(ptr);
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

