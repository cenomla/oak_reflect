#pragma once

#include <oak_util/types.h>
#include <oak_util/containers.h>

#include "type_info.h"

namespace oak {

	struct TypeCategoryCreateInfo {
		Allocator *allocator = nullptr;
		i64 typeCapacity = 0;
	};

	struct TypeCategory {
		Slice<TypeInfo const*> types;
		i64 capacity = 0;

		void init(TypeCategoryCreateInfo const& createInfo);
		void destroy(Allocator *allocator);

		void add_type(TypeInfo const *typeInfo);
		i64 type_index(TypeInfo const *typeInfo) const;

		TypeInfo const* find_type_with_name(String name, i64 *index = nullptr) const;
		TypeInfo const* find_type_with_uid(u64 uid, i64 *index = nullptr) const;
	};

}

