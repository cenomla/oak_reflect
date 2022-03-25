#pragma once

#include <oak_util/types.h>
#include <oak_util/containers.h>

#include "type_info.h"

namespace oak {

	struct OAK_REFLECT_API TypeCategory {
		Vector<TypeInfo const*> types;

		void add_type(TypeInfo const *typeInfo);
		i64 type_index(TypeInfo const *typeInfo) const;

		TypeInfo const* find_type_with_name(String name, i64 *index = nullptr) const;
		TypeInfo const* find_type_with_uid(u64 uid, i64 *index = nullptr) const;
	};

}

