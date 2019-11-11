#pragma once

#include <oak_util/types.h>
#include <oak_util/containers.h>

#include "type_info.h"

namespace oak {

	struct TypeInfoListCreateInfo {
	};

	struct TypeInfoList {

		i64 typeInfoDataCapacity = 0;

		Slice<TypeInfo const*> types;

		HashMap<u64, i64> uidToTypeInfoMap;

		void init(TypeInfoListCreateInfo const& createInfo);

		u64 type_id_from_name(String name);

	};

	struct TypeCatagoryCreateInfo {
		Allocator *allocator = nullptr;
		i64 typeCapacity = 0;
	};

	struct TypeCatagory {

		Slice<TypeInfo const*> types;
		HashMap<u64, i64> uidToIndexMap;

		void init(TypeCatagoryCreateInfo const& createInfo);
		void destroy(Allocator *allocator);

		void add_type(TypeInfo const *typeInfo);
		i64 type_index(TypeInfo const *typeInfo) const;
	};

}

