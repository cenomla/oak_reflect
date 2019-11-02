#pragma once

#include <oak_util/types.h>
#include <oak_util/containers.h>

#include "type_info.h"

namespace oak {

	struct TypeInfoListCreateInfo {
		Slice<byte> source;
	};

	struct TypeInfoList {

		i64 typeInfoDataCapacity = 0;

		HashMap<u64, TypeInfo const*> idToTypeInfoMap;

		void init(TypeInfoListCreateInfo const& createInfo);

		void parseTypeInfoData(Slice<byte> source);

		u64 type_id_from_name(String name);

	};

}

