#include <oak_reflect/type_info_list.h>

namespace oak {

	void TypeInfoList::init(TypeInfoListCreateInfo const& createInfo) {
	}

	void TypeInfoList::parseTypeInfoData(Slice<byte> source) {
	}

	u64 TypeInfoList::type_id_from_name(String name) {
		// TODO: complete this better
		return HashFn<String>{}(name);
	}

}

