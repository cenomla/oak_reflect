#include <oak_reflect/type_info_list.h>

namespace oak {

	void TypeInfoList::init(TypeInfoListCreateInfo const& createInfo) {
	}

	u64 TypeInfoList::type_id_from_name(String name) {
		// TODO: complete this better
		return HashFn<String>{}(name);
	}


	void TypeCatagory::init(TypeCatagoryCreateInfo const& createInfo) {
		uidToIndexMap.init(createInfo.allocator, createInfo.typeCapacity);
		types.data = allocate<TypeInfo const*>(createInfo.allocator, uidToIndexMap.capacity);
	}

	void TypeCatagory::destroy(Allocator *allocator) {
		deallocate(allocator, types.data, uidToIndexMap.capacity);
		uidToIndexMap.destroy(allocator);
	}

	void TypeCatagory::add_type(TypeInfo const* typeInfo) {
		uidToIndexMap.insert(typeInfo->uid, types.count);
		types[types.count++] = typeInfo;
	}

	i64 TypeCatagory::type_index(TypeInfo const* typeInfo) const {
		if (auto idx = uidToIndexMap.find(typeInfo->uid); idx != -1) {
			return get<i64*>(uidToIndexMap.data)[idx];
		}
		return -1;
	}

}

