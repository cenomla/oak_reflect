#include <oak_reflect/type_info_list.h>

namespace oak {

	void TypeCategory::init(TypeCategoryCreateInfo const& createInfo) {
		capacity = createInfo.typeCapacity;
		types.data = allocate<TypeInfo const*>(createInfo.allocator, capacity);
	}

	void TypeCategory::destroy(Allocator *allocator) {
		deallocate(allocator, types.data, capacity);
	}

	void TypeCategory::clear() {
		types.count = 0;
	}

	void TypeCategory::add_type(TypeInfo const* typeInfo) {
		assert(types.count < capacity);
		types[types.count++] = typeInfo;
	}

	i64 TypeCategory::type_index(TypeInfo const* typeInfo) const {
		for (i64 i = 0; i < types.count; ++i) {
			if (types[i]->uid == typeInfo->uid) {
				return i;
			}
		}
		return -1;
	}

	TypeInfo const* TypeCategory::find_type_with_name(String name, i64 *index) const {
		for (i64 i = 0; i < types.count; ++i) {
			auto type = types[i];
			if (type_name(type) == name) {
				if (index)
					*index = i;
				return type;
			}
		}
		return nullptr;
	}

	TypeInfo const* TypeCategory::find_type_with_uid(u64 uid, i64 *index) const {
		for (i64 i = 0; i < types.count; ++i) {
			auto type = types[i];
			if (type->uid == uid) {
				if (index)
					*index = i;
				return type;
			}
		}
		return nullptr;
	}

}

