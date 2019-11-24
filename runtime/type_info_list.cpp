#include <oak_reflect/type_info_list.h>

namespace oak {

	void TypeCategory::init(TypeCategoryCreateInfo const& createInfo) {
		capacity = createInfo.typeCapacity;
		types.data = allocate<TypeInfo const*>(createInfo.allocator, capacity);
	}

	void TypeCategory::destroy(Allocator *allocator) {
		deallocate(allocator, types.data, capacity);
	}

	void TypeCategory::add_type(TypeInfo const* typeInfo) {
		assert(types.count < capacity);
		types[types.count++] = typeInfo;
	}

	i64 TypeCategory::type_index(TypeInfo const* typeInfo) const {
		for (i64 i = 0; i < types.count; ++i) {
			if (types[i] == typeInfo) {
				return i;
			}
		}
		return -1;
	}

	TypeInfo const* TypeCategory::find_type_with_name(String name) const {
		for (auto type : types) {
			if (type_name(type) == name) {
				return type;
			}
		}
		return nullptr;
	}

}

