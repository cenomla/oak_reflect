#define OAK_REFLECT_EXPORT_SYMBOLS

#include <oak_reflect/type_info_list.h>

namespace oak {

	void TypeCategory::add_type(TypeInfo const* typeInfo) {
		// Don't add duplicates
		for (auto type : types) {
			if (type->uid == typeInfo->uid) {
				return;
			}
		}
		push(&types, typeInfo);
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
		if (index)
			*index = -1;
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
		if (index)
			*index = -1;
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

