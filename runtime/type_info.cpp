#include <oak_reflect/type_info.h>

#include <oak_util/algorithm.h>

namespace oak {

namespace {
	constexpr String reflectAttribute = "reflect;";
}

	bool attribute_value_in_annotation(String annotation, String attribute, String *value) {
		// TODO: Make this function consteval in c++20 or maybe just optimize it using SIMD

		// States:
		// 0 = look for attribute
		// 1 = found attribute
		// 2 = get value
		// 3 = skip value
		i64 state = 0;
		i64 resultStart = -1;
		i64 i;

		for (i = reflectAttribute.count; i < annotation.count;) {
			switch (annotation[i]) {
				case ' ':
					// Skip spaces
					++i;
					break;
				case ',':
					// If we found the attribute finish parsing otherwise keep looking
					if (state == 1 || state == 2)
						goto doneParsing;
					else
						state = 0;
					++i;
					break;
				case '=':
					// If we have found an attribute name then switch to getting the attribute value
					// If we didn't find anything then skip this section
					if (state == 1) {
						state = 2;
						resultStart = i + 1;
					} else {
						state = 3;
					}
					++i;
					break;
				default:
					switch (state) {
						case 0:
							if (sub_slice(slc(annotation), i, i + attribute.count) == attribute) {
								// We potentially found the attribute, skip to it's value
								state = 1;
								i += attribute.count;
							} else {
								// This attribute doesn't match the one we're looking for, skip it
								state = 3;
								++i;
							}
							break;
						case 1:
							// The potential attribute match turns out to just be a sub string of the attribute we're
							// looking at, skip this section
							state = 3;
						case 2: case 3:
							++i;
							break;
					}
					break;
			}
		}

	doneParsing:
		if (state == 1) {
			// We found a matching attribute but it has no value
			if (value)
				*value = {};
			return true;
		}

		if (state == 2) {
			// We found a matching attribute and it's value is from resultStart to i
			if (value) {
				*value = { annotation.data + resultStart, i - resultStart };
			}
			return true;
		}

		return false;
	}

	Vector<String> attributes_in_annotation(String annotation) {
		Vector<String> result;
		auto attributeCount = slice_count<char const>(annotation, ',', reflectAttribute.count) + 1;
		result.reserve(temporaryAllocator, attributeCount);

		i64 end = annotation.count;
		for (i64 i = reflectAttribute.count - 1; i < end; ++i) {
			if (annotation[i] == ';' || annotation[i] == ',') {
				i64 j = i + 1;
				for (; j <= end; ++j) {
					if (j == end || annotation[j] == ',') {
						auto str = String{ annotation.data + i + 1, j - i - 1 };
						if (str.count)
							push(&result, str);
						break;
					}
				}
				i = j - 1;
			}
		}

		return result;
	}

}
