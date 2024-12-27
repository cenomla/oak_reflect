#pragma once

#include <oak_util/types.h>
#include <oak_util/hash.h>

namespace oak {

	namespace detail {

		template<usize C>
		constexpr u64 type_uid_from_literal(char const (&str)[C]) {
			return hash_string(str, C);
		}

	}

}

#define OAK_TYPE_UID(...) oak::detail::type_uid_from_literal(#__VA_ARGS__)

