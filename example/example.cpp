#include <cstdio>
#include <type_traits>

#include <oak_util/types.h>

#include "example.h"
#include "example.reflect.h"

int main(int, char const **) {


	static void (*v)(oak::TypeInfo const*,int) = [](oak::TypeInfo const* typeInfo, int depth = 0) {
		auto printDepth = [](int depth) {
			for (int i = 0; i < depth; ++i)
				std::printf("  ");
		};

		printDepth(depth);

		switch (typeInfo->kind) {
			case oak::TypeInfoKind::NONE:
				std::printf("noTypeInfo\n");
				break;
			case oak::TypeInfoKind::VOID:
				std::printf("void\n");
				break;
			case oak::TypeInfoKind::PRIMITIVE:
				std::printf("%s\n", static_cast<oak::PrimitiveTypeInfo const*>(typeInfo)->name.data);
				break;
			case oak::TypeInfoKind::POINTER:
				std::printf("Pointer to\n");
				v(static_cast<oak::PointerTypeInfo const*>(typeInfo)->to, depth + 1);
				break;
			case oak::TypeInfoKind::ARRAY:
				std::printf("Array of %li\n", static_cast<oak::ArrayTypeInfo const*>(typeInfo)->count);
				v(static_cast<oak::ArrayTypeInfo const*>(typeInfo)->of, depth + 1);
				break;
			case oak::TypeInfoKind::STRUCT:
				{
					auto si = static_cast<oak::StructTypeInfo const*>(typeInfo);
					std::printf("Struct - %s(%s)\n", si->name.data, si->annotation.data);
					for (auto const& field : si->fields) {
						printDepth(depth + 1);
						std::printf("Field[%li] - %s(%s)\n", field.offset, field.name.data, field.annotation.data);
						v(field.typeInfo, depth + 2);
					}
				}
				break;
			case oak::TypeInfoKind::UNION:
				{
					auto ui = static_cast<oak::UnionTypeInfo const*>(typeInfo);
					std::printf("Union - %s(%s)\n", ui->name.data, ui->annotation.data);
					for (auto const& field : ui->fields) {
						printDepth(depth + 1);
						std::printf("Field[%li] - %s(%s)\n", field.offset, field.name.data, field.annotation.data);
						v(field.typeInfo, depth + 2);
					}
				}
				break;
			case oak::TypeInfoKind::ENUM:
				{
					auto ei = static_cast<oak::EnumTypeInfo const*>(typeInfo);
					std::printf("Enum - %s(%s)\n", ei->name.data, ei->annotation.data);
					v(ei->underlyingType, depth + 1);
					for (auto const& constant : ei->constants) {
						printDepth(depth + 1);
						std::printf("Constant[%s] - %lu\n", constant.name.data, constant.value);
					}
				}
				break;
			case oak::TypeInfoKind::FUNCTION:
				{
					auto fi = static_cast<oak::FunctionTypeInfo const*>(typeInfo);
					std::printf("Function\n");

					printDepth(depth);
					std::printf("Returns\n");
					v(fi->returnTypeInfo, depth + 1);

					printDepth(depth);
					std::printf("Args\n");
					for (auto const& arg : fi ->argTypeInfos) {
						v(arg, depth + 1);
					}
				}
				break;
			case oak::TypeInfoKind::MEMBER_FUNCTION:
				{
					auto mfi = static_cast<oak::MemberFunctionTypeInfo const*>(typeInfo);
					std::printf("Function\n");

					printDepth(depth);
					std::printf("Class\n");
					v(mfi->classTypeInfo, depth + 1);

					printDepth(depth);
					std::printf("Returns\n");
					v(mfi->returnTypeInfo, depth + 1);

					printDepth(depth);
					std::printf("Args\n");
					for (auto const& arg : mfi ->argTypeInfos) {
						v(arg, depth + 1);
					}
				}
				break;
		}

	};

	v(&oak::Reflect<test::ClassDef>::typeInfo, 0);
	v(&oak::Reflect<test::TClassDef<float>>::typeInfo, 0);
	v(&oak::Reflect<UnionDef>::typeInfo, 0);
	v(&oak::Reflect<Things>::typeInfo, 0);
	v(&oak::Reflect<test::ClassSub>::typeInfo, 0);
	v(&oak::Reflect<TaggedUnionDef>::typeInfo, 0);

}
