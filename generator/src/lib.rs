use log::{trace};
use anyhow::{anyhow, Context, Error, Result};
use indexmap::IndexMap;
use itertools::Itertools;
use convert_case::{Case, Casing};
use protobuf::descriptor::{
    field_descriptor_proto::Type as ProtoType, DescriptorProto, EnumDescriptorProto,
    FileDescriptorProto, ServiceDescriptorProto,
};
use protobuf_parse::Parser;
use std::{
    collections::HashMap,
    fmt::Debug,
    path::{Path, PathBuf},
};
use thiserror::Error;
use wit_parser::{
    Docs, Enum, EnumCase, Field, Function, FunctionKind, Interface, InterfaceId,
    PackageId, PackageName, Record, Resolve, Results, Type, TypeDef, TypeDefKind, TypeId,
    TypeOwner, UnresolvedPackage,
};

const WIT_NAMESPACE: &str = "gen";

#[derive(Error, Debug)]
pub enum IdfMappingError {
    #[error("Mapping for type type `{0}` is not implemented")]
    UnimplementedType(String),
    #[error("Message must be named")]
    UnnamedMessage(),
    #[error("Unable to match `{0}` to protobuf enum")]
    UnmatchedEnum(i32),
    #[error("Protobuf type `{0}` must have an enum")]
    UnnamedType(String),
    #[error("Field in type `{0}` is unnamed")]
    UnnamedMessageField(String),
}

pub struct IdfMapper {
    orig_package: Option<String>,
    type_map: HashMap<String, TypeId>,
    pkg: UnresolvedPackage,
}

impl IdfMapper {
    fn lookup_type_id(&mut self, name: &String) -> Result<TypeId> {
        self.type_map
            .get(&IdfMapper::convert_case(name))
            .ok_or(anyhow!("Type name {} does not exist.", name))
            .copied()
    }

    // fn lookup_type(&mut self, name: &String) -> Result<&TypeDef> {
    //     let ty_id = self.lookup_type_id(name)?;
    //     self.pkg
    //         .types
    //         .get(ty_id)
    //         .ok_or(anyhow!("Type name {} not defined.", name))
    // }

    fn map_name(name: String) -> PackageName {
        PackageName {
            namespace: String::from(WIT_NAMESPACE),
            name,
            version: None,
        }
    }

    fn convert_case(name: &String) -> String {
        name.to_case(Case::Kebab)
    }

    fn map_type(proto_type: ProtoType) -> Result<Type, IdfMappingError> {
        match proto_type {
            ProtoType::TYPE_DOUBLE => Ok(Type::Float64),
            ProtoType::TYPE_FLOAT => Ok(Type::Float32),
            ProtoType::TYPE_INT64 => Ok(Type::S64),
            ProtoType::TYPE_UINT64 => Ok(Type::U64),
            ProtoType::TYPE_INT32 => Ok(Type::S32),
            ProtoType::TYPE_FIXED64 => Ok(Type::U64),
            ProtoType::TYPE_FIXED32 => Ok(Type::S32),
            ProtoType::TYPE_BOOL => Ok(Type::Bool),
            ProtoType::TYPE_STRING => Ok(Type::String),
            // ProtoType::TYPE_BYTES => Ok(TypeDefKind::List(Type::U8),
            ProtoType::TYPE_UINT32 => Ok(Type::U32),
            ProtoType::TYPE_SFIXED32 => Ok(Type::S32),
            ProtoType::TYPE_SFIXED64 => Ok(Type::S64),
            ProtoType::TYPE_SINT32 => Ok(Type::S32),
            ProtoType::TYPE_SINT64 => Ok(Type::S64),
            ProtoType::TYPE_BYTES => Err(IdfMappingError::UnimplementedType("bytes".to_string())),
            ProtoType::TYPE_MESSAGE => {
                Err(IdfMappingError::UnimplementedType("message".to_string()))
            }
            ProtoType::TYPE_ENUM => Err(IdfMappingError::UnimplementedType("enum".to_string())),
            ProtoType::TYPE_GROUP => Err(IdfMappingError::UnimplementedType("group".to_string())),
        }
    }

    /// Converts a protobuf message to a WIT record
    fn map_message(&mut self, message: &DescriptorProto, owner: TypeOwner) -> Result<TypeId> {
        let msg_name = message
            .name
            .as_ref()
            .map(IdfMapper::convert_case)
            .context(IdfMappingError::UnnamedMessage())?;
        let fields = message
            .field
            .iter()
            .map(|x| {
                let unmapped_type = x
                    .type_
                    .ok_or(IdfMappingError::UnnamedType(msg_name.clone()))?
                    .enum_value()
                    .map_err(|x| IdfMappingError::UnmatchedEnum(x))?;

                let mapped_type = Self::map_type(unmapped_type)?;
                let field_name = x
                    .name
                    .as_ref()
                    .map(IdfMapper::convert_case)
                    .ok_or(IdfMappingError::UnnamedMessageField(msg_name.clone()))?;

                Ok::<wit_parser::Field, IdfMappingError>(Field {
                    docs: Docs::default(),
                    name: field_name,
                    ty: mapped_type,
                })
            })
            .try_collect()?;
        Ok(self.pkg.types.alloc(TypeDef {
            docs: Docs::default(),
            kind: TypeDefKind::Record(Record { fields }),
            name: Some(msg_name),
            owner,
        }))
    }

    fn map_enum(&mut self, enum_def: &EnumDescriptorProto, owner: TypeOwner) -> Result<TypeId> {
        // TODO enum error
        let enum_name = enum_def
            .name
            .as_ref()
            .map(IdfMapper::convert_case)
            .context(IdfMappingError::UnnamedMessage())?;
        let cases = enum_def
            .value
            .iter()
            .map(|x| {
                let case_name = x
                    .name
                    .as_ref()
                    .map(IdfMapper::convert_case)
                    .ok_or(IdfMappingError::UnnamedType(enum_name.clone()))?;
                Ok::<EnumCase, IdfMappingError>(EnumCase {
                    docs: Docs::default(),
                    name: case_name,
                })
            })
            .try_collect()?;

        Ok(self.pkg.types.alloc(TypeDef {
            docs: Docs::default(),
            kind: TypeDefKind::Enum(Enum { cases }),
            name: Some(enum_name),
            owner,
        }))
    }

    fn update_type_owner(&mut self, ty_id: TypeId, owner: TypeOwner) -> Result<()> {
        let ty_ref = self
            .pkg
            .types
            .get_mut(ty_id)
            .ok_or(anyhow!("Invalid type reference"))?;

        println!("{:#?}", ty_ref.owner);
        if ty_ref.owner != TypeOwner::None && ty_ref.owner != owner {
            return Err(anyhow!(
                "Owner for type {} already set not to us",
                ty_ref.name.as_ref().unwrap()
            ));
        }
        ty_ref.owner = owner;

        Ok(())
    }

    fn map_service(&mut self, service: &ServiceDescriptorProto) -> Result<InterfaceId> {
        let service_name = service
            .name
            .as_ref()
            .map(IdfMapper::convert_case)
            .context(IdfMappingError::UnnamedMessage())?;
        let mut referenced_types: Vec<TypeId> = Vec::new();
        let mut func_map = IndexMap::<String, Function>::new();
        let funcs: Vec<Function> = service
            .method
            .iter()
            .map(|x| {
                let method_name = x
                    .name
                    .as_ref()
                    .map(IdfMapper::convert_case)
                    .ok_or(anyhow!("Method in service {} must be named.", service_name))?;
                let input_type = x.input_type.clone().map_or(Ok(Vec::new()), |x| {
                    let type_id = self.lookup_type_id(&x)?;
                    referenced_types.push(type_id);
                    Ok::<Vec<(String, Type)>, Error>(vec![("arg".to_string(), Type::Id(type_id))])
                })?;
                let output_type =
                    x.output_type
                        .clone()
                        .map_or(Ok(Results::Named(Vec::new())), |x| {
                            let type_id = self.lookup_type_id(&x)?;
                            referenced_types.push(type_id);
                            Ok::<Results, Error>(Results::Anon(Type::Id(type_id)))
                        })?;

                Ok::<Function, Error>(Function {
                    docs: Docs::default(),
                    name: method_name,
                    kind: FunctionKind::Method,
                    params: input_type,
                    results: output_type,
                })
            })
            .try_collect()?;

        for f in funcs {
            func_map.insert(f.name.clone(), f);
        }
        let mut type_map: IndexMap<String, TypeId> = IndexMap::new();
        for ty in referenced_types.iter() {
            match self.pkg.types.get(*ty) {
                Some(t) => {
                    let type_name = t.name.clone().unwrap();
                    type_map.insert(type_name, *ty);
                }
                None => {}
            }
        }

        let iface = self.pkg.interfaces.alloc(Interface {
            name: Some(service_name),
            docs: Docs::default(),
            types: type_map,
            functions: func_map,
            package: None,
        });

        for ty_id in referenced_types {
            self.update_type_owner(ty_id, TypeOwner::Interface(iface))?;
        }

        Ok(iface)
    }

    fn add_types_tp_map(&mut self, tys: &Vec<TypeId>) -> Result<()> {
        for t in tys {
            let ty_name = self
                .pkg
                .types
                .get(*t)
                .ok_or(anyhow!("Missing type reference"))?
                .name
                .clone()
                .context("Type without name. This shouldn't happen")?;

            if self.type_map.contains_key(&ty_name) {
                return Err(anyhow!("Type {} already exists", ty_name));
            }
            let ty_name_ = self.proto_type_ref(ty_name);
            println!("{}", ty_name_);
            assert!(self.type_map.insert(ty_name_, *t).is_none());
        }
        Ok(())
    }

    /// Returns the internal proto reference to a name.
    /// Maps name to .package.name
    fn proto_type_ref(&self, name: String) -> String {
        match self.orig_package.clone() {
            None => format!(".{}", name),
            Some(pkg) => format!(".{}.{}", pkg, name),
        }
    }

    fn map_protocol(&mut self, proto: &FileDescriptorProto) -> Result<()> {
        self.orig_package = proto.package.clone();
        //self.name = map+proto.package;

        let proto_name = proto
            .package
            .clone()
            .context("Protocol must have a package name")
            .map(IdfMapper::map_name)?;

        let records = proto
            .message_type
            .iter()
            // TODO: Set proper owner
            .map(|x| self.map_message(x, TypeOwner::None))
            .try_collect()?;
        self.add_types_tp_map(&records)?;

        let enums = proto
            .enum_type
            .iter()
            .map(|x| self.map_enum(x, TypeOwner::None))
            .try_collect()?;
        self.add_types_tp_map(&enums)?;

        let _ifaces: Vec<InterfaceId> = proto
            .service
            .iter()
            .map(|x| self.map_service(x))
            .try_collect()?;

        self.pkg.name = proto_name;

        Ok(())
    }

    pub fn parse() -> Result<Self> {
        let include_dir: PathBuf = [env!("CARGO_MANIFEST_DIR"), "examples"].iter().collect();
        let proto_path: PathBuf = include_dir.join("helloworld.proto");

        let upkg = UnresolvedPackage::parse(Path::new(""), "package foo:bar")?;
        let mut this = Self {
            orig_package: None,
            type_map: HashMap::new(),
            pkg: upkg,
        };

        let parsed = Parser::new()
            .pure()
            .include(include_dir)
            .input(proto_path)
            .parse_and_typecheck()?
            .file_descriptors;

        if parsed.len() != 1 {
            return Err(anyhow!("Can only handle a single protocol"));
        }

        this.map_protocol(&parsed[0])?;

        println! {"{:#?}", this.pkg};

        Ok(this)
    }

    pub fn resolve(&self) -> Result<(Resolve, PackageId)> {
        let mut resolver = Resolve::default();
        let pkg_id = resolver.push(self.pkg.clone())?;
        Ok((resolver, pkg_id))
    }
}

#[cfg(test)]
mod test_parser {
    use wit_component::WitPrinter;

    use super::*;

    #[test]
    fn can_parse() -> Result<()> {
        let result = IdfMapper::parse()?;
        let (resolver, pkg_id) = result.resolve()?;
        //println!("{:#?}", res);

        let pretty = WitPrinter::default().print(&resolver, pkg_id)?;

        println!("{}", pretty);

        Ok(())
    }

    #[test]
    fn test_map_name() {
        let result = IdfMapper::map_name(String::from("foo"));
        assert_eq!(
            format!("{}:{}", WIT_NAMESPACE, "foo"),
            format!("{}", result)
        );
    }
}
