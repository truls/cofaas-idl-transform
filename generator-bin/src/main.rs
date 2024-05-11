use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
    str::FromStr,
};

use generator::IdfMapper;
use wit_component::WitPrinter;

use clap::Parser;

use anyhow::{anyhow, Error, Result};

struct ProtoArg {
    world_name: String,
    export_proto: PathBuf,
    import_proto: Option<PathBuf>,
}

impl FromStr for ProtoArg {
    type Err = &'static str;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let parts = value.split(":").collect::<Vec<&str>>();
        if parts.len() == 2  {
            Ok(ProtoArg{ world_name: parts[0].to_string(),
                         import_proto: None,
                         export_proto: PathBuf::from_str(parts[1]).unwrap(),
            })
        } else if parts.len() == 3  {
            Ok(ProtoArg{ world_name: parts[0].to_string(),
                         import_proto: Some(PathBuf::from_str(parts[2]).unwrap()),
                         export_proto: PathBuf::from_str(parts[1]).unwrap(),
            })
        } else {
            Err("Argument must be of format <world_name>:<export_proto>[:<import_proto>")
        }
    }
}

impl Clone for ProtoArg {

    fn clone(&self) -> Self {
        Self { world_name: self.world_name.clone(),
               export_proto: self.export_proto.clone(),
               import_proto: self.import_proto.clone()

        }
    }
}

#[derive(Parser)]
/// Generates a WIT interface definition from a gRPC protobuf definition
struct Cli {
    /// THe export protocol. Use format world_name:export_proto[:import_proto]
    #[arg(long, required = true, value_parser = clap::value_parser!(ProtoArg))]
    worlds: Vec<ProtoArg>,
    /// An optional output file (empty for stdout)
    #[arg(long)]
    output_file: Option<PathBuf>,
}

fn generate_wit(worlds: Vec<ProtoArg>) -> Result<String> {
    let mut mapper = IdfMapper::new();

    for world in worlds {
        let import_if = match world.import_proto {
            Some(x) => Some(mapper.push_protocol(x)?),
            None => None,
        };
        let export_if = mapper.push_protocol(world.export_proto)?;

        mapper.push_world(&world.world_name, import_if, Some(export_if));
    }

    let (resolver, pkg_id) = mapper.resolve()?;
    let pretty = WitPrinter::default().print(&resolver, pkg_id)?;

    Ok(pretty)
}

fn main() -> Result<()> {
    let args = Cli::parse();

    let res = generate_wit(args.worlds).map_err(|x| {
        anyhow!(format!(
            "Failed to generate WIT interface wit error: \n\n {x}"
        ))
    })?;

    match args.output_file {
        None => io::stdout().write_all(res.as_bytes()),
        Some(f) => File::open(f)?.write_all(res.as_bytes()),
    }
    .map_err(|x| anyhow!(format!("Failed to to write output with error {x}")))
}
