use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
};

use generator::generate_wit;

use clap::Parser;

use anyhow::{anyhow, Error};

#[derive(Parser)]
/// Generates a WIT interface definition from a gRPC protobuf definition
struct Cli {
    /// The proto file to read
    proto_file: PathBuf,
    /// An optional output file (empty for stdout)
    output_file: Option<PathBuf>,
}

fn main() -> Result<(), Error> {
    let args = Cli::parse();

    let res = generate_wit(args.proto_file).map_err(|x| {
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
