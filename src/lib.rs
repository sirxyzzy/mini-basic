extern crate pest;
#[macro_use]
extern crate log;

use std::path::Path;
use std::fs;

mod parser;

pub struct ParseOptions {
    pub pretty_print: bool
}

/// Parse a single file
pub fn parse_file<P: AsRef<Path>>(path: &P, options: &ParseOptions) -> Result<(), Box<dyn std::error::Error>> {
    trace!("Reading {}", path.as_ref().display());
    let source = fs::read_to_string(path)?; 
    trace!("Read {} characters", source.len());
    Ok(parser::parse_source(&source, options)?)
}