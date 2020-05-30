extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate log;
extern crate thiserror;

use thiserror::Error;
use std::path::Path;
use std::fs;

mod parser;
mod ast;
mod pesthelpers;

use ast::AstBuilder;

type ParseResult<T> = std::result::Result<T, ParseError>;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Error parsing file: {source}")]
    SyntaxError {
        #[from]
        source: pest::error::Error<parser::Rule>,      
    },
    #[error("Error reading file: {source}")]
    Io {
        #[from]
        source: std::io::Error,
    },
    #[error("Error generating AST because {reason}")]
    AstError {
        reason: String,
        rule_type: parser::Rule,
        start_pos: usize,
        end_pos: usize    
    }
}

pub struct ParseOptions {
    pub pretty_print: bool
}

/// Parse a single file
pub fn parse_file<P: AsRef<Path>>(path: &P, options: &ParseOptions) -> ParseResult<()> {
    trace!("Reading {}", path.as_ref().display());
    let source = fs::read_to_string(path)?; 
    trace!("Read {} characters", source.len());

    let main_node = parser::parse_source(&source, options)?;

    let ast = AstBuilder::build(main_node)?;

    Ok(())
}