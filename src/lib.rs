#![allow(dead_code)]

extern crate pest;

#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate log;

extern crate thiserror;

extern crate itertools;


mod parser;
mod ast;
mod vars;
mod interpret;

use thiserror::Error;
use std::path::Path;
use std::fs;
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

    #[error("Error generating AST because {reason} at {start_pos}..{end_pos}")]
    AstError {
        reason: String,
        rule_type: parser::Rule,
        start_pos: usize,
        end_pos: usize    
    },

    #[error("Error {reason}")]
    ValidationError {
        reason: String 
    },

    #[error("Error {reason}")]
    RuntimeError {
        reason: String,
        line_number: u16
    },
}

pub struct ParseOptions {
    pub show_parse_tree: bool,
    pub show_ast: bool
}

/// Parse a single file
pub fn parse_file<P: AsRef<Path>>(path: &P, options: &ParseOptions) -> ParseResult<()> {
    
    trace!("Reading {}", path.as_ref().display());
    let source = fs::read_to_string(path)?; 
    trace!("Read {} characters", source.len());

    trace!("Parsing");
    let parse_tree = parser::parse_source(&source, options)?;

    if options.show_parse_tree {
        parser::print_pair(&parse_tree);
    }

    trace!("Building AST");
    let ast = AstBuilder::build(parse_tree, options)?;

    if options.show_ast {
        ast::print_ast(&ast);
    }

    trace!("Executing program");
    let mut runner = interpret::Runner::new(ast);

    runner.run();

    Ok(())
}