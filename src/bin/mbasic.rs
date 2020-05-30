#[macro_use]
extern crate log;
extern crate mini_basic;

use mini_basic::{parse_file, ParseOptions};
use std::path::Path;
use walkdir::WalkDir;
use std::time::Instant;
use std::fs::File;
use std::io::{BufRead, BufReader};


use clap::Clap;
/// Parse a BASIC file or files
/// This program uses env_logger, for detailed tracing set RUST_LOG=trace
#[derive(Clap)]
#[clap(version = "1.0", author = "Andy P++ <andy@failfree.net>")]
struct Opts {
    /// The path to a single BASIC file, or a folder full of test files
    #[clap(short, long)]
    source: String,

    /// Open an interactive shell
    #[clap(short, long)]
    interactive: bool,    

    /// Set verbose to list parsed files, and show the location of parse fails
    #[clap(short,long)]
    verbose: bool,

    /// Developer option, run the NBS test suite
    #[clap(long)]
    nbs: bool,

    // Pretty print the parse tree, the syntax of the program
    #[clap(short,long)]
    parse_tree: bool,  

    // Pretty print the AST tree, the semantics of the program
    #[clap(short,long)]
    ast: bool,  
}

/// See if a given path has one of a number of file extensions (case insensitive)
/// The list of extensions to match MUST all be lowercase
pub fn match_ext<P: AsRef<Path>>(path: &P, extensions: &[&str]) -> bool {
    if let Some(ext) = path.as_ref().extension() {
        if let Some(sext) = ext.to_str() {
            return extensions.contains(&sext.to_lowercase().as_str())
        }
    }
    false
}

fn first_line<P: AsRef<Path>>(path: &P) -> Result<String, Box<dyn std::error::Error>>
where P: AsRef<Path>, {
    let file = File::open(path)?;
    let mut line =String::new();
    let _ = BufReader::new(file).read_line(&mut line)?;
    Ok(line.trim().to_owned())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let now = Instant::now();

    let opts: Opts = Opts::parse();

    env_logger::init();

    if opts.interactive {
        println!("Running interactively");
    }

    let path = Path::new(&opts.source);

    let options = ParseOptions { show_parse_tree: opts.parse_tree, show_ast: opts.ast};

    if path.is_dir() {
        // Batch load of MIBs
        let mut parsed_ok = 0;
        let mut parse_failed = 0;
        let mut failed_expectation = 0;

        // Extensions we care about
        let extensions = vec!["bas", "basic"];

        for path in WalkDir::new(path).into_iter()
                 .filter_map(|e| e.ok())
                 .filter(|e| e.file_type().is_file())
                 .map(|e| e.into_path()) // Dir entries keep a file lock, so consume them into paths
                 .filter(|p| match_ext(p, &extensions)) {
            
            let mut expect_error = false;

            if opts.nbs {
                // Peek first line and only run test is the word "error" is not present!
                match first_line(&path) {
                    Ok(line) => {
                        if opts.verbose {
                            println!("NBS: {}",line);
                        }

                        if line.contains("ERROR") {
                            if opts.verbose {
                                println!("Expecting error for NBS: {}", line);
                            }
                            expect_error = true;
                        }
                        else
                        {

                        }
                    }
                    Err(e) => { 
                        error!("NBS file error: {}", e);
                        continue;
                    }
                }
            }

            match parse_file(&path, &options) {
                Ok(_info) => {
                    if expect_error {
                        error!("Parse succeeded for {} which was not expected", path.display());
                        failed_expectation += 1;
                    }
                    parsed_ok += 1;
                    if opts.verbose {
                        println!("Parsed {}", path.display());
                    }
                },
                Err(e) => {
                    if !expect_error {
                        failed_expectation += 1;
                        error!("Parse failed for {}", path.display());
                    }
                    parse_failed += 1;
                    if opts.verbose {
                        println!("{}: {}", path.display() ,e)
                    }
                }
            }
        }

        if opts.nbs {
            println!("{} files failed expected result", failed_expectation);
        }

        println!("{} files parsed, {} files failed to parse in {}ms", parsed_ok, parse_failed, now.elapsed().as_millis());
    } else {
        trace!("Parsing {}", path.display());
        match parse_file(&path, &options) {
            Err(e) => error!("Parse failed {}", e),
            Ok(_info) => ()
        }
        trace!("Took {}ms", now.elapsed().as_millis()); 
    }

    Ok(())
}

