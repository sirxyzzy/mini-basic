#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate log;

#[macro_use]
extern crate anyhow;

extern crate mini_basic;
extern crate regex;

use mini_basic::{run_file, ParseOptions};
use std::path::{ Path, PathBuf };
use walkdir::WalkDir;
use std::time::Instant;
use std::fs::File;
use std::io::{BufRead, BufReader};
use clap::Clap;
use anyhow::{Result};
use regex::Regex;
use std::io;
use std::io::Write;

/// Parse a BASIC file or files
/// This program uses env_logger, for detailed tracing set RUST_LOG=trace
#[derive(Clap)]
#[clap(version = "1.0", author = "Andy P++ <andy@failfree.net>")]
struct Opts {
    /// The path to a single BASIC file, or a folder full of test files
    #[clap(short, long)]
    source: PathBuf,

    /// Open an interactive shell
    // #[clap(short, long)]
    // interactive: bool,    

    /// Set verbose to list parsed files, and show the location of parse fails
    #[clap(short,long)]
    verbose: bool,

    /// Pretty print the parse tree, the syntax of the program
    #[clap(short,long)]
    parse_tree: bool,  

    /// Pretty print the AST tree, the semantics of the program
    #[clap(short,long)]
    ast: bool,
    
    /// Specify lowest test to run
    #[clap(short,long)]
    from: Option<usize>,

    /// Specify highest test to run
    #[clap(short,long)]
    to: Option<usize>,

    /// Read a line between NBS tests
    #[clap(long)]
    pause: bool,

    /// Run NBS tests
    #[clap(long)]
    nbs: bool
}

lazy_static! {
    static ref OPTS: Opts = Opts::parse();
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

fn get_nbs_header<P: AsRef<Path>>(path: &P) -> Result<Vec<String>>
where P: AsRef<Path> {
    trace!("Reading header from {}", path.as_ref().display());
    let file = File::open(path)?;
    let mut lines: Vec<String> = Vec::new();
    let mut reader = BufReader::new(file);
    for _i in 1..10 {
        let mut line = String::new();
        let result = reader.read_line(&mut line)?;

        if result == 0 || line.contains("BEGIN TEST") {
            trace!("End header after {} lines", lines.len());
        } else {
            lines.push(line);
        }
    }

    Ok(lines)
}
 
fn print_header(lines: &Vec<String>) {
    for line in lines {
        println!("{}", line.trim());
    }
}

fn get_test_number<P: AsRef<Path>>(path: &P) -> Option<usize> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r#"\d+"#).unwrap();
    }

    let path = path.as_ref().to_string_lossy();

    match RE.find(&path) {
        None => None,
        Some(m) => {
            match m.as_str().parse::<usize>() {
                Ok(s) => Some(s),
                Err(_) => None
            }

        }
    }
}

fn check_nbs<P: AsRef<Path>>(path: &P) -> Option<(bool, Vec<String>)> {
    let mut expect_error = false; 
    let nbs_header: Vec<String>;

    let file_number = match get_test_number(path) {
        Some(n) => n,
        None => return None
    };

    if let Some(from) = OPTS.from {
        if file_number < from {
            trace!("Skipping {}", path.as_ref().display());
            return None;
        }
    }

    if let Some(to) = OPTS.to {
        if file_number >= to {
            trace!("Skipping {}", path.as_ref().display());
            return None;
        }        
    }

    // Try to read test header
    match get_nbs_header(&path) {
        Ok(h) => nbs_header = h,
        Err(e) => {
            error!("NBS file error: {}", e);
            return None;
        }
    }

    // Peek first line to determine whether we expect test to run, or not
    let first_line = nbs_header.first();

    match first_line {
        Some(line) => {
            if OPTS.verbose {
                println!("NBS: {}", line);
            }

            if line.contains("ERROR") {
                if OPTS.verbose {
                    println!("Found NBS: {}", line);
                }
                expect_error = true;
            }
        }
        None => { 
            error!("NBS file could not be read {:?}", path.as_ref().display());
            return None;
        }
    }

    Some((expect_error, nbs_header))
}

fn read_line() -> String {
    let mut response = String::new();
    io::stdout().write_all(b"PAUSED> ").unwrap();
    let _ = io::stdout().flush();
    match io::stdin().read_line(&mut response) {
        Ok(_) => response.trim().to_string(),
        Err(e) => panic!("Why can't I read input! {}", e)
    }
}

fn main() -> Result<()> {
    let now = Instant::now();

    // Static, get at it anywhere
    // opts = Opts::parse();

    env_logger::init();
  
    let path = OPTS.source.clone();
    
    let options = ParseOptions { show_parse_tree: OPTS.parse_tree, show_ast: OPTS.ast};

    if OPTS.nbs  {

        if !path.is_dir() {
            error!("Path must be a folder to run the nbs test suite!");
            return Err(anyhow!("Path must be a folder to run the nbs test suite!"));
        }
            // Batch load of MIBs
        let mut parsed_ok = 0;
        let mut parse_failed = 0;
        let mut failed_expectation = 0;

        // Extensions we care about
        let extensions = vec!["bas", "basic"];

        for path in WalkDir::new(&path).into_iter()
                    .filter_map(|e| e.ok())
                    .filter(|e| e.file_type().is_file())
                    .map(|e| e.into_path()) // Dir entries keep a file lock, so consume them into paths
                    .filter(|p| match_ext(p, &extensions)) {
            
            trace!("Considering {}", path.display());

            match check_nbs(&path) {
                None => continue,
                Some((expect_error, nbs_header)) => {
                    match run_file(&path, &options) {
                        Ok(_info) => {
                            if expect_error {
                                error!("Parse succeeded for {} which was not expected", path.display());
                                failed_expectation += 1;
                                if OPTS.verbose {
                                    print_header(&nbs_header);
                                }
                            }
                            parsed_ok += 1;
                            if OPTS.verbose {
                                println!("Parsed {}", path.display());
                            }
                        },
                        Err(e) => {
                            if !expect_error {
                                failed_expectation += 1;
                                error!("Parse failed for {}", path.display());
                                if OPTS.verbose {
                                    print_header(&nbs_header);
                                }
                            }
                            parse_failed += 1;
                            if OPTS.verbose {
                                println!("{}: {}", path.display() ,e)
                            }
                        }
                    }

                    if OPTS.pause {
                        read_line();
                    }         
                }
            }
        }

        println!("{} files parsed, {} files failed to parse in {}ms", parsed_ok, parse_failed, now.elapsed().as_millis());
        println!("{} files did not get expected result", failed_expectation); 

    } else {
        // Normal run (not NBS)

        if path.is_dir() {
            // Batch load of MIBs
            let mut parsed_ok = 0;
            let mut parse_failed = 0;
    
            // Extensions we care about
            let extensions = vec!["bas", "basic"];
    
            for path in WalkDir::new(&path).into_iter()
                     .filter_map(|e| e.ok())
                     .filter(|e| e.file_type().is_file())
                     .map(|e| e.into_path()) // Dir entries keep a file lock, so consume them into paths
                     .filter(|p| match_ext(p, &extensions)) {
                
                trace!("Considering {}", path.display());   
    
                match run_file(&path, &options) {
                    Ok(_info) => {
                        parsed_ok += 1;
                        if OPTS.verbose {
                            println!("Parsed {}", path.display());
                        }
                    },
                    Err(e) => {
                        error!("Parse failed for {}", path.display());
                        parse_failed += 1;
                        if OPTS.verbose {
                            println!("{}: {}", path.display() ,e)
                        }
                    }
                }

                if OPTS.pause {
                    read_line();
                }
            }
    
            println!("{} files parsed, {} files failed to parse in {}ms", parsed_ok, parse_failed, now.elapsed().as_millis());
        } else {
            trace!("Parsing {}", path.display());
            match run_file(&path, &options) {
                Err(e) => error!("{}", e),
                Ok(_info) => ()
            }
            trace!("Took {}ms", now.elapsed().as_millis()); 
        }
    
    }


    Ok(())
}