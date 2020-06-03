use super::*;
use super::ast::*;
use std::collections::{HashMap};

#[derive(PartialEq,Debug)]
pub enum State {
    Running, Stopped, Ended, Error(String)
}

pub struct Runner {
    lines: Vec<AstNode>,
    line_index: HashMap<u16, usize>,
    current: usize,  // index into lines...
    state: State,
    call_stack: Vec<usize>,
    numeric_vars: Vec<Option<f32>>,
    array_vars: Vec<Option<Vec<f32>>>,
    string_vars: Vec<Option<String>>,
}

impl Runner {
    pub fn new(program: AstNode) -> Runner {
        let lines = 
            match program { 
                AstNode::Program{lines} => lines,
                _ => panic!("Interpret can only run a Program!")
        };

        // For fast access, create a hashmap from line number to index into lines
        let line_index: HashMap<u16, usize> = lines.iter().enumerate().map(|p| (get_line_number(p.1), p.0)).collect();

        Runner { 
            lines, 
            line_index, 
            current:0, 
            state:State::Stopped, 
            call_stack: Vec::new(),
            numeric_vars: vec![None; 11*26],
            array_vars: vec![None; 26],
            string_vars: vec![None; 26] 
        }
    }

    pub fn run(&mut self) -> Result<(), ParseError> {
        // Normally we execute lines in order, so we use an iterator,
        // which we reset any time we do a jump
        self.state = State::Running;
        self.current = 0;     

        while self.state == State::Running {
            self.run_line()?; // Run line updates "current"
        }
        Ok(())
    }

    fn stop_running(&mut self, reason: &str) {
        self.state = State::Error(reason.to_string());
    }

    fn set_current_line_number(&mut self, line_number: u16) -> ParseResult<()> {
        let index = self.line_number_to_index(line_number)?;
        self.current = index;
        Ok(())
    }

    fn run_line(&mut self) -> ParseResult<()> {
        trace!("Running line {} [{}]", self.current_line_number(), self.current);

        let mut next_index = self.current + 1;

        match self.lines[self.current] {
            AstNode::EndStatement{..} => {
                self.state = State::Ended;
            }
            AstNode::GotoStatement{line_ref, ..} => {
                next_index = self.line_number_to_index(line_ref)?;
            }
            AstNode::GosubStatement{line_ref, ..} => {
                next_index = self.line_number_to_index(line_ref)?;
                self.call_stack.push(self.current);
            }
            AstNode::ReturnStatement{..} => {
                match self.call_stack.pop() {
                    Some(v) => next_index = v,
                    None => self.stop_running("Returned too many times! Stack empty"),
                }
            }
            _ => self.current += 1
        }

        assert!(next_index < self.lines.len(), "Should never fall off end!");

        trace!("Stepping from {} to {}", self.current, next_index);
        self.current = next_index;

        match &self.state {
            State::Error(r) => {
                error!("Runtime error");
                Err(ParseError::RuntimeError{reason: r.clone(), line_number: self.current_line_number() } )
            }

            State::Running => Ok(()),

            State::Ended => {
                info!("Ended");
                Ok(())
            }
            State::Stopped => {
                info!("Stopped");
                Ok(())                
            }
        }
    }

    fn get_error(&self, reason: &str) -> ParseError {
        ParseError::RuntimeError{reason: reason.to_string(), line_number: self.current_line_number() } 
    }

    pub fn current_line_number(&self) -> u16 {
        get_line_number(&self.lines[self.current])
    }

    fn index_to_line_number(&self, index: usize) -> ParseResult<u16> {
        Ok(get_line_number(&self.lines[index]))
    }

    fn line_number_to_index(&self, line_number: u16) -> ParseResult<usize> {
        match self.line_index.get(&line_number) {
            Some(i) => Ok(*i),
            None => Err(self.get_error(&format!("Invalid line number: {}", line_number))) 
        }
    }
}