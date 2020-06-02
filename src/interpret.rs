// Interpret depends heavily on the AST
use super::ast::*;
use std::collections::{HashMap};

#[derive(PartialEq,Debug)]
pub enum State {
    Running, Stopped, Ended
}

pub struct Runner {
    lines: Vec<AstNode>,
    line_index: HashMap<u16, usize>,
    current: usize,  // index into lines...
    state: State
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

        Runner { lines, line_index, current:0, state:State::Stopped}
    }

    pub fn run(&mut self) {
        // Normally we execute lines in order, so we use an iterator,
        // which we reset any time we do a jump
        self.state = State::Running;
        self.current = 0;     

        while self.state == State::Running {
            self.run_line(); // Run line updates "current"
        }
    }

    fn run_line(&mut self) {
        let line = &self.lines[self.current];

        trace!("Running {:?}", line);

        match line {
            AstNode::EndStatement{..} => {
                self.state = State::Ended;
            }
            _ => self.current += 1
        }

        assert!(self.current < self.lines.len(), "Should never fall off end!");
    }
}
