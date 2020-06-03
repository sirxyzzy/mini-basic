use super::*;
use super::ast::*;
use std::collections::{HashMap};

#[derive(PartialEq,Debug)]
pub enum State {
    Running, Stopped, Ended, Error(String)
}

type Number = f64;

pub struct Runner {
    lines: Vec<AstNode>,
    line_index: HashMap<u16, usize>,
    current: usize,  // index into lines...
    state: State,
    call_stack: Vec<usize>,
    numeric_vars: Vec<Option<Number>>,
    array_vars: Vec<Option<Vec<Number>>>,
    array_vars2: Vec<Option<Vec<Vec<Number>>>>,
    string_vars: Vec<Option<String>>,
}

impl Runner {
    pub fn new(program: AstNode) -> Runner {
        let lines = 
            match program { 
                AstNode::Program{lines} => lines,
                _ => panic!("Interpret can only run a Program!")
        };

        // For fast access, create a hashmap from line Number to index into lines
        let line_index: HashMap<u16, usize> = lines.iter().enumerate().map(|p| (get_line_number(p.1), p.0)).collect();

        Runner { 
            lines, 
            line_index, 
            current:0, 
            state:State::Stopped, 
            call_stack: Vec::new(),
            numeric_vars: vec![None; 11*26],
            array_vars: vec![None; 26],
            array_vars2: vec![None; 26],
            string_vars: vec![None; 26] 
        }
    }

    pub fn run(&mut self) -> Result<()> {
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

    fn set_current_line_number(&mut self, line_number: u16) -> Result<()> {
        let index = self.line_number_to_index(line_number)?;
        self.current = index;
        Ok(())
    }

    fn run_line(&mut self) -> Result<()> {
        trace!("Running line {} [{}]", self.current_line_number(), self.current);

        let mut next_index = self.current + 1;

        match &self.lines[self.current] {
            AstNode::EndStatement{..} => {
                self.state = State::Ended;
            }
            AstNode::GotoStatement{line_ref, ..} => {
                next_index = self.line_number_to_index(*line_ref)?;
            }
            AstNode::GosubStatement{line_ref, ..} => {
                next_index = self.line_number_to_index(*line_ref)?;
                self.call_stack.push(self.current);
            }
            AstNode::ReturnStatement{..} => {
                match self.call_stack.pop() {
                    Some(v) => next_index = v,
                    None => self.stop_running("Returned too many times! Stack empty"),
                }
            }
            AstNode::LetStatement{var, val, ..} => {
                match var.as_ref() {
                    AstNode::NumRef(id) => {
                        self.set_num_var(*id, self.eval(val.as_ref())?);
                    }
                    _ => return Err(self.runtime_error_unimplemented())
                }
            }
            _ => return Err(self.runtime_error_unimplemented())
        }

        assert!(next_index < self.lines.len(), "Should never fall off end!");

        trace!("Stepping from {} to {}", self.current, next_index);
        self.current = next_index;

        match &self.state {
            State::Error(r) => {
                error!("Runtime error");
                Err(Error::RuntimeError(r.clone(), self.current_line_number()) )
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

    fn eval(&self, expression: &AstNode) -> Result<Number> {
        match expression {
            AstNode::BinOp{op, left, right} => self.eval_binop(op, left, right),
            AstNode::MonOp{op, arg} => self.eval_monop(op, arg),
            AstNode::Op(op) => self.eval_op(op),
    
            AstNode::NumVal(x) => Ok(*x),
            AstNode::NumRef(id) =>  Ok(self.get_num_var(*id)?),
            AstNode::ArrayRef1{id, index} => self.get_array_var(*id, self.eval(index)?),
            AstNode::ArrayRef2{id, index1, index2} => self.get_array_var2(*id, 
                                                            self.eval(index1)?,
                                                            self.eval(index2)?), 

            x => panic!("Unexpected node in expression {:?} as bin_op", x)
        }

    }

    fn eval_binop(&self, op: &OpCode, left: &AstNode, right: &AstNode) -> Result<Number> {
        match op {
            OpCode::Plus => Ok(self.eval(left)? + self.eval(right)?),
            OpCode::Minus => Ok(self.eval(left)? - self.eval(right)?),
            OpCode::Multiply => Ok(self.eval(left)? * self.eval(right)?),
            OpCode::Divide => Ok(self.eval(left)? / self.eval(right)?),
            OpCode::Pow => Ok(self.eval(left)?.powf(self.eval(right)?)),
            o => panic!("Unexpected op {:?} as bin_op", o)
        }
    }

    fn eval_monop(&self, op: &OpCode, operand: &AstNode) -> Result<Number> {
        let v = self.eval(operand)?;
        match op {
            OpCode::Abs => Ok(v.abs()),
            OpCode::Atn => Ok(v.atan()),
            OpCode::Cos => Ok(v.cos()),
            OpCode::Exp => Ok(v.exp()),
            OpCode::Int => Ok(v.round()),
            OpCode::Log => Ok(v.log(10.0)),
            OpCode::Sgn => Ok(v.signum()),
            OpCode::Sin => Ok(v.sin()),
            OpCode::Sqr => Ok(v*v),
            OpCode::Tan => Ok(v.tan()),
            o => panic!("Unexpected op {:?} as mon_op", o)
        }
    }

    fn eval_op(&self, op: &OpCode) -> Result<Number> {
        match op {
            OpCode::Rnd => Ok(rand::random::<Number>()),
            o => panic!("Unexpected op {:?} as op", o)
        }
    }

    fn runtime_error(&self, reason: &str) -> Error {
        Error::RuntimeError(reason.to_string(), self.current_line_number() )
    }

    fn runtime_error_unimplemented(&self) -> Error {
        Error::RuntimeError("unimplemented".to_string(), self.current_line_number() )
    }

    pub fn current_line_number(&self) -> u16 {
        get_line_number(&self.lines[self.current])
    }

    fn index_to_line_number(&self, index: usize) -> Result<u16> {
        Ok(get_line_number(&self.lines[index]))
    }

    fn line_number_to_index(&self, line_number: u16) -> Result<usize> {
        match self.line_index.get(&line_number) {
            Some(i) => Ok(*i),
            None => Err(self.runtime_error(&format!("Invalid line Number: {}", line_number))) 
        }
    }

    fn check_index(&self, x: f64, max: usize) -> Result<usize> {    
        if x < 0.0 || x >= max as f64 {
            Err(self.runtime_error(&format!("Index {} out of range, 0..{}", x, max)))
        } else {
            Ok(x as usize)
        }
    }

    fn get_array_var(&self, var_index: usize, index: Number) -> Result<Number> {
        assert!(var_index < 26);
        match &self.array_vars[var_index] {
            Some(v) => Ok(v[self.check_index(index, v.len())?]),
            None => Err(self.runtime_error("Trying to access uninitialized variable"))
        }
    }

    fn get_array_var2(&self, var_index: usize, index1: Number, index2: Number) -> Result<Number> {
        assert!(var_index < 26);
        match &self.array_vars2[var_index] {
            Some(v) => {
                let v2 = &v[self.check_index(index1, v.len())?];
                Ok(v2[self.check_index(index2, v2.len())?]) 
            }
            None => Err(self.runtime_error("Trying to access uninitialized variable"))
        }
    }

    fn get_num_var(&self, index: usize) -> Result<Number> {
        assert!(index < 11 * 26);

        match self.numeric_vars[index] {
            Some(v) => Ok(v),
            None => Err(self.runtime_error("Trying to access uninitialized variable"))
        }
    }

    fn set_num_var(&mut self, index: usize, value: Number) {
        assert!(index < 11 * 26);

        self.numeric_vars[index] = Some(value);
    }

    fn set_string_var(&mut self, index: usize, value: String) {
        assert!(index < 11 * 26);

        self.string_vars[index] = Some(value);
    }
}
