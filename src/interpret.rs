

use super::*;
use super::ast::*;
use std::collections::{HashMap};
use std::cell::RefCell;

#[derive(PartialEq,Debug,Clone)]
pub enum State {
    Running, Stopped, Ended, Error(String)
}

type Number = f64;

pub struct Runner {
    lines: Vec<AstNode>,
    line_index: HashMap<u16, usize>,

    state: RefCell<ProgramState>
}

pub struct ProgramState {
    current: usize,  // index into lines...
    run_state: State,
    call_stack: Vec<usize>,
    numeric_vars: Vec<Option<Number>>,
    array_vars: Vec<Option<Vec<Number>>>,
    array_vars2: Vec<Option<Vec<Vec<Number>>>>,
    string_vars: Vec<Option<String>>,
}

impl ProgramState {

}

impl Runner {
    fn run_state(&self) -> State {
        self.state.borrow().run_state.clone()
    }

    fn set_run_state(&self, state: State) {
        self.state.borrow_mut().run_state = state;
    }

    fn current(&self) -> usize {
        self.state.borrow().current
    }

    fn set_current(&self, current: usize) {
        self.state.borrow_mut().current = current;
    }

    fn push(&self, index: usize) {
        self.state.borrow_mut().call_stack.push(index)
    }

    fn pop(&self) -> Option<usize> {
        self.state.borrow_mut().call_stack.pop()
    }

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
            state: RefCell::new(ProgramState  {
                current:0, 
                run_state:State::Stopped, 
                call_stack: Vec::new(),
                numeric_vars: vec![None; 11*26],
                array_vars: vec![None; 26],
                array_vars2: vec![None; 26],
                string_vars: vec![None; 26] }) 
        }
    }

    pub fn run(&self) -> Result<()> {
        // Normally we execute lines in order, so we use an iterator,
        // which we reset any time we do a jump
        {
            let mut state = self.state.borrow_mut();

            // Initial state
            state.run_state = State::Running;
            state.current = 0;     
        }

        while self.run_line()? == State::Running {
            // Keep running more lines...
        }

        Ok(())
    }

    fn stop_running(&self, reason: &str) {
        self.set_run_state(State::Error(reason.to_string()))
    }

    fn set_current_line_number(&self, line_number: u16) -> Result<()> {
        let index = self.line_number_to_index(line_number)?;
        self.set_current( index);
        Ok(())
    }



    fn run_line(&self) -> Result<State> {
        trace!("Running line {} [{}]", self.current_line_number(), self.current());

        let current = self.current();

        let mut next_index = current + 1;

        let line = &self.lines[current];


        match line {
            AstNode::EndStatement{..} => {
                self.set_run_state(State::Ended);
            }
            AstNode::GotoStatement{line_ref, ..} => {
                next_index = self.line_number_to_index(*line_ref)?;
            }
            AstNode::GosubStatement{line_ref, ..} => {
                next_index = self.line_number_to_index(*line_ref)?;
                self.push(self.current());
            }
            AstNode::ReturnStatement{..} => {
                match self.pop() {
                    Some(v) => next_index = v,
                    None => self.stop_running("Returned too many times! Stack empty"),
                }
            }
            AstNode::LetStatement{var, val, ..} => {
                let v = var.as_ref(); 
                match v {
                    AstNode::NumRef(id) => {
                        self.set_num_var(*id, self.eval_numeric(val.as_ref())?);
                    }
                    x => return Err(self.runtime_unexpected_node(x))
                }
            }
            AstNode::PrintStatement{items, ..} => {
                for item in items {
                    match item {
                        AstNode::PrintComma => print!("\t"),
                        AstNode::PrintSemi =>  print!(" "),
                        AstNode::TabCall(_) => (/* what? */),
                        AstNode::StringExpression(expression) => print!("{}", self.eval_string(expression)?),
                        x => return Err(self.runtime_unexpected_node(&x))
                    }
                }
                println!();
            }
            x => return Err(self.runtime_unexpected_node(&x))
        }

        assert!(next_index < self.lines.len(), "Should never fall off end!");

        trace!("Stepping from {} to {}", self.current(), next_index);
        self.set_current(next_index);

        match self.run_state() {
            State::Error(r) => {
                error!("Runtime error");
                Err(Error::RuntimeError(r.clone(), self.current_line_number()) )
            }

            State::Running => Ok(State::Running),

            State::Ended => {
                info!("Ended");
                Ok(State::Ended)
            }
            State::Stopped => {
                info!("Stopped");
                Ok(State::Stopped)                
            }
        }
    }

    fn eval_string(&self, _expression: &AstNode) -> Result<String> {
        Ok("string thing".to_owned())
    }

    fn eval_numeric(&self, expression: &AstNode) -> Result<Number> {
        match expression { 
            AstNode::NumericExpression(box e) => {
                match e
                {
                    AstNode::BinOp{op, left, right} => self.eval_binop(op, left, right),
                    AstNode::MonOp{op, arg} => self.eval_monop(op, arg),
                    AstNode::Op(op) => self.eval_op(op),
            
                    AstNode::NumVal(x) => Ok(*x),
                    AstNode::NumRef(id) =>  Ok(self.get_num_var(*id)?),
                    AstNode::ArrayRef1{id, index} => self.get_array_var(*id, self.eval_numeric(index)?),
                    AstNode::ArrayRef2{id, index1, index2} => self.get_array_var2(*id, 
                                                                    self.eval_numeric(index1)?,
                                                                    self.eval_numeric(index2)?), 
        
                    x => panic!("Unexpected node in expression {:?} as bin_op", x)
                }
            }
            x => panic!("Expected numeric expression but got {:?}", x)
        }
    }

    fn eval_binop(&self, op: &OpCode, left: &AstNode, right: &AstNode) -> Result<Number> {
        match op {
            OpCode::Plus => Ok(self.eval_numeric(left)? + self.eval_numeric(right)?),
            OpCode::Minus => Ok(self.eval_numeric(left)? - self.eval_numeric(right)?),
            OpCode::Multiply => Ok(self.eval_numeric(left)? * self.eval_numeric(right)?),
            OpCode::Divide => Ok(self.eval_numeric(left)? / self.eval_numeric(right)?),
            OpCode::Pow => Ok(self.eval_numeric(left)?.powf(self.eval_numeric(right)?)),
            o => panic!("Unexpected op {:?} as bin_op", o)
        }
    }

    fn eval_monop(&self, op: &OpCode, operand: &AstNode) -> Result<Number> {
        let v = self.eval_numeric(operand)?;
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

    fn runtime_unexpected_node(&self, node: &AstNode) -> Error {
        Error::RuntimeError(format!("unexpected node {:?}", node), self.current_line_number() )
    }

    pub fn current_line_number(&self) -> u16 {
        get_line_number(&self.lines[self.current()])
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
        match &self.state.borrow().array_vars[var_index] {
            Some(v) => Ok(v[self.check_index(index, v.len())?]),
            None => Err(self.runtime_error("Trying to access uninitialized variable"))
        }
    }

    fn get_array_var2(&self, var_index: usize, index1: Number, index2: Number) -> Result<Number> {
        assert!(var_index < 26);
        match &self.state.borrow().array_vars2[var_index] {
            Some(v) => {
                let v2 = &v[self.check_index(index1, v.len())?];
                Ok(v2[self.check_index(index2, v2.len())?]) 
            }
            None => Err(self.runtime_error("Trying to access uninitialized variable"))
        }
    }

    fn get_num_var(&self, index: usize) -> Result<Number> {
        assert!(index < 11 * 26);

        match self.state.borrow().numeric_vars[index] {
            Some(v) => Ok(v),
            None => Err(self.runtime_error("Trying to access uninitialized variable"))
        }
    }

    fn set_num_var(&self, index: usize, value: Number) {
        assert!(index < 11 * 26);

        self.state.borrow_mut().numeric_vars[index] = Some(value);
    }

    fn set_string_var(&self, index: usize, value: String) {
        assert!(index < 11 * 26);

        self.state.borrow_mut().string_vars[index] = Some(value);
    }
}
