

use super::*;
use super::ast::*;
use std::collections::{HashMap};
use std::cell::RefCell;
use std::io;
use vm::*;

#[derive(PartialEq,Debug,Clone)]
pub enum State {
    Running, Stopped, Ended, Error(String)
}

pub type Number = f64;

pub struct Runner {
    lines: Vec<AstNode>,
    line_index: HashMap<u16, usize>, // Line number to index of line in lines vector

    state: RefCell<vm::VirtualMachine>
}

impl Runner {
    pub fn new(program: AstNode) -> Runner {
        let lines = 
            match program { 
                AstNode::Program{lines} => lines,
                _ => panic!("Interpret can only run a Program!")
        };

        let line_index: HashMap<u16, usize> = lines.iter().enumerate().map(|p| (get_line_number(p.1), p.0)).collect();

        Runner { 
            lines, 
            line_index, 
            state: RefCell::new(vm::VirtualMachine::new())
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

    fn read_line(&self) -> Result<String> {
        let mut response = String::new();
        io::stdin().read_line(&mut response)?;
        Ok(response)
    }

    fn declare_array1(&self, id:VarId, bound: usize) -> Result<()> {
        trace!("Declaring array {}[{}]", id, bound);
        self.state.borrow_mut().array_vars.declare(id, bound)    
    }

    fn declare_array2(&self, id:VarId, bound1: usize, bound2: usize) -> Result<()> {
        trace!("Declaring array {}[{},{}]", id, bound1, bound2);

        let mut state = self.state.borrow_mut();
        state.array2_vars.declare(id, bound1, bound2)
    }

    fn run_line(&self) -> Result<State> {
        let current = self.current();

        let mut next_index = current + 1;

        let line = &self.lines[current];

        trace!("{}: Line {}", current, self.current_line_number());

        match line {
            AstNode::DimensionStatement{declarations, ..} => {
                for d in declarations {
                    match d {
                        AstNode::ArrayDecl1{id, bound} => self.declare_array1(*id, *bound)?,
                        AstNode::ArrayDecl2{id, bound1, bound2} => self.declare_array2(*id, *bound1, *bound2)?,
                        x => return Err(self.runtime_unexpected_node(x))
                    }
                }
            }
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
                match var {
                    box AstNode::NumRef(id) => {
                        let value = self.evaluate_numeric(val)?;
                        trace!("Assigning {} to {}", value, id);
                        self.set_num_var(*id, value);
                    }
                    box AstNode::ArrayRef1{id, index} => {
                        let index = self.evaluate_numeric(index)?;
                        let value = self.evaluate_numeric(val)?;

                        trace!("Assigning {} to {}[{}]", value, id, index);
                        self.set_num_var(*id, value);

                    }
                    box AstNode::ArrayRef2{id, index1, index2} => {
                        let index1 = self.evaluate_numeric(index1)?;
                        let index2 = self.evaluate_numeric(index2)?;
                        let value = self.evaluate_numeric(val)?;

                        trace!("Assigning {} to {}[{},{}]", value, id, index1, index2);
                        self.set_num_var(*id, value);
                    }
                    
                    box AstNode::StringRef(id) => {
                        let value = self.evaluate_string(val.as_ref())?;

                        trace!("Assigning {} to {}", value, id);
                        self.set_string_var(*id, value);                       
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
                        AstNode::StringExpression(expression) => print!("{}", self.evaluate_string(expression)?),
                        AstNode::NumericExpression(expression) => print!("{}", self.evaluate_numeric(expression)?),
                        x => return Err(self.runtime_unexpected_node(&x))
                    }
                }
                println!();
            }
            AstNode::RemarkStatement{..} => (),
            AstNode::DataStatement{..} => (),

            AstNode::InputStatement{vars,..} => {
                for v in vars {
                    match v {
                        AstNode::StringRef(id,..) => self.set_string_var(*id, self.read_line()?),
                        AstNode::NumRef(id,..) =>  {
                            let number = self.read_line()?.trim().parse::<f64>()?;
                            self.set_num_var(*id, number)
                        }
                        x => return Err(self.runtime_unexpected_node(&x))
                    }
                }
            }

            AstNode::ForStatement{id, from, to, step, line} => {
                let from_value = self.evaluate_numeric(from)?;
                let to_value = self.evaluate_numeric(to)?;
                let step_value = match step {
                    Some(step_exp) => self.evaluate_numeric(step_exp)?,
                    None => 1.0
                };

                // The initial value
                self.set_num_var(*id, from_value);

                self.push_for(
                    vm::ForContext {
                        for_line: self.current(),
                        limit: to_value,
                        step: step_value });

                trace!("Started {} FOR {} = {} TO {} STEP {}", line, id, from_value, to_value, step_value);
            }

            AstNode::NextStatement{id, line} => {
                let context = self.peek_for();

                match context {
                    Some(context) => {
                        let v = self.get_num_var(*id).expect("Where is my for loop index variable?");

                        let step = context.step;

                        let v1 = v + step;

                        // Even if we hit our limit, the index variable should be seen to be incremented
                        self.set_num_var(*id, v1);

                        let ended = 
                            if step < 0.0 {
                                // Negative range, not sure if that is allowed!
                                v1 < context.limit
                            } else {
                                // Positive range
                                v1 > context.limit
                            };

                        if !ended {
                            trace!("NEXT {} is {}", id, v1);
                            // Loop back to for statement
                            next_index = context.for_line + 1; // goto line right after for statement
                        } else {
                            self.pop_for();
                            trace!("Ended {} NEXT {}", line, id);
                        }
                    },
                    None => return Err(self.runtime_error("Encountered next outside for loop!"))
                }
            }

            AstNode::IfThenStatement{expr, then, line} => {
                let condition = self.evaluate_relational(expr)?;
                if condition {
                    next_index = self.line_number_to_index(*then)?;
                    trace!("Branching {} IF THEN {}", line, then)
                }    
            }

            AstNode::DefStatement{..} => () ,

            x => return Err(self.runtime_unexpected_node(&x))
        }

        assert!(next_index < self.lines.len(), "Should never fall off end!");

        if next_index != current + 1 {
            trace!("Jumping to {} (index={})", self.index_to_line_number(next_index), next_index)
        }

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

    fn evaluate_string(&self, expression: &AstNode) -> Result<String> {
        match expression
        {
            AstNode::StringExpression(e) => self.evaluate_string(e), // We may be wrapped in a StringExpression node
            AstNode::StringRef(id) => Ok(self.string_var(*id)?.clone()),
            AstNode::StringVal(v) => Ok(v.clone()),       
            x => panic!("Unexpected node in evaluate_string : {:?} ", x)
        }
    }

    fn evaluate_relational(&self, expression: &AstNode) -> Result<bool> {
        match expression {
            AstNode::BinOp{op, left, right} => {
                // This is arbitrary, in a way, but we check the expression type
                // decide if we are comparing String, or Numeric expressions
                match left {
                    box AstNode::StringExpression(_) => {
                        let left_string = self.evaluate_string(left)?;
                        let right_string = self.evaluate_string(right)?;

                        match op {
                            OpCode::Ge => Ok(left_string >= right_string),
                            OpCode::Le => Ok(left_string <= right_string),
                            OpCode::Gt => Ok(left_string > right_string),
                            OpCode::Lt => Ok(left_string < right_string),
                            OpCode::Eq => Ok(left_string == right_string),
                            OpCode::Neq => Ok(left_string != right_string),

                            x => panic!("In relational unexpected relational op {:?}", x)
                        }
                    }
                    box AstNode::NumericExpression(_) => {
                        let left_number = self.evaluate_numeric(left)?;
                        let right_number= self.evaluate_numeric(right)?;

                        match op {
                            OpCode::Ge => Ok(left_number >= right_number),
                            OpCode::Le => Ok(left_number <= right_number),
                            OpCode::Gt => Ok(left_number > right_number),
                            OpCode::Lt => Ok(left_number < right_number),
                            OpCode::Eq => Ok(left_number == right_number),
                            OpCode::Neq => Ok(left_number != right_number),

                            x => panic!("In relational unexpected relational op {:?}", x)
                        }
                    }
                    x => panic!("In relational expected string or numeric expression but got {:?}", x)
                }

            }
            x => panic!("In relational expected binop but got {:?}", x)
        }
    }

    fn evaluate_numeric(&self, expression: &AstNode) -> Result<Number> {
        match expression {
            AstNode::NumericExpression(e) => self.evaluate_numeric(e), // At the top level, we may have a numeric expression node
            AstNode::BinOp{op, left, right} => self.evaluate_binop(op, left, right),
            AstNode::MonOp{op, arg} => self.evaluate_monop(op, arg),
            AstNode::Op(op) => self.evaluate_op(op),
    
            AstNode::NumVal(x) => Ok(*x),
            AstNode::NumRef(id) =>  Ok(self.get_num_var(*id)?),
            AstNode::ArrayRef1{id, index} => Ok(self.get_array_var(*id, self.evaluate_numeric(index)?)?),
            AstNode::ArrayRef2{id, index1, index2} => Ok(self.get_array_var2(*id, 
                                                            self.evaluate_numeric(index1)?,
                                                            self.evaluate_numeric(index2)?)?), 

            x => panic!("Unexpected node in expression {:?} as bin_op", x)
        }
    }

    fn evaluate_binop(&self, op: &OpCode, left: &AstNode, right: &AstNode) -> Result<Number> {
        match op {
            OpCode::Plus => Ok(self.evaluate_numeric(left)? + self.evaluate_numeric(right)?),
            OpCode::Minus => Ok(self.evaluate_numeric(left)? - self.evaluate_numeric(right)?),
            OpCode::Multiply => Ok(self.evaluate_numeric(left)? * self.evaluate_numeric(right)?),
            OpCode::Divide => Ok(self.evaluate_numeric(left)? / self.evaluate_numeric(right)?),
            OpCode::Pow => Ok(self.evaluate_numeric(left)?.powf(self.evaluate_numeric(right)?)),
            o => panic!("Unexpected op {:?} as bin_op", o)
        }
    }

    fn evaluate_monop(&self, op: &OpCode, operand: &AstNode) -> Result<Number> {
        let v = self.evaluate_numeric(operand)?;
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

    fn evaluate_op(&self, op: &OpCode) -> Result<Number> {
        match op {
            OpCode::Rnd => Ok(rand::random::<Number>()),
            o => panic!("Unexpected op {:?} as op", o)
        }
    }

    fn runtime_error(&self, reason: &str) -> Error {
        Error::RuntimeError(reason.to_string(), self.current_line_number() )
    }

    fn runtime_map_error(&self, e:Error) -> Error {
        Error::RuntimeError(e.to_string(), self.current_line_number() )
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

    fn index_to_line_number(&self, index: usize) -> u16 {
        get_line_number(&self.lines[index])
    }

    fn line_number_to_index(&self, line_number: u16) -> Result<usize> {
        match self.line_index.get(&line_number) {
            Some(i) => Ok(*i),
            None => Err(self.runtime_error(&format!("Invalid line Number: {}", line_number))) 
        }
    }

    fn get_array_var(&self, var_index: VarId, index: Number) -> Result<Number> {
        self.state.borrow().array_vars.get(var_index, index.round() as usize).map_err(|e| self.runtime_map_error(e))
    }

    fn get_array_var2(&self, var_index: VarId, index1: Number, index2: Number) -> Result<Number> {
        self.state.borrow()
            .array2_vars.get(var_index, index1.round() as usize, index2.round() as usize)
            .map_err(|e| self.runtime_map_error(e))
    }

    fn get_num_var(&self, index: VarId) -> Result<Number> {
        self.state.borrow()
            .numeric_vars.get(index)
            .map_err(|e| self.runtime_map_error(e))
    }

    fn set_num_var(&self, index: VarId, value: Number) {
        self.state.borrow_mut().numeric_vars.set(index, value);
    }

    fn string_var(&self, index: VarId) -> Result<String> {
        self.state.borrow()
            .string_vars.get(index)
            .map_err(|e| self.runtime_map_error(e))
    }

    fn set_string_var(&self, index: VarId, value: String) {
        self.state.borrow_mut().string_vars.set(index, value);
    }

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

    fn push_for(&self, c: ForContext) {
        self.state.borrow_mut().for_stack.push(c)
    }

    fn pop_for(&self) -> Option<ForContext> {
        self.state.borrow_mut().for_stack.pop()
    }

    fn peek_for(&self) -> Option<ForContext> {
        self.state.borrow().for_stack.peek()
    }
}
