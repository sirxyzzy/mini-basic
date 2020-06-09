use super::*;
use std::collections::{HashMap};
use super::interpret::*;
use super::ast::*;
use super::vars::VarId;

#[derive(Debug,Copy,Clone)]
pub struct ForContext {
    pub for_line: usize,
    pub for_var: VarId,
    pub limit: Number,
    pub step: Number
}


pub struct PrintBuffer {
    line: String,
    zone_size: usize
}

impl PrintBuffer {
    pub fn new() -> PrintBuffer {
        PrintBuffer { line: String::with_capacity(120), zone_size: 14 }
    }

    pub fn add(&mut self, text: &str) {
        self.line.push_str(text);
    }

    pub fn tab(&mut self) {
        // Pad to the next zone
        let padding = self.zone_size - (self.line.len() % self.zone_size);

        for _i in 0..padding {
            self.line.push(' ');
        }
    }

    pub fn tabstop(&mut self, tab_size: Number) {
        assert!(tab_size >= 1.0);
        let size = tab_size.round() as usize - 1;

        if size < self.line.len() {
            // This will reduce the line size to zero
            self.print();
        }

        let padding = size - self.line.len();

        for _i in 0..padding {
            self.line.push(' ');
        }
    }

    pub fn print(&mut self) {
        println!("{}", self.line);
        self.line.clear();
    }
}

pub struct VmStack<T:Copy> {
    things: Vec<T>
}

impl<T:Copy> VmStack<T> {
    pub fn new() -> VmStack<T> {
        VmStack::<T>{ things:vec![] }
    }

    pub fn peek(&self) -> Option<T> {
        self.things.last().copied()
    }

    pub fn pop(&mut self) -> Option<T> {
        self.things.pop()
    }

    pub fn push(&mut self, value: T) {
        self.things.push(value)
    }
}

pub struct VarStore<T:Clone> {
    things: HashMap<VarId, T>
}

impl<T:Clone> VarStore<T> {
    pub fn new() -> VarStore<T> {
        VarStore::<T>{ things:HashMap::new() }
    }

    pub fn get(&self, id: VarId) -> Result<T> {
        match self.things.get(&id) {
            Some(v) => Ok(v.clone()),
            None => Err(Error::UninitializedValue(id))
        }
    }

    pub fn set(&mut self, id: VarId, value: T) {
        self.things.insert(id, value);
    }
}

pub struct ArrayStore<T: Clone+Default> {
    things: HashMap<VarId, Vec<T> >
}

impl<T: Clone+Default> ArrayStore<T> {
    pub fn new() -> ArrayStore<T> {
        ArrayStore::<T>{ things:HashMap::new() }
    }

    pub fn declare(&mut self, id: VarId, bound: usize) -> Result<()> {
        // plus 1 on bound, cuz Basic does that
        self.things.insert(id, vec![Default::default(); bound+1]);
        Ok(())
    }

    pub fn get(&self, id: VarId, index: Number) -> Result<T> {
        let index = index.round() as usize;
        match self.things.get(&id) {
            Some(v1) => {
                if index >= v1.len() {
                    Err(Error::ArrayIndexOutOfRange(id, index))
                } else {
                    Ok(v1.get(index).unwrap().clone())
                }
            }
            None => Err(Error::UninitializedArray(id))
        }
    }

    pub fn set(&mut self, id: VarId, index: Number, value: T) -> Result<()> {
        let index = index.round() as usize;
        let v = self.things.entry(id).or_insert_with(||  vec![Default::default(); 11]);
        // The rules are arrays get auto created, size 11 (indexes 0..10) if they don't 
        // exist already!

        if index >= v.len() {
            return Err(Error::ArrayIndexOutOfRange(id, index))
        }

        v[index] = value;
        Ok(())
    }
}

pub struct Array2Store<T: Clone+Default> {
    things: HashMap<VarId, (usize, Vec<Option<T>>) >
}

impl<T: Clone+Default> Array2Store<T> {
    pub fn new() -> Array2Store<T> {
        Array2Store::<T>{ things:HashMap::new() }
    }

    pub fn declare(&mut self, id: VarId, bound1: usize, bound2: usize) -> Result<()> {
        self.things.insert(id, (bound2 + 1, vec![Default::default(); (bound1 + 1) * (bound2 + 1)]));
        Ok(())
    }
   
    pub fn get(&self, id: VarId, index1: Number, index2: Number) -> Result<T> {
        let index1 = index1.round() as usize;
        let index2 = index2.round() as usize;
        match self.things.get(&id) {
            Some((bound, v1)) => {
                if index2 >= *bound {
                    Err(Error::ArrayIndexOutOfRange(id, index2))
                } else {
                    let index = (index1 * bound) + index2;

                    if index >= v1.len() {
                        Err(Error::ArrayIndexOutOfRange(id, index1))
                    } else {
                        match v1.get(index).unwrap() {
                            Some(v2) => Ok(v2.clone()),
                            None => Err(Error::UninitializedArrayValue(id))
                        }
                    }
                }
            }
            None => Err(Error::UninitializedArray(id))
        }
    }

    pub fn set(&mut self, id: VarId, index1: Number, index2: Number, value: T) -> Result<()> {
        let index1 = index1.round() as usize;
        let index2 = index2.round() as usize;
        match self.things.get_mut(&id) {
            Some((bound, v)) => {
                if index2 > *bound {
                    Err(Error::ArrayIndexOutOfRange(id, index2))
                }
                else {
                    let index = (index1 * (*bound)) + index2;
                    if index >= v.len() {
                        Err(Error::ArrayIndexOutOfRange(id, index1))     
                    } else {
                        v[index] = Some(value);
                        Ok(())                      
                    }
                }
            }
            None => Err(Error::UninitializedArray(id))
        }
    }
}

pub struct VirtualMachine {
    pub current: usize,  // index into lines...
    pub run_state: State,
    pub call_stack: VmStack<usize>,
    pub for_stack: VmStack<ForContext>,
    pub numeric_vars: VarStore<Number>,
    pub array_vars: ArrayStore<Number>,
    pub array2_vars: Array2Store<Number>,
    pub string_vars: VarStore<String>,
    pub print_buffer: PrintBuffer,
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            current: 0,  // index into lines...
            run_state: State::Stopped,
            call_stack: VmStack::<usize>::new(),
            for_stack: VmStack::<ForContext>::new(),
            numeric_vars: VarStore::<Number>::new(),
            array_vars: ArrayStore::<Number>::new(),
            array2_vars: Array2Store::<Number>::new(),
            string_vars: VarStore::<String>::new(),
            print_buffer: PrintBuffer::new()        
        }
    }
    
    fn evaluate_numeric(&self, expression: &AstNode) -> Result<Number> {
        match expression {
            AstNode::NumericExpression(e) => self.evaluate_numeric(e), // At the top level, we may have a numeric expression node
            AstNode::BinOp{op, left, right} => self.evaluate_binop(op, left, right),
            AstNode::MonOp{op, arg} => self.evaluate_monop(op, arg),
            AstNode::Op(op) => self.evaluate_op(op),
    
            AstNode::NumVal(x) => Ok(*x),
            AstNode::NumRef(id) =>  Ok(self.numeric_vars.get(*id)?),
            AstNode::ArrayRef1{id, index} => Ok(self.array_vars.get(*id, self.evaluate_numeric(index)?)?),
            AstNode::ArrayRef2{id, index1, index2} => Ok(self.array2_vars.get(*id, 
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

}


