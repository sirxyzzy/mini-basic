use super::*;
use std::collections::{HashMap};
use super::interpret::*;

#[derive(Debug,Clone)]
pub struct ForContext {
    pub for_line: usize,
    pub limit: Number,
    pub step: Number
}

pub struct VmStack<T> {
    things: Vec<T>
}

impl<T> VmStack<T> {
    pub fn new() -> VmStack<T> {
        VmStack::<T>{ things:vec![] }
    }

    pub fn pop(&mut self) -> Option<T> {
        self.things.pop()
    }

    pub fn push(&mut self, value: T) {
        self.things.push(value)
    }
}

pub struct VarStore<T:Clone> {
    things: HashMap<usize, T>
}

impl<T:Clone> VarStore<T> {
    pub fn new() -> VarStore<T> {
        VarStore::<T>{ things:HashMap::new() }
    }

    pub fn get(&self, id: usize) -> Result<T> {
        match self.things.get(&id) {
            Some(v) => Ok(v.clone()),
            None => Err(Error::UninitializedValue(id))
        }
    }

    pub fn set(&mut self, id: usize, value: T) {
        self.things.insert(id, value);
    }
}

pub struct ArrayStore<T: Clone> {
    things: HashMap<usize, Vec<Option<T>> >
}

impl<T: Clone> ArrayStore<T> {
    pub fn new() -> ArrayStore<T> {
        ArrayStore::<T>{ things:HashMap::new() }
    }

    pub fn declare(&mut self, id: usize, bound: usize) -> Result<()> {
        self.things.insert(id, vec![None; bound]);
        Ok(())
    }

    pub fn get(&self, id: usize, index: usize) -> Result<T> {
        match self.things.get(&id) {
            Some(v1) => {
                if index >= v1.len() {
                    Err(Error::ArrayIndexOutOfRange(id, index))
                } else {
                    match v1.get(index).unwrap() {
                        Some(v2) => Ok(v2.clone()),
                        None => Err(Error::UninitializedValue(id))
                    }
                }
            }
            None => Err(Error::UninitializedValue(id))
        }
    }

    pub fn set(&mut self, id: usize, index: usize, value: T) -> Result<()> {
        let v = self.things.entry(id).or_insert_with(||  vec![None, None, None, None, None, None, None, None, None, None, None]);
        // The rules are arrays get auto created, size 11 (indexes 0..10) if they don't 
        // exist already!

        if index >= v.len() {
            return Err(Error::ArrayIndexOutOfRange(id, index))
        }

        v[index] = Some(value);
        Ok(())
    }
}

pub struct Array2Store<T: Clone> {
    things: HashMap<usize, (usize, Vec<Option<T>>) >
}

impl<T: Clone> Array2Store<T> {
    pub fn new() -> Array2Store<T> {
        Array2Store::<T>{ things:HashMap::new() }
    }

    pub fn declare(&mut self, id: usize, bound1: usize, bound2: usize) -> Result<()> {
        self.things.insert(id, (bound2, vec![None; bound1 * bound2]));
        Ok(())
    }
   
    pub fn get(&self, id: usize, index1: usize, index2: usize) -> Result<T> {
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
                            None => Err(Error::UninitializedValue(id))
                        }
                    }
                }
            }
            None => Err(Error::UninitializedValue(id))
        }
    }

    pub fn set(&mut self, id: usize, index1: usize, index2: usize, value: T) -> Result<()> {
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
            None => Err(Error::UninitializedValue(id))
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
        }
    } 
}


