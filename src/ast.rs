use super::parser::{ Pair, Rule};
use super::{ ParseResult, ParseError};

type AstResult = ParseResult<Box<AstNode>>;

// Since the tree is immutable, we can build it with refs
pub enum AstNode {
    BinOp {
        op: OpCode,
        left: Box<AstNode>,
        right: Box<AstNode>
    },

    Op {
        op: OpCode,
        child: Box<AstNode>,        
    },

    NumVal (f64),

    StringVal (String),

    NumRef (usize),

    StringRef (usize),

    ArrayRef1 {
        id: usize,
        index: Box<AstNode>
    },

    ArrayRef2 {
        id: usize,
        index1: Box<AstNode>,
        index2: Box<AstNode>        
    }   
}

#[derive(Debug)]
pub enum OpCode {
    // Infix
    Plus,
    Minus,
    Multiply,
    Divide,
    Pow,

    // Prefix
    Neg,

    // Supplied
    Abs,
    Atn,
    Cos,
    Exp,
    Int,
    Log,
    Rnd,
    Sgn,
    Sin,
    Sqr,
    Tan,

    // Defined
    Def {
        name: String
    }
}

pub fn print_ast(node: &AstNode) {
    print_ast_helper(node, 0);
}

fn print_ast_helper(node: &AstNode, level:usize) {
    print!("{:indent$}", "", indent=level*2);
    match node {
        AstNode::BinOp{op, left, right} => {
            println!("{:?}", op);
            print_ast_helper(left, level+1);
            print_ast_helper(right, level+1);
        }
        AstNode::Op{op, child} => {
            println!("{:?}", op);
            print_ast_helper(child, level+1);            
        }
        AstNode::NumVal(x) => println!("{}", x),
        AstNode::StringVal(s) => println!("'{}'", s),
        AstNode::NumRef(id) =>  println!("{}", numeric_var_name(*id)),
        AstNode::StringRef(id) =>  println!("{}", string_var_name(*id)),
        AstNode::ArrayRef1{id, index} => {
            println!("{}[]", array_var_name(*id));
            print_ast_helper(&index, level+1);             
        }
        AstNode::ArrayRef2{id, index1, index2} => {
            println!("{}[,]", array_var_name(*id));
            print_ast_helper(&index1, level+1);
            print_ast_helper(&index2, level+1);                 
        }
    }
}

fn id_to_char(id: usize) -> char {
    assert!(id < 26, format!("Index {} out of range 0..26", id));
    ((id as u8) + b'A') as char
}

fn array_var_name(id: usize) -> String {
    format!("{}", id_to_char(id))
}

fn string_var_name(id: usize) -> String {
    format!("{}$", id_to_char(id))
}

fn numeric_var_name(id: usize) -> String{
    assert!(id < (26 * 11), "String index out of range");
    match id < 26 {
        true => format!("{}", id_to_char(id % 26)), // A..Z
        _ => format!("{}{}", id_to_char(id % 26), (id / 26) - 1) // A0.. Z9
    }
}

pub struct AstBuilder {
    // Somewhere to store side effects such as variables
}

fn ast_error(reason: &str, pair: &Pair) -> ParseError {
    let span = pair.as_span();
    ParseError::AstError{
        reason: reason.to_owned(),
        lines: span.lines().map(|s| s.to_owned()).collect(),
    }
}

fn ast_unexpected(pair: &Pair) -> ParseError {
    let span = pair.as_span();
    ParseError::AstError{
        reason: format!("Unexpected rule {:?}", pair.as_rule()),
        lines: span.lines().map(|s| s.to_owned()).collect(),
    }
}

pub fn expect_first<'i>(pair: &Pair<'i>) -> ParseResult<Pair<'i>> {
    match pair.clone().into_inner().peek() {
        None => Err(ast_error("Expected at least one sub-pair", pair)),
        Some(p) => Ok(p)
    }
}

impl AstBuilder {   
    // AST productions
    pub fn build(pair: Pair) -> AstResult {
        Ok(Box::new(AstNode::StringVal("placemarker".to_owned())))
    } 
    
    pub fn EOI(_pair: Pair) -> ParseResult<()> {
        Ok(())
    }

    pub fn line_number(pair: Pair) -> ParseResult<u16> {
        pair.as_str().parse::<u16>().map_err(|_|  ast_error("Could not parse line number", &pair))
    }

    pub fn end_line(_pair: Pair) -> ParseResult<()> {
        Ok(())
    }

    pub fn end_statement(_pair: Pair) -> ParseResult<()> {
        Ok(())
    }

    pub fn numeric_rep(pair: Pair) -> ParseResult<f64> {
        pair.as_str().parse::<f64>().map_err(|_|  ast_error("Could not parse line number", &pair))
    }

    pub fn expression(pair: Pair) -> AstResult {
        let pair = expect_first(&pair)?;
        match pair.as_rule() {
            Rule::string_expression => Self::string_expression(pair),
            Rule::numeric_expression => Self::numeric_expression(pair),
            _ => Err(ast_unexpected(&pair))
        }
    }

    pub fn numeric_expression(pair: Pair) -> AstResult {
        Ok(Box::new(AstNode::NumVal(99.0)))
    }

    pub fn string_expression(pair: Pair) -> AstResult {
        let pair = expect_first(&pair)?;
        match pair.as_rule() {
            Rule::string_variable => Self::string_variable(pair),
            Rule::string_constant => Self::quoted_string(pair),
            _ => Err(ast_unexpected(&pair))
        }
    }

    pub fn string_variable(pair: Pair) -> AstResult {
        let first = pair.as_str().as_bytes()[0];
        let id =(first - b'A') as usize;
    
        // Yeah, in theory only 0..25, but check that
        debug_assert!(id < 26, "Failed to get valid index for string variable");

        Ok(Box::new(AstNode::StringRef(id)))
    }

    pub fn quoted_string(pair: Pair) -> AstResult {
        let mut inner = pair.as_str();
        inner = &inner[1..inner.len()-1];
        Ok(Box::new(AstNode::StringVal(inner.to_owned()))) 
    }
}

