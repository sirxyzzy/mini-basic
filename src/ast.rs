use super::parser::{ Pair, Rule};
use super::{ ParseResult, ParseError};
use std::error::Error;


// Since the tree is immutable, we can build it with refs
#[derive(Debug)]
pub enum AstNode {
    BinOp {
        op: OpCode,
        left: Box<AstNode>,
        right: Box<AstNode>
    },

    MonOp {
        op: OpCode,
        arg: Box<AstNode>        
    },

    Op (OpCode),

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
    Def (String)
}

impl OpCode {
    fn from_pair(pair: &Pair) -> Option<OpCode> {
        match pair.as_rule() {
            Rule::numeric_function_name |
            Rule::sign |
            Rule::multiplier |
            Rule::numeric_supplied_function => Self::from_pair(&first_child(pair)),

            Rule::plus =>  Some(OpCode::Plus),
            Rule::minus =>  Some(OpCode::Minus),
            Rule::multiply =>  Some(OpCode::Multiply),
            Rule::divide =>  Some(OpCode::Divide),
            Rule::pow =>  Some(OpCode::Pow),
            Rule::abs =>  Some(OpCode::Abs),
            Rule::atn =>  Some(OpCode::Atn),
            Rule::cos =>  Some(OpCode::Cos),
            Rule::exp =>  Some(OpCode::Exp),
            Rule::int =>  Some(OpCode::Int),
            Rule::log =>  Some(OpCode::Log),
            Rule::rnd =>  Some(OpCode::Rnd),
            Rule::sgn =>  Some(OpCode::Sgn),
            Rule::sin =>  Some(OpCode::Sin),
            Rule::sqr =>  Some(OpCode::Sqr),
            Rule::tan =>  Some(OpCode::Tan),
            Rule::numeric_defined_function => Some(OpCode::Def(pair.as_str().to_owned())),
            _ => None
        }
    } 
}


pub fn print_ast(node: &AstNode) {
    println!("AST...");
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
        AstNode::MonOp{op, arg} => {
            println!("{:?}", op);
            print_ast_helper(arg, level+1);            
        }
        AstNode::Op(op) => {
            println!("{:?}", op);   
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

impl ParseError {
    fn from_error<E: Error>(error: E, pair: &Pair) -> ParseError {
        let span = pair.as_span();
        ParseError::AstError{
            reason: error.to_string(),
            rule_type: pair.as_rule(),
            start_pos: span.start_pos().pos(),
            end_pos: span.end_pos().pos(),
        }
    }

    fn unexpected(pair: &Pair) -> ParseError {
        let span = pair.as_span();
        ParseError::AstError{
            reason: format!("Unexpected rule {:?}", pair.as_rule()),
            rule_type: pair.as_rule(),
            start_pos: span.start_pos().pos(),
            end_pos: span.end_pos().pos(),
        }
    }

    fn expected(thing: &str, pair: &Pair) -> ParseError {
        let span = pair.as_span();
        ParseError::AstError{
            reason: format!("Expected {} in {:?}", thing, pair.as_rule()),
            rule_type: pair.as_rule(),
            start_pos: span.start_pos().pos(),
            end_pos: span.end_pos().pos(),
        }
    }

    fn not_implemented(pair: &Pair) -> ParseError {
        let span = pair.as_span();
        ParseError::AstError{
            reason: format!("<{:?}> is not implemented", pair.as_rule()),
            rule_type: pair.as_rule(),
            start_pos: span.start_pos().pos(),
            end_pos: span.end_pos().pos(),
        }
    }
}

pub fn first_child<'i>(pair: &Pair<'i>) -> Pair<'i> {
    pair.clone().into_inner().peek().unwrap()
}

pub fn assert_rule(pair: &Pair, expected: Rule) {
    assert_eq!(pair.as_rule(), expected, "Expected <{:?}>", expected)
}

impl AstBuilder {   
    // AST productions
    pub fn build(pair: Pair) -> ParseResult<AstNode> {
        Ok(AstNode::StringVal("placemarker".to_owned()))
    } 
    
    pub fn EOI(_pair: Pair) -> ParseResult<()> {
        Ok(())
    }

    pub fn line_number(pair: Pair) -> ParseResult<u16> {
        pair.as_str().parse::<u16>().map_err(|e|  ParseError::from_error(e, &pair))
    }

    pub fn end_line(_pair: Pair) -> ParseResult<()> {
        Ok(())
    }

    pub fn end_statement(_pair: Pair) -> ParseResult<()> {
        Ok(())
    }

    pub fn numeric_rep(pair: Pair) -> ParseResult<AstNode> {
        let number_string = pair.as_str().to_string();
        // I don't understand why, but sometimes I get whitespace around my number
        // which breaks the parse, so I trim!
        match number_string.trim().parse::<f64>() {
            Ok(v) => Ok(AstNode::NumVal(v)),
            Err(e) => {
                println!("I don't like this literal! '{}' I got because of this {:?}", number_string, pair);
                Err(ParseError::from_error(e, &pair))
            }
        }
    }

    //
    // { numeric_function_name ~ argument_list? }
    //
    pub fn numeric_function_ref(pair: Pair) -> ParseResult<AstNode> {
        assert_rule(&pair, Rule::numeric_function_ref);

        let mut pairs = pair.clone().into_inner();
        let op = OpCode::from_pair(&pairs.next().unwrap()).unwrap();

        match pairs.next() {
            Some(p) => {
                let arg_ast = Self::numeric_expression(first_child(&first_child(&p)))?;
                Ok(AstNode::MonOp{op, arg: Box::new(arg_ast)}) 
            }
            None => Ok(AstNode::Op(op))
        }
    }

    pub fn expression(pair: Pair) -> ParseResult<AstNode> {
        let pair = first_child(&pair);
        match pair.as_rule() {
            Rule::string_expression => Self::string_expression(pair),
            Rule::numeric_expression => Self::numeric_expression(pair),
            _ => Err(ParseError::unexpected(&pair))
        }
    }

    //
    // numeric_expression = { sign? ~ term ~ (sign ~ term)* }
    //
    // Signs are opcodes, here limited to - or +
    // For example, - t1 + t2 - t3 + t4 would translate to an ast tree like this
    //             +
    //          -      t4
    //      +      t3
    //   -    t2 
    //  t1
    //
    // We therefore iterate, always knowing a prior "sign", and a prior sub-tree
    // we only build the tree as we hit terms
    //  
    pub fn numeric_expression(pair: Pair) -> ParseResult<AstNode> {
        assert_eq!(pair.as_rule(), Rule::numeric_expression, "Expected numeric expression!");

        let mut prior_op: Option<OpCode> = None;
        let mut tree: Option<AstNode> = None;

        for p in pair.clone().into_inner()
        {
            match p.as_rule() {

                Rule::sign => {
                    // Just track the sign, we will consume it when we hit a term
                    prior_op = OpCode::from_pair(&p);
                }

                Rule::term => {
                    let term_tree = Self::term(p)?;
                    match prior_op {

                        None => {
                            // This can ONLY happen at the beginning
                            // with no leading sign
                            assert!(tree.is_none());
                            tree = Some(term_tree);
                        }

                        Some(op) => {
                            // We are going to consume the op, so clear the prior op
                            // it should get populated again next time around
                            prior_op = None;

                            match tree {
                                None => { 
                                    // This must be the first term, with a monadic + or -
                                    tree = Some(
                                        AstNode::MonOp { op, arg:Box::new(term_tree) }
                                    )
                                }
                                Some(prev_tree) => {
                                    // The more common case, a binary operator on what we already have, and the next term
                                    tree = Some(
                                        AstNode::BinOp { op, left:Box::new(prev_tree), right:Box::new(term_tree) })
                                }
                            }
                        }
                    }
                }
                _ => return Err(ParseError::unexpected(&p))
            }
        }

        match tree {
            None => Err(ParseError::expected("some expression", &pair)),
            Some(t) => Ok(t)
        }
    }

    //
    // term = { factor ~ (multiplier ~ factor)* }
    //
    pub fn term(pair: Pair) -> ParseResult<AstNode> {
        assert_eq!(pair.as_rule(), Rule::term, "Expected term!");

        let mut prior_op: Option<OpCode> = None;
        let mut tree: Option<AstNode> = None;

        for p in pair.clone().into_inner() {
            match p.as_rule() {
                Rule::multiplier => {
                    // Just track the multiplier, we will consume it when we hit a term
                    prior_op = OpCode::from_pair(&p);
                }

                Rule::factor => {
                    let factor_tree = Self::factor(p)?;
                    match prior_op {

                        None => {
                            // This can ONLY happen at the beginning
                            assert!(tree.is_none());
                            tree = Some(factor_tree);
                        }

                        Some(op) => {
                            // We are going to consume the op, so clear the prior op
                            // it should get populated again next time around
                            prior_op = None;

                            // We MUST already have a left tree, assert that
                            assert!(tree.is_some());

                            tree = Some(AstNode::BinOp { op, left:Box::new(tree.unwrap()), right:Box::new(factor_tree) })
                        }
                    }

                }
                _ => return Err(ParseError::unexpected(&p))
            }
        }

        match tree {
            None => Err(ParseError::expected("some factor", &pair)),
            Some(t) => Ok(t)
        }
    }

    //
    // factor = { primary ~ (pow ~ primary)* }
    //
    pub fn factor(pair: Pair) -> ParseResult<AstNode> {
        assert_eq!(pair.as_rule(), Rule::factor, "Expected factor!");

        let mut prior_op: Option<OpCode> = None;
        let mut tree: Option<AstNode> = None;

        for p in pair.clone().into_inner() {
            match p.as_rule() {
                Rule::pow => {
                    // Just track the multiplier, we will consume it when we hit a term
                    prior_op = OpCode::from_pair(&p);
                }

                Rule::primary => {
                    let primary_tree = Self::primary(p)?;
                    match prior_op {

                        None => {
                            // This can ONLY happen at the beginning
                            assert!(tree.is_none());
                            tree = Some(primary_tree);
                        }

                        Some(op) => {
                            // We are going to consume the op, so clear the prior op
                            // it should get populated again next time around
                            prior_op = None;

                            // We MUST already have a left tree, assert that
                            assert!(tree.is_some());

                            tree = Some(AstNode::BinOp { op, left:Box::new(tree.unwrap()), right:Box::new(primary_tree) })
                        }
                    }

                }
                _ => return Err(ParseError::unexpected(&p))
            }
        }

        match tree {
            None => Err(ParseError::expected("some factor", &pair)),
            Some(t) => Ok(t)
        }
    }


    //
    // primary = {  
    //     numeric_rep 
    //   | numeric_function_ref 
    //   | numeric_variable
    //   | ( "(" ~ numeric_expression ~ ")" ) }
    //
    pub fn primary(pair: Pair) -> ParseResult<AstNode> {
        assert_rule(&pair, Rule::primary);

        let sub_pair = first_child(&pair);
        match sub_pair.as_rule() {
            Rule::numeric_rep => Self::numeric_rep(sub_pair),
            Rule::numeric_function_ref => Self::numeric_function_ref(sub_pair),
            Rule::numeric_variable => Self::numeric_variable(sub_pair),  
            Rule::numeric_expression => Self::numeric_expression(sub_pair),
            _ => Err(ParseError::unexpected(&pair))
        }
    }

    //
    // numeric_variable = { numeric_array_element | simple_numeric_variable }
    //
    pub fn numeric_variable(pair: Pair) -> ParseResult<AstNode> {
        assert_rule(&pair, Rule::numeric_variable);

        let sub_pair = first_child(&pair);
        match sub_pair.as_rule() {
            Rule::numeric_array_element => Self::numeric_array_element(sub_pair),
            Rule::simple_numeric_variable => Self::simple_numeric_variable(sub_pair),
            _ => Err(ParseError::unexpected(&pair))
        }
    }

    fn id_from_first(bytes: &[u8]) -> usize {
        (bytes[0] - b'A') as usize
    }

    //
    // numeric_array_element  = { numeric_array_name ~ subscript }
    // subscript = { "(" ~ numeric_expression ~ ( "," ~ numeric_expression )?  ~ ")" }
    //
    pub fn numeric_array_element(pair: Pair) -> ParseResult<AstNode> {
        let mut pairs = pair.into_inner();

        let id = Self::id_from_first(pairs.next().unwrap().as_str().as_bytes());

        let mut sub_pairs = pairs.next().unwrap().into_inner();

        let index1 = Box::new(Self::numeric_expression(sub_pairs.next().unwrap())?);

        Ok(match sub_pairs.next() {
            Some(p) => {
                let index2 = Box::new(Self::numeric_expression(p)?);
                AstNode::ArrayRef2{ id, index1, index2}
            }
            None => {
                AstNode::ArrayRef1{ id, index:index1}
            }
        })
    }

    pub fn string_expression(pair: Pair) -> ParseResult<AstNode> {
        let pair = first_child(&pair);
        match pair.as_rule() {
            Rule::string_variable => Self::string_variable(pair),
            Rule::string_constant => Self::quoted_string(pair),
            _ => Err(ParseError::unexpected(&pair))
        }
    }

    pub fn simple_numeric_variable(pair: Pair) -> ParseResult<AstNode> {
        let bytes = pair.as_str().as_bytes();

        let mut id = Self::id_from_first(bytes);

        if bytes.len() > 1 {
            id = (id + 1) * 10 + ((bytes[1] - b'0') as usize);
        }
    
        // Yeah, in theory only 0..25, but check that
        debug_assert!(id < 26 * 11, "Failed to get valid index for numeric variable");

        Ok(AstNode::NumRef(id))
    }

    pub fn string_variable(pair: Pair) -> ParseResult<AstNode> {
        let id = Self::id_from_first(pair.as_str().as_bytes());
    
        // Yeah, in theory only 0..25, but check that
        debug_assert!(id < 26, "Failed to get valid index for string variable");

        Ok(AstNode::StringRef(id))
    }

    pub fn quoted_string(pair: Pair) -> ParseResult<AstNode> {
        let mut inner = pair.as_str();
        inner = &inner[1..inner.len()-1];
        Ok(AstNode::StringVal(inner.to_owned())) 
    }
}

