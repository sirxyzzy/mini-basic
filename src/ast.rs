use super::parser::{ Pair, Rule};
use super::*;
use std::error::Error;
use std::collections::BTreeMap;
use vars;


// Since the tree is immutable, we can build it with refs
#[derive(Debug)]
pub enum AstNode {
    Program {
        lines: BTreeMap<u16, AstNode>
    },

    // All statements have a line number
    // and all must be referenced in 
    DataStatement{ line: u16 },
    DefStatement{ line: u16 },
    DimensionStatement{ line: u16 },
    GosubStatement{ line: u16 },
    GotoStatement{ line: u16 },
    IfThenStatement{ line: u16, expr: Box<AstNode>, then: u16 },
    InputStatement{ line: u16 },
    LetStatement{ line: u16, var: Box<AstNode>, val: Box<AstNode> },
    OnGotoStatement{ line: u16 },
    OptionStatement{ line: u16 },
    PrintStatement{ line: u16, items: Vec<AstNode> },
    RandomizeStatement{ line: u16 },
    ReadStatement{ line: u16 },
    RemarkStatement{ line: u16 },
    RestoreStatement{ line: u16 },
    ReturnStatement{ line: u16 },
    StopStatement{ line: u16 },
    ForStatement{ line: u16, id: usize, from: Box<AstNode>, to: Box<AstNode>, step: Option<Box<AstNode>>},
    NextStatement{ line: u16, id: usize, for_line: u16},
    EndStatement{ line: u16 },

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

    // Print gorp
    PrintSemi,
    PrintComma,
    TabCall (Box<AstNode>),

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
    },    
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

    // Relational
    Ge,
    Le,
    Gt,
    Lt,
    Eq,
    Neq,

    // Defined
    Def (String)
}

impl OpCode {
    fn from_pair(pair: Pair) -> Option<OpCode> {
        match pair.as_rule() {
            Rule::numeric_function_name |
            Rule::sign |
            Rule::multiplier |
            Rule::numeric_supplied_function => Self::from_pair(first_child(pair)),

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

            Rule::ge  =>  Some(OpCode::Ge),
            Rule::le  =>  Some(OpCode::Le),
            Rule::lt  =>  Some(OpCode::Lt),
            Rule::gt  =>  Some(OpCode::Gt),
            Rule::eq  =>  Some(OpCode::Eq),
            Rule::neq  =>  Some(OpCode::Neq),

            Rule::relation |
            Rule::equality_relation => Self::from_pair(descendant(pair)),

            _ => {
                println!("{:?} is not a supported opcode!", pair);
                None
            }
        }
    } 
}

//
// If I am a statement, provide the line number
//
pub fn statement_line_number(node: &AstNode) -> Option<u16> {
    match node {
        AstNode::DataStatement{ line,..} |
        AstNode::DefStatement{ line, .. } |
        AstNode::DimensionStatement{ line, .. } |
        AstNode::GosubStatement{ line, .. } |
        AstNode::GotoStatement{ line, .. } |
        AstNode::IfThenStatement{ line, .. } |
        AstNode::InputStatement{ line, .. } |
        AstNode::LetStatement{ line, .. } |
        AstNode::OnGotoStatement{ line, .. } |
        AstNode::OptionStatement{ line, .. } |
        AstNode::PrintStatement{ line, .. } |
        AstNode::RandomizeStatement{ line, .. } |
        AstNode::ReadStatement{ line, .. } |
        AstNode::RemarkStatement{ line, .. } |
        AstNode::RestoreStatement{ line, .. } |
        AstNode::ReturnStatement{ line, .. } |
        AstNode::StopStatement{ line, .. } |
        AstNode::ForStatement{ line, .. } |
        AstNode::NextStatement{ line, .. } |
        AstNode::EndStatement{ line, .. } => Some(*line),
        _ => None
    }
}

//
// Am I a statement, yes if I have a line number!
//
pub fn is_statement(node: &AstNode) -> bool {
    match statement_line_number(node) {
        Some(_) => true,
        None => false
    }
}

/// Pretty print an AST, starting at any node
pub fn print_ast(node: &AstNode) {
    print_ast_helper("AST", node, 0);
}

fn print_ast_helper(label: &str, node: &AstNode, level:usize) {
    print!("{:indent$}{} ", "", label, indent=level*2);
    match node {
        AstNode::BinOp{op, left, right} => {
            println!("{:?}", op);
            print_ast_helper(" left",left, level+1);
            print_ast_helper("right", right, level+1);
        }

        AstNode::MonOp{op, arg} => {
            println!("{:?}", op);
            print_ast_helper("Arg", arg, level+1);            
        }

        AstNode::Op(op) => {
            println!("{:?}", op);   
        }

        AstNode::NumVal(x) => println!("{}", x),
        AstNode::StringVal(s) => println!("'{}'", s),
        AstNode::NumRef(id) =>  println!("{}", vars::id_to_num_name(*id)),
        AstNode::StringRef(id) =>  println!("{}", vars::id_to_string_name(*id)),
        AstNode::ArrayRef1{id, index} => {
            println!("{}[..]", vars::id_to_array_name(*id));
            print_ast_helper("", index, level+1);             
        }

        AstNode::ArrayRef2{id, index1, index2} => {
            println!("{}[..,..]", vars::id_to_array_name(*id));
            print_ast_helper("", index1, level+1);
            print_ast_helper("", index2, level+1);                 
        }

        AstNode::Program{lines} => {
            println!("Program");

            for line in lines.iter() {
                print_ast_helper(&format!("{:4}", line.0), line.1, level+1);
            }
        }

        AstNode::ForStatement{id, from, to, step, ..} => {
            println!("FOR {}", vars::id_to_num_name(*id));
            print_ast_helper("from", from, level+1);
            print_ast_helper("  to", to, level+1);
            if let Some(step) =  step {
                print_ast_helper("step", step, level+1);
            }
        },

        AstNode::NextStatement{id, for_line, ..} => 
            println!("NEXT {} ->{}", vars::id_to_num_name(*id), for_line),

        AstNode::LetStatement{var, val, ..} => {
            println!("LET");
            print_ast_helper("     ", var, level+1);
            print_ast_helper("value", val, level+1);
        },

        AstNode::IfThenStatement{expr, then, ..} => {
            println!("IF THEN {}", then);
            print_ast_helper("", expr, level+1);    
        }

        AstNode::PrintStatement{items, ..} => {
            println!("PRINT");
            for item in items.iter() {
                print_ast_helper("", item, level+1);
            }
        }

        AstNode::RemarkStatement{..} => println!("REM ..."),
        AstNode::RandomizeStatement{..} => println!("RANDOMIZE"),
        AstNode::RestoreStatement{..} => println!("RESTORE"),
        AstNode::ReturnStatement{..} => println!("RETURN"),
        AstNode::StopStatement{..} => println!("STOP"),
        AstNode::EndStatement{..} => println!("END"),

        _ => println!("{:?}", node) 
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

    fn new(reason: &str, pair: &Pair) -> ParseError {
        let span = pair.as_span();
        ParseError::AstError{
            reason: format!("{} {:?}", reason, pair.as_rule()),
            rule_type: pair.as_rule(),
            start_pos: span.start_pos().pos(),
            end_pos: span.end_pos().pos(),
        }
    }

    fn unexpected(context: &str, pair: &Pair) -> ParseError {
        Self::new(&format!("In {}, unexpected rule", context), pair)
    }

    fn expected(thing: &str, pair: &Pair) -> ParseError {
        Self::new(&format!("Expected {} in", thing), pair)
    }

    fn not_implemented(pair: &Pair) -> ParseError {
        Self::new("Not implemented", pair)
    }
}

pub fn first_child<'i>(pair: Pair<'i>) -> Pair<'i>{
    let rule = pair.as_rule();
    pair.into_inner().peek().expect(&format!("Expected first child in {:?}", rule))
}

pub fn first_two_children<'i>(pair: Pair<'i>) -> (Pair<'i>, Pair<'i>) {
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    
    let c1 = pairs.next().expect(&format!("Expected first child in {:?}", rule));
    let c2 = pairs.next().expect(&format!("Expected second child in {:?}", rule));

    (c1,c2)
}

pub fn first_three_children<'i>(pair: Pair<'i>) -> (Pair<'i>, Pair<'i>, Pair<'i>) {
    let rule = pair.as_rule();
    let mut pairs = pair.into_inner();
    
    let c1 = pairs.next().expect(&format!("Expected first child in {:?}", rule));
    let c2 = pairs.next().expect(&format!("Expected second child in {:?}", rule));
    let c3 = pairs.next().expect(&format!("Expected second child in {:?}", rule));

    (c1,c2,c3)
}

pub fn descendant<'i>(pair: Pair<'i>) -> Pair<'i> {
    let mut here = pair;
    loop {
       let child = here.clone().into_inner().peek();

       match child {
           Some(c) => here = c,
           None => return here
       }
   }
}

pub fn assert_rule(pair: &Pair, expected: Rule) {
    assert_eq!(pair.as_rule(), expected, "Expected <{:?}>", expected)
}

impl AstBuilder {   
    // Build the AST
    pub fn build(pair: Pair, _options: &ParseOptions) -> ParseResult<AstNode> {
        Self::program(pair)
    }
    
    // program = ${ block ~ end_line }
    pub fn program(pair: Pair) -> ParseResult<AstNode> {
        assert_rule(&pair, Rule::program);

        let mut pairs = pair.into_inner();

        let lines = Self::block(pairs.next().unwrap())?;
        
        let mut line_map = BTreeMap::new();

        lines.into_iter().for_each(|l| {
            line_map.insert(statement_line_number(&l).unwrap(), l);
        });

        let end = Self::end_statement(pairs.next().unwrap())?;

        line_map.insert(statement_line_number(&end).unwrap(), end);

        Ok(AstNode::Program{lines:line_map})
    }

    // block = ${ (line | for_block)* }
    pub fn block(pair: Pair) -> ParseResult<Vec<AstNode>> {
        assert_rule(&pair, Rule::block);

        let mut lines: Vec<AstNode> = Vec::new(); 
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::line => lines.push(Self::line(p)?),
                Rule::for_block => lines.append(&mut Self::for_block(p)?),
                _ => return Err(ParseError::unexpected("block", &p))
            }
        }
        Ok(lines)
    }
    
    // line = !{ line_number ~ statement ~ end_of_line }
    pub fn line(pair: Pair) -> ParseResult<AstNode> {
        let mut pairs = pair.into_inner();
        let line = Self::line_number(pairs.next().unwrap())?;
        let statement = Self::statement(pairs.next().unwrap(), line)?;
        Ok(statement)
    }

    //
    // Statements have a single child, the parse tree for a particular statement
    //
    pub fn statement(pair: Pair, line: u16) -> ParseResult<AstNode> {
        let p = first_child(pair);
        match p.as_rule() {
            Rule::data_statement => Ok(AstNode::DataStatement{line}),
            Rule::def_statement => Ok(AstNode::DefStatement{line}),
            Rule::dimension_statement => Ok(AstNode::DimensionStatement{line}),
            Rule::gosub_statement => Ok(AstNode::GosubStatement{line}),
            Rule::goto_statement => Ok(AstNode::GotoStatement{line}),
            Rule::if_then_statement => Self::if_then_statement(p, line),
            Rule::input_statement => Ok(AstNode::InputStatement{line}),
            Rule::let_statement => Self::let_statement( p,line),
            Rule::on_goto_statement => Ok(AstNode::OnGotoStatement{line}),
            Rule::option_statement => Ok(AstNode::OptionStatement{line}),
            Rule::print_statement => Self::print_statement( p,line),
            Rule::randomize_statement => Ok(AstNode::RandomizeStatement{line}),
            Rule::read_statement => Ok(AstNode::ReadStatement{line}),
            Rule::remark_statement => Ok(AstNode::RemarkStatement{line}),
            Rule::restore_statement => Ok(AstNode::RestoreStatement{line}),
            Rule::return_statement => Ok(AstNode::ReturnStatement{line}),
            Rule::stop_statement => Ok(AstNode::StopStatement{line}),
            _ => panic!("Not a statement, we should never reach here!")
        }
    }

    // print_statement = { ("PRINT " ~ print_list) | "PRINT" }
    fn print_statement(pair: Pair, line: u16) -> ParseResult<AstNode> {
        match pair.into_inner().peek() {
            None => Ok(AstNode::PrintStatement{line, items: Vec::new()}),
            Some(child) => {
                let items = Self::print_list(child)?;
                Ok(AstNode::PrintStatement{line, items})
            }
        }
    }

    // print_list = { (print_item? ~ print_separator)* ~ print_item? }
    fn print_list(pair: Pair) -> ParseResult<Vec<AstNode>> {
        let mut print_items: Vec<AstNode> = Vec::new();
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::print_item => print_items.push(Self::print_item(p)?),
                Rule::print_separator => print_items.push(Self::print_separator(p)?),
                _ => return Err(ParseError::unexpected("print item", &p))
            }
        }

        Ok(print_items)
    }

    fn print_separator(pair: Pair) -> ParseResult<AstNode> {
        match pair.as_str() {
            "," => Ok(AstNode::PrintComma),
            ";" => Ok(AstNode::PrintSemi),
            _ => Err(ParseError::unexpected("print separator", &pair))
        }
    }

    // print_item = { tab_call | expression }
    fn print_item(pair: Pair) -> ParseResult<AstNode> {
        let p = first_child(pair);

        match p.as_rule() {
            Rule::tab_call => Ok(Self::tab_call(p)?),
            Rule::expression => Ok(Self::expression(p)?),
            _ => Err(ParseError::unexpected("print separator", &p))
        }
    }

    // tab_call = { "TAB" ~ "(" ~ numeric_expression ~ ")" }
    fn tab_call(pair: Pair) -> ParseResult<AstNode> {
        let p = first_child(pair);

        match p.as_rule() {
            Rule::numeric_expression => Ok(Self::numeric_expression(p)?),
            _ => Err(ParseError::unexpected("tab_call", &p))
        }
    }

    // if_then_statement = { "IF " ~ relational_expression ~ "THEN" ~ line_number_ref }
    fn if_then_statement(pair: Pair, line: u16) -> ParseResult<AstNode> {
        let (rel_pair,line_pair) = first_two_children(pair);
        let expr = Box::new(Self::relational_expression(rel_pair)?);
        let then = Self::line_number(line_pair)?;
        Ok(AstNode::IfThenStatement{line, expr, then})
    }

    // relational_expression = { (numeric_expression ~ relation ~ numeric_expression) 
    //    | (string_expression ~ equality_relation ~ string_expression) }
    fn relational_expression(pair: Pair) -> ParseResult<AstNode> {

        let (left_pair, rel, right_pair) = first_three_children(pair);

        match left_pair.as_rule() {
            Rule::numeric_expression => {
                let left = Self::numeric_expression(left_pair)?;
                let right = Self::numeric_expression(right_pair)?;
                let op = OpCode::from_pair(rel).expect("I want a valid opcode!");
                Ok(AstNode::BinOp{op, left:Box::new(left), right:Box::new(right)})
            },
            Rule::string_expression => {
                let left = Self::string_expression(left_pair)?;
                let right = Self::string_expression(right_pair)?;
                let op = OpCode::from_pair(rel).unwrap();
                Ok(AstNode::BinOp{op, left:Box::new(left), right:Box::new(right)})
            },
            _ => Err(ParseError::unexpected("relational expression", &left_pair))
        }
    }

    // let_statement = { numeric_let_statement | string_let_statement }
    // numeric_let_statement   = { "LET " ~ numeric_variable ~ "=" ~ numeric_expression }
    // string_let_statement = { "LET " ~ string_variable  ~ "=" ~ string_expression }
    fn let_statement(pair: Pair, line: u16) -> ParseResult<AstNode> {
        let p = first_child(pair);
        match p.as_rule() {
            Rule::numeric_let_statement => {
                let (var_pair, expression_pair) = first_two_children(p);
                let var = Box::new(Self::numeric_variable(var_pair)?);
                let val = Box::new(Self::numeric_expression(expression_pair)?);
                Ok(AstNode::LetStatement{line, var, val})
            }
            Rule::string_let_statement => {
                let (var_pair, expression_pair) = first_two_children(p);
                let var = Box::new(Self::string_variable(var_pair)?);
                let val = Box::new(Self::string_expression(expression_pair)?);
                Ok(AstNode::LetStatement{line, var, val})
            },
            _ => Err(ParseError::unexpected("let", &p))
        }
    }

    //
    // for
    //
    // Gets a bit complicated as I have a highly structured parse tree,
    // but want a flattened AST (a vec of lines)
    //

    // for_block = { for_line ~ for_body }
    pub fn for_block(pair: Pair) -> ParseResult<Vec<AstNode>> {
        let mut lines: Vec<AstNode> = Vec::new();

        let mut pairs = pair.into_inner();
        let for_line = Self::for_line(pairs.next().unwrap())?;

        match for_line {
            AstNode::ForStatement{line, id,..} => {
                let mut body_lines = Self::for_body(pairs.next().unwrap(), line, id)?;
                lines.push(for_line);
                lines.append(&mut body_lines);
                Ok(lines)
            },
            _ => panic!("Only possibility is a for statement!")
        }
    }

    // for_line = !{ line_number ~ for_statement ~ end_of_line }
    pub fn for_line(pair: Pair) -> ParseResult<AstNode> {
        let mut pairs = pair.into_inner();

        let line = Self::line_number(pairs.next().unwrap())?;

        Self::for_statement(pairs.next().unwrap(), line)
    }

    // for_statement = { "FOR" ~ simple_numeric_variable ~  "=" ~ numeric_expression  ~ "TO" ~ numeric_expression ~ ( "STEP" ~ numeric_expression)?  }
    pub fn for_statement(pair: Pair, line: u16) -> ParseResult<AstNode> {
        let mut pairs = pair.into_inner();

        let id = vars::num_name_to_id(pairs.next().unwrap().as_str());
        
        let from = Box::new(Self::numeric_expression(pairs.next().unwrap())?);
        let to = Box::new(Self::numeric_expression(pairs.next().unwrap())?);

        let step = match pairs.next() {
            Some(p) => Some(Box::new(Self::numeric_expression(p)?)),
            None => None
        };

        Ok(AstNode::ForStatement{line, id, from, to, step})
    }

    // for_body { block ~ next_line }
    pub fn for_body(pair: Pair, for_line: u16, id: usize) -> ParseResult<Vec<AstNode>> {
        let mut pairs = pair.into_inner();

        let mut lines = Self::block(pairs.next().unwrap())?;

        let next_line = Self::next_line(pairs.next().unwrap(), id, for_line)?;
        lines.push(next_line);

        Ok(lines)
    }

    // next_line = !{ line_number ~ next_statement ~ end_of_line }
    // next_statement = { "NEXT" ~ simple_numeric_variable }
    pub fn next_line(pair: Pair, expected_id: usize, for_line: u16) -> ParseResult<AstNode> {
        let mut pairs = pair.clone().into_inner();

        let line = Self::line_number(pairs.next().unwrap())?;

        // Validate matching id, next_statement/simple_numeric_variable
        let sub_pair = first_child(pairs.next().unwrap());
        let id = vars::num_name_to_id(sub_pair.as_str());

        if expected_id != id {
            let m = format!("Mismatched NEXT, expected control variable {} but got {}", 
                vars::id_to_num_name(expected_id), vars::id_to_num_name(id));
            Err(ParseError::new(&m, &pair))
        }
        else {
            Ok(AstNode::NextStatement{line, id, for_line})
        }
    }

    // This can handle both line numbers and line number refs
    pub fn line_number(pair: Pair) -> ParseResult<u16> {
        pair.as_str().to_string().trim().parse::<u16>().map_err(|e|  ParseError::from_error(e, &pair))
    }

    pub fn end_line(_pair: Pair) -> ParseResult<()> {
        Ok(())
    }

    pub fn end_statement(pair: Pair) -> ParseResult<AstNode> {
        let line = Self::line_number(first_child(pair))?;
        Ok(AstNode::EndStatement{line})
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

        let mut pairs = pair.into_inner();
        let op = OpCode::from_pair(pairs.next().unwrap()).unwrap();

        match pairs.next() {
            Some(p) => {
                let arg_ast = Self::numeric_expression(first_child(first_child(p)))?;
                Ok(AstNode::MonOp{op, arg: Box::new(arg_ast)}) 
            }
            None => Ok(AstNode::Op(op))
        }
    }

    pub fn expression(pair: Pair) -> ParseResult<AstNode> {
        let pair = first_child(pair);
        match pair.as_rule() {
            Rule::string_expression => Self::string_expression(pair),
            Rule::numeric_expression => Self::numeric_expression(pair),
            _ => Err(ParseError::unexpected("expression", &pair))
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

        for p in pair.into_inner()
        {
            match p.as_rule() {

                Rule::sign => {
                    // Just track the sign, we will consume it when we hit a term
                    prior_op = OpCode::from_pair(p);
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
                _ => return Err(ParseError::unexpected("numeric expression", &p))
            }
        }

        Ok(tree.unwrap())
    }

    //
    // term = { factor ~ (multiplier ~ factor)* }
    //
    pub fn term(pair: Pair) -> ParseResult<AstNode> {
        assert_eq!(pair.as_rule(), Rule::term, "Expected term!");

        let mut prior_op: Option<OpCode> = None;
        let mut tree: Option<AstNode> = None;

        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::multiplier => {
                    // Just track the multiplier, we will consume it when we hit a term
                    prior_op = OpCode::from_pair(p);
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
                _ => return Err(ParseError::unexpected("term", &p))
            }
        }

        Ok(tree.unwrap())
    }

    //
    // factor = { primary ~ (pow ~ primary)* }
    //
    pub fn factor(pair: Pair) -> ParseResult<AstNode> {
        assert_eq!(pair.as_rule(), Rule::factor, "Expected factor!");

        let mut prior_op: Option<OpCode> = None;
        let mut tree: Option<AstNode> = None;

        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::pow => {
                    // Just track the multiplier, we will consume it when we hit a term
                    prior_op = OpCode::from_pair(p);
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
                _ => return Err(ParseError::unexpected("factor", &p))
            }
        }

        Ok(tree.unwrap())
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

        let sub_pair = first_child(pair);
        match sub_pair.as_rule() {
            Rule::numeric_rep => Self::numeric_rep(sub_pair),
            Rule::numeric_function_ref => Self::numeric_function_ref(sub_pair),
            Rule::numeric_variable => Self::numeric_variable(sub_pair),  
            Rule::numeric_expression => Self::numeric_expression(sub_pair),
            _ => Err(ParseError::unexpected("primary", &sub_pair))
        }
    }

    //
    // numeric_variable = { numeric_array_element | simple_numeric_variable }
    //
    pub fn numeric_variable(pair: Pair) -> ParseResult<AstNode> {
        assert_rule(&pair, Rule::numeric_variable);

        let sub_pair = first_child(pair);
        match sub_pair.as_rule() {
            Rule::numeric_array_element => Self::numeric_array_element(sub_pair),
            Rule::simple_numeric_variable => Self::simple_numeric_variable(sub_pair),
            _ => Err(ParseError::unexpected("numeric variable", &sub_pair))
        }
    }

    //
    // numeric_array_element  = { numeric_array_name ~ subscript }
    // subscript = { "(" ~ numeric_expression ~ ( "," ~ numeric_expression )?  ~ ")" }
    //
    pub fn numeric_array_element(pair: Pair) -> ParseResult<AstNode> {
        let mut pairs = pair.into_inner();

        let id = vars::array_name_to_id(pairs.next().unwrap().as_str());

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
        let pair = first_child(pair);
        match pair.as_rule() {
            Rule::string_variable => Self::string_variable(pair),
            Rule::string_constant => Self::quoted_string(pair),
            _ => Err(ParseError::unexpected("string expression", &pair))
        }
    }

    pub fn simple_numeric_variable(pair: Pair) -> ParseResult<AstNode> {
        let id = vars::num_name_to_id(pair.as_str());
        Ok(AstNode::NumRef(id))
    }

    pub fn string_variable(pair: Pair) -> ParseResult<AstNode> {
        let id = vars::string_name_to_id(pair.as_str());
        Ok(AstNode::StringRef(id))
    }

    pub fn quoted_string(pair: Pair) -> ParseResult<AstNode> {
        let mut inner = pair.as_str();
        inner = &inner[1..inner.len()-1];
        Ok(AstNode::StringVal(inner.to_owned())) 
    }
}

