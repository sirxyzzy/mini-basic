

use super::parser::{ Pair, Pairs, Rule};
use super::*;
use std::collections::HashSet;
use vars;
use std::str::FromStr;
use itertools::Itertools;


// Since the tree is immutable, we can build it with refs
#[derive(Debug)]
pub enum AstNode {
    Program {
        lines: Vec<AstNode>
    },

    // All statements have a line number
    // and all must be referenced in 
    DataStatement{ line: u16, datums: Vec<AstNode>},
    DefStatement{ line: u16, id: usize, parameters: Option<Vec<usize>>, expression: Box<AstNode> },
    DimensionStatement{ line: u16, declarations: Vec<AstNode>},
    GosubStatement{ line: u16, line_ref: u16},
    GotoStatement{ line: u16, line_ref: u16},
    IfThenStatement{ line: u16, expr: Box<AstNode>, then: u16 },
    InputStatement{ line: u16, vars: Vec<AstNode>},
    LetStatement{ line: u16, var: Box<AstNode>, val: Box<AstNode> },
    OnGotoStatement{ line: u16, expr: Box<AstNode>, line_refs: Vec<u16>},
    OptionStatement{ line: u16, base: u16 },
    PrintStatement{ line: u16, items: Vec<AstNode> },
    RandomizeStatement{ line: u16 },
    ReadStatement{ line: u16, vars: Vec<AstNode> },
    RemarkStatement{ line: u16 },
    RestoreStatement{ line: u16 },
    ReturnStatement{ line: u16 },
    StopStatement{ line: u16 },
    ForStatement{ line: u16, id: usize, from: Box<AstNode>, to: Box<AstNode>, step: Option<Box<AstNode>>},
    NextStatement{ line: u16, id: usize},
    EndStatement{ line: u16 },

    NumericExpression(Box<AstNode>),
    StringExpression(Box<AstNode>),
    // RelationalExpression(Box<AstNode>),

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

    //
    // Declarations
    //
    
    // Array definitions
    ArrayDecl1 { id: usize, bound: usize },
    ArrayDecl2 { id: usize, bound1: usize, bound2: usize },

    // Definition of user defined function
    // functions are invoked through an OpCode::Def
    DefinedFunction {
     
    },
}

#[derive(Debug,Clone,Copy)]
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
    Def (usize)
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
            Rule::numeric_defined_function => Some(OpCode::Def(vars::def_name_to_id(pair.as_str()))),

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
pub fn get_line_number_maybe(node: &AstNode) -> Option<u16> {
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

/// A convenience to get a line number assuming we have a line!
pub fn get_line_number(node: &AstNode) -> u16 {
    get_line_number_maybe(node).unwrap()
}


//
// Am I a statement, yes if I have a line number!
//
pub fn is_statement(node: &AstNode) -> bool {
    match get_line_number_maybe(node) {
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
                print_ast_helper(&format!("{:4}", get_line_number(line)), line, level+1);
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

        AstNode::NextStatement{id, ..} => 
            println!("NEXT {}", vars::id_to_num_name(*id)),

        AstNode::LetStatement{var, val, ..} => {
            println!("LET");
            print_ast_helper("     ", var, level+1);
            print_ast_helper("value", val, level+1);
        },

        AstNode::IfThenStatement{expr, then, ..} => {
            println!("IF THEN {}", then);
            print_ast_helper("", expr, level+1);    
        }

        AstNode::PrintStatement{items, ..} => print_ast_list_helper("PRINT", items, level),
        AstNode::InputStatement{vars, ..} => print_ast_list_helper("INPUT", vars, level),
        AstNode::DataStatement{datums, ..} => print_ast_list_helper("DATA", datums, level),

        AstNode::RemarkStatement{..} => println!("REM ..."),
        AstNode::RandomizeStatement{..} => println!("RANDOMIZE"),
        AstNode::RestoreStatement{..} => println!("RESTORE"),
        AstNode::ReturnStatement{..} => println!("RETURN"),
        AstNode::StopStatement{..} => println!("STOP"),
        AstNode::EndStatement{..} => println!("END"),

        // DefStatement{ line: u16, name: String, parameters: Option<Vec<usize>>, expression: Box<AstNode> }
        AstNode::DefStatement{ id, parameters, expression, ..} => {
            match parameters {
                Some(p) => {
                    let params = p.iter().map(|id| vars::id_to_num_name(*id)).join(", ");
                    println!("DEF {} ({})", vars::id_to_def_name(*id), params)
                }
                _ => println!("DEF {} ", vars::id_to_def_name(*id))
            }
            print_ast_helper("", expression, level+1);
        }

        AstNode::DimensionStatement{declarations, ..} => print_ast_list_helper("DIM", declarations, level),
        AstNode::ReadStatement{vars, ..} => print_ast_list_helper("READ", vars, level),

        AstNode::GosubStatement{line_ref, ..} => println!("GOSUB {}", line_ref), 
        AstNode::GotoStatement{line_ref, ..} => println!("GOTO {}", line_ref),

        AstNode::OnGotoStatement{expr, line_refs, ..} => {
            let line_refs = line_refs.iter().map(|line_ref| line_ref.to_string()).join(", ");
            println!("ON expr: GOTO {}", line_refs);
            print_ast_helper("expr", expr, level+1);
        }
        
        AstNode::OptionStatement{base, ..} => println!("OPTION BASE {}", base), 

        AstNode::ArrayDecl1{id, bound} => println!("{}[{}]", vars::id_to_array_name(*id), bound),
        AstNode::ArrayDecl2{id, bound1, bound2} => {
            println!("{}[{},{}]", vars::id_to_array_name(*id), bound1, bound2)
        }

        _ => println!("{:?}", node) 
    }

    fn print_ast_list_helper(thing: &str, nodes: &Vec<AstNode>, level:usize) {
        println!("{}", thing);
        for var in nodes.iter() {
            print_ast_helper("", var, level+1);
        }  
    }
}

pub struct AstBuilder {
    // Somewhere to store side effects such as variables
}

impl Error {
    fn from_error<E: std::error::Error>(error: E, pair: &Pair) -> Error {
        let span = pair.as_span();
        Error::AstError{
            reason: error.to_string(),
            rule_type: pair.as_rule(),
            start_pos: span.start_pos().pos(),
            end_pos: span.end_pos().pos(),
        }
    }

    fn new(reason: &str, pair: &Pair) -> Error {
        let span = pair.as_span();
        Error::AstError{
            reason: format!("{} {:?}", reason, pair.as_rule()),
            rule_type: pair.as_rule(),
            start_pos: span.start_pos().pos(),
            end_pos: span.end_pos().pos(),
        }
    }

    fn expected(expected:Rule, actual: Option<Rule>, location: &Pair) -> Error {
        Self::new(&format!("Got {:?} but expected {:?} in", actual, expected), location)
    }  

    fn unexpected(context: &str, pair: &Pair) -> Error {
        Self::new(&format!("Expected {} in", context), pair)
    }

    fn not_implemented(pair: &Pair) -> Error {
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

pub fn collect_pairs(pair: Pair, rule: Rule) -> Vec<Pair> {
    let mut pairs: Vec<Pair> = Vec::new();  
    collect_pairs_helper(pair, rule, &mut pairs, 0);
    pairs
}

pub fn collect_pairs_helper<'i>(pair: Pair<'i>, rule: Rule, pairs: &mut Vec<Pair<'i>>, level: i32)  {
    for p in pair.clone().into_inner() {
        if p.as_rule() == rule {
            pairs.push(p);
        } else {
            collect_pairs_helper(p, rule, pairs, level+1);
        }
    }    
}

struct Matcher<'a> {
    pair: Pair<'a>,
    pairs: Pairs<'a>,
}

impl <'a> Matcher<'a> {

    pub fn new(pair: Pair) -> Matcher {
        let pairs = pair.clone().into_inner();
        Matcher{ pair, pairs }
    }

    pub fn new_expect(pair: Pair, expected_rule: Rule) -> Matcher {
        let rule = pair.as_rule();
        if rule != expected_rule {
            panic!("Expected pair to be a {:?} but I found a {:?}", expected_rule, rule);
        }
        let pairs = pair.clone().into_inner();
        Matcher{ pair, pairs }
    }

    pub fn expect(&mut self, rule: Rule) -> Pair {
        match self.pairs.next() {
            Some(p) if p.as_rule() == rule => p,
            Some(x) => panic!("Got {:?} when I wanted a {:?} from a {:?}", x.as_rule(), rule, self.pair),
            None => panic!("Got None when I wanted a {:?} from a {:?}", rule, self.pair),
        }
    }

    pub fn expect_ast(&mut self, rule: Rule, map_fn: fn(Pair) -> Result<AstNode>) -> Result<AstNode> {
        map_fn(self.expect(rule))
    }

    pub fn expect_ast_list(&mut self, rule: Rule, map_fn: fn(Pair) -> Result<Vec<AstNode>>) -> Result<Vec<AstNode>> {
        map_fn(self.expect(rule))
    }

    pub fn expect_as_string(&mut self, rule: Rule) -> String {
        self.expect(rule).as_str().trim().to_string()
    }

    pub fn expect_as<T: FromStr>(&mut self, rule: Rule) -> T {
        parse_pair::<T>(self.expect(rule))
    }

    /// Helper, build a list of ASTs, from the children of a given node
    /// Only process children of a given type
    /// Pass an AstBuilder function (or closure) to process each element
    fn expect_asts(&mut self, rule: Rule, map_fn: fn(Pair) -> Result<AstNode>) -> Result<Vec<AstNode>> {
        let mut list: Vec<AstNode> = Vec::new();
        for p in &mut self.pairs {
            if p.as_rule() == rule {
                list.push(map_fn(p)?)
            }
        }
        Ok(list)
    }

    // From where we are get a list of the given type, stop on end of list or rule type mismatch
    fn list_of<T: FromStr>(&mut self, rule: Rule) -> Vec<T> {
        let mut list: Vec<T> = Vec::new();
        for p in &mut self.pairs {
            if p.as_rule() == rule {
                list.push(parse_pair::<T>(p))
            } else {
                break;
            }
        }
        list
    }

    fn all_asts(&mut self, map_fn: fn(Pair) -> Result<AstNode>) -> Result<Vec<AstNode>> {
        let mut list: Vec<AstNode> = Vec::new();
    
        for p in &mut self.pairs {
            list.push(map_fn(p)?);
        }
    
        Ok(list)
    }

    pub fn maybe_expect(&mut self, rule: Rule) -> Option<Pair> {
        match self.pairs.peek() {
            Some(p) if p.as_rule() == rule => self.pairs.next(),
            _ => None
        }
    }

    pub fn maybe_expect_as_string(&mut self, rule: Rule) -> Option<String> {
        match self.maybe_expect(rule) {
            Some(p) => Some(p.as_str().trim().to_string()),
            None => None
        }
    }

    pub fn maybe_expect_ast(&mut self, rule: Rule, map_fn: fn(Pair) -> Result<AstNode>) -> Option<AstNode> {
        match self.maybe_expect(rule) {
            Some(p) => map_fn(p).ok(),
            None => None,
        }
    }

    pub fn maybe_collect_from(&mut self, rule: Rule, sub_rule: Rule) -> Option<Vec<Pair>> {
        match self.maybe_expect(rule) {
            Some(p) => Some(collect_pairs(p, sub_rule)),
            None => None,
        }
    }
}

/// Parse a pair into an arbitrary parsable type
pub fn parse_pair<T: FromStr>(pair: Pair) -> T {
    // I don't understand why, but sometimes I get whitespace around stuff
    // which breaks parsing, so I trim!
    match pair.as_str().trim().parse::<T>() {
        Ok(x) => x,
        Err(_) => panic!("Could not parse {} into a {}", pair.as_str(), std::any::type_name::<T>())
    }      
}

pub fn assert_rule(pair: &Pair, expected: Rule) {
    assert_eq!(pair.as_rule(), expected, "Expected <{:?}>", expected)
}


impl AstBuilder {   
    // Main entry point, build the AST for a whole program
    pub fn build(pair: Pair, _options: &ParseOptions) -> Result<AstNode> {
        Self::program(pair)
    }

    pub fn validate_program(node: AstNode) -> Result<()> {
        // Apply program wide validations such as line number references
        
        let mut errors = 0;

        match node {
            AstNode::Program{lines} => {
                let line_numbers: HashSet<u16> = 
                    lines.iter().map(|l| get_line_number(l)).collect();

                for entry in lines.iter() {
                    match entry {
                        AstNode::GotoStatement{line, line_ref, ..} => {
                            if !line_numbers.contains(line_ref) {
                                error!("E1001 {} GOTO {} <- invalid line number", line, line_ref);
                                errors += 1;
                            }
                        }
                        AstNode::GosubStatement{line, line_ref, ..} => {
                            if !line_numbers.contains(line_ref) {
                                error!("E1002 {} GOSUB {} <- invalid line number", line, line_ref);
                                errors += 1;
                            }
                        }
                        AstNode::OnGotoStatement{line, line_refs, ..} => {
                            for line_ref in line_refs {
                                if !line_numbers.contains(line_ref) {
                                    error!("E1003 {} ON GOTO {} <- invalid line number", line, line_ref);
                                    errors += 1;
                                }
                            }
                        }
                        _ => ()
                    }
                }
            }
            _ => panic!("Not a program!")
        }

        if errors > 0 {
            let reason = format!("{} errors, failing parse", errors);
            Err(Error::ValidationError{reason})
        } else {
            Ok(())
        }
    }
    
    // program = ${ block ~ end_line }
    pub fn program(pair: Pair) -> Result<AstNode> {
        assert_rule(&pair, Rule::program);

        let mut pairs = pair.into_inner();

        let mut lines = Self::block(pairs.next().unwrap())?;

        let end = Self::end_statement(pairs.next().unwrap())?;

        lines.push( end);

        Ok(AstNode::Program{lines})
    }

    // block = ${ (line | for_block)* }
    pub fn block(pair: Pair) -> Result<Vec<AstNode>> {
        assert_rule(&pair, Rule::block);

        let mut lines: Vec<AstNode> = Vec::new(); 
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::line => lines.push(Self::line(p)?),
                Rule::for_block => lines.append(&mut Self::for_block(p)?),
                _ => panic!("block {:?}", p)
            }
        }
        Ok(lines)
    }
    
    // line = !{ line_number ~ statement ~ end_of_line }
    pub fn line(pair: Pair) -> Result<AstNode> {
        let mut pairs = pair.into_inner();
        let line = Self::line_number(pairs.next().unwrap());
        let statement = Self::statement(pairs.next().unwrap(), line)?;
        Ok(statement)
    }

    //
    // Statements have a single child, the parse tree for a particular statement
    //
    pub fn statement(pair: Pair, line: u16) -> Result<AstNode> {
        let p = first_child(pair);
        match p.as_rule() {
            Rule::data_statement => Self::data_statement(p, line),
            Rule::def_statement => Self::def_statement(p, line),
            Rule::dimension_statement => Self::dimension_statement(p, line),
            Rule::gosub_statement => Self::gosub_statement(p, line),
            Rule::goto_statement => Self::goto_statement(p, line),
            Rule::if_then_statement => Self::if_then_statement(p, line),
            Rule::input_statement => Self::input_statement( p,line),
            Rule::let_statement => Self::let_statement( p,line),
            Rule::on_goto_statement => Self::on_goto_statement( p,line),
            Rule::option_statement => Self::option_statement( p,line),
            Rule::print_statement => Self::print_statement( p,line),
            Rule::randomize_statement => Ok(AstNode::RandomizeStatement{line}),
            Rule::read_statement => Self::read_statement( p,line),
            Rule::remark_statement => Ok(AstNode::RemarkStatement{line}),
            Rule::restore_statement => Ok(AstNode::RestoreStatement{line}),
            Rule::return_statement => Ok(AstNode::ReturnStatement{line}),
            Rule::stop_statement => Ok(AstNode::StopStatement{line}),
            _ => panic!("Not a statement, we should never reach here!")
        }
    }

    // read_statement = { "READ " ~ variable_list }
    fn read_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let mut mc = Matcher::new(pair);
        let vars = mc.expect_ast_list(Rule::variable_list, Self::variable_list)?;
        Ok(AstNode::ReadStatement{line, vars})
    }

    // option_statement = { "OPTION " ~ "BASE" ~ base }
    // base = @{"0" | "1"}
    fn option_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let mut mc = Matcher::new(pair);

        let base = mc.expect_as::<u16>(Rule::base);

        Ok(AstNode::OptionStatement{line, base})
    }

    // on_goto_statement = { "ON " ~ numeric_expression ~ "GO" ~ "TO" ~ line_number_ref ~ ( "," ~ line_number_ref)*  }
    fn on_goto_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let mut mc = Matcher::new(pair);
        let expr = box mc.expect_ast(Rule::numeric_expression, Self::numeric_expression)?;
        let line_refs = mc.list_of::<u16>(Rule::line_number_ref);
        Ok(AstNode::OnGotoStatement{line, expr, line_refs})
    }

    fn gosub_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let line_ref = Matcher::new(pair).expect_as(Rule::line_number_ref);
        Ok(AstNode::GosubStatement{line, line_ref})
    }

    fn goto_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let line_ref = Matcher::new(pair).expect_as(Rule::line_number_ref);
        Ok(AstNode::GotoStatement{line, line_ref})
    }    

    // def_statement = { "DEF " ~ numeric_defined_function ~ parameter_list? ~ "=" ~ numeric_expression }
    fn def_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let mut mc = Matcher::new(pair);

        let id = vars::def_name_to_id(&mc.expect_as_string(Rule::numeric_defined_function));
        let parameters: Option<Vec<usize>> =
            match mc.maybe_collect_from(Rule::parameter_list, Rule::simple_numeric_variable) {
                Some(v) => Some(v.iter().map(|p| vars::num_name_to_id( p.as_str().trim())).collect()),
                None => None
            };

        let expression = mc.expect_ast(Rule::numeric_expression, Self::numeric_expression)?;

        Ok(AstNode::DefStatement{line, id, parameters: parameters, expression: box expression})

    }

    // data_statement = { "DATA " ~ datum ~ ("," ~ datum)* }
    fn data_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let list = Matcher::new(pair).expect_asts(Rule::datum, Self::datum)?;
        Ok(AstNode::DataStatement{line, datums: list})
    }

    // datum = { quoted_string | unquoted_string }
    fn datum(pair: Pair) -> Result<AstNode> {
        let p = first_child(pair);

        match p.as_rule() {
            Rule::quoted_string => Ok(Self::quoted_string(p)?),
            Rule::unquoted_string => Ok(Self::unquoted_string(p)?),
            _ => panic!("datum, not a string (quoted or unquoted) {:?}", p)
        }
    }

    // dimension_statement = { "DIM " ~ array_declaration ~ ("," ~ array_declaration)* }
    fn dimension_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let list = Matcher::new(pair).expect_asts(Rule::array_declaration, Self::array_declaration)?;
        Ok(AstNode::DimensionStatement{line, declarations: list})
    }

    // array_declaration = { numeric_array_name ~ "(" ~ bounds ~ ")" }
    // bounds = { integer ~ ("," ~ integer)? }
    fn array_declaration(pair: Pair) -> Result<AstNode> {
        let mut pairs = pair.into_inner();

        // The id for the array
        let id = vars::array_name_to_id(pairs.next().unwrap().as_str());

        // Iterator over bounds pairs
        let mut bps = pairs.next().unwrap().into_inner();

        // Must have at least one!
        let bound1_pair = bps.next().expect("Must have at least one bound");
        let bound1 = parse_pair::<usize>(bound1_pair);

        // Optional second
        let bound2_pair = bps.next();

        match bound2_pair {
            None => {
                Ok(AstNode::ArrayDecl1{id, bound: bound1})
            }
            Some(b) => {
                let bound2 = parse_pair::<usize>(b);
                Ok(AstNode::ArrayDecl2{id, bound1, bound2})
            }
        }
    }

    // input_statement = { "INPUT " ~ variable_list }
    // variable_list = { variable ~ ("," ~ variable)* }
    fn input_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let mut var_list: Vec<AstNode> = Vec::new();

        let list_pair = first_child(pair);

        for p in list_pair.into_inner() {
            var_list.push(Self::variable(p)?);
        }
        Ok(AstNode::InputStatement{line, vars: var_list})
    }

    // variable_list = { variable ~ ("," ~ variable)* }
    fn variable_list(pair: Pair) -> Result<Vec<AstNode>> {
        Ok(Matcher::new(pair).expect_asts(Rule::variable, Self::variable)?)
    }

    // variable = { string_variable | numeric_variable }
    fn variable(pair: Pair) -> Result<AstNode> {
        let child = first_child(pair);
        match child.as_rule() {
            Rule::string_variable => Ok(Self::string_variable(child)?),
            Rule::numeric_variable => Ok(Self::numeric_variable(child)?),
            _ => panic!("variable  {:?}", child)
        }
    }

    // print_statement = { ("PRINT " ~ print_list) | "PRINT" }
    fn print_statement(pair: Pair, line: u16) -> Result<AstNode> {
        match pair.into_inner().peek() {
            None => Ok(AstNode::PrintStatement{line, items: Vec::new()}),
            Some(child) => {
                let items = Self::print_list(child)?;
                Ok(AstNode::PrintStatement{line, items})
            }
        }
    }

    // print_list = { (print_item? ~ print_separator)* ~ print_item? }
    fn print_list(pair: Pair) -> Result<Vec<AstNode>> {
        Matcher::new(pair).all_asts(|p| {
            match p.as_rule() {
                Rule::print_item => Ok(Self::print_item(p)?),
                Rule::print_separator => Ok(Self::print_separator(p)?),
                _ => panic!("print item  {:?}", p)
            }
        })
    }

    fn print_separator(pair: Pair) -> Result<AstNode> {
        match pair.as_str() {
            "," => Ok(AstNode::PrintComma),
            ";" => Ok(AstNode::PrintSemi),
            _ => panic!("print separator {:?}", pair)
        }
    }

    // print_item = { tab_call | expression }
    fn print_item(pair: Pair) -> Result<AstNode> {
        let p = first_child(pair);

        match p.as_rule() {
            Rule::tab_call => Ok(Self::tab_call(p)?),
            Rule::expression => Ok(Self::expression(p)?),
            _ => panic!("print separator {:?}", p)
        }
    }

    // tab_call = { "TAB" ~ "(" ~ numeric_expression ~ ")" }
    fn tab_call(pair: Pair) -> Result<AstNode> {
        let p = first_child(pair);

        match p.as_rule() {
            Rule::numeric_expression => Ok(AstNode::TabCall(box Self::numeric_expression(p)?)),
            _ => panic!("tab_call {:?}", p)
        }
    }

    // if_then_statement = { "IF " ~ relational_expression ~ "THEN" ~ line_number_ref }
    fn if_then_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let (rel_pair,line_pair) = first_two_children(pair);
        let expr = box Self::relational_expression(rel_pair)?;
        let then = Self::line_number(line_pair);
        Ok(AstNode::IfThenStatement{line, expr, then})
    }

    // relational_expression = { (numeric_expression ~ relation ~ numeric_expression) 
    //    | (string_expression ~ equality_relation ~ string_expression) }
    fn relational_expression(pair: Pair) -> Result<AstNode> {

        let (left_pair, rel, right_pair) = first_three_children(pair);

        match left_pair.as_rule() {
            Rule::numeric_expression => {
                let left = box Self::numeric_expression(left_pair)?;
                let right = box Self::numeric_expression(right_pair)?;
                let op = OpCode::from_pair(rel).expect("I want a valid opcode!");
                Ok(AstNode::BinOp{op, left, right})
            },
            Rule::string_expression => {
                let left = box Self::string_expression(left_pair)?;
                let right = box Self::string_expression(right_pair)?;
                let op = OpCode::from_pair(rel).unwrap();
                Ok(AstNode::BinOp{op, left, right})
            },
            _ => panic!("relational expression {:?}", left_pair)
        }
    }

    // let_statement = { numeric_let_statement | string_let_statement }
    // numeric_let_statement   = { "LET " ~ numeric_variable ~ "=" ~ numeric_expression }
    // string_let_statement = { "LET " ~ string_variable  ~ "=" ~ string_expression }
    fn let_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let p = first_child(pair);
        match p.as_rule() {
            Rule::numeric_let_statement => {
                let (var_pair, expression_pair) = first_two_children(p);
                let var = box Self::numeric_variable(var_pair)?;
                let val = box Self::numeric_expression(expression_pair)?;
                Ok(AstNode::LetStatement{line, var, val})
            }
            Rule::string_let_statement => {
                let (var_pair, expression_pair) = first_two_children(p);
                let var = box Self::string_variable(var_pair)?;
                let val = box Self::string_expression(expression_pair)?;
                Ok(AstNode::LetStatement{line, var, val})
            },
            _ => panic!("let {:?}", p)
        }
    }

    //
    // for
    //
    // Gets a bit complicated as I have a highly structured parse tree,
    // but want a flattened AST (a vec of lines)
    //

    // for_block = { for_line ~ for_body }
    pub fn for_block(pair: Pair) -> Result<Vec<AstNode>> {
        let mut lines: Vec<AstNode> = Vec::new();

        let mut pairs = pair.into_inner();
        let for_line = Self::for_line(pairs.next().unwrap())?;

        match for_line {
            AstNode::ForStatement{id,..} => {
                let mut body_lines = Self::for_body(pairs.next().unwrap(), id)?;
                lines.push(for_line);
                lines.append(&mut body_lines);
                Ok(lines)
            },
            _ => panic!("Only possibility is a for statement!")
        }
    }

    // for_line = !{ line_number ~ for_statement ~ end_of_line }
    pub fn for_line(pair: Pair) -> Result<AstNode> {
        let mut pairs = pair.into_inner();

        let line = Self::line_number(pairs.next().unwrap());

        Self::for_statement(pairs.next().unwrap(), line)
    }

    // for_statement = { "FOR" ~ simple_numeric_variable ~  "=" ~ numeric_expression  ~ "TO" ~ numeric_expression ~ ( "STEP" ~ numeric_expression)?  }
    pub fn for_statement(pair: Pair, line: u16) -> Result<AstNode> {
        let mut pairs = pair.into_inner();

        let id = vars::num_name_to_id(pairs.next().unwrap().as_str());
        
        let from = box Self::numeric_expression(pairs.next().unwrap())?;
        let to = box Self::numeric_expression(pairs.next().unwrap())?;

        let step = match pairs.next() {
            Some(p) => Some(box Self::numeric_expression(p)?),
            None => None
        };

        Ok(AstNode::ForStatement{line, id, from, to, step})
    }

    // for_body { block ~ next_line }
    pub fn for_body(pair: Pair, id: usize) -> Result<Vec<AstNode>> {
        let mut pairs = pair.into_inner();

        let mut lines = Self::block(pairs.next().unwrap())?;

        let next_line = Self::next_line(pairs.next().unwrap(), id)?;
        lines.push(next_line);

        Ok(lines)
    }

    // next_line = !{ line_number ~ next_statement ~ end_of_line }
    // next_statement = { "NEXT" ~ simple_numeric_variable }
    pub fn next_line(pair: Pair, expected_id: usize) -> Result<AstNode> {
        let mut pairs = pair.clone().into_inner();

        let line = Self::line_number(pairs.next().unwrap());

        // Validate matching id, next_statement/simple_numeric_variable
        let sub_pair = first_child(pairs.next().unwrap());
        let id = vars::num_name_to_id(sub_pair.as_str());

        if expected_id != id {
            let m = format!("Mismatched NEXT, expected control variable {} but got {}", 
                vars::id_to_num_name(expected_id), vars::id_to_num_name(id));
            Err(Error::new(&m, &pair)) // This may happen, we don't check this in the grammar
        }
        else {
            Ok(AstNode::NextStatement{line, id})
        }
    }

    // This can handle both line numbers and line number refs
    pub fn line_number(pair: Pair) -> u16 {
        pair.as_str().trim().parse::<u16>().expect(&format!("Could not parse line number {}", pair))
    }

    pub fn end_line(_pair: Pair) -> Result<()> {
        Ok(())
    }

    pub fn end_statement(pair: Pair) -> Result<AstNode> {
        let line = Self::line_number(first_child(pair));
        Ok(AstNode::EndStatement{line})
    }

    pub fn numeric_rep(pair: Pair) -> Result<AstNode> {
        let v= parse_pair::<f64>(pair);
        Ok(AstNode::NumVal(v))
    }

    //
    // { numeric_function_name ~ argument_list? }
    //
    pub fn numeric_function_ref(pair: Pair) -> Result<AstNode> {
        assert_rule(&pair, Rule::numeric_function_ref);

        let mut pairs = pair.into_inner();
        let op = OpCode::from_pair(pairs.next().unwrap()).unwrap();

        match pairs.next() {
            Some(p) => {
                let arg_ast = Self::numeric_expression(first_child(first_child(p)))?;
                Ok(AstNode::MonOp{op, arg: box arg_ast})
            }
            None => Ok(AstNode::Op(op))
        }
    }

    pub fn expression(pair: Pair) -> Result<AstNode> {
        let pair = first_child(pair);
        match pair.as_rule() {
            Rule::string_expression => Self::string_expression(pair),
            Rule::numeric_expression => Self::numeric_expression(pair),
            _ => panic!("expression {:?}", pair)
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
    pub fn numeric_expression(pair: Pair) -> Result<AstNode> {
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
                                        AstNode::MonOp { op, arg:box term_tree }
                                    )
                                }
                                Some(prev_tree) => {
                                    // The more common case, a binary operator on what we already have, and the next term
                                    tree = Some(
                                        AstNode::BinOp { op, left:box prev_tree, right:box term_tree })
                                }
                            }
                        }
                    }
                }
                _ => panic!("numeric expression {:?}", p)
            }
        }

        Ok(AstNode::NumericExpression(box tree.unwrap()))
    }

    //
    // term = { factor ~ (multiplier ~ factor)* }
    //
    pub fn term(pair: Pair) -> Result<AstNode> {
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

                            tree = Some(AstNode::BinOp { op, left:box tree.unwrap(), right:box factor_tree })
                        }
                    }

                }
                _ => panic!("term {:?}", p)
            }
        }

        Ok(tree.unwrap())
    }

    //
    // factor = { primary ~ (pow ~ primary)* }
    //
    pub fn factor(pair: Pair) -> Result<AstNode> {
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

                            tree = Some(AstNode::BinOp { op, left:box tree.unwrap(), right:box primary_tree })
                        }
                    }

                }
                _ => panic!("factor {:?}", p)
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
    pub fn primary(pair: Pair) -> Result<AstNode> {
        assert_rule(&pair, Rule::primary);

        let sub_pair = first_child(pair);
        match sub_pair.as_rule() {
            Rule::numeric_rep => Self::numeric_rep(sub_pair),
            Rule::numeric_function_ref => Self::numeric_function_ref(sub_pair),
            Rule::numeric_variable => Self::numeric_variable(sub_pair),  
            Rule::numeric_expression => Self::numeric_expression(sub_pair),
            _ => panic!("primary {:?}", sub_pair)
        }
    }

    //
    // numeric_variable = { numeric_array_element | simple_numeric_variable }
    //
    pub fn numeric_variable(pair: Pair) -> Result<AstNode> {
        assert_rule(&pair, Rule::numeric_variable);

        let sub_pair = first_child(pair);
        match sub_pair.as_rule() {
            Rule::numeric_array_element => Self::numeric_array_element(sub_pair),
            Rule::simple_numeric_variable => Self::simple_numeric_variable(sub_pair),
            _ => panic!("numeric variable {:?}", sub_pair)
        }
    }

    //
    // numeric_array_element  = { numeric_array_name ~ subscript }
    // subscript = { "(" ~ numeric_expression ~ ( "," ~ numeric_expression )?  ~ ")" }
    //
    pub fn numeric_array_element(pair: Pair) -> Result<AstNode> {
        let mut pairs = pair.into_inner();

        let id = vars::array_name_to_id(pairs.next().unwrap().as_str());

        let mut sub_pairs = pairs.next().unwrap().into_inner();

        let index1 = box Self::numeric_expression(sub_pairs.next().unwrap())?;

        Ok(match sub_pairs.next() {
            Some(p) => {
                let index2 = box Self::numeric_expression(p)?;
                AstNode::ArrayRef2{ id, index1, index2}
            }
            None => {
                AstNode::ArrayRef1{ id, index:index1}
            }
        })
    }

    pub fn string_expression(pair: Pair) -> Result<AstNode> {
        let pair = first_child(pair);

        match pair.as_rule() {
            Rule::string_variable => {
                Ok(AstNode::StringExpression(box Self::string_variable(pair)?))
            }
            Rule::string_constant => {
                Ok(AstNode::StringExpression(box Self::quoted_string(pair)?))
            }
            _ => panic!("string expression {:?}", pair)
        }
    }

    pub fn simple_numeric_variable(pair: Pair) -> Result<AstNode> {
        let id = vars::num_name_to_id(pair.as_str());
        Ok(AstNode::NumRef(id))
    }

    pub fn string_variable(pair: Pair) -> Result<AstNode> {
        let id = vars::string_name_to_id(pair.as_str());
        Ok(AstNode::StringRef(id))
    }

    pub fn quoted_string(pair: Pair) -> Result<AstNode> {
        let mut inner = pair.as_str();
        inner = &inner[1..inner.len()-1];
        Ok(AstNode::StringVal(inner.to_owned()))
    }

    pub fn unquoted_string(pair: Pair) -> Result<AstNode> {
        Ok(AstNode::StringVal(pair.as_str().trim().to_owned()))
    }
}

