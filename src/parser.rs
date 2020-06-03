use pest::Parser;

#[derive(Parser)] // This allows Pest to add all the parse methods
#[grammar = "basic.pest"]
struct BasicParser;

use super::Result;
use super::ParseOptions;

pub type Pair<'i> = pest::iterators::Pair<'i, Rule>;
pub type Pairs<'i> = pest::iterators::Pairs<'i, Rule>;

pub fn parse_source<'i>(source: &'i str, _options: &ParseOptions) -> Result<Pair<'i>> {
    let pairs = BasicParser::parse(Rule::program, source)?;
    let program_pair = pairs.peek().unwrap();
    Ok(program_pair)
}

pub fn print_pair(pair: &Pair) {
    print_single_node(pair);
    print_nodes(&pair.clone().into_inner(), 1)
}

pub fn print_nodes(pairs: &Pairs, level: usize) {
    for pair in pairs.clone() {
        // A Pair is a combination of the rule which matched and a span of input
        print!("{:indent$}", "", indent=level*2);
        print_single_node(&pair);

        // A Pair can be converted to an iterator of the tokens which make it up:
        print_nodes(&pair.into_inner(), level+1);
    }
}

fn print_single_node(pair: &Pair) {
    let text = pair.as_str().to_owned().trim().to_owned();
    let rule = pair.as_rule();

    if text.len() < 32 {
        println!("<{:?}> '{}'", rule, text)
    } else {
        println!("<{:?}> ...", rule)
    }  
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AstBuilder, AstNode, print_ast};

    #[test]
    fn program1() {
        let pair = parse(Rule::program, "999 END");
        print_pair(&pair);
    }

    #[test]
    fn program2() {
        let source = r#"10 REM This is a simple program
20 END"#;
        let pair = parse(Rule::program, source);
        print_pair(&pair);
    }
    #[test]
    fn program3() {
        // Must have an end
        parse_fail(Rule::program, "10 REM This is a simple program");
    }

    #[test]
    fn program4() {
        // No spaces before line numbers
        let source = r#"10 REM This is a simple program
 20 END"#;
        parse_fail(Rule::program, source);
    }

    #[test]
    fn program5() {
        // Must have an END
        let source = r#"10 REM This is a simple program
"#;
        parse_fail(Rule::program, source);
    }

    #[test]
    fn numeric_let1() {
        let _pair = parse(Rule::numeric_let_statement, "LET S=SQR(N)");
    }

    #[test]
    fn numeric_let2() {
        let _pair = parse(Rule::numeric_let_statement, "LET P = 3.14159");
    }

    #[test]
    fn numeric_let3() {
        let _pair = parse(Rule::numeric_let_statement, "LET A(X,3) = SIN(X)*Y + 1");
    }

    #[test]
    fn string_let1() {
        let _pair = parse(Rule::string_let_statement, "LET A$ = \"ABC\"");
    }

    #[test]
    fn string_let2() {
        let _pair = parse(Rule::string_let_statement, "LET A$ = B$");
    }


    #[test]
    fn def_statement1() {
        let _pair = parse(Rule::def_statement, "DEF FNF(X) = X^4 - 1");
    }

    #[test]
    fn def_statement2() {
        let _pair = parse(Rule::def_statement, "DEF FNP = 3.14159");
    }

    #[test]
    fn def_statement3() {
        let _pair = parse(Rule::def_statement, "DEF FNA(X) = A*X + B");
    }

    #[test]
    fn goto() {
        let _pair = parse(Rule::goto_statement, "GOTO 999");
        let _pair = parse(Rule::goto_statement, "GO TO 999");
    }  

    #[test]    
    fn gosub() {
        let _pair = parse(Rule::gosub_statement, "GOSUB 22");
        let _pair = parse(Rule::gosub_statement, "GO SUB 22");
    }

    #[test]    
    fn if1() {
        let _pair = parse(Rule::if_then_statement, "IF X > Y+83 THEN 200");
    }

    #[test]    
    fn if2() {
        let _pair = parse(Rule::if_then_statement, "IF A$ <> B$ THEN 550");
    }

    #[test]    
    fn on_goto() {
        let _pair = parse(Rule::on_goto_statement, "ON L+1 GO TO 300,400,500");
    }

    #[test]    
    fn print() {
        let _pair = parse(Rule::print_statement, r#"PRINT X"#);
        let _pair = parse(Rule::print_statement, r#"PRINT X; (X+Z)/2"#);
        let _pair = parse(Rule::print_statement, r#"PRINT"#);
        let _pair = parse(Rule::print_statement, r#"PRINT TAB(10); A$; "IS DONE.""#);
        let _pair = parse(Rule::print_statement, r#"PRINT "X EQUALS", 10"#);
        let _pair = parse(Rule::print_statement, r#"PRINT X, Y"#);
        let _pair = parse(Rule::print_statement, r#"PRINT ,,,X"#);
    }

    #[test]    
    fn input() {
        let _pair = parse(Rule::input_statement, "INPUT X");
        let _pair = parse(Rule::input_statement, "INPUT X, A$, Y(2)");
        let _pair = parse(Rule::input_statement, "INPUT A, B, C");
    }

    #[test]    
    fn read() {
        let _pair = parse(Rule::read_statement, "READ X");
        let _pair = parse(Rule::read_statement, "READ X, Y, Z");
        let _pair = parse(Rule::read_statement, "READ X(1), A$, C");
    }

    #[test]
    fn data() {
        let _pair = parse(Rule::data_statement, r#"DATA 3.14159, PI, 5E-10, ",""#);
    }

    #[test]
    fn unquoted_string() {
        let _pair = parse(Rule::unquoted_string, "DATA");
        let _pair = parse(Rule::unquoted_string, "DA TA");
        let _pair = parse(Rule::unquoted_string, "3.14159");
    }

    #[test]
    fn dimension() {
        let _pair = parse(Rule::dimension_statement, "DIM A (6), B(10,10)");
        let _pair = parse(Rule::dimension_statement, "DIM A(6,2)");
    }

    #[test]
    fn line_number() {
        let _pair = parse(Rule::line_number, "1 ");
        let _pair = parse(Rule::line_number, "10 ");
        let _pair = parse(Rule::line_number, "123 ");
        let _pair = parse(Rule::line_number, "1234 ");
    }

    #[test]
    fn variable_names_num()
    {
        for id in 0..(26*11) {
            let name = crate::vars::id_to_num_name(id);
            let j = crate::vars::num_name_to_id(&name);
            println!("{} -> {} -> {}", id, name, j );
            assert_eq!(id, j, "Difference for {}", name);
        }
    }

    #[test]
    fn variable_names_string()
    {
        for id in 0..26 {
            let name = crate::vars::id_to_string_name(id);
            let j = crate::vars::string_name_to_id(&name);
            println!("{} -> {} -> {}", id, name, j );
            assert_eq!(id, j, "Difference for {}", name);
        }
    }
    
    #[test]
    fn variable_names_array()
    {
        for id in 0..26 {
            let name = crate::vars::id_to_array_name(id);
            let j = crate::vars::array_name_to_id(&name);
            println!("{} -> {} -> {}", id, name, j );
            assert_eq!(id, j, "Difference for {}", name);
        }
    }

    #[test]
    fn expression() {     
        exp("1");
        exp("-1");
        exp("- 1 + 2 - 3");
        exp("1 * 2");
        exp("1 * (-2)");
        exp("1 + 2 + 3 + 4 + 5 + 6");
        exp("1 + (2 + (3 + (4 + (5 + 6))))");
        exp("1 - 2 * 3 + 4 / 5 ^ 2");

        exp("- A + 2 - Z9");
        
        exp("SIN (1) * M(3 + COS(B)) / N(46, A)");

        exp("1");
        exp(".2");
        exp("1.2");
        exp("1.2E-3");

        // String expressions are really limited!
        exp("A$");
        exp("\"-ABCD-\"");
    }

    #[test]
    fn numeric_rep() {
        num_rep("123", 123.0);
        num_rep("123.", 123.0);
        num_rep("123.2", 123.2);
        num_rep(".123", 0.123);

        num_rep("123E2", 12300.0);
        num_rep("123.E2", 12300.0);
        num_rep("123.2E2", 12320.0);
        num_rep(".123E2", 12.3);

        num_rep("123E+2", 12300.0);
        num_rep("123.E+2", 12300.0);
        num_rep("123.2E+2", 12320.0);
        num_rep(".123E+2", 12.3);

        num_rep("123E-2", 1.23);
        num_rep("123.E-2", 1.23);
        num_rep("123.2E-2", 1.232);
        num_rep(".123E-2", 0.00123);

        num_rep("2.345", 2.345);
        num_rep("3.14159", 3.14159);
        num_rep("122E+14", 122E+14);
    }

    fn num_rep(input: &str, expected: f64) {
        println!("Checking numeric {}", input);

        let pair = parse(Rule::numeric_rep, input);

        match AstBuilder::numeric_rep(pair) {
            Err(e) => panic!("Failed numeric parse {} because {}", input, e),
            Ok(AstNode::NumVal(v)) => assert_eq!(v, expected),
            Ok(x) =>  panic!("I did not expect a {:?}", x)
        }     
    }

    fn exp(input: &str) {
        println!("Parsing: {}", input);

        let pair = parse(Rule::expression, input);
        print_pair(&pair);

        match AstBuilder::expression(pair) {
            Ok(ast) => print_ast(&ast),
            Err(e) => panic!("{}", e)
        }
    }
 
    #[test]
    fn line_number_fail() {
        parse_fail(Rule::line_number, "12345 ");
    }   

    //
    // test helpers
    //
    fn parse(rule: Rule, input: &str) -> Pair {
        match BasicParser::parse(rule, input) {
            Ok(pairs) => {
                let pair = pairs.peek().unwrap();
                assert_eq!(pair.as_rule(), rule);
                if pair.as_str() != input {
                    println!("Expected rule({:?}) to fully consume '{}' but only matched '{}'", rule, input, pair.as_str());
                    print_pair(&pair);
                    panic!("Failed test");
                }
                pair 
            },
            Err(e) => panic!("Parse failed: {}", e)
        }
    }

    fn parse_fail(rule: Rule, input: &str) {
        assert!(BasicParser::parse(rule, input).is_err(), "Expected rule({:?}) to fail to parse '{}'", rule, input);
    }
}