use pest::Parser;

#[derive(Parser)] // This allows Pest to add all the parse methods
#[grammar = "basic.pest"]
struct BasicParser;

use super::ParseResult;
use super::ParseOptions;
use super::interpret::{AstBuilder, print_ast};
use super::pesthelpers::*;

pub type Pair<'i> = pest::iterators::Pair<'i, Rule>;
pub type Pairs<'i> = pest::iterators::Pairs<'i, Rule>;

pub fn parse_source<'i>(source: &'i str, options: &ParseOptions) -> ParseResult<Pair<'i>> {
    let pairs = BasicParser::parse(Rule::program, source)?;
    let program_pair = pairs.peek().unwrap();

    if options.pretty_print {
        print_pair(&program_pair);
    }

    Ok(program_pair)
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn expression() {     
        exp("1");
        exp("-1");
        exp("- 1 + 2 - 3");
        exp("1 * 2");
        exp("1 * (-2)");
        exp("1 + 2 + 3 + 4 + 5 + 6");
        exp("1 + (2 + (3 + (4 + (5 + 6))))");
        exp("1 - 2 * 3 + 4 / 5 ^ 2");

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
        num_rep("1", 1.0);
        num_rep("1.2", 1.2);
        num_rep(".1", 0.1);
        num_rep("2.345", 2.345);
        num_rep("3.14159", 3.14159);
        num_rep("122E+14", 122E+14);
    }

    fn num_rep(input: &str, expected: f64) {
        let pair = parse(Rule::numeric_rep, input);
        let value = AstBuilder::numeric_rep(pair).unwrap();
        assert_eq!(value, expected)
    }

    fn exp(input: &str) {
        let pair = parse(Rule::expression, input);
        print_pair(&pair);
        let ast = AstBuilder::expression(pair).unwrap();
        print_ast(&ast);
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