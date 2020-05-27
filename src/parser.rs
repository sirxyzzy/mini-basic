use pest_consume::{ Parser, match_nodes, Error};

#[derive(Parser)] // This allows Pest to add all the parse methods
#[grammar = "basic.pest"]
struct BasicParser;

// Pull in the model
use crate::*;

// Some type simplifications, for brevity
type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;
type Nodes<'i> = pest_consume::Nodes<'i, Rule, ()>;

struct Block {
    content: String
}

pub fn parse_source(source: &str, options: &ParseOptions) -> Result<()> {
    let nodes = BasicParser::parse(Rule::program, source)?;
    let main_node = nodes.single()?;

    if options.pretty_print {
        print_node(main_node.clone());
    }

    BasicParser::program(main_node)?;

    Ok(())
}

// This is the other half of the parser, using pest_consume
// It traverses the Node tree generated by Pest (Nodes are a wrapper around Pest Pairs)
// and generates custom structures (MibInfo and friends) that represents the content of the MIB
#[pest_consume::parser]
impl BasicParser {
    fn EOI(_node: Node) -> Result<()> {
        Ok(())
    }

    fn program(node: Node) -> Result<()> {
        Ok(match_nodes!(node.into_children();
            [block(mut blocks)..,end_line(_)] => blocks.for_each(|b| println!("{}", b.content)),
        ))
    }

    fn block(node: Node) -> Result<Block> {
        Ok(Block{content: node.as_str().to_owned()})
    }

    fn line_number(node: Node) -> Result<u16> {
        node.as_str().parse::<u16>().map_err(|e| node.error(e))
    }

    fn end_line(node: Node) -> Result<()> {
        Ok(())
    }

    fn end_statement(node: Node) -> Result<()> {
        Ok(())
    }
}

//
// Helpers to print a readable parse tree, mainly for debug purposes
//

fn print_node(node: Node) {
    print_single_node(&node);
    print_nodes(node.children(), 1)
}

fn print_nodes(nodes: Nodes, level: usize) {
    for node in nodes {
        // A node is a combination of the rule which matched and a span of input
        print!("{:indent$}", "", indent=level*2);
        print_single_node(&node);

        // A node can be converted to an iterator of the tokens which make it up:
        print_nodes(node.children(), level+1);
    }
}

fn print_single_node(node: &Node) {
    println!("<<{:?}>>", node.as_rule())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn program1() {
        let _node = BasicParser::program(parse(Rule::program, "999 END")).unwrap();
    }

    #[test]
    fn program2() {
        let source = r#"10 REM This is a simple program
20 END"#;
        let _node = BasicParser::program(parse(Rule::program, source)).unwrap();
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

    //
    // test helpers
    //
    fn parse(rule: Rule, input: &str) -> Node {
        match BasicParser::parse(rule, input) {
            Ok(nodes) => {
                let node = nodes.single().unwrap();
                assert_eq!(node.as_rule(), rule);
                if node.as_str() != input {
                    println!("Expected rule({:?}) to fully consume '{}'", rule, input);
                    print_node(node);
                    panic!("Failed test");
                }
                node 
            },
            Err(e) => panic!("Parse failed: {}", e)
        }
    }

    fn parse_fail(rule: Rule, input: &str) {
        assert!(BasicParser::parse(rule, input).is_err(), "Expected rule({:?}) to fail to parse '{}'", rule, input);
    }
}