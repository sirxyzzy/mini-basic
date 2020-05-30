use super::parser::{Pair, Pairs};

//
// To support consumption of pests parse results (Pairs)
//

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




