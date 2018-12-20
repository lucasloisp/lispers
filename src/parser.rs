use nom::digit;
use std::str::FromStr;

#[derive(Debug,PartialEq)]
pub struct Program {
    op: Operator,
    expr: Vec<Expression>
}

#[derive(Debug, PartialEq)]
enum Operator {
    Add, Subtract, Multiply, Divide
}

#[derive(Debug,PartialEq)]
enum Expression {
    Number(i32),
    Program(Program)
}

fn sym_to_operator(sym: &str) -> Option<Operator> {
    match sym {
        "+" => Some(Operator::Add),
        "-" => Some(Operator::Subtract),
        "*" => Some(Operator::Multiply),
        "/" => Some(Operator::Divide),
        _ => None
    }
}

named!(parse_operator<&str, Operator>, map_opt!(take!(1), sym_to_operator));


named!(parse_number<&str, Expression>, map!(map_res!(digit, FromStr::from_str), Expression::Number));

named!(parse_expression<&str, Expression>, alt!(
        parse_number | delimited!(char!('('), map!(parse_program, Expression::Program), char!(')'))
        ));
 


named!(pub parse_program<&str, Program>, ws!(do_parse!(
        op: parse_operator >>
        expr: many1!(parse_expression) >>
        (Program { op, expr })
)));


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsed_operator_coincides_with_symbol() {
        assert_eq!(Ok(("", Operator::Add)), parse_operator("+"));
        assert_eq!(Ok(("", Operator::Subtract)), parse_operator("-"));
        assert_eq!(Ok(("", Operator::Multiply)), parse_operator("*"));
        assert_eq!(Ok(("", Operator::Divide)), parse_operator("/"));
    }

    #[test]
    fn parsed_number_is_correct() {
        assert_eq!(Ok(("", Expression::Number(45))), parse_number("45"));
        assert_eq!(Ok((" ", Expression::Number(45))), parse_number("45 "));
    }

}
