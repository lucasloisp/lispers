use nom::digit;
use nom::types::CompleteStr as Input;
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

named!(parse_operator<Input, Operator>, map_opt!(take!(1), |s:Input| sym_to_operator(s.0)));


named!(parse_number<Input, Expression>, map!(map_res!(digit, |s:Input| { FromStr::from_str(s.0) }), Expression::Number));

named!(parse_expression<Input, Expression>, alt!(
        parse_number | delimited!(char!('('), map!(parse_program, Expression::Program), char!(')'))
        ));
 


named!(pub parse_program<Input, Program>, ws!(do_parse!(
        op: parse_operator >>
        expr: many1!(parse_expression) >>
        (Program { op, expr })
)));


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsed_operator_coincides_with_symbol() {
        assert_eq!(Ok((Input(""), Operator::Add)), parse_operator(Input("+")));
        assert_eq!(Ok((Input(""), Operator::Subtract)), parse_operator(Input("-")));
        assert_eq!(Ok((Input(""), Operator::Multiply)), parse_operator(Input("*")));
        assert_eq!(Ok((Input(""), Operator::Divide)), parse_operator(Input("/")));
    }

    #[test]
    fn parsed_number_is_correct() {
        assert_eq!(Ok((Input(""), Expression::Number(45))), parse_number(Input("45")));
        assert_eq!(Ok((Input(" "), Expression::Number(45))), parse_number(Input("45 ")));
    }

}
