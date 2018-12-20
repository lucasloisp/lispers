use nom::types::CompleteStr as Input;
use nom::{digit, multispace, space};
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct Program {
    op: Operator,
    expr: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, PartialEq)]
enum Expression {
    Number(i32),
    Program(Program),
}

fn sym_to_operator(sym: &str) -> Option<Operator> {
    match sym {
        "+"|"add" => Some(Operator::Add),
        "-" => Some(Operator::Subtract),
        "*" => Some(Operator::Multiply),
        "/" => Some(Operator::Divide),
        _ => None,
    }
}

named!(parse_operator<Input, Operator>, map_opt!(take!(1), |s:Input| sym_to_operator(s.0)));

named!(parse_number<Input, Expression>, map!(map_res!(
            pair!(opt!(char!('-')), digit)
            , |(m, s): (Option<char>, Input)| {
                i32::from_str(s.0).map(|n| n * m.map(|_| -1).unwrap_or(1i32))
            }), Expression::Number));

named!(parse_expression<Input, Expression>, alt!(
        parse_number | delimited!(char!('('), map!(parse_program, Expression::Program), char!(')'))
        ));

named!(pub parse_program<Input, Program>, do_parse!(
        op: parse_operator >>
        space >>
        expr: separated_list!(multispace, parse_expression) >>
        opt!(space) >>
        (Program { op, expr })
));

named!(pub parse_main<Input, Program>, terminated!(delimited!(opt!(char!('(')), parse_program, opt!(char!(')'))), nom::eol));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsed_operator_coincides_with_symbol() {
        assert_eq!(Ok((Input(""), Operator::Add)), parse_operator(Input("+")));
        assert_eq!(
            Ok((Input(""), Operator::Subtract)),
            parse_operator(Input("-"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Multiply)),
            parse_operator(Input("*"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Divide)),
            parse_operator(Input("/"))
        );
    }

    #[test]
    fn parsed_number_is_correct() {
        assert_eq!(
            Ok((Input(""), Expression::Number(45))),
            parse_number(Input("45"))
        );
        assert_eq!(
            Ok((Input(" "), Expression::Number(45))),
            parse_number(Input("45 "))
        );

        assert_eq!(
            Ok((Input(""), Expression::Number(-45))),
            parse_number(Input("-45"))
        );
        assert_eq!(
            Ok((Input(" "), Expression::Number(-45))),
            parse_number(Input("-45 "))
        );
    }

    #[test]
    fn no_space_after_number_sign() {
        assert!(parse_number(Input("- 45")).is_err());
        assert!(parse_number(Input("- 45 ")).is_err());
    }

    #[test]
    fn parsed_expression_recognizes_operator() {
        let op = Operator::Add;
        let expr = vec![Expression::Number(2), Expression::Number(4)];
        assert_eq!(
            Ok((Input(""), Expression::Program(Program { op, expr }))),
            parse_expression(Input("(+ 2 4)"))
        );

        let op = Operator::Multiply;
        let expr = vec![
            Expression::Number(2),
            Expression::Number(4),
            Expression::Number(5),
            Expression::Number(6),
        ];
        assert_eq!(
            Ok((Input(""), Expression::Program(Program { op, expr }))),
            parse_expression(Input("(* 2 4 5 6)"))
        );

        let op = Operator::Subtract;
        let expr = vec![Expression::Number(2), Expression::Number(-4)];
        assert_eq!(
            Ok((Input(""), Expression::Program(Program { op, expr }))),
            parse_expression(Input("(- 2 -4)"))
        );
    }

}
