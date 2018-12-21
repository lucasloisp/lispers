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
    Modulus,
    Pow,
}

#[derive(Debug, PartialEq)]
enum Expression {
    Number(i32),
    Program(Program),
}

impl Program {
    pub fn eval(&self) -> i32 {
        self.expr.iter().skip(2).map(Expression::eval).fold(
            self.op.apply(self.expr[0].eval(), self.expr[1].eval()),
            |a, e| self.op.apply(a, e),
        )
    }
}

impl Operator {
    fn apply(&self, a: i32, b: i32) -> i32 {
        match self {
            Operator::Add => a + b,
            Operator::Subtract => a - b,
            Operator::Multiply => a * b,
            Operator::Divide => a / b,
            Operator::Modulus => a % b,
            Operator::Pow => {
                if b >= 0 {
                    a.pow(b as u32)
                } else {
                    0
                }
            }
        }
    }
}

impl Expression {
    fn eval(&self) -> i32 {
        match self {
            Expression::Number(n) => *n,
            Expression::Program(pr) => pr.eval(),
        }
    }
}

fn sym_to_operator(sym: &str) -> Option<Operator> {
    match sym {
        "+" | "add" => Some(Operator::Add),
        "-" | "sub" => Some(Operator::Subtract),
        "*" | "mul" => Some(Operator::Multiply),
        "/" | "div" => Some(Operator::Divide),
        "%" | "mod" => Some(Operator::Modulus),
        "^" | "pow" => Some(Operator::Pow),
        _ => None,
    }
}

named!(parse_operator<Input, Operator>, map_opt!(take_till!(char::is_whitespace), |s:Input| sym_to_operator(s.0)));

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
        assert_eq!(
            Ok((Input(""), Operator::Modulus)),
            parse_operator(Input("%"))
        );
        assert_eq!(Ok((Input(""), Operator::Pow)), parse_operator(Input("^")));
    }

    #[test]
    fn operator_can_be_expressed_by_name() {
        assert_eq!(Ok((Input(""), Operator::Add)), parse_operator(Input("add")));
        assert_eq!(
            Ok((Input(""), Operator::Subtract)),
            parse_operator(Input("sub"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Multiply)),
            parse_operator(Input("mul"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Divide)),
            parse_operator(Input("div"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Modulus)),
            parse_operator(Input("mod"))
        );
        assert_eq!(Ok((Input(""), Operator::Pow)), parse_operator(Input("pow")));
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

    #[test]
    fn modulus_expression_evaluates_correctly() {
        assert_eq!(
            4,
            Program {
                op: Operator::Modulus,
                expr: vec![Expression::Number(10), Expression::Number(6)]
            }
            .eval()
        );
    }

    #[test]
    fn power_expression_evaluates_correctly() {
        assert_eq!(
            16,
            Program {
                op: Operator::Pow,
                expr: vec![Expression::Number(4), Expression::Number(2)]
            }
            .eval()
        );
    }

    #[test]
    fn expressions_evaluate_correctly() {
        let pr = Program {
            op: Operator::Add,
            expr: vec![Expression::Number(5), Expression::Number(6)],
        };
        assert_eq!(11, pr.eval());

        let pr1 = Program {
            op: Operator::Multiply,
            expr: vec![Expression::Number(10), Expression::Number(10)],
        };
        assert_eq!(100, pr1.eval());

        let pr2 = Program {
            op: Operator::Add,
            expr: vec![
                Expression::Number(1),
                Expression::Number(1),
                Expression::Number(1),
            ],
        };
        assert_eq!(3, pr2.eval());

        let pr3 = Program {
            op: Operator::Subtract,
            expr: vec![Expression::Number(50), Expression::Number(101)],
        };
        assert_eq!(-51, pr3.eval());

        let pr4 = Program {
            op: Operator::Divide,
            expr: vec![Expression::Number(150), Expression::Number(50)],
        };
        assert_eq!(3, pr4.eval());

        let pr = Program {
            op: Operator::Subtract,
            expr: vec![Expression::Program(pr1), Expression::Program(pr2)],
        };
        assert_eq!(97, pr.eval());
    }

}
