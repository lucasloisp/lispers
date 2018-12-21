use nom::types::CompleteStr as Input;
use nom::{digit, multispace, space};
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct SExpr {
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
    Min,
    Max,
}

#[derive(Debug, PartialEq)]
enum Expression {
    Number(i32),
    Op(Operator),
    S(SExpr),
}

impl SExpr {
    pub fn eval(&self) -> i32 {
        if let Expression::Op(ref op) = self.expr[0] {
            self.expr.iter().skip(3).map(Expression::eval).fold(
                op.apply(self.expr[1].eval(), self.expr[2].eval()),
                |a, e| op.apply(a, e),
            )
        } else {
            println!("Damn");
            println!("{:?}", self);
            0
        }
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
            Operator::Max => std::cmp::max(a, b),
            Operator::Min => std::cmp::min(a, b),
        }
    }
}

impl Expression {
    fn eval(&self) -> i32 {
        match self {
            Expression::Number(n) => *n,
            Expression::S(sexpr) => sexpr.eval(),
            _ => 0,
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
        "max" => Some(Operator::Max),
        "min" => Some(Operator::Min),
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
        parse_number | map!(parse_operator, Expression::Op) | map!(parse_sexpr, Expression::S)
        ));

named!(parse_sexpr<Input, SExpr>, do_parse!(
        char!('(') >>
        expr: separated_list!(multispace, parse_expression) >>
        char!(')') >>
        (SExpr { expr })
));

named!(pub parse_main<Input, SExpr>, terminated!(parse_sexpr, nom::eol));

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
        assert_eq!(Ok((Input(""), Operator::Min)), parse_operator(Input("min")));
        assert_eq!(Ok((Input(""), Operator::Max)), parse_operator(Input("max")));
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
        let expr = vec![
            Expression::Op(op),
            Expression::Number(2),
            Expression::Number(4),
        ];
        assert_eq!(
            Ok((Input(""), Expression::S(SExpr { expr }))),
            parse_expression(Input("(+ 2 4)"))
        );

        let op = Operator::Multiply;
        let expr = vec![
            Expression::Op(op),
            Expression::Number(2),
            Expression::Number(4),
            Expression::Number(5),
            Expression::Number(6),
        ];
        assert_eq!(
            Ok((Input(""), Expression::S(SExpr { expr }))),
            parse_expression(Input("(* 2 4 5 6)"))
        );

        let op = Operator::Subtract;
        let expr = vec![
            Expression::Op(op),
            Expression::Number(2),
            Expression::Number(-4),
        ];
        assert_eq!(
            Ok((Input(""), Expression::S(SExpr { expr }))),
            parse_expression(Input("(- 2 -4)"))
        );
    }

    #[test]
    fn min_max_evaluates_correclty() {
        assert_eq!(
            1,
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Min),
                    Expression::Number(1),
                    Expression::Number(5),
                    Expression::Number(3)
                ]
            }
            .eval()
        );
        assert_eq!(
            5,
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Max),
                    Expression::Number(1),
                    Expression::Number(5),
                    Expression::Number(3)
                ]
            }
            .eval()
        );
    }

    #[test]
    fn modulus_expression_evaluates_correctly() {
        assert_eq!(
            4,
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Modulus),
                    Expression::Number(10),
                    Expression::Number(6)
                ]
            }
            .eval()
        );
    }

    #[test]
    fn power_expression_evaluates_correctly() {
        assert_eq!(
            16,
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Pow),
                    Expression::Number(4),
                    Expression::Number(2)
                ]
            }
            .eval()
        );
    }

    #[test]
    fn expressions_evaluate_correctly() {
        let pr = SExpr {
            expr: vec![
                Expression::Op(Operator::Add),
                Expression::Number(5),
                Expression::Number(6),
            ],
        };
        assert_eq!(11, pr.eval());

        let pr1 = SExpr {
            expr: vec![
                Expression::Op(Operator::Multiply),
                Expression::Number(10),
                Expression::Number(10),
            ],
        };
        assert_eq!(100, pr1.eval());

        let pr2 = SExpr {
            expr: vec![
                Expression::Op(Operator::Add),
                Expression::Number(1),
                Expression::Number(1),
                Expression::Number(1),
            ],
        };
        assert_eq!(3, pr2.eval());

        let pr3 = SExpr {
            expr: vec![
                Expression::Op(Operator::Subtract),
                Expression::Number(50),
                Expression::Number(101),
            ],
        };
        assert_eq!(-51, pr3.eval());

        let pr4 = SExpr {
            expr: vec![
                Expression::Op(Operator::Divide),
                Expression::Number(150),
                Expression::Number(50),
            ],
        };
        assert_eq!(3, pr4.eval());

        let pr = SExpr {
            expr: vec![
                Expression::Op(Operator::Subtract),
                Expression::S(pr1),
                Expression::S(pr2),
            ],
        };
        assert_eq!(97, pr.eval());
    }

}
