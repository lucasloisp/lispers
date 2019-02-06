use nom::types::CompleteStr as Input;
use nom::{digit, multispace};
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub struct SExpr {
    expr: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct QExpr {
    expr: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Pow,
    Min,
    Max,
    List,
    Head,
    Tail,
    Eval,
    Join,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Number(i32),
    Op(Operator),
    S(SExpr),
    Q(QExpr),
}

#[derive(Debug, PartialEq)]
pub enum LError {
    TypeError,
    NoOperator,
    NoEval,
    EmptyList,
    MissingArgs,
    TooManyArgs,
}

type LResult = Result<Expression, LError>;

impl SExpr {
    pub fn eval(self) -> LResult {
        if self.expr.is_empty() {
            return Err(LError::NoOperator);
        }

        let mut expr = self.expr;

        let mut d = expr.drain(..);
        if let Expression::Op(op) = d.next().unwrap() {
            op.call(d)
        } else {
            Err(LError::NoOperator)
        }
    }
}

impl Operator {
    fn call<I>(&self, args: I) -> LResult
    where
        I: IntoIterator<Item = Expression>,
    {
        use Operator::*;
        let mut d = args.into_iter();
        match self {
            List => Ok(Expression::Q(QExpr { expr: d.collect() })),
            Head => match d.next() {
                Some(Expression::Q(QExpr { mut expr })) => match expr.drain(..).next() {
                    Some(e) => {
                        if d.next().is_some() {
                            return Err(LError::TooManyArgs);
                        }
                        Ok(Expression::Q(QExpr { expr: vec![e] }))
                    }
                    None => Err(LError::EmptyList),
                },
                None => Err(LError::MissingArgs),
                _ => Err(LError::TypeError),
            },
            Eval => match d.next() {
                Some(Expression::Q(QExpr { expr })) => SExpr { expr }.eval(),
                _ => Err(LError::TypeError),
            },
            Tail => match d.next() {
                Some(Expression::Q(QExpr { mut expr })) => {
                    if d.next().is_some() {
                        return Err(LError::TooManyArgs);
                    }
                    if expr.is_empty() {
                        return Err(LError::EmptyList);
                    }
                    let mut d = expr.drain(..);
                    d.next();
                    Ok(Expression::Q(QExpr { expr: d.collect() }))
                }
                None => Err(LError::MissingArgs),
                _ => Err(LError::TypeError),
            },
            Join => {
                let expr: Vec<Expression> = d
                    .filter_map(|e| match e {
                        Expression::Q(q) => Some(q),
                        _ => None,
                    })
                    .map(|QExpr { expr }| expr)
                    .flatten()
                    .collect();
                Ok(Expression::Q(QExpr { expr }))
            }
            Add | Multiply | Divide | Subtract | Min | Max | Pow | Modulus => {
                let fst = d.next().ok_or(LError::MissingArgs)?.eval();
                d.map(Expression::eval)
                    .fold(fst, |a, e| a.and_then(|a| e.and_then(|e| self.apply(a, e))))
            }
        }
    }
    fn apply(&self, a: Expression, b: Expression) -> LResult {
        if let (Expression::Number(a), Expression::Number(b)) = (a, b) {
            Ok(Expression::Number(match self {
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
                Operator::List => a + b,
                Operator::Head => a + b,
                Operator::Tail => a + b,
                Operator::Eval => a + b,
                Operator::Join => a + b,
            }))
        } else {
            Err(LError::TypeError)
        }
    }
}

impl Expression {
    fn eval(self) -> LResult {
        match self {
            n @ Expression::Number(_) => Ok(n),
            Expression::S(sexpr) => sexpr.eval(),
            q @ Expression::Q(_) => Ok(q),
            _ => Err(LError::NoEval),
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
        "list" => Some(Operator::List),
        "eval" => Some(Operator::Eval),
        "head" => Some(Operator::Head),
        "tail" => Some(Operator::Tail),
        "join" => Some(Operator::Join),
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
        parse_number | map!(parse_operator, Expression::Op) | map!(parse_sexpr, Expression::S) | map!(parse_qexpr, Expression::Q)
        ));

named!(parse_qexpr<Input, QExpr>, do_parse!(
        char!('{') >>
        expr: separated_list!(multispace, parse_expression) >>
        char!('}') >>
        (QExpr { expr })
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
    fn builtin_function_names_are_parsed_correctly() {
        assert_eq!(
            Ok((Input(""), Operator::List)),
            parse_operator(Input("list"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Head)),
            parse_operator(Input("head"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Tail)),
            parse_operator(Input("tail"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Eval)),
            parse_operator(Input("eval"))
        );
        assert_eq!(
            Ok((Input(""), Operator::Join)),
            parse_operator(Input("join"))
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
    fn builtin_functions_are_parsed_adequately() {
        assert_eq!(
            Ok((
                Input(""),
                Expression::S(SExpr {
                    expr: vec![
                        Expression::Op(Operator::List),
                        Expression::Number(1),
                        Expression::Number(2)
                    ]
                })
            )),
            parse_expression(Input("(list 1 2)"))
        );
    }

    #[test]
    fn parsed_q_expression_is_correct() {
        assert_eq!(
            Ok((Input(""), QExpr { expr: vec![] })),
            parse_qexpr(Input("{}"))
        );

        assert_eq!(
            Ok((
                Input(""),
                QExpr {
                    expr: vec![Expression::Number(4), Expression::Number(1)]
                }
            )),
            parse_qexpr(Input("{4 1}"))
        );

        assert_eq!(
            Ok((
                Input(""),
                QExpr {
                    expr: vec![
                        Expression::Op(Operator::Add),
                        Expression::Number(4),
                        Expression::Number(1)
                    ]
                }
            )),
            parse_qexpr(Input("{+ 4 1}"))
        );
    }

    #[test]
    fn min_max_evaluates_correclty() {
        assert_eq!(
            Ok(Expression::Number(1)),
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
            Ok(Expression::Number(5)),
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
            Ok(Expression::Number(4)),
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
            Ok(Expression::Number(16)),
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
        assert_eq!(Ok(Expression::Number(11)), pr.eval());

        let pr1 = SExpr {
            expr: vec![
                Expression::Op(Operator::Multiply),
                Expression::Number(10),
                Expression::Number(10),
            ],
        };
        assert_eq!(Ok(Expression::Number(100)), pr1.eval());

        let pr2 = SExpr {
            expr: vec![
                Expression::Op(Operator::Add),
                Expression::Number(1),
                Expression::Number(1),
                Expression::Number(1),
            ],
        };
        assert_eq!(Ok(Expression::Number(3)), pr2.eval());

        let pr3 = SExpr {
            expr: vec![
                Expression::Op(Operator::Subtract),
                Expression::Number(50),
                Expression::Number(101),
            ],
        };
        assert_eq!(Ok(Expression::Number(-51)), pr3.eval());

        let pr4 = SExpr {
            expr: vec![
                Expression::Op(Operator::Divide),
                Expression::Number(150),
                Expression::Number(50),
            ],
        };
        assert_eq!(Ok(Expression::Number(3)), pr4.eval());

        let pr1 = SExpr {
            expr: vec![
                Expression::Op(Operator::Multiply),
                Expression::Number(10),
                Expression::Number(10),
            ],
        };

        let pr2 = SExpr {
            expr: vec![
                Expression::Op(Operator::Add),
                Expression::Number(1),
                Expression::Number(1),
                Expression::Number(1),
            ],
        };
        let pr = SExpr {
            expr: vec![
                Expression::Op(Operator::Subtract),
                Expression::S(pr1),
                Expression::S(pr2),
            ],
        };
        assert_eq!(Ok(Expression::Number(97)), pr.eval());
    }

    #[test]
    fn gives_type_error_on_arithmetic_ops() {
        let ops = vec![
            Operator::Add,
            Operator::Divide,
            Operator::Max,
            Operator::Min,
            Operator::Modulus,
            Operator::Multiply,
            Operator::Pow,
            Operator::Subtract,
        ];
        for op in ops {
            assert_eq!(
                Err(LError::TypeError),
                SExpr {
                    expr: vec![
                        Expression::Op(op),
                        Expression::Q(QExpr {
                            expr: vec![Expression::Number(1)],
                        }),
                        Expression::Number(4),
                    ],
                }
                .eval()
            );
        }
    }

    #[test]
    fn list_function_builds_q_expression() {
        assert_eq!(
            Ok(Expression::Q(QExpr { expr: vec![] })),
            SExpr {
                expr: vec![Expression::Op(Operator::List)]
            }
            .eval()
        );
        assert_eq!(
            Ok(Expression::Q(QExpr {
                expr: vec![Expression::Number(1)]
            })),
            SExpr {
                expr: vec![Expression::Op(Operator::List), Expression::Number(1)]
            }
            .eval()
        );
        assert_eq!(
            Ok(Expression::Q(QExpr {
                expr: vec![
                    Expression::Number(1),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(2)]
                    })
                ]
            })),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::List),
                    Expression::Number(1),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(2)]
                    })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn head_function_gets_first_of_qexpr() {
        assert_eq!(
            Ok(Expression::Q(QExpr {
                expr: vec![Expression::Number(1)]
            })),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Head),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1), Expression::Number(4)]
                    })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn head_function_complains_arg_count() {
        assert_eq!(
            Err(LError::MissingArgs),
            SExpr {
                expr: vec![Expression::Op(Operator::Head),]
            }
            .eval()
        );
        assert_eq!(
            Err(LError::TooManyArgs),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Head),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1)]
                    }),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1)]
                    })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn head_function_throws_error_on_empty() {
        assert_eq!(
            Err(LError::EmptyList),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Head),
                    Expression::Q(QExpr { expr: vec![] })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn tail_function_gets_tail_of_qexpr() {
        assert_eq!(
            Ok(Expression::Q(QExpr { expr: vec![] })),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Tail),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1),]
                    })
                ]
            }
            .eval()
        );
        assert_eq!(
            Ok(Expression::Q(QExpr {
                expr: vec![
                    Expression::Number(2),
                    Expression::Number(3),
                    Expression::Number(4)
                ]
            })),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Tail),
                    Expression::Q(QExpr {
                        expr: vec![
                            Expression::Number(1),
                            Expression::Number(2),
                            Expression::Number(3),
                            Expression::Number(4),
                        ]
                    })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn tail_function_complains_arg_count() {
        assert_eq!(
            Err(LError::MissingArgs),
            SExpr {
                expr: vec![Expression::Op(Operator::Tail),]
            }
            .eval()
        );
        assert_eq!(
            Err(LError::TooManyArgs),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Tail),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1)]
                    }),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1)]
                    })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn tail_function_throws_error_on_empty() {
        assert_eq!(
            Err(LError::EmptyList),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Tail),
                    Expression::Q(QExpr { expr: vec![] })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn eval_turns_qexpr_into_sexpr_and_evals() {
        assert_eq!(
            Ok(Expression::Number(3)),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Eval),
                    Expression::Q(QExpr {
                        expr: vec![
                            Expression::Op(Operator::Add),
                            Expression::Number(1),
                            Expression::Number(1),
                            Expression::Number(1),
                        ]
                    })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn join_function_concats_qexprs() {
        assert_eq!(
            Ok(Expression::Q(QExpr { expr: vec![] })),
            SExpr {
                expr: vec![Expression::Op(Operator::Join),]
            }
            .eval()
        );
        assert_eq!(
            Ok(Expression::Q(QExpr {
                expr: vec![Expression::Number(1), Expression::Number(3)]
            })),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Join),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1), Expression::Number(3)]
                    })
                ]
            }
            .eval()
        );
        assert_eq!(
            Ok(Expression::Q(QExpr {
                expr: vec![
                    Expression::Number(1),
                    Expression::Number(3),
                    Expression::Number(2),
                    Expression::Number(4)
                ]
            })),
            SExpr {
                expr: vec![
                    Expression::Op(Operator::Join),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(1), Expression::Number(3)]
                    }),
                    Expression::Q(QExpr {
                        expr: vec![Expression::Number(2), Expression::Number(4)]
                    })
                ]
            }
            .eval()
        );
    }

    #[test]
    fn arithmetic_on_single_arg() {
        let ops = vec![
            Operator::Add,
            Operator::Subtract,
            Operator::Multiply,
            Operator::Divide,
            Operator::Max,
            Operator::Min,
            Operator::Modulus,
            Operator::Pow,
        ];
        for op in ops {
            assert_eq!(
                Ok(Expression::Number(5)),
                SExpr {
                    expr: vec![Expression::Op(op), Expression::Number(5),],
                }
                .eval()
            );
        }
    }
}
