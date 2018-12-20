

#[derive(Debug)]
pub struct Program {
    op: Operator,
    expr: Vec<Expression>
}

#[derive(Debug)]
enum Operator {
    Add, Subtract, Multiply, Divide
}

#[derive(Debug)]
enum Expression {
    Number(i32),
    Program(Program)
}

fn sym_to_operator(sym: &str) -> Option<Operator> {
    Some(Operator::Add)
}

named!(parse_operator<&str, Operator>, map_opt!(take!(1), sym_to_operator));

named!(parse_number<&str, Expression>, map!(map_res!(take_until!(" "), |x| i32::from_str_radix(x, 10)), Expression::Number));

named!(parse_expression<&str, Expression>, alt!(
        parse_number | delimited!(char!('('), map!(parse_program, Expression::Program), char!(')'))
        ));
 


named!(parse_program<&str, Program>, ws!(do_parse!(
        op: parse_operator >>
        expr: many1!(parse_expression) >>
        (Program { op, expr })
)));
