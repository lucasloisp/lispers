#[macro_use]
extern crate nom;

use std::io;
use std::io::Write;

mod parser;

fn main() {
    println!("Lispers Version 0.0.0.0.1");
    println!("Press Ctrl+c to Exit");

    loop {
        let mut input = String::new();

        print!("lisper> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut input);

        println!(
            "{:?}",
            parser::parse_program(nom::types::CompleteStr(&input))
        );
    }
}
