use monkey_rs::lexer::Lexer;
use monkey_rs::token::Token;
use std::io::{self, Write};
use std::process;

fn main() {
    println!("Hello! This is the Monkey programming language");
    println!("Feel free to type in commands");
    start_repl();
}

fn start_repl() {
    const PROMPT: &'static str = ">> ";

    loop {
        let mut repl_input = String::new();

        print!("\n{PROMPT}");
        io::stdout().flush().unwrap_or_else(|err| {
            eprintln!("Internal REPL error when flushing stdout: {err}");
            process::exit(1);
        });
        io::stdin()
            .read_line(&mut repl_input)
            .unwrap_or_else(|err| {
                eprintln!("Internal REPL error when reading input: {err}");
                process::exit(1);
            });

        let mut lexer = Lexer::new(&repl_input);
        loop {
            let token = lexer.next_token();
            if let Token::EOF = token {
                break;
            }
            println!("{:?}", token);
        }
    }
}
