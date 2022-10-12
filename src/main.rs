use clap::Parser as ArgParser;
use monkey_rs::evaluator::Evaluator;
use monkey_rs::lexer::Lexer;
use monkey_rs::parser::Parser;
use rustyline::config::{Builder, EditMode};
use rustyline::error::ReadlineError;
use rustyline::{Editor, Result};

#[derive(Debug, ArgParser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value_t = false)]
    vi: bool,
}

fn main() -> Result<()> {
    const PROMPT: &str = ">> ";
    println!("Hello! This is the Monkey programming language");
    println!("Feel free to type in commands");

    let args = Args::parse();
    let mut edit_mode = EditMode::Emacs;
    if args.vi {
        edit_mode = EditMode::Vi;
    }

    let config = Builder::new()
        .indent_size(4)
        .tab_stop(4)
        .edit_mode(edit_mode)
        .build();
    let mut editor = Editor::<()>::with_config(config)?;
    loop {
        let readline = editor.readline(PROMPT);
        match readline {
            Ok(line) => {
                editor.add_history_entry(line.as_str());

                let lexer = Lexer::new(&line);
                let parser = Parser::new(lexer);
                match parser.parse_program() {
                    Ok(program) => {
                        let evaluator = Evaluator::new();
                        match evaluator.eval(program) {
                            Some(obj) => println!("{obj}"),
                            None => println!("Ran into an error evaluating the expression"),
                        }
                    }
                    Err(errs) => {
                        println!("Woops! We ran into some monkey business here!");
                        println!("parser errors:");
                        for parser_err in errs {
                            println!("  {parser_err}");
                        }
                    }
                }
            }
            Err(ReadlineError::Interrupted) => break,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error reading repl input: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}
