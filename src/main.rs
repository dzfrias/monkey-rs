use clap::Parser as ArgParser;
use monkey_rs::evaluator::Evaluator;
use monkey_rs::lexer::Lexer;
use monkey_rs::parser::Parser;
use rustyline::error::ReadlineError;
use rustyline::{Config, EditMode, Editor, Result};

fn main() -> Result<()> {
    let args = Args::parse();
    start_repl(args.vi)
}

#[derive(Debug, ArgParser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value_t = false)]
    vi: bool,
}

fn start_repl(vi: bool) -> Result<()> {
    const PROMPT: &str = ">> ";
    println!("Hello! This is the Monkey programming language");
    println!("Feel free to type in commands\n");

    let config = Config::builder()
        .indent_size(4)
        .tab_stop(4)
        .edit_mode(if vi { EditMode::Vi } else { EditMode::Emacs })
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
                            Ok(obj) => println!("{obj}"),
                            Err(err) => println!("Runtime error: {}", err),
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
