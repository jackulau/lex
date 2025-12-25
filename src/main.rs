//! CLI tool for the Lex lexer.
//!
//! Usage:
//!   lex <FILE>              Tokenize a file
//!   lex --code "<CODE>"     Tokenize inline code
//!   lex --help              Show help
//!   lex --version           Show version

const VERSION: &str = env!("CARGO_PKG_VERSION");

use lex::{DefaultLanguage, Lexer, Token, TokenKind};
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        print_usage(&args[0]);
        process::exit(1);
    }

    let mut source = String::new();
    let mut verbose = false;
    let mut output_format = OutputFormat::Pretty;
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "--help" | "-h" => {
                print_usage(&args[0]);
                process::exit(0);
            }
            "--version" | "-V" => {
                println!("lex {}", VERSION);
                process::exit(0);
            }
            "--verbose" | "-v" => {
                verbose = true;
                i += 1;
            }
            "--output" | "-o" => {
                if i + 1 >= args.len() {
                    eprintln!("Error: --output requires an argument");
                    process::exit(1);
                }
                output_format = match args[i + 1].as_str() {
                    "pretty" => OutputFormat::Pretty,
                    "json" => OutputFormat::Json,
                    "debug" => OutputFormat::Debug,
                    _ => {
                        eprintln!("Error: unknown output format '{}'", args[i + 1]);
                        eprintln!("Valid formats: pretty, json, debug");
                        process::exit(1);
                    }
                };
                i += 2;
            }
            "--code" | "-c" => {
                if i + 1 >= args.len() {
                    eprintln!("Error: --code requires an argument");
                    process::exit(1);
                }
                source = args[i + 1].clone();
                i += 2;
            }
            arg if arg.starts_with('-') => {
                eprintln!("Error: unknown option '{}'", arg);
                print_usage(&args[0]);
                process::exit(1);
            }
            file => {
                match fs::read_to_string(file) {
                    Ok(content) => source = content,
                    Err(e) => {
                        eprintln!("Error reading file '{}': {}", file, e);
                        process::exit(1);
                    }
                }
                i += 1;
            }
        }
    }

    if source.is_empty() {
        eprintln!("Error: no input provided");
        print_usage(&args[0]);
        process::exit(1);
    }

    // Tokenize
    let (tokens, errors) = Lexer::tokenize(&source, DefaultLanguage);

    // Output tokens
    match output_format {
        OutputFormat::Pretty => print_pretty(&tokens, &source, verbose),
        OutputFormat::Json => print_json(&tokens, &source, verbose),
        OutputFormat::Debug => print_debug(&tokens),
    }

    // Report errors
    if !errors.is_empty() {
        eprintln!("\n--- Errors ---");
        for error in &errors {
            eprintln!("  {}", error);
        }
        process::exit(1);
    }
}

#[derive(Clone, Copy)]
enum OutputFormat {
    Pretty,
    Json,
    Debug,
}

fn print_usage(program: &str) {
    eprintln!("Usage: {} [OPTIONS] <FILE>", program);
    eprintln!("       {} --code \"<CODE>\"", program);
    eprintln!();
    eprintln!("Options:");
    eprintln!("  -c, --code <CODE>    Tokenize inline code");
    eprintln!("  -o, --output <FMT>   Output format: pretty, json, debug (default: pretty)");
    eprintln!("  -v, --verbose        Show detailed position information");
    eprintln!("  -h, --help           Show this help message");
    eprintln!("  -V, --version        Show version information");
    eprintln!();
    eprintln!("Examples:");
    eprintln!("  {} program.rs", program);
    eprintln!("  {} --code \"let x = 42;\"", program);
    eprintln!("  {} --output json program.rs", program);
}

fn print_pretty(tokens: &[Token], source: &str, verbose: bool) {
    let lang = DefaultLanguage;

    for token in tokens {
        let lexeme = &source[token.span.start..token.span.end];
        let kind_str = format_kind(&token.kind, &lang);

        if verbose {
            println!(
                "{:<20} {:?} ({}:{} - {}:{})",
                kind_str,
                lexeme,
                token.span.start_loc.line,
                token.span.start_loc.column,
                token.span.end_loc.line,
                token.span.end_loc.column,
            );
        } else {
            println!("{:<20} {:?}", kind_str, lexeme);
        }
    }
}

fn print_json(tokens: &[Token], source: &str, verbose: bool) {
    let lang = DefaultLanguage;

    println!("[");
    for (i, token) in tokens.iter().enumerate() {
        let lexeme = &source[token.span.start..token.span.end];
        let kind_str = format_kind(&token.kind, &lang);
        let comma = if i < tokens.len() - 1 { "," } else { "" };

        if verbose {
            println!(
                r#"  {{ "kind": "{}", "lexeme": {}, "line": {}, "column": {}, "start": {}, "end": {} }}{}"#,
                kind_str,
                json_escape(lexeme),
                token.span.start_loc.line,
                token.span.start_loc.column,
                token.span.start,
                token.span.end,
                comma
            );
        } else {
            println!(
                r#"  {{ "kind": "{}", "lexeme": {}, "line": {}, "column": {} }}{}"#,
                kind_str,
                json_escape(lexeme),
                token.span.start_loc.line,
                token.span.start_loc.column,
                comma
            );
        }
    }
    println!("]");
}

fn print_debug(tokens: &[Token]) {
    for token in tokens {
        println!("{:#?}", token);
    }
}

fn format_kind(kind: &TokenKind, lang: &DefaultLanguage) -> String {
    use lex::LanguageSpec;

    match kind {
        TokenKind::Keyword(id) => {
            if let Some(name) = lang.keyword_name(id.id()) {
                format!("Keyword({})", name)
            } else {
                format!("Keyword({})", id.id())
            }
        }
        TokenKind::BoolLiteral(b) => format!("BoolLiteral({})", b),
        _ => format!("{}", kind),
    }
}

fn json_escape(s: &str) -> String {
    let mut result = String::with_capacity(s.len() + 2);
    result.push('"');
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            c if c.is_control() => {
                result.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => result.push(c),
        }
    }
    result.push('"');
    result
}
