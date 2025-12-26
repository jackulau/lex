//! Comprehensive benchmarks for the Lex lexer.
//!
//! Run with: `cargo bench`
//! View reports: `open target/criterion/report/index.html`

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use lex::token::TokenKind;
use lex::{DefaultLanguage, LanguageBuilder, LanguageSpec, Lexer};


/// Generate a sequence of simple identifiers.
fn generate_identifiers(count: usize) -> String {
    (0..count)
        .map(|i| format!("identifier_{}", i))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate a sequence of keywords.
fn generate_keywords(count: usize) -> String {
    let keywords = ["if", "else", "while", "for", "fn", "let", "return", "match"];
    (0..count)
        .map(|i| keywords[i % keywords.len()])
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate a sequence of integer literals.
fn generate_integers(count: usize) -> String {
    (0..count)
        .map(|i| format!("{}", i * 42))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate a sequence of float literals.
fn generate_floats(count: usize) -> String {
    (0..count)
        .map(|i| format!("{}.{}", i, i * 7))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate a sequence of hex literals.
fn generate_hex_literals(count: usize) -> String {
    (0..count)
        .map(|i| format!("0x{:X}", i * 255))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate a sequence of string literals.
fn generate_strings(count: usize) -> String {
    (0..count)
        .map(|i| format!(r#""string_{}""#, i))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate strings with escape sequences.
fn generate_escape_strings(count: usize) -> String {
    (0..count)
        .map(|i| format!(r#""line{}\n\ttab\r\\slash""#, i))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate Unicode escape strings.
fn generate_unicode_escape_strings(count: usize) -> String {
    (0..count)
        .map(|_| r#""\u{1F600}\u{1F64F}\u{1F680}""#)
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate a sequence of operators.
fn generate_operators(count: usize) -> String {
    let ops = ["+", "-", "*", "/", "%", "==", "!=", "<=", ">=", "&&", "||"];
    (0..count)
        .map(|i| ops[i % ops.len()])
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate single-line comments.
fn generate_single_line_comments(count: usize) -> String {
    (0..count)
        .map(|i| format!("// comment line {}\nidentifier", i))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Generate multi-line comments.
fn generate_multi_line_comments(count: usize) -> String {
    (0..count)
        .map(|i| format!("/* multi-line\ncomment {} */ identifier", i))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate Unicode identifiers (CJK).
fn generate_cjk_identifiers(count: usize) -> String {
    let chars = ['日', '本', '語', '中', '文', '한', '글', '漢', '字'];
    (0..count)
        .map(|i| {
            let c1 = chars[i % chars.len()];
            let c2 = chars[(i + 1) % chars.len()];
            let c3 = chars[(i + 2) % chars.len()];
            format!("{}{}{}", c1, c2, c3)
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate Greek identifiers.
fn generate_greek_identifiers(count: usize) -> String {
    let chars = ['α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ'];
    (0..count)
        .map(|i| {
            let c1 = chars[i % chars.len()];
            let c2 = chars[(i + 1) % chars.len()];
            format!("{}{}", c1, c2)
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate Cyrillic identifiers.
fn generate_cyrillic_identifiers(count: usize) -> String {
    let chars = ['а', 'б', 'в', 'г', 'д', 'е', 'ж', 'з'];
    (0..count)
        .map(|i| {
            let c1 = chars[i % chars.len()];
            let c2 = chars[(i + 1) % chars.len()];
            format!("{}{}", c1, c2)
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate mixed ASCII and Unicode identifiers.
fn generate_mixed_unicode_identifiers(count: usize) -> String {
    (0..count)
        .map(|i| {
            if i % 3 == 0 {
                format!("ascii_{}", i)
            } else if i % 3 == 1 {
                format!("αβ{}", i)
            } else {
                format!("日本{}", i)
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate realistic code snippet.
fn generate_realistic_code(functions: usize) -> String {
    let mut code = String::new();
    for i in 0..functions {
        code.push_str(&format!(
            r#"
fn function_{}(x: i32, y: i32) -> i32 {{
    let result = x + y * 2;
    if result > 100 {{
        return result - 50;
    }} else if result < 0 {{
        return 0;
    }}
    // Some computation
    let temp = result * 3;
    /* Multi-line
       comment here */
    return temp + {};
}}
"#,
            i, i
        ));
    }
    code
}

/// Generate deeply nested expression.
fn generate_nested_expression(depth: usize) -> String {
    let mut expr = "x".to_string();
    for i in 0..depth {
        expr = format!("({} + {} * {} - {})", expr, i, i + 1, i + 2);
    }
    expr
}

/// Generate long identifier names.
fn generate_long_identifiers(count: usize, length: usize) -> String {
    (0..count)
        .map(|i| {
            let base = format!("very_long_identifier_name_{}_", i);
            base.repeat(length / base.len() + 1)[..length].to_string()
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate long string literals.
fn generate_long_strings(count: usize, length: usize) -> String {
    let content: String = (0..length).map(|i| (b'a' + (i % 26) as u8) as char).collect();
    (0..count)
        .map(|_| format!(r#""{}""#, content))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate numbers with underscores.
fn generate_underscore_numbers(count: usize) -> String {
    (0..count)
        .map(|i| format!("{}_{:03}_{:03}", i, i * 7, i * 13))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate scientific notation floats.
fn generate_scientific_floats(count: usize) -> String {
    (0..count)
        .map(|i| {
            if i % 2 == 0 {
                format!("{}e{}", i, i % 10)
            } else {
                format!("{}.{}E-{}", i, i * 3, i % 10)
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate binary literals.
fn generate_binary_literals(count: usize) -> String {
    (0..count)
        .map(|i| format!("0b{:b}", i))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate octal literals.
fn generate_octal_literals(count: usize) -> String {
    (0..count)
        .map(|i| format!("0o{:o}", i))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate code with many maximal munch scenarios.
fn generate_maximal_munch_heavy(count: usize) -> String {
    (0..count)
        .map(|i| {
            // Deliberately create scenarios where maximal munch matters
            if i % 5 == 0 {
                "a==b".to_string()
            } else if i % 5 == 1 {
                "x<=y".to_string()
            } else if i % 5 == 2 {
                "p>=q".to_string()
            } else if i % 5 == 3 {
                "m!=n".to_string()
            } else {
                "c&&d".to_string()
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate whitespace-heavy code.
fn generate_whitespace_heavy(count: usize) -> String {
    (0..count)
        .map(|i| format!("    \t  \n  identifier_{}  \t\n   ", i))
        .collect::<Vec<_>>()
        .join("")
}

/// Generate character literals.
fn generate_char_literals(count: usize) -> String {
    let chars = ['a', 'b', 'c', '0', '1', 'Z'];
    (0..count)
        .map(|i| format!("'{}'", chars[i % chars.len()]))
        .collect::<Vec<_>>()
        .join(" ")
}

/// Generate character literals with escapes.
fn generate_char_escape_literals(count: usize) -> String {
    let escapes = [r"'\n'", r"'\t'", r"'\r'", r"'\\'", r"'\0'", r"'\''"];
    (0..count)
        .map(|i| escapes[i % escapes.len()])
        .collect::<Vec<_>>()
        .join(" ")
}

// =============================================================================
// Benchmark Helpers
// =============================================================================

/// Benchmark tokenizing a string and count tokens.
fn bench_tokenize(source: &str) -> usize {
    let (tokens, _) = Lexer::tokenize(black_box(source), DefaultLanguage);
    tokens.len()
}

/// Benchmark tokenizing with a custom language.
fn bench_tokenize_custom<L: LanguageSpec>(source: &str, language: L) -> usize {
    let (tokens, _) = Lexer::tokenize(black_box(source), language);
    tokens.len()
}

/// Benchmark using iterator interface.
fn bench_tokenize_iter(source: &str) -> usize {
    let lexer = Lexer::new(black_box(source), DefaultLanguage);
    lexer.count()
}

// =============================================================================
// Benchmark Groups
// =============================================================================

/// Micro-benchmarks for single token types.
fn bench_single_tokens(c: &mut Criterion) {
    let mut group = c.benchmark_group("single_token");

    // Single identifier
    group.bench_function("identifier", |b| {
        b.iter(|| bench_tokenize("identifier"))
    });

    // Single keyword
    group.bench_function("keyword", |b| {
        b.iter(|| bench_tokenize("return"))
    });

    // Single integer
    group.bench_function("integer", |b| {
        b.iter(|| bench_tokenize("42"))
    });

    // Single float
    group.bench_function("float", |b| {
        b.iter(|| bench_tokenize("3.14159"))
    });

    // Single hex
    group.bench_function("hex", |b| {
        b.iter(|| bench_tokenize("0xDEADBEEF"))
    });

    // Single string
    group.bench_function("string", |b| {
        b.iter(|| bench_tokenize(r#""hello world""#))
    });

    // Single operator
    group.bench_function("operator_single", |b| {
        b.iter(|| bench_tokenize("+"))
    });

    // Multi-char operator
    group.bench_function("operator_multi", |b| {
        b.iter(|| bench_tokenize("=="))
    });

    // Unicode identifier
    group.bench_function("unicode_identifier", |b| {
        b.iter(|| bench_tokenize("日本語"))
    });

    group.finish();
}

/// Benchmarks for identifier scanning.
fn bench_identifiers(c: &mut Criterion) {
    let mut group = c.benchmark_group("identifiers");
    group.sample_size(100);

    for count in [10, 100, 1000] {
        let input = generate_identifiers(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("ascii", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    // Long identifiers
    for length in [32, 128, 512] {
        let input = generate_long_identifiers(100, length);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("long", length),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Benchmarks for keyword scanning.
fn bench_keywords(c: &mut Criterion) {
    let mut group = c.benchmark_group("keywords");

    for count in [10, 100, 1000] {
        let input = generate_keywords(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("mixed_keywords", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Benchmarks for Unicode identifier scanning.
fn bench_unicode_identifiers(c: &mut Criterion) {
    let mut group = c.benchmark_group("unicode_identifiers");
    group.sample_size(100);

    for count in [10, 100, 500] {
        // CJK
        let input = generate_cjk_identifiers(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("cjk", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Greek
        let input = generate_greek_identifiers(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("greek", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Cyrillic
        let input = generate_cyrillic_identifiers(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("cyrillic", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Mixed
        let input = generate_mixed_unicode_identifiers(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("mixed", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Benchmarks for number literal scanning.
fn bench_numbers(c: &mut Criterion) {
    let mut group = c.benchmark_group("numbers");
    group.sample_size(100);

    for count in [10, 100, 1000] {
        // Integers
        let input = generate_integers(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("integers", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Floats
        let input = generate_floats(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("floats", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Hex
        let input = generate_hex_literals(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("hex", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Binary
        let input = generate_binary_literals(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("binary", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Octal
        let input = generate_octal_literals(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("octal", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Scientific notation
        let input = generate_scientific_floats(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("scientific", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // With underscores
        let input = generate_underscore_numbers(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("underscore", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Benchmarks for string literal scanning.
fn bench_strings(c: &mut Criterion) {
    let mut group = c.benchmark_group("strings");
    group.sample_size(100);

    for count in [10, 100, 500] {
        // Simple strings
        let input = generate_strings(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("simple", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Escape sequences
        let input = generate_escape_strings(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("escapes", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Unicode escapes
        let input = generate_unicode_escape_strings(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("unicode_escapes", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    // Long strings
    for length in [100, 1000, 10000] {
        let input = generate_long_strings(10, length);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("long", length),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Benchmarks for character literal scanning.
fn bench_char_literals(c: &mut Criterion) {
    let mut group = c.benchmark_group("char_literals");

    for count in [10, 100, 500] {
        // Simple chars
        let input = generate_char_literals(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("simple", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Escape chars
        let input = generate_char_escape_literals(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("escapes", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Benchmarks for operator scanning.
fn bench_operators(c: &mut Criterion) {
    let mut group = c.benchmark_group("operators");

    for count in [10, 100, 1000] {
        // Mixed operators
        let input = generate_operators(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("mixed", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Maximal munch heavy
        let input = generate_maximal_munch_heavy(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("maximal_munch", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Benchmarks for comment scanning.
fn bench_comments(c: &mut Criterion) {
    let mut group = c.benchmark_group("comments");
    group.sample_size(100);

    for count in [10, 100, 500] {
        // Single-line comments
        let input = generate_single_line_comments(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("single_line", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );

        // Multi-line comments
        let input = generate_multi_line_comments(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("multi_line", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    // Test nested comments with custom language
    let nested_comment = "/* outer /* nested /* deeply */ nested */ outer */ identifier";
    group.bench_function("nested_depth_3", |b| {
        b.iter(|| {
            let nested_lang = LanguageBuilder::new()
                .multi_line_comment("/*", "*/")
                .nested_comments(true)
                .build();
            bench_tokenize_custom(black_box(nested_comment), nested_lang)
        })
    });

    group.finish();
}

/// Benchmarks for whitespace handling.
fn bench_whitespace(c: &mut Criterion) {
    let mut group = c.benchmark_group("whitespace");

    for count in [10, 100, 500] {
        let input = generate_whitespace_heavy(count);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("heavy", count),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    // Test with preserve_whitespace
    let ws_input = generate_whitespace_heavy(100);
    group.bench_function("preserve_whitespace", |b| {
        b.iter(|| {
            let preserve_lang = LanguageBuilder::new()
                .preserve_whitespace(true)
                .build();
            bench_tokenize_custom(black_box(&ws_input), preserve_lang)
        })
    });

    group.finish();
}

/// Scalability benchmarks with varying input sizes.
fn bench_scalability(c: &mut Criterion) {
    let mut group = c.benchmark_group("scalability");
    group.sample_size(50);

    // Test with realistic code at different scales
    for functions in [1, 10, 50, 100] {
        let input = generate_realistic_code(functions);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("realistic_code", format!("{}_funcs", functions)),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    // Test with deeply nested expressions
    for depth in [5, 10, 20, 50] {
        let input = generate_nested_expression(depth);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("nested_expr", depth),
            &input,
            |b, input| b.iter(|| bench_tokenize(input)),
        );
    }

    group.finish();
}

/// Comparison benchmarks: next_token vs iterator vs tokenize.
fn bench_api_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("api_comparison");

    let input = generate_realistic_code(10);

    // Using tokenize()
    group.bench_function("tokenize_all", |b| {
        b.iter(|| bench_tokenize(black_box(&input)))
    });

    // Using iterator
    group.bench_function("iterator", |b| {
        b.iter(|| bench_tokenize_iter(black_box(&input)))
    });

    // Using next_token manually
    group.bench_function("next_token_manual", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(&input), DefaultLanguage);
            let mut count = 0;
            loop {
                let token = lexer.next_token();
                if token.kind == TokenKind::Eof {
                    break;
                }
                count += 1;
            }
            count
        })
    });

    // Using peek + next
    group.bench_function("peek_and_next", |b| {
        b.iter(|| {
            let mut lexer = Lexer::new(black_box(&input), DefaultLanguage);
            let mut count = 0;
            loop {
                if lexer.peek().kind == TokenKind::Eof {
                    break;
                }
                lexer.next_token();
                count += 1;
            }
            count
        })
    });

    group.finish();
}

/// Benchmarks comparing custom languages vs default.
fn bench_language_comparison(c: &mut Criterion) {
    let mut group = c.benchmark_group("language_comparison");

    let input = generate_realistic_code(10);

    // Default language
    group.bench_function("default_language", |b| {
        b.iter(|| bench_tokenize(black_box(&input)))
    });

    // Minimal language (fewer keywords, operators)
    group.bench_function("minimal_language", |b| {
        b.iter(|| {
            let minimal_lang = LanguageBuilder::new()
                .keywords(&["fn", "let", "if", "return"])
                .operator("=", TokenKind::Eq)
                .operator("+", TokenKind::Plus)
                .operator("-", TokenKind::Minus)
                .build();
            bench_tokenize_custom(black_box(&input), minimal_lang)
        })
    });

    // Language with many operators
    group.bench_function("many_operators_language", |b| {
        b.iter(|| {
            let many_ops_lang = LanguageBuilder::new()
                .operator("<<=", TokenKind::LtLt)
                .operator(">>=", TokenKind::GtGt)
                .operator("===", TokenKind::EqEq)
                .operator("!==", TokenKind::BangEq)
                .operator("...", TokenKind::Dot)
                .operator("..", TokenKind::Dot)
                .operator("==", TokenKind::EqEq)
                .operator("!=", TokenKind::BangEq)
                .operator("<=", TokenKind::LtEq)
                .operator(">=", TokenKind::GtEq)
                .operator("&&", TokenKind::AmpAmp)
                .operator("||", TokenKind::PipePipe)
                .operator("<<", TokenKind::LtLt)
                .operator(">>", TokenKind::GtGt)
                .operator("::", TokenKind::ColonColon)
                .operator("->", TokenKind::Arrow)
                .operator("=>", TokenKind::FatArrow)
                .operator("+", TokenKind::Plus)
                .operator("-", TokenKind::Minus)
                .operator("*", TokenKind::Star)
                .operator("/", TokenKind::Slash)
                .operator("%", TokenKind::Percent)
                .operator("=", TokenKind::Eq)
                .operator("<", TokenKind::Lt)
                .operator(">", TokenKind::Gt)
                .operator("!", TokenKind::Bang)
                .operator("&", TokenKind::Amp)
                .operator("|", TokenKind::Pipe)
                .operator("^", TokenKind::Caret)
                .operator("~", TokenKind::Tilde)
                .build();
            bench_tokenize_custom(black_box(&input), many_ops_lang)
        })
    });

    group.finish();
}

/// Stress test benchmarks.
fn bench_stress_tests(c: &mut Criterion) {
    let mut group = c.benchmark_group("stress_tests");
    group.sample_size(20);

    // Very long input
    let huge_input = generate_realistic_code(500);
    group.throughput(Throughput::Bytes(huge_input.len() as u64));
    group.bench_function("huge_input_500_funcs", |b| {
        b.iter(|| bench_tokenize(black_box(&huge_input)))
    });

    // Many small tokens
    let many_small = generate_operators(10000);
    group.throughput(Throughput::Bytes(many_small.len() as u64));
    group.bench_function("10k_operators", |b| {
        b.iter(|| bench_tokenize(black_box(&many_small)))
    });

    // All Unicode
    let unicode_heavy = generate_cjk_identifiers(5000);
    group.throughput(Throughput::Bytes(unicode_heavy.len() as u64));
    group.bench_function("5k_unicode_identifiers", |b| {
        b.iter(|| bench_tokenize(black_box(&unicode_heavy)))
    });

    group.finish();
}

/// Empty and edge case benchmarks.
fn bench_edge_cases(c: &mut Criterion) {
    let mut group = c.benchmark_group("edge_cases");

    // Empty input
    group.bench_function("empty", |b| {
        b.iter(|| bench_tokenize(""))
    });

    // Single character
    group.bench_function("single_char", |b| {
        b.iter(|| bench_tokenize("x"))
    });

    // Only whitespace
    group.bench_function("only_whitespace", |b| {
        b.iter(|| bench_tokenize("     \n\t   \n   "))
    });

    // Only comments
    let only_comments = "// comment 1\n// comment 2\n/* multi */ // more";
    group.bench_function("only_comments", |b| {
        b.iter(|| bench_tokenize(black_box(only_comments)))
    });

    // Very deeply nested comment
    let deep_nested = "/* /* /* /* /* content */ */ */ */ */";
    group.bench_function("deeply_nested_comment", |b| {
        b.iter(|| {
            let nested_lang = LanguageBuilder::new()
                .multi_line_comment("/*", "*/")
                .nested_comments(true)
                .build();
            bench_tokenize_custom(black_box(deep_nested), nested_lang)
        })
    });

    // All delimiters
    let delimiters = "(){}[]();,:.".repeat(100);
    group.bench_function("all_delimiters", |b| {
        b.iter(|| bench_tokenize(black_box(&delimiters)))
    });

    group.finish();
}

/// Real-world code patterns benchmark.
fn bench_real_world_patterns(c: &mut Criterion) {
    let mut group = c.benchmark_group("real_world_patterns");
    group.sample_size(100);

    // Typical function definition
    let function_def = r#"
        fn calculate_fibonacci(n: u64) -> u64 {
            if n <= 1 {
                return n;
            }
            let mut a = 0;
            let mut b = 1;
            for _ in 2..=n {
                let temp = a + b;
                a = b;
                b = temp;
            }
            return b;
        }
    "#;
    group.bench_function("function_definition", |b| {
        b.iter(|| bench_tokenize(black_box(function_def)))
    });

    // Struct with methods
    let struct_code = r#"
        struct Point {
            x: f64,
            y: f64,
        }

        impl Point {
            fn new(x: f64, y: f64) -> Point {
                Point { x: x, y: y }
            }

            fn distance(&self, other: &Point) -> f64 {
                let dx = self.x - other.x;
                let dy = self.y - other.y;
                return (dx * dx + dy * dy).sqrt();
            }
        }
    "#;
    group.bench_function("struct_with_impl", |b| {
        b.iter(|| bench_tokenize(black_box(struct_code)))
    });

    // Heavy string manipulation
    let string_heavy = r#"
        let greeting = "Hello, world!";
        let escaped = "Line 1\nLine 2\tTabbed\r\nWindows";
        let unicode = "\u{1F600} Smile \u{2764} Heart";
        let path = "C:\\Users\\Name\\Documents\\file.txt";
        let json = "{\"key\": \"value\", \"number\": 42}";
    "#;
    group.bench_function("string_heavy", |b| {
        b.iter(|| bench_tokenize(black_box(string_heavy)))
    });

    // Math expression heavy
    let math_heavy = r#"
        let result = (a + b * c - d / e) % f;
        let comparison = x < y && y <= z || a >= b && c != d;
        let bitwise = (flags & mask) | (value << 8) >> 2;
        let compound = ((x + 1) * (y - 2)) / ((z + 3) % (w - 4));
    "#;
    group.bench_function("math_expression_heavy", |b| {
        b.iter(|| bench_tokenize(black_box(math_heavy)))
    });

    // Match expression
    let match_code = r#"
        match value {
            0 => "zero",
            1 => "one",
            2..=9 => "single digit",
            10..=99 => "double digit",
            _ => "large",
        }
    "#;
    group.bench_function("match_expression", |b| {
        b.iter(|| bench_tokenize(black_box(match_code)))
    });

    group.finish();
}

// =============================================================================
// Criterion Configuration
// =============================================================================

criterion_group!(
    benches,
    bench_single_tokens,
    bench_identifiers,
    bench_keywords,
    bench_unicode_identifiers,
    bench_numbers,
    bench_strings,
    bench_char_literals,
    bench_operators,
    bench_comments,
    bench_whitespace,
    bench_scalability,
    bench_api_comparison,
    bench_language_comparison,
    bench_stress_tests,
    bench_edge_cases,
    bench_real_world_patterns,
);

criterion_main!(benches);
