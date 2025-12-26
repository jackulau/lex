//! Core lexer implementation.
//!
//! This module implements the main lexer using the Dragon Book approach:
//! character-by-character scanning with maximal munch for operators.

use crate::error::{LexError, LexErrorKind};
use crate::language::LanguageSpec;
use crate::source::Source;
use crate::span::{Location, Span};
use crate::token::{KeywordId, Token, TokenKind};
use crate::unicode::{is_xid_continue, is_xid_start};

/// The main lexer, parameterized by language specification.
pub struct Lexer<'a, L: LanguageSpec> {
    /// Source text cursor.
    source: Source<'a>,
    /// Language specification.
    language: L,
    /// Accumulated lexer errors.
    errors: Vec<LexError>,
    /// Peeked token for lookahead.
    peeked: Option<Token>,
}

impl<'a, L: LanguageSpec> Lexer<'a, L> {
    /// Create a new lexer for the given source text.
    pub fn new(source: &'a str, language: L) -> Self {
        Self {
            source: Source::new(source),
            language,
            errors: Vec::new(),
            peeked: None,
        }
    }

    /// Get the next token.
    pub fn next_token(&mut self) -> Token {
        if let Some(token) = self.peeked.take() {
            return token;
        }
        self.scan_token()
    }

    /// Peek at the next token without consuming it.
    pub fn peek(&mut self) -> &Token {
        if self.peeked.is_none() {
            self.peeked = Some(self.scan_token());
        }
        self.peeked.as_ref().unwrap()
    }

    /// Get accumulated errors.
    pub fn errors(&self) -> &[LexError] {
        &self.errors
    }

    /// Check if there were any errors.
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get the source text for a span.
    pub fn source_text(&self, span: Span) -> &'a str {
        self.source.slice(span.start, span.end)
    }

    /// Get the language specification.
    pub fn language(&self) -> &L {
        &self.language
    }

    /// Tokenize the entire source, returning tokens and errors.
    pub fn tokenize(source: &'a str, language: L) -> (Vec<Token>, Vec<LexError>) {
        let mut lexer = Lexer::new(source, language);
        let tokens: Vec<Token> = lexer.by_ref().collect();
        (tokens, lexer.errors)
    }
}

impl<'a, L: LanguageSpec> Lexer<'a, L> {
    /// Main scanning function - character-by-character with state-based recognition.
    fn scan_token(&mut self) -> Token {
        // Skip whitespace (unless preserving)
        if !self.language.preserve_whitespace() {
            self.skip_whitespace();
        } else if let Some(token) = self.try_scan_whitespace() {
            return token;
        }

        // Record start position
        let (start, start_loc) = self.source.save();

        // Check for EOF
        let Some(c) = self.source.peek() else {
            return self.make_token(TokenKind::Eof, start, start_loc);
        };

        // Check for comments first
        if let Some(token) = self.try_scan_comment(start, start_loc) {
            return token;
        }

        // Check for raw string prefix (e.g., r"...")
        if let Some(kind) = self.try_scan_raw_string(start, start_loc) {
            return self.make_token(kind, start, start_loc);
        }

        // State-based recognition using first character
        let kind = match c {
            // Identifiers and keywords
            c if is_xid_start(c) => self.scan_identifier(),

            // Numeric literals
            '0'..='9' => self.scan_number(),

            // String literals
            '"' => self.scan_string('"'),

            // Character literals
            '\'' => self.scan_char(),

            // Operators and delimiters
            _ => self.scan_operator_or_delimiter(),
        };

        self.make_token(kind, start, start_loc)
    }

    /// Create a token from the current position.
    fn make_token(&self, kind: TokenKind, start: usize, start_loc: Location) -> Token {
        Token::new(kind, self.source.span_from(start, start_loc))
    }

    /// Report an error.
    fn report_error(&mut self, kind: LexErrorKind, span: Span) {
        self.errors.push(LexError::new(kind, span));
    }

    /// Skip whitespace characters.
    fn skip_whitespace(&mut self) {
        self.source.skip_while(|c| c.is_whitespace());
    }

    /// Try to scan whitespace (when preserving).
    fn try_scan_whitespace(&mut self) -> Option<Token> {
        let (start, start_loc) = self.source.save();
        if self.source.peek()?.is_whitespace() {
            self.source.skip_while(|c| c.is_whitespace());
            Some(self.make_token(TokenKind::Whitespace, start, start_loc))
        } else {
            None
        }
    }

    /// Try to scan a comment.
    fn try_scan_comment(&mut self, start: usize, start_loc: Location) -> Option<Token> {
        let styles = self.language.comment_styles();

        // Check for single-line comment
        if let Some(prefix) = styles.single_line {
            if self.source.starts_with(prefix) {
                return Some(self.scan_single_line_comment(prefix, start, start_loc));
            }
        }

        // Check for multi-line comment
        if let Some((start_delim, end_delim)) = styles.multi_line {
            if self.source.starts_with(start_delim) {
                return Some(self.scan_multi_line_comment(
                    start_delim,
                    end_delim,
                    styles.nested,
                    start,
                    start_loc,
                ));
            }
        }

        None
    }

    /// Try to scan a raw string literal.
    fn try_scan_raw_string(&mut self, start: usize, start_loc: Location) -> Option<TokenKind> {
        let config = self.language.string_config();
        let prefix = config.raw_prefix?;

        if !self.source.starts_with(prefix) {
            return None;
        }

        // Check if the character after the prefix is a string delimiter
        let prefix_len = prefix.len();
        let next_char = self.source.peek_nth(prefix_len)?;

        if !config.delimiters.contains(&next_char) {
            return None;
        }

        // Consume the prefix
        self.source.consume(prefix);

        // Scan the raw string (delimiter is the next character)
        Some(self.scan_raw_string(next_char, start, start_loc))
    }

    /// Scan a single-line comment.
    fn scan_single_line_comment(
        &mut self,
        prefix: &str,
        start: usize,
        start_loc: Location,
    ) -> Token {
        // Consume the prefix
        self.source.consume(prefix);

        // Consume until end of line
        self.source.skip_while(|c| c != '\n');

        if self.language.preserve_comments() {
            self.make_token(TokenKind::Comment, start, start_loc)
        } else {
            // Recursively get next token
            self.scan_token()
        }
    }

    /// Scan a multi-line comment.
    fn scan_multi_line_comment(
        &mut self,
        start_delim: &str,
        end_delim: &str,
        nested: bool,
        start: usize,
        start_loc: Location,
    ) -> Token {
        // Consume the start delimiter
        self.source.consume(start_delim);

        let mut depth = 1;

        while depth > 0 {
            if self.source.is_eof() {
                let span = self.source.span_from(start, start_loc);
                self.report_error(LexErrorKind::UnterminatedComment, span);
                return self.make_token(TokenKind::Error, start, start_loc);
            }

            if nested && self.source.starts_with(start_delim) {
                self.source.consume(start_delim);
                depth += 1;
            } else if self.source.starts_with(end_delim) {
                self.source.consume(end_delim);
                depth -= 1;
            } else {
                self.source.advance();
            }
        }

        if self.language.preserve_comments() {
            self.make_token(TokenKind::Comment, start, start_loc)
        } else {
            self.scan_token()
        }
    }

    /// Scan an identifier or keyword.
    fn scan_identifier(&mut self) -> TokenKind {
        let start = self.source.position();

        // Consume XID_Start
        self.source.advance();

        // Consume XID_Continue*
        self.source.skip_while(is_xid_continue);

        let text = self.source.slice(start, self.source.position());

        // Check for boolean literals
        if text == "true" {
            return TokenKind::BoolLiteral(true);
        }
        if text == "false" {
            return TokenKind::BoolLiteral(false);
        }

        // Check if keyword
        if let Some(kw_id) = self.language.lookup_keyword(text) {
            TokenKind::Keyword(KeywordId::new(kw_id))
        } else {
            TokenKind::Ident
        }
    }

    /// Scan a number literal.
    fn scan_number(&mut self) -> TokenKind {
        let start = self.source.position();

        // Check for hex/octal/binary prefix
        if self.source.peek() == Some('0') {
            self.source.advance();
            match self.source.peek() {
                Some('x' | 'X') => {
                    self.source.advance();
                    return self.scan_hex_number(start);
                }
                Some('o' | 'O') => {
                    self.source.advance();
                    return self.scan_octal_number(start);
                }
                Some('b' | 'B') => {
                    self.source.advance();
                    return self.scan_binary_number(start);
                }
                _ => {}
            }
        }

        // Consume integer part
        self.consume_digits();

        // Check for decimal point
        let mut is_float = false;
        if self.source.peek() == Some('.') {
            // Look ahead to distinguish 1.2 from 1..2
            if let Some(c) = self.source.peek_nth(1) {
                if c.is_ascii_digit() {
                    is_float = true;
                    self.source.advance(); // consume '.'
                    self.consume_digits();
                }
            }
        }

        // Check for exponent
        if let Some('e' | 'E') = self.source.peek() {
            is_float = true;
            self.source.advance();
            if let Some('+' | '-') = self.source.peek() {
                self.source.advance();
            }
            self.consume_digits();
        }

        if is_float {
            TokenKind::FloatLiteral
        } else {
            TokenKind::IntLiteral
        }
    }

    /// Scan a hexadecimal number.
    fn scan_hex_number(&mut self, start: usize) -> TokenKind {
        let digit_start = self.source.position();
        self.source.skip_while(|c| c.is_ascii_hexdigit() || c == '_');

        if self.source.position() == digit_start {
            let span = self.source.span_from(start, Location::new());
            self.report_error(
                LexErrorKind::InvalidNumber("expected hex digits after 0x".into()),
                span,
            );
            return TokenKind::Error;
        }

        TokenKind::IntLiteral
    }

    /// Scan an octal number.
    fn scan_octal_number(&mut self, start: usize) -> TokenKind {
        let digit_start = self.source.position();
        self.source.skip_while(|c| matches!(c, '0'..='7' | '_'));

        if self.source.position() == digit_start {
            let span = self.source.span_from(start, Location::new());
            self.report_error(
                LexErrorKind::InvalidNumber("expected octal digits after 0o".into()),
                span,
            );
            return TokenKind::Error;
        }

        TokenKind::IntLiteral
    }

    /// Scan a binary number.
    fn scan_binary_number(&mut self, start: usize) -> TokenKind {
        let digit_start = self.source.position();
        self.source.skip_while(|c| matches!(c, '0' | '1' | '_'));

        if self.source.position() == digit_start {
            let span = self.source.span_from(start, Location::new());
            self.report_error(
                LexErrorKind::InvalidNumber("expected binary digits after 0b".into()),
                span,
            );
            return TokenKind::Error;
        }

        TokenKind::IntLiteral
    }

    /// Consume decimal digits (with optional underscores).
    fn consume_digits(&mut self) {
        self.source.skip_while(|c| c.is_ascii_digit() || c == '_');
    }

    /// Scan a string literal.
    fn scan_string(&mut self, delimiter: char) -> TokenKind {
        let (start, start_loc) = self.source.save();

        self.source.advance(); // consume opening delimiter

        loop {
            match self.source.peek() {
                None => {
                    let span = self.source.span_from(start, start_loc);
                    self.report_error(LexErrorKind::UnterminatedString, span);
                    return TokenKind::Error;
                }
                Some(c) if c == delimiter => {
                    self.source.advance();
                    break;
                }
                Some('\\') => {
                    self.source.advance();
                    self.scan_escape_sequence();
                }
                Some('\n') if !self.language.string_config().multiline => {
                    let span = self.source.span_from(start, start_loc);
                    self.report_error(LexErrorKind::NewlineInString, span);
                    return TokenKind::Error;
                }
                _ => {
                    self.source.advance();
                }
            }
        }

        TokenKind::StringLiteral
    }

    /// Scan a raw string literal (no escape sequence processing).
    fn scan_raw_string(&mut self, delimiter: char, start: usize, start_loc: Location) -> TokenKind {
        self.source.advance(); // consume opening delimiter

        loop {
            match self.source.peek() {
                None => {
                    let span = self.source.span_from(start, start_loc);
                    self.report_error(LexErrorKind::UnterminatedString, span);
                    return TokenKind::Error;
                }
                Some(c) if c == delimiter => {
                    self.source.advance();
                    break;
                }
                Some('\n') if !self.language.string_config().multiline => {
                    let span = self.source.span_from(start, start_loc);
                    self.report_error(LexErrorKind::NewlineInString, span);
                    return TokenKind::Error;
                }
                _ => {
                    self.source.advance();
                }
            }
        }

        TokenKind::RawStringLiteral
    }

    /// Scan a character literal.
    fn scan_char(&mut self) -> TokenKind {
        let (start, start_loc) = self.source.save();

        self.source.advance(); // consume opening quote

        let mut count = 0;

        loop {
            match self.source.peek() {
                None | Some('\n') => {
                    let span = self.source.span_from(start, start_loc);
                    self.report_error(LexErrorKind::UnterminatedChar, span);
                    return TokenKind::Error;
                }
                Some('\'') => {
                    self.source.advance();
                    break;
                }
                Some('\\') => {
                    self.source.advance();
                    self.scan_escape_sequence();
                    count += 1;
                }
                _ => {
                    self.source.advance();
                    count += 1;
                }
            }
        }

        if count == 0 {
            let span = self.source.span_from(start, start_loc);
            self.report_error(LexErrorKind::EmptyCharLiteral, span);
            return TokenKind::Error;
        }

        if count > 1 {
            let span = self.source.span_from(start, start_loc);
            self.report_error(LexErrorKind::MultiCharLiteral, span);
            return TokenKind::Error;
        }

        TokenKind::CharLiteral
    }

    /// Scan an escape sequence in a string or character literal.
    fn scan_escape_sequence(&mut self) {
        let (start, start_loc) = self.source.save();

        match self.source.peek() {
            Some('n' | 'r' | 't' | '\\' | '"' | '\'' | '0') => {
                self.source.advance();
            }
            Some('x') => {
                // \xNN hex escape
                self.source.advance();
                for _ in 0..2 {
                    if let Some(c) = self.source.peek() {
                        if c.is_ascii_hexdigit() {
                            self.source.advance();
                        }
                    }
                }
            }
            Some('u') => {
                // \u{NNNN} Unicode escape
                self.source.advance();
                if self.source.peek() == Some('{') {
                    self.source.advance();
                    self.source.skip_while(|c| c.is_ascii_hexdigit());
                    if self.source.peek() == Some('}') {
                        self.source.advance();
                    } else {
                        let span = self.source.span_from(start, start_loc);
                        self.report_error(
                            LexErrorKind::InvalidUnicodeEscape("expected '}'".into()),
                            span,
                        );
                    }
                } else {
                    let span = self.source.span_from(start, start_loc);
                    self.report_error(
                        LexErrorKind::InvalidUnicodeEscape("expected '{'".into()),
                        span,
                    );
                }
            }
            Some(c) => {
                let span = self.source.span_from(start, start_loc);
                self.report_error(LexErrorKind::InvalidEscape(c), span);
                self.source.advance();
            }
            None => {
                let span = self.source.span_from(start, start_loc);
                self.report_error(LexErrorKind::UnexpectedEof, span);
            }
        }
    }

    /// Scan an operator or delimiter using maximal munch.
    fn scan_operator_or_delimiter(&mut self) -> TokenKind {
        // Try operators in order (longest first - maximal munch)
        for op in self.language.operators() {
            if self.source.consume(op.text) {
                return op.kind;
            }
        }

        // Single-character delimiters
        let (start, start_loc) = self.source.save();
        let c = self.source.advance().unwrap();

        match c {
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            '.' => TokenKind::Dot,
            _ => {
                let span = self.source.span_from(start, start_loc);
                self.report_error(LexErrorKind::UnexpectedChar(c), span);
                TokenKind::Error
            }
        }
    }
}

impl<'a, L: LanguageSpec> Iterator for Lexer<'a, L> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.kind == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::language::DefaultLanguage;

    fn tokenize(source: &str) -> Vec<Token> {
        let (tokens, _) = Lexer::tokenize(source, DefaultLanguage);
        tokens
    }

    fn token_kinds(source: &str) -> Vec<TokenKind> {
        tokenize(source).into_iter().map(|t| t.kind).collect()
    }

    #[test]
    fn test_empty_source() {
        let tokens = tokenize("");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_whitespace_only() {
        let tokens = tokenize("   \n\t  ");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_identifiers() {
        let kinds = token_kinds("foo bar _baz");
        assert_eq!(
            kinds,
            vec![TokenKind::Ident, TokenKind::Ident, TokenKind::Ident]
        );
    }

    #[test]
    fn test_unicode_identifiers() {
        let kinds = token_kinds("日本語 αβγ кот");
        assert_eq!(
            kinds,
            vec![TokenKind::Ident, TokenKind::Ident, TokenKind::Ident]
        );
    }

    #[test]
    fn test_keywords() {
        let kinds = token_kinds("if else while fn let");
        assert!(kinds.iter().all(|k| matches!(k, TokenKind::Keyword(_))));
    }

    #[test]
    fn test_boolean_literals() {
        let kinds = token_kinds("true false");
        assert_eq!(
            kinds,
            vec![TokenKind::BoolLiteral(true), TokenKind::BoolLiteral(false)]
        );
    }

    #[test]
    fn test_integer_literals() {
        let kinds = token_kinds("42 0 123_456");
        assert!(kinds.iter().all(|k| *k == TokenKind::IntLiteral));
    }

    #[test]
    fn test_hex_octal_binary() {
        let kinds = token_kinds("0x1F 0o17 0b1010");
        assert!(kinds.iter().all(|k| *k == TokenKind::IntLiteral));
    }

    #[test]
    fn test_float_literals() {
        let kinds = token_kinds("3.14 1.0 2.5e10 1E-5");
        assert!(kinds.iter().all(|k| *k == TokenKind::FloatLiteral));
    }

    #[test]
    fn test_string_literals() {
        let kinds = token_kinds(r#""hello" "world""#);
        assert!(kinds.iter().all(|k| *k == TokenKind::StringLiteral));
    }

    #[test]
    fn test_string_escape_sequences() {
        let kinds = token_kinds(r#""hello\nworld" "\t\r\\" "\u{1F600}""#);
        assert!(kinds.iter().all(|k| *k == TokenKind::StringLiteral));
    }

    #[test]
    fn test_raw_string_literals() {
        let kinds = token_kinds(r#"r"hello" r"world""#);
        assert!(kinds.iter().all(|k| *k == TokenKind::RawStringLiteral));
    }

    #[test]
    fn test_raw_string_no_escape_processing() {
        // Raw strings should not process escape sequences
        let source = r#"r"hello\nworld""#;
        let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
        assert!(errors.is_empty(), "raw strings should not report escape errors");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::RawStringLiteral);
    }

    #[test]
    fn test_raw_string_with_backslashes() {
        // Raw strings should preserve backslashes without treating them as escapes
        let source = r#"r"C:\Users\test""#;
        let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
        assert!(errors.is_empty());
        assert_eq!(tokens[0].kind, TokenKind::RawStringLiteral);
    }

    #[test]
    fn test_r_identifier_not_followed_by_quote() {
        // 'r' alone or followed by non-quote should be identifier
        let kinds = token_kinds("r rx r123");
        assert!(kinds.iter().all(|k| *k == TokenKind::Ident));
    }

    #[test]
    fn test_char_literals() {
        let kinds = token_kinds("'a' '\\n' '\\u{1F600}'");
        assert!(kinds.iter().all(|k| *k == TokenKind::CharLiteral));
    }

    #[test]
    fn test_operators() {
        let kinds = token_kinds("+ - * / % == != < > <= >= && || !");
        use TokenKind::*;
        assert_eq!(
            kinds,
            vec![
                Plus, Minus, Star, Slash, Percent, EqEq, BangEq, Lt, Gt, LtEq, GtEq, AmpAmp,
                PipePipe, Bang
            ]
        );
    }

    #[test]
    fn test_maximal_munch() {
        // == should be recognized as EqEq, not two Eq tokens
        let kinds = token_kinds("a == b");
        assert_eq!(kinds[1], TokenKind::EqEq);

        // <= should be recognized as LtEq, not Lt followed by Eq
        let kinds = token_kinds("a <= b");
        assert_eq!(kinds[1], TokenKind::LtEq);
    }

    #[test]
    fn test_delimiters() {
        let kinds = token_kinds("() {} [] ; , : .");
        use TokenKind::*;
        assert_eq!(
            kinds,
            vec![
                LParen, RParen, LBrace, RBrace, LBracket, RBracket, Semicolon, Comma, Colon, Dot
            ]
        );
    }

    #[test]
    fn test_single_line_comment() {
        let kinds = token_kinds("a // comment\nb");
        assert_eq!(kinds, vec![TokenKind::Ident, TokenKind::Ident]);
    }

    #[test]
    fn test_multi_line_comment() {
        let kinds = token_kinds("a /* comment */ b");
        assert_eq!(kinds, vec![TokenKind::Ident, TokenKind::Ident]);
    }

    #[test]
    fn test_token_positions() {
        let source = "let x = 42;";
        let tokens = tokenize(source);

        // 'let' starts at column 1
        assert_eq!(tokens[0].span.start_loc.column, 1);

        // 'x' starts at column 5
        assert_eq!(tokens[1].span.start_loc.column, 5);

        // '=' starts at column 7
        assert_eq!(tokens[2].span.start_loc.column, 7);

        // '42' starts at column 9
        assert_eq!(tokens[3].span.start_loc.column, 9);

        // ';' starts at column 11
        assert_eq!(tokens[4].span.start_loc.column, 11);
    }

    #[test]
    fn test_multiline_positions() {
        let source = "a\nb\nc";
        let tokens = tokenize(source);

        assert_eq!(tokens[0].span.start_loc.line, 1);
        assert_eq!(tokens[1].span.start_loc.line, 2);
        assert_eq!(tokens[2].span.start_loc.line, 3);
    }

    #[test]
    fn test_source_text() {
        let source = "hello world";
        let mut lexer = Lexer::new(source, DefaultLanguage);
        let token = lexer.next_token();
        assert_eq!(lexer.source_text(token.span), "hello");
    }

    #[test]
    fn test_error_unterminated_string() {
        let (tokens, errors) = Lexer::tokenize(r#""unterminated"#, DefaultLanguage);
        assert!(!errors.is_empty());
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Error));
    }

    #[test]
    fn test_error_unexpected_char() {
        let (tokens, errors) = Lexer::tokenize("@", DefaultLanguage);
        assert!(!errors.is_empty());
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Error));
    }

    #[test]
    fn test_full_program() {
        let source = r#"
            fn main() {
                let x = 42;
                if x > 0 {
                    return true;
                }
            }
        "#;
        let (tokens, errors) = Lexer::tokenize(source, DefaultLanguage);
        assert!(errors.is_empty());
        assert!(!tokens.is_empty());
    }
}
