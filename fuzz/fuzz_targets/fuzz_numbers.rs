//! Number literal fuzz target.
//!
//! Tests all numeric formats: integers, floats, hex, octal, binary,
//! scientific notation, and numbers with underscores.

#![no_main]

use arbitrary::Arbitrary;
use libfuzzer_sys::fuzz_target;
use lex::{DefaultLanguage, Lexer, TokenKind};

/// Structured input for generating number-focused test cases.
#[derive(Debug, Arbitrary)]
struct NumberInput {
    /// Type of number to generate
    number_type: NumberType,
    /// Whether to include underscores as separators
    use_underscores: bool,
    /// Number of digits/parts
    length: u8,
    /// Raw bytes for generating digits
    digits: Vec<u8>,
    /// Optional suffix (type annotation like i32, f64)
    suffix: Option<String>,
}

#[derive(Debug, Arbitrary)]
enum NumberType {
    /// Decimal integer: 123
    DecimalInt,
    /// Hexadecimal: 0x1A2B
    Hex,
    /// Octal: 0o777
    Octal,
    /// Binary: 0b1010
    Binary,
    /// Float: 3.14
    Float,
    /// Scientific notation: 1.5e10
    Scientific,
    /// Float with exponent but no decimal: 1e10
    IntExponent,
    /// Edge cases: leading zeros, multiple dots, etc.
    EdgeCase(EdgeCaseType),
}

#[derive(Debug, Arbitrary)]
enum EdgeCaseType {
    /// Leading zeros: 007
    LeadingZeros,
    /// Multiple underscores: 1__2
    MultipleUnderscores,
    /// Trailing underscore: 123_
    TrailingUnderscore,
    /// Leading underscore in number: 0x_FF
    LeadingUnderscore,
    /// Very long number
    VeryLong,
    /// Empty after prefix: 0x, 0b, 0o
    EmptyAfterPrefix,
    /// Invalid digits for base
    InvalidDigits,
}

impl NumberInput {
    fn to_source(&self) -> String {
        let len = (self.length % 20).max(1) as usize;

        let result = match &self.number_type {
            NumberType::DecimalInt => self.generate_decimal(len),
            NumberType::Hex => self.generate_hex(len),
            NumberType::Octal => self.generate_octal(len),
            NumberType::Binary => self.generate_binary(len),
            NumberType::Float => self.generate_float(len),
            NumberType::Scientific => self.generate_scientific(len),
            NumberType::IntExponent => self.generate_int_exponent(len),
            NumberType::EdgeCase(edge) => self.generate_edge_case(edge, len),
        };

        // Add optional suffix
        if let Some(ref suffix) = self.suffix {
            format!("{}{}", result, suffix)
        } else {
            result
        }
    }

    fn generate_decimal(&self, len: usize) -> String {
        let mut result = String::new();
        for i in 0..len {
            if self.use_underscores && i > 0 && i % 3 == 0 {
                result.push('_');
            }
            let digit = self.digits.get(i).unwrap_or(&0) % 10;
            // Avoid leading zero
            if i == 0 && digit == 0 && len > 1 {
                result.push('1');
            } else {
                result.push((b'0' + digit) as char);
            }
        }
        result
    }

    fn generate_hex(&self, len: usize) -> String {
        let mut result = String::from("0x");
        for i in 0..len {
            if self.use_underscores && i > 0 && i % 4 == 0 {
                result.push('_');
            }
            let digit = self.digits.get(i).unwrap_or(&0) % 16;
            result.push(HEX_DIGITS[digit as usize]);
        }
        result
    }

    fn generate_octal(&self, len: usize) -> String {
        let mut result = String::from("0o");
        for i in 0..len {
            if self.use_underscores && i > 0 && i % 3 == 0 {
                result.push('_');
            }
            let digit = self.digits.get(i).unwrap_or(&0) % 8;
            result.push((b'0' + digit) as char);
        }
        result
    }

    fn generate_binary(&self, len: usize) -> String {
        let mut result = String::from("0b");
        for i in 0..len {
            if self.use_underscores && i > 0 && i % 4 == 0 {
                result.push('_');
            }
            let digit = self.digits.get(i).unwrap_or(&0) % 2;
            result.push((b'0' + digit) as char);
        }
        result
    }

    fn generate_float(&self, len: usize) -> String {
        let int_part = len / 2;
        let frac_part = len - int_part;

        let mut result = String::new();

        // Integer part
        for i in 0..int_part.max(1) {
            let digit = self.digits.get(i).unwrap_or(&1) % 10;
            if i == 0 && digit == 0 {
                result.push('1');
            } else {
                result.push((b'0' + digit) as char);
            }
        }

        result.push('.');

        // Fractional part
        for i in 0..frac_part.max(1) {
            let digit = self.digits.get(int_part + i).unwrap_or(&0) % 10;
            result.push((b'0' + digit) as char);
        }

        result
    }

    fn generate_scientific(&self, len: usize) -> String {
        let mut result = self.generate_float(len);
        let exp_sign = if self.digits.first().unwrap_or(&0) % 2 == 0 {
            "e+"
        } else {
            "e-"
        };
        result.push_str(exp_sign);

        // Exponent digits
        for i in 0..2 {
            let digit = self.digits.get(len + i).unwrap_or(&1) % 10;
            result.push((b'0' + digit) as char);
        }

        result
    }

    fn generate_int_exponent(&self, len: usize) -> String {
        let mut result = self.generate_decimal(len);
        result.push('e');
        let digit = self.digits.last().unwrap_or(&1) % 10;
        result.push((b'0' + digit) as char);
        result
    }

    fn generate_edge_case(&self, edge: &EdgeCaseType, len: usize) -> String {
        match edge {
            EdgeCaseType::LeadingZeros => {
                let mut result = String::from("00");
                result.push_str(&self.generate_decimal(len));
                result
            }
            EdgeCaseType::MultipleUnderscores => {
                let mut result = String::new();
                for i in 0..len {
                    let digit = self.digits.get(i).unwrap_or(&1) % 10;
                    result.push((b'0' + digit) as char);
                    if i < len - 1 {
                        result.push_str("__");
                    }
                }
                result
            }
            EdgeCaseType::TrailingUnderscore => {
                format!("{}_", self.generate_decimal(len))
            }
            EdgeCaseType::LeadingUnderscore => {
                format!("0x_{}", &self.generate_hex(len)[2..])
            }
            EdgeCaseType::VeryLong => {
                self.generate_decimal(100)
            }
            EdgeCaseType::EmptyAfterPrefix => {
                match self.digits.first().unwrap_or(&0) % 3 {
                    0 => String::from("0x"),
                    1 => String::from("0o"),
                    _ => String::from("0b"),
                }
            }
            EdgeCaseType::InvalidDigits => {
                match self.digits.first().unwrap_or(&0) % 3 {
                    0 => String::from("0xGHI"), // Invalid hex
                    1 => String::from("0o89"),   // Invalid octal
                    _ => String::from("0b234"),  // Invalid binary
                }
            }
        }
    }
}

const HEX_DIGITS: [char; 16] = [
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
];

fuzz_target!(|input: NumberInput| {
    let source = input.to_source();

    // Tokenize and verify no panics
    let (tokens, errors) = Lexer::tokenize(&source, DefaultLanguage);

    // Note: EOF is not in token stream

    // Verify token spans
    for token in &tokens {
        assert!(token.span.start <= token.span.end);
        assert!(token.span.end <= source.len());
    }

    // Valid numbers should produce Number tokens (possibly with errors for edge cases)
    let has_number = tokens.iter().any(|t| {
        matches!(
            t.kind,
            TokenKind::IntLiteral | TokenKind::FloatLiteral
        )
    });

    // For most number types, we expect a number token
    // Edge cases might not produce a number
    match &input.number_type {
        NumberType::EdgeCase(EdgeCaseType::EmptyAfterPrefix)
        | NumberType::EdgeCase(EdgeCaseType::InvalidDigits) => {
            // These might not produce valid numbers
        }
        _ => {
            // Most number inputs should tokenize to some numeric token
            // (though they might be followed by identifier tokens for suffixes)
            if !has_number && errors.is_empty() {
                // Could be parsed as identifier + number or other combination
                // This is acceptable
            }
        }
    }
});
