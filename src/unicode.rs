//! Unicode utilities for lexical analysis.
//!
//! This module provides Unicode-aware character classification for identifiers,
//! following UAX #31 (Unicode Identifier and Pattern Syntax).
//!
//! # Unicode Identifier Rules
//!
//! According to UAX #31, identifiers are defined as:
//! - XID_Start: Characters that can start an identifier
//! - XID_Continue: Characters that can continue an identifier
//!
//! This implementation supports the full Unicode range, allowing identifiers
//! in any script (Latin, Greek, Cyrillic, CJK, etc.).

/// Check if a character can start an identifier (XID_Start or '_').
///
/// XID_Start includes:
/// - Letters (uppercase, lowercase, titlecase, modifier, other)
/// - Letter numbers (Nl)
/// - Underscore (by convention in most programming languages)
///
/// # Examples
///
/// ```
/// use lex::unicode::is_xid_start;
///
/// assert!(is_xid_start('a'));
/// assert!(is_xid_start('日'));
/// assert!(is_xid_start('α'));
/// assert!(is_xid_start('_'));
/// assert!(!is_xid_start('1'));
/// assert!(!is_xid_start('-'));
/// ```
pub fn is_xid_start(c: char) -> bool {
    c == '_' || is_unicode_xid_start(c)
}

/// Check if a character can continue an identifier (XID_Continue).
///
/// XID_Continue includes:
/// - All XID_Start characters
/// - Decimal digits (Nd)
/// - Combining marks (Mn, Mc)
/// - Connector punctuation (Pc)
///
/// # Examples
///
/// ```
/// use lex::unicode::is_xid_continue;
///
/// assert!(is_xid_continue('a'));
/// assert!(is_xid_continue('1'));
/// assert!(is_xid_continue('_'));
/// assert!(is_xid_continue('日'));
/// assert!(!is_xid_continue('-'));
/// assert!(!is_xid_continue(' '));
/// ```
pub fn is_xid_continue(c: char) -> bool {
    is_unicode_xid_continue(c)
}

/// Check if a character is XID_Start (without underscore).
fn is_unicode_xid_start(c: char) -> bool {
    // Use Unicode general categories to determine XID_Start
    // This is a simplified but accurate check based on Unicode categories
    matches!(unicode_category(c),
        // Letter categories
        UnicodeCategory::Lu |  // Uppercase Letter
        UnicodeCategory::Ll |  // Lowercase Letter
        UnicodeCategory::Lt |  // Titlecase Letter
        UnicodeCategory::Lm |  // Modifier Letter
        UnicodeCategory::Lo |  // Other Letter
        UnicodeCategory::Nl    // Letter Number
    )
}

/// Check if a character is XID_Continue.
fn is_unicode_xid_continue(c: char) -> bool {
    is_unicode_xid_start(c) || matches!(unicode_category(c),
        UnicodeCategory::Mn |  // Nonspacing Mark
        UnicodeCategory::Mc |  // Spacing Combining Mark
        UnicodeCategory::Nd |  // Decimal Number
        UnicodeCategory::Pc    // Connector Punctuation
    ) || c == '_'
}

/// Unicode general category (subset relevant for identifiers).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum UnicodeCategory {
    // Letters
    Lu, // Uppercase Letter
    Ll, // Lowercase Letter
    Lt, // Titlecase Letter
    Lm, // Modifier Letter
    Lo, // Other Letter
    // Marks
    Mn, // Nonspacing Mark
    Mc, // Spacing Combining Mark
    // Numbers
    Nd, // Decimal Number
    Nl, // Letter Number
    // Punctuation
    Pc, // Connector Punctuation
    // Other
    Other,
}

/// Determine the Unicode general category of a character.
///
/// This uses Rust's built-in character classification methods where possible,
/// supplemented with range checks for additional categories.
fn unicode_category(c: char) -> UnicodeCategory {
    // Fast path for ASCII
    if c.is_ascii() {
        if c.is_ascii_uppercase() {
            return UnicodeCategory::Lu;
        }
        if c.is_ascii_lowercase() {
            return UnicodeCategory::Ll;
        }
        if c.is_ascii_digit() {
            return UnicodeCategory::Nd;
        }
        if c == '_' {
            return UnicodeCategory::Pc;
        }
        return UnicodeCategory::Other;
    }

    // Use Rust's Unicode-aware methods
    if c.is_uppercase() {
        return UnicodeCategory::Lu;
    }
    if c.is_lowercase() {
        return UnicodeCategory::Ll;
    }

    // Check for other letter categories
    if c.is_alphabetic() {
        // Distinguish between letter types
        let code = c as u32;

        // Modifier letters (Lm) - common ranges
        if is_modifier_letter(code) {
            return UnicodeCategory::Lm;
        }

        // Titlecase letters are rare, check explicitly
        if is_titlecase_letter(code) {
            return UnicodeCategory::Lt;
        }

        // Default to Lo (Other Letter) for alphabetic characters
        // that aren't Lu, Ll, Lt, or Lm
        return UnicodeCategory::Lo;
    }

    // Decimal digits (Nd) - includes non-ASCII digits
    if c.is_numeric() {
        let code = c as u32;
        if is_decimal_digit(code) {
            return UnicodeCategory::Nd;
        }
        // Letter numbers (Nl) like Roman numerals
        if is_letter_number(code) {
            return UnicodeCategory::Nl;
        }
    }

    // Combining marks
    let code = c as u32;
    if is_nonspacing_mark(code) {
        return UnicodeCategory::Mn;
    }
    if is_spacing_combining_mark(code) {
        return UnicodeCategory::Mc;
    }

    // Connector punctuation (like undertie ‿)
    if is_connector_punctuation(code) {
        return UnicodeCategory::Pc;
    }

    UnicodeCategory::Other
}

/// Check if a code point is a modifier letter (Lm).
fn is_modifier_letter(code: u32) -> bool {
    matches!(code,
        0x02B0..=0x02C1 |   // Modifier letters
        0x02C6..=0x02D1 |
        0x02E0..=0x02E4 |
        0x02EC |
        0x02EE |
        0x0374 |           // Greek
        0x037A |
        0x0559 |           // Armenian
        0x0640 |           // Arabic
        0x06E5..=0x06E6 |
        0x07F4..=0x07F5 |
        0x07FA |
        0x081A |
        0x0824 |
        0x0828 |
        0x08C9..=0x08D2 |
        0x0971 |
        0x0E46 |           // Thai
        0x0EC6 |           // Lao
        0x10FC |
        0x17D7 |           // Khmer
        0x1843 |
        0x1AA7 |
        0x1C78..=0x1C7D |
        0x1D2C..=0x1D6A |  // Phonetic extensions
        0x1D78 |
        0x1D9B..=0x1DBF |
        0x2071 |
        0x207F |
        0x2090..=0x209C |
        0x2C7C..=0x2C7D |
        0x2D6F |
        0x2E2F |
        0x3005 |           // CJK
        0x3031..=0x3035 |
        0x303B |
        0x309D..=0x309E |
        0x30FC..=0x30FE |
        0xA015 |
        0xA4F8..=0xA4FD |
        0xA60C |
        0xA67F |
        0xA69C..=0xA69D |
        0xA717..=0xA71F |
        0xA770 |
        0xA788 |
        0xA7F2..=0xA7F4 |
        0xA7F8..=0xA7F9 |
        0xA9CF |
        0xA9E6 |
        0xAA70 |
        0xAADD |
        0xAAF3..=0xAAF4 |
        0xAB5C..=0xAB5F |
        0xAB69 |
        0xFF70 |
        0xFF9E..=0xFF9F
    )
}

/// Check if a code point is a titlecase letter (Lt).
fn is_titlecase_letter(code: u32) -> bool {
    matches!(code,
        0x01C5 | 0x01C8 | 0x01CB | 0x01F2 |
        0x1F88..=0x1F8F |
        0x1F98..=0x1F9F |
        0x1FA8..=0x1FAF |
        0x1FBC | 0x1FCC | 0x1FFC
    )
}

/// Check if a code point is a decimal digit (Nd).
fn is_decimal_digit(code: u32) -> bool {
    matches!(code,
        0x0030..=0x0039 |  // ASCII digits
        0x0660..=0x0669 |  // Arabic-Indic
        0x06F0..=0x06F9 |  // Extended Arabic-Indic
        0x07C0..=0x07C9 |  // NKo
        0x0966..=0x096F |  // Devanagari
        0x09E6..=0x09EF |  // Bengali
        0x0A66..=0x0A6F |  // Gurmukhi
        0x0AE6..=0x0AEF |  // Gujarati
        0x0B66..=0x0B6F |  // Oriya
        0x0BE6..=0x0BEF |  // Tamil
        0x0C66..=0x0C6F |  // Telugu
        0x0CE6..=0x0CEF |  // Kannada
        0x0D66..=0x0D6F |  // Malayalam
        0x0DE6..=0x0DEF |  // Sinhala
        0x0E50..=0x0E59 |  // Thai
        0x0ED0..=0x0ED9 |  // Lao
        0x0F20..=0x0F29 |  // Tibetan
        0x1040..=0x1049 |  // Myanmar
        0x1090..=0x1099 |
        0x17E0..=0x17E9 |  // Khmer
        0x1810..=0x1819 |  // Mongolian
        0x1946..=0x194F |
        0x19D0..=0x19D9 |
        0x1A80..=0x1A89 |
        0x1A90..=0x1A99 |
        0x1B50..=0x1B59 |
        0x1BB0..=0x1BB9 |
        0x1C40..=0x1C49 |
        0x1C50..=0x1C59 |
        0xA620..=0xA629 |
        0xA8D0..=0xA8D9 |
        0xA900..=0xA909 |
        0xA9D0..=0xA9D9 |
        0xA9F0..=0xA9F9 |
        0xAA50..=0xAA59 |
        0xABF0..=0xABF9 |
        0xFF10..=0xFF19    // Fullwidth
    )
}

/// Check if a code point is a letter number (Nl).
fn is_letter_number(code: u32) -> bool {
    matches!(code,
        0x16EE..=0x16F0 |  // Runic
        0x2160..=0x2182 |  // Roman numerals
        0x2185..=0x2188 |
        0x3007 |           // CJK
        0x3021..=0x3029 |
        0x3038..=0x303A |
        0xA6E6..=0xA6EF |
        0x10140..=0x10174 |
        0x10341 |
        0x1034A |
        0x103D1..=0x103D5 |
        0x12400..=0x1246E
    )
}

/// Check if a code point is a nonspacing mark (Mn).
fn is_nonspacing_mark(code: u32) -> bool {
    matches!(code,
        0x0300..=0x036F |  // Combining Diacritical Marks
        0x0483..=0x0487 |
        0x0591..=0x05BD |
        0x05BF |
        0x05C1..=0x05C2 |
        0x05C4..=0x05C5 |
        0x05C7 |
        0x0610..=0x061A |
        0x064B..=0x065F |
        0x0670 |
        0x06D6..=0x06DC |
        0x06DF..=0x06E4 |
        0x06E7..=0x06E8 |
        0x06EA..=0x06ED |
        0x0711 |
        0x0730..=0x074A |
        0x07A6..=0x07B0 |
        0x07EB..=0x07F3 |
        0x07FD |
        0x0816..=0x0819 |
        0x081B..=0x0823 |
        0x0825..=0x0827 |
        0x0829..=0x082D |
        0x0859..=0x085B |
        0x0898..=0x089F |
        0x08CA..=0x08E1 |
        0x08E3..=0x0902 |
        0x093A |
        0x093C |
        0x0941..=0x0948 |
        0x094D |
        0x0951..=0x0957 |
        0x0962..=0x0963 |
        0x0981 |
        0x09BC |
        0x09C1..=0x09C4 |
        0x09CD |
        0x09E2..=0x09E3 |
        0x09FE |
        0x0A01..=0x0A02 |
        0x0A3C |
        0x0A41..=0x0A42 |
        0x0A47..=0x0A48 |
        0x0A4B..=0x0A4D |
        0x0A51 |
        0x0A70..=0x0A71 |
        0x0A75 |
        0x0A81..=0x0A82 |
        0x0ABC |
        0x0AC1..=0x0AC5 |
        0x0AC7..=0x0AC8 |
        0x0ACD |
        0x0AE2..=0x0AE3 |
        0x0AFA..=0x0AFF |
        0x0B01 |
        0x0B3C |
        0x0B3F |
        0x0B41..=0x0B44 |
        0x0B4D |
        0x0B55..=0x0B56 |
        0x0B62..=0x0B63 |
        0x0B82 |
        0x0BC0 |
        0x0BCD |
        0x0C00 |
        0x0C04 |
        0x0C3C |
        0x0C3E..=0x0C40 |
        0x0C46..=0x0C48 |
        0x0C4A..=0x0C4D |
        0x0C55..=0x0C56 |
        0x0C62..=0x0C63 |
        0x0C81 |
        0x0CBC |
        0x0CBF |
        0x0CC6 |
        0x0CCC..=0x0CCD |
        0x0CE2..=0x0CE3 |
        0x0D00..=0x0D01 |
        0x0D3B..=0x0D3C |
        0x0D41..=0x0D44 |
        0x0D4D |
        0x0D62..=0x0D63 |
        0x0D81 |
        0x0DCA |
        0x0DD2..=0x0DD4 |
        0x0DD6 |
        0x0E31 |
        0x0E34..=0x0E3A |
        0x0E47..=0x0E4E |
        0x0EB1 |
        0x0EB4..=0x0EBC |
        0x0EC8..=0x0ECE |
        0x0F18..=0x0F19 |
        0x0F35 |
        0x0F37 |
        0x0F39 |
        0x0F71..=0x0F7E |
        0x0F80..=0x0F84 |
        0x0F86..=0x0F87 |
        0x0F8D..=0x0F97 |
        0x0F99..=0x0FBC |
        0x0FC6 |
        0x102D..=0x1030 |
        0x1032..=0x1037 |
        0x1039..=0x103A |
        0x103D..=0x103E |
        0x1058..=0x1059 |
        0x105E..=0x1060 |
        0x1071..=0x1074 |
        0x1082 |
        0x1085..=0x1086 |
        0x108D |
        0x109D |
        0x135D..=0x135F |
        0x1712..=0x1714 |
        0x1732..=0x1733 |
        0x1752..=0x1753 |
        0x1772..=0x1773 |
        0x17B4..=0x17B5 |
        0x17B7..=0x17BD |
        0x17C6 |
        0x17C9..=0x17D3 |
        0x17DD |
        0x180B..=0x180D |
        0x180F |
        0x1885..=0x1886 |
        0x18A9 |
        0x1920..=0x1922 |
        0x1927..=0x1928 |
        0x1932 |
        0x1939..=0x193B |
        0x1A17..=0x1A18 |
        0x1A1B |
        0x1A56 |
        0x1A58..=0x1A5E |
        0x1A60 |
        0x1A62 |
        0x1A65..=0x1A6C |
        0x1A73..=0x1A7C |
        0x1A7F |
        0x1AB0..=0x1ABD |
        0x1ABF..=0x1ACE |
        0x1B00..=0x1B03 |
        0x1B34 |
        0x1B36..=0x1B3A |
        0x1B3C |
        0x1B42 |
        0x1B6B..=0x1B73 |
        0x1B80..=0x1B81 |
        0x1BA2..=0x1BA5 |
        0x1BA8..=0x1BA9 |
        0x1BAB..=0x1BAD |
        0x1BE6 |
        0x1BE8..=0x1BE9 |
        0x1BED |
        0x1BEF..=0x1BF1 |
        0x1C2C..=0x1C33 |
        0x1C36..=0x1C37 |
        0x1CD0..=0x1CD2 |
        0x1CD4..=0x1CE0 |
        0x1CE2..=0x1CE8 |
        0x1CED |
        0x1CF4 |
        0x1CF8..=0x1CF9 |
        0x1DC0..=0x1DFF |
        0x20D0..=0x20DC |
        0x20E1 |
        0x20E5..=0x20F0 |
        0x2CEF..=0x2CF1 |
        0x2D7F |
        0x2DE0..=0x2DFF |
        0x302A..=0x302D |
        0x3099..=0x309A |
        0xA66F |
        0xA674..=0xA67D |
        0xA69E..=0xA69F |
        0xA6F0..=0xA6F1 |
        0xA802 |
        0xA806 |
        0xA80B |
        0xA825..=0xA826 |
        0xA82C |
        0xA8C4..=0xA8C5 |
        0xA8E0..=0xA8F1 |
        0xA8FF |
        0xA926..=0xA92D |
        0xA947..=0xA951 |
        0xA980..=0xA982 |
        0xA9B3 |
        0xA9B6..=0xA9B9 |
        0xA9BC..=0xA9BD |
        0xA9E5 |
        0xAA29..=0xAA2E |
        0xAA31..=0xAA32 |
        0xAA35..=0xAA36 |
        0xAA43 |
        0xAA4C |
        0xAA7C |
        0xAAB0 |
        0xAAB2..=0xAAB4 |
        0xAAB7..=0xAAB8 |
        0xAABE..=0xAABF |
        0xAAC1 |
        0xAAEC..=0xAAED |
        0xAAF6 |
        0xABE5 |
        0xABE8 |
        0xABED |
        0xFB1E |
        0xFE00..=0xFE0F |
        0xFE20..=0xFE2F
    )
}

/// Check if a code point is a spacing combining mark (Mc).
fn is_spacing_combining_mark(code: u32) -> bool {
    matches!(code,
        0x0903 |
        0x093B |
        0x093E..=0x0940 |
        0x0949..=0x094C |
        0x094E..=0x094F |
        0x0982..=0x0983 |
        0x09BE..=0x09C0 |
        0x09C7..=0x09C8 |
        0x09CB..=0x09CC |
        0x09D7 |
        0x0A03 |
        0x0A3E..=0x0A40 |
        0x0A83 |
        0x0ABE..=0x0AC0 |
        0x0AC9 |
        0x0ACB..=0x0ACC |
        0x0B02..=0x0B03 |
        0x0B3E |
        0x0B40 |
        0x0B47..=0x0B48 |
        0x0B4B..=0x0B4C |
        0x0B57 |
        0x0BBE..=0x0BBF |
        0x0BC1..=0x0BC2 |
        0x0BC6..=0x0BC8 |
        0x0BCA..=0x0BCC |
        0x0BD7 |
        0x0C01..=0x0C03 |
        0x0C41..=0x0C44 |
        0x0C82..=0x0C83 |
        0x0CBE |
        0x0CC0..=0x0CC4 |
        0x0CC7..=0x0CC8 |
        0x0CCA..=0x0CCB |
        0x0CD5..=0x0CD6 |
        0x0CF3 |
        0x0D02..=0x0D03 |
        0x0D3E..=0x0D40 |
        0x0D46..=0x0D48 |
        0x0D4A..=0x0D4C |
        0x0D57 |
        0x0D82..=0x0D83 |
        0x0DCF..=0x0DD1 |
        0x0DD8..=0x0DDF |
        0x0DF2..=0x0DF3 |
        0x0F3E..=0x0F3F |
        0x0F7F |
        0x102B..=0x102C |
        0x1031 |
        0x1038 |
        0x103B..=0x103C |
        0x1056..=0x1057 |
        0x1062..=0x1064 |
        0x1067..=0x106D |
        0x1083..=0x1084 |
        0x1087..=0x108C |
        0x108F |
        0x109A..=0x109C |
        0x1715 |
        0x1734 |
        0x17B6 |
        0x17BE..=0x17C5 |
        0x17C7..=0x17C8 |
        0x1923..=0x1926 |
        0x1929..=0x192B |
        0x1930..=0x1931 |
        0x1933..=0x1938 |
        0x1A19..=0x1A1A |
        0x1A55 |
        0x1A57 |
        0x1A61 |
        0x1A63..=0x1A64 |
        0x1A6D..=0x1A72 |
        0x1B04 |
        0x1B35 |
        0x1B3B |
        0x1B3D..=0x1B41 |
        0x1B43..=0x1B44 |
        0x1B82 |
        0x1BA1 |
        0x1BA6..=0x1BA7 |
        0x1BAA |
        0x1BE7 |
        0x1BEA..=0x1BEC |
        0x1BEE |
        0x1BF2..=0x1BF3 |
        0x1C24..=0x1C2B |
        0x1C34..=0x1C35 |
        0x1CE1 |
        0x1CF7 |
        0x302E..=0x302F |
        0xA823..=0xA824 |
        0xA827 |
        0xA880..=0xA881 |
        0xA8B4..=0xA8C3 |
        0xA952..=0xA953 |
        0xA983 |
        0xA9B4..=0xA9B5 |
        0xA9BA..=0xA9BB |
        0xA9BE..=0xA9C0 |
        0xAA2F..=0xAA30 |
        0xAA33..=0xAA34 |
        0xAA4D |
        0xAA7B |
        0xAA7D |
        0xAAEB |
        0xAAEE..=0xAAEF |
        0xAAF5 |
        0xABE3..=0xABE4 |
        0xABE6..=0xABE7 |
        0xABE9..=0xABEA |
        0xABEC
    )
}

/// Check if a code point is connector punctuation (Pc).
fn is_connector_punctuation(code: u32) -> bool {
    matches!(code,
        0x005F |           // Underscore
        0x203F..=0x2040 |  // Undertie, Character tie
        0x2054 |           // Inverted undertie
        0xFE33..=0xFE34 |  // Presentation forms
        0xFE4D..=0xFE4F |
        0xFF3F             // Fullwidth underscore
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_identifier_start() {
        // Letters can start identifiers
        assert!(is_xid_start('a'));
        assert!(is_xid_start('z'));
        assert!(is_xid_start('A'));
        assert!(is_xid_start('Z'));
        assert!(is_xid_start('_'));

        // Digits cannot start identifiers
        assert!(!is_xid_start('0'));
        assert!(!is_xid_start('9'));

        // Punctuation cannot start identifiers
        assert!(!is_xid_start('-'));
        assert!(!is_xid_start('+'));
        assert!(!is_xid_start(' '));
    }

    #[test]
    fn test_ascii_identifier_continue() {
        // Letters can continue identifiers
        assert!(is_xid_continue('a'));
        assert!(is_xid_continue('Z'));
        assert!(is_xid_continue('_'));

        // Digits can continue identifiers
        assert!(is_xid_continue('0'));
        assert!(is_xid_continue('9'));

        // Punctuation cannot continue identifiers
        assert!(!is_xid_continue('-'));
        assert!(!is_xid_continue(' '));
    }

    #[test]
    fn test_unicode_letters() {
        // Greek
        assert!(is_xid_start('α'));
        assert!(is_xid_start('Ω'));
        assert!(is_xid_continue('α'));

        // Cyrillic
        assert!(is_xid_start('я'));
        assert!(is_xid_start('Б'));

        // CJK
        assert!(is_xid_start('日'));
        assert!(is_xid_start('本'));
        assert!(is_xid_start('語'));

        // Korean
        assert!(is_xid_start('한'));
        assert!(is_xid_start('글'));

        // Arabic
        assert!(is_xid_start('م'));

        // Hebrew
        assert!(is_xid_start('א'));
    }

    #[test]
    fn test_unicode_digits() {
        // Arabic-Indic digits
        assert!(!is_xid_start('٠')); // Cannot start
        assert!(is_xid_continue('٠')); // Can continue

        // Devanagari digits
        assert!(!is_xid_start('०'));
        assert!(is_xid_continue('०'));
    }

    #[test]
    fn test_combining_marks() {
        // Combining acute accent (U+0301)
        assert!(!is_xid_start('\u{0301}'));
        assert!(is_xid_continue('\u{0301}'));

        // Combining diacritical marks can continue identifiers
        // This allows identifiers like "café" where é = e + combining acute
    }

    #[test]
    fn test_special_cases() {
        // Zero-width joiner and non-joiner should not be identifier chars
        assert!(!is_xid_start('\u{200C}')); // ZWNJ
        assert!(!is_xid_start('\u{200D}')); // ZWJ

        // Emoji should not be identifier chars
        assert!(!is_xid_start('😀'));
        assert!(!is_xid_continue('😀'));
    }

    #[test]
    fn test_connector_punctuation() {
        // Underscore
        assert!(is_xid_start('_'));
        assert!(is_xid_continue('_'));

        // Undertie (U+203F)
        assert!(!is_xid_start('\u{203F}')); // Cannot start
        assert!(is_xid_continue('\u{203F}')); // Can continue
    }
}
