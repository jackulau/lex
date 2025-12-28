/**
 * TypeScript type definitions for the Lex WASM module.
 *
 * These types describe the JSON structures returned by the tokenize functions.
 */

/**
 * A token produced by the lexer.
 */
export interface JsToken {
  /** The type of token (e.g., "Ident", "IntLiteral", "Plus"). */
  kind: TokenKind;
  /** The actual text of the token from source. */
  lexeme: string;
  /** Start byte offset (inclusive). */
  start: number;
  /** End byte offset (exclusive). */
  end: number;
  /** Start line (1-indexed). */
  startLine: number;
  /** Start column (1-indexed). */
  startColumn: number;
  /** End line (1-indexed). */
  endLine: number;
  /** End column (1-indexed). */
  endColumn: number;
  /** For keyword tokens, the keyword text; for bool literals, the value. */
  value?: string;
}

/**
 * A lexer error.
 */
export interface JsLexError {
  /** The error message. */
  message: string;
  /** Error kind identifier. */
  kind: LexErrorKind;
  /** Start byte offset. */
  start: number;
  /** End byte offset. */
  end: number;
  /** Line number (1-indexed). */
  line: number;
  /** Column number (1-indexed). */
  column: number;
}

/**
 * The result of tokenizing source code.
 */
export interface TokenizeResult {
  /** The tokens produced. */
  tokens: JsToken[];
  /** Any errors encountered. */
  errors: JsLexError[];
  /** Whether tokenization completed without errors. */
  success: boolean;
}

/**
 * Token kind identifiers.
 */
export type TokenKind =
  | "Keyword"
  | "Ident"
  | "IntLiteral"
  | "FloatLiteral"
  | "StringLiteral"
  | "RawStringLiteral"
  | "CharLiteral"
  | "BoolLiteral"
  | "Plus"
  | "Minus"
  | "Star"
  | "Slash"
  | "Percent"
  | "Eq"
  | "EqEq"
  | "BangEq"
  | "Lt"
  | "Gt"
  | "LtEq"
  | "GtEq"
  | "Bang"
  | "AmpAmp"
  | "PipePipe"
  | "Amp"
  | "Pipe"
  | "Caret"
  | "Tilde"
  | "LtLt"
  | "GtGt"
  | "LParen"
  | "RParen"
  | "LBrace"
  | "RBrace"
  | "LBracket"
  | "RBracket"
  | "Semicolon"
  | "Comma"
  | "Colon"
  | "ColonColon"
  | "Dot"
  | "Arrow"
  | "FatArrow"
  | "Comment"
  | "Whitespace"
  | "Eof"
  | "Error";

/**
 * Lexer error kind identifiers.
 */
export type LexErrorKind =
  | "UnexpectedChar"
  | "UnterminatedString"
  | "UnterminatedComment"
  | "UnterminatedChar"
  | "NewlineInString"
  | "InvalidEscape"
  | "InvalidNumber"
  | "InvalidUnicodeEscape"
  | "EmptyCharLiteral"
  | "MultiCharLiteral"
  | "UnexpectedEof";

/**
 * Configuration for the lexer.
 * Create with `new LexerConfig()` and configure before passing to `tokenizeWithConfig`.
 */
export class LexerConfig {
  constructor();

  /** Add a keyword to the language. */
  addKeyword(keyword: string): void;

  /** Set the single-line comment prefix (e.g., "//", "#"). */
  setSingleLineComment(prefix: string): void;

  /** Set the multi-line comment delimiters (e.g., "/*" and "*/"). */
  setMultiLineComment(start: string, end: string): void;

  /** Enable or disable nested multi-line comments. */
  setNestedComments(nested: boolean): void;

  /** Enable or disable preserving whitespace tokens. */
  setPreserveWhitespace(preserve: boolean): void;

  /** Enable or disable preserving comment tokens. */
  setPreserveComments(preserve: boolean): void;
}

/**
 * Initialize the WASM module.
 * Must be called before using any other functions.
 */
export default function init(): Promise<void>;

/**
 * Tokenize source code using the default C-like language.
 * @param source The source code to tokenize.
 * @returns A JSON string containing the TokenizeResult.
 */
export function tokenize(source: string): string;

/**
 * Tokenize source code with a custom configuration.
 * @param source The source code to tokenize.
 * @param config The lexer configuration.
 * @returns A JSON string containing the TokenizeResult.
 */
export function tokenizeWithConfig(source: string, config: LexerConfig): string;

/**
 * Get information about the default language keywords.
 * @returns A JSON string containing an array of keyword strings.
 */
export function getDefaultKeywords(): string;

/**
 * Get the library version.
 * @returns The version string.
 */
export function getVersion(): string;

// Convenience type for parsed results
export type ParsedTokenizeResult = TokenizeResult;

/**
 * Helper function to parse tokenize result (for documentation purposes).
 * In actual usage, call JSON.parse(tokenize(source)) directly.
 */
export function parseTokenizeResult(jsonResult: string): TokenizeResult;
