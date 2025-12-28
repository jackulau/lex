# Lex WebAssembly Module

This directory contains the web demo and TypeScript definitions for using Lex in the browser.

## Building

### Prerequisites

Install the WASM target and `wasm-bindgen-cli`:

```bash
# Add the WASM compilation target
rustup target add wasm32-unknown-unknown

# Install wasm-bindgen CLI
cargo install wasm-bindgen-cli
```

### Build for Web

From the project root directory:

```bash
# Build the WASM library
cargo build --target wasm32-unknown-unknown --features wasm --release

# Generate JavaScript bindings
wasm-bindgen target/wasm32-unknown-unknown/release/lex.wasm \
    --out-dir web/pkg \
    --target web \
    --typescript
```

This creates a `web/pkg/` directory containing:
- `lex.js` - JavaScript glue code
- `lex_bg.wasm` - WebAssembly binary
- `lex.d.ts` - Auto-generated TypeScript definitions

### Running the Demo

After building, serve the `web/` directory with any HTTP server:

```bash
# Using Python
python3 -m http.server 8080 -d web

# Using Node.js http-server
npx http-server web -p 8080

# Using Rust's simple-http-server
cargo install simple-http-server
simple-http-server web -p 8080
```

Then open http://localhost:8080 in your browser.

## Usage in JavaScript/TypeScript

### Basic Usage

```javascript
import init, { tokenize } from './pkg/lex.js';

// Initialize WASM (required once)
await init();

// Tokenize source code
const result = JSON.parse(tokenize('let x = 42;'));

if (result.success) {
    for (const token of result.tokens) {
        console.log(`${token.kind}: "${token.lexeme}" at ${token.startLine}:${token.startColumn}`);
    }
} else {
    for (const error of result.errors) {
        console.error(`Error at ${error.line}:${error.column}: ${error.message}`);
    }
}
```

### Custom Language Configuration

```javascript
import init, { tokenizeWithConfig, LexerConfig } from './pkg/lex.js';

await init();

// Create custom configuration
const config = new LexerConfig();

// Add keywords for your language
config.addKeyword('func');
config.addKeyword('var');
config.addKeyword('let');
config.addKeyword('const');

// Set comment style
config.setSingleLineComment('#');
config.setMultiLineComment('/*', '*/');

// Optional: preserve whitespace/comments in output
config.setPreserveWhitespace(false);
config.setPreserveComments(true);

// Tokenize with custom config
const result = JSON.parse(tokenizeWithConfig('func main() { # comment }', config));
```

### TypeScript Types

The `lex.d.ts` file provides TypeScript definitions. Key types:

```typescript
interface JsToken {
    kind: TokenKind;      // e.g., "Ident", "Keyword", "IntLiteral"
    lexeme: string;       // The actual text
    start: number;        // Byte offset (start)
    end: number;          // Byte offset (end)
    startLine: number;    // Line number (1-indexed)
    startColumn: number;  // Column number (1-indexed)
    endLine: number;
    endColumn: number;
    value?: string;       // For keywords: the keyword text
}

interface TokenizeResult {
    tokens: JsToken[];
    errors: JsLexError[];
    success: boolean;
}
```

## API Reference

### Functions

| Function | Description |
|----------|-------------|
| `tokenize(source: string): string` | Tokenize with default C-like language |
| `tokenizeWithConfig(source: string, config: LexerConfig): string` | Tokenize with custom config |
| `getDefaultKeywords(): string` | Get list of default keywords (JSON array) |
| `getVersion(): string` | Get library version |

### LexerConfig Methods

| Method | Description |
|--------|-------------|
| `new LexerConfig()` | Create new config |
| `addKeyword(keyword: string)` | Add a keyword |
| `setSingleLineComment(prefix: string)` | Set single-line comment prefix |
| `setMultiLineComment(start: string, end: string)` | Set multi-line comment delimiters |
| `setNestedComments(nested: boolean)` | Enable/disable nested comments |
| `setPreserveWhitespace(preserve: boolean)` | Keep whitespace tokens |
| `setPreserveComments(preserve: boolean)` | Keep comment tokens |

## Token Kinds

Tokens are categorized as:

- **Identifiers**: `Ident`
- **Keywords**: `Keyword` (with `value` containing keyword text)
- **Literals**: `IntLiteral`, `FloatLiteral`, `StringLiteral`, `RawStringLiteral`, `CharLiteral`, `BoolLiteral`
- **Operators**: `Plus`, `Minus`, `Star`, `Slash`, `Percent`, `Eq`, `EqEq`, `BangEq`, `Lt`, `Gt`, `LtEq`, `GtEq`, `Bang`, `AmpAmp`, `PipePipe`, `Amp`, `Pipe`, `Caret`, `Tilde`, `LtLt`, `GtGt`
- **Delimiters**: `LParen`, `RParen`, `LBrace`, `RBrace`, `LBracket`, `RBracket`, `Semicolon`, `Comma`, `Colon`, `ColonColon`, `Dot`, `Arrow`, `FatArrow`
- **Special**: `Comment`, `Whitespace`, `Eof`, `Error`

## Bundle Size

The WASM binary is typically 50-100KB gzipped thanks to zero external dependencies in the core lexer.
