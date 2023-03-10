use logos::{Logos, Lexer};

use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    raw: RawToken,
    slice: &'a str
}

pub struct TokenIterator<'a> {
    lex: Lexer<'a, RawToken>,
}

pub fn lex<'a>(text: &'a str) -> TokenIterator<'a> {
    TokenIterator { lex: RawToken::lexer(text) }
}

impl<'a> Iterator for TokenIterator<'a> {
   type Item = Token<'a>; 
   fn next(&mut self) -> Option<Self::Item> {
       let raw = self.lex.next()?;
       Some(Token { raw, slice: self.lex.slice() })
   }
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
enum RawToken {
    // Tokens can be literal strings, of any length.
    #[token("local")]
    KeywordLocal,

    #[token("function")]
    KeywordFunction,

    #[token("end")]
    KeywordEnd,

    #[token("true")]
    KeywordTrue,

    #[token("false")]
    KeywordFalse,

    #[token(".")]
    SymbolPeriod,

    #[token(":")]
    SymbolColon,

    #[token(";")]
    SymbolSemi,

    #[token("{")]
    SymbolOpenBrace,

    #[token("}")]
    SymbolCloseBrace,

    #[token("=")]
    SymbolAssign,

    #[token("[")]
    SymbolLeftBracket,

    #[token("]")]
    SymbolRightBracket,

    #[token(",")]
    SymbolComma,

    #[token("(")]
    SymbolLeftParen,

    #[token(")")]
    SymbolRightParen,

    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Ident,

    #[regex(r"(?:0|[1-9][0-9]*)(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?")]
    Number,

    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"")]
    StringDoubleQuoted,

    #[regex("'(?s:[^'\\\\]|\\\\.)*'")]
    StringSingleQuoted,

    #[token("\n")]
    Newline,

    // Logos requires one token variant to handle errors,
    // it can be named anything you wish.
    #[error]
    // We can also use this variant to define whitespace,
    // or any other matches we wish to skip.
    #[regex(r"[ \t\f]+", logos::skip)]
    Error,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decl() {
        let mut it = lex("a = 10");
        assert_eq!(it.next().unwrap().raw, RawToken::Ident);
        assert_eq!(it.next().unwrap().raw, RawToken::SymbolAssign);
        assert_eq!(it.next().unwrap().raw, RawToken::Number);
        assert_eq!(it.next(), None)
    }

    #[test]
    fn local_decl() {
        let mut it = lex("local a = 10");
        assert_eq!(it.next().unwrap().raw, RawToken::KeywordLocal);
        assert_eq!(it.next(), Some(Token {raw: RawToken::Ident, slice: "a"}));
        assert_eq!(it.next().unwrap().raw, RawToken::SymbolAssign);
        assert_eq!(it.next(), Some(Token { raw: RawToken::Number, slice: "10" }));
        assert_eq!(it.next(), None)
    }

    #[test]
    fn local_decl_no_value() {
        let mut it = lex("local a");
        assert_eq!(it.next().unwrap().raw, RawToken::KeywordLocal);
        assert_eq!(it.next(), Some(Token { raw: RawToken::Ident, slice: "a" }));
        assert_eq!(it.next(), None)
    }

    #[test]
    fn int() {
        let mut it = lex("10 100000000000000000");
        assert_eq!(it.next(), Some(Token { raw: RawToken::Number, slice: "10" }));
        assert_eq!(it.next(), Some(Token { raw: RawToken::Number, slice: "100000000000000000"}));
        assert_eq!(it.next(), None)
    }

    #[test]
    fn float() {
        let mut it = lex("10.0 90.8 2222.0 67.333333333");
        assert_eq!(it.next(), Some(Token { raw: RawToken::Number, slice: "10.0" }));
        assert_eq!(it.next(), Some(Token { raw: RawToken::Number, slice: "90.8" }));
        assert_eq!(it.next(), Some(Token { raw: RawToken::Number, slice: "2222.0" }));
        assert_eq!(it.next(), Some(Token { raw: RawToken::Number, slice: "67.333333333" }));
        assert_eq!(it.next(), None)
    }
}
