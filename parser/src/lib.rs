#![feature(result_option_inspect)]
use bstr::BStr;
use code::Statement;
use lexer::Token;
use lhs::{Str, Literal};
use derive_more::{From, AsRef, TryInto, Deref};
use nom::{
    branch::alt,
    bytes::{complete::{is_not, tag, take_while, take_till, take_while1}},
    character::{complete::{alphanumeric1, space0, one_of}},
    combinator::{eof, opt, recognize, verify, map, map_res, not},
    sequence::{delimited, terminated},
    IResult, Parser, multi::{many0, separated_list0, many1}, FindToken,
};

pub mod lhs;
pub mod code;
mod utils;
mod lexer;
mod error;
// mod buf;

pub use code::*;
use utils::{ws, paren_comma_list};

// pub type Buf<'a> = &'a [Token<'a>];
pub type BufElement = u8;
pub type Buf<'a> = &'a [u8];
pub type OwnedBuf = Vec<u8>;
// pub use buf::Buf;

use error::Error;

pub type R<'a, T = Buf<'a>> = IResult<Buf<'a>, T, Error<'a>>;

trait Parseable: Sized {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self>;
}

impl<T> Parseable for Box<T> where T: Parseable {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        T::parse.map(Box::new).parse(i)
    }
}

#[derive(Clone, From, PartialEq, Eq, AsRef, Hash)]
pub struct Ident(Str);

impl Ident {
   pub fn new<'a>(ident: impl AsRef<[u8]> + 'a) -> Result<Self, Error<'static>> {
       Ok( Self::parse(ident.as_ref()).map_err(|e| Error::Message(e.to_string()))?.1 )
   } 
}

impl std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Ident({:?})", String::from_utf8_lossy(&self.0.content))
    }
}

impl Parseable for Ident {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        map(alphanumeric1, |content: Buf| Ident(Str{ content: content.to_owned() } ))(i)
        // let Some(tk) = i.get(0) else { return Err(Error::msg("Unexpected token")) };
    }
}


pub fn parse(i: Buf) -> R<Vec<Statement>> {
    many1(terminated(Statement::parse, eol))(i)
}

fn eol(i: Buf) -> R {
    alt((tag("\n"), tag(";"), eof))(i)
}

fn consume_til_eol(i: Buf) -> R {
    // assuming no tables or escaped newline
   terminated(take_while(|b| b != b'\n' && b != b';' ), eol)(i)
}

#[derive(From, Clone, Debug, PartialEq)]
pub struct Call {
    pub function: Expr,
    pub args: Vec<Expr>,
}

impl Parseable for Call {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        // TODO: lookahead paren
        let (i, function) = take_while1(|b| b != b'(')(i)?;
        let (i, args) = ws(paren_comma_list(Expr::parse))(i)?;
        let (_, function) = ws(Expr::parse)(function)?;
        Ok((i, Call {
            args,function
        }))
    }
}

#[derive(From, Clone, Debug, PartialEq)]
pub struct SubExpr(pub Expr);

impl Parseable for SubExpr {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        let (i, expr) = delimited(tag("("), Expr::parse, tag(")"))(i)?;
        Ok((i, SubExpr(expr)))
    }
}

#[derive(From, TryInto, Clone, Debug, PartialEq)]
pub enum Expr {
    Var(Ident),
    Literal(Literal),
    BinaryOp(Box<BinaryOp>),
    Call(Box<Call>),
    SubExpr(Box<SubExpr>)
}

impl Parseable for Expr {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        alt_from!(Box<BinaryOp>, Literal, Box<Call>, Ident, Box<SubExpr>)(i)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOp {
    pub lhs: Expr,
    pub rhs: Expr,
    pub op: BinOp,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
   Add,
   Sub,
   Mul,
   Div,
}

impl Parseable for BinaryOp {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        let operators = b"+-*/".as_slice();
        let (i, lhs) = ws(take_while1(|b| !operators.contains(&b) ))(i)?;
        let (i, op)  = one_of(operators)(i)?;
        let (_, lhs) = ws(Expr::parse)(lhs)?;
        let (i, rhs) = ws(Expr::parse)(i)?;
        let op = match op {
            '+' => BinOp::Add,
            '-' => BinOp::Sub,
            '*' => BinOp::Mul,
            '/' => BinOp::Div,
            _ => unreachable!()
        };
        Ok((i, BinaryOp{lhs, rhs, op}))
    } 
}

#[cfg(test)]
mod test {
    use crate::lhs::{Num, Function};

    use super::*;

    impl<const N: usize> PartialEq<&[u8; N]> for Ident {
        fn eq(&self, other: &&[u8; N]) -> bool {
             self.0.eq(&other.as_slice())
         } 
    }

    #[test]
    fn consume_eol_eof() {
        let (i, buf) = consume_til_eol(b"abc").unwrap();
        assert!(i.is_empty());
        assert_eq!(buf, b"abc");
    }
    #[test]
    fn consume_eol() {
        let (i, buf) = consume_til_eol(b"abc\nend").unwrap();
        assert_eq!(i, b"end");
        assert_eq!(buf, b"abc");
    }

    #[test]
    fn consume_eol_empty() {
        let (i, buf) = consume_til_eol(b"\nend").unwrap();
        assert_eq!(i, b"end");
        assert!(buf.is_empty());
    }

    #[test]
    fn binop_add() {
        let (i, op) = BinaryOp::parse(b"10 + a").unwrap();
        assert_eq!(op.lhs, Num::new(10.0).unwrap().into());
        assert_eq!(op.rhs, Ident::new(b"a").unwrap().into());
        assert!(i.is_empty());
    }

    // Call tests
    #[test]
    fn call_zero() {
        let (i, call) = Call::parse(b"name()").unwrap();
        assert_eq!(call.function, Ident::new(b"name").unwrap().into());
        assert!(call.args.is_empty());
    }

    #[test]
    fn call_one() {
        let (i, call) = Call::parse(b"print(10)").unwrap();
        assert_eq!(call.function, Ident::new(b"print").unwrap().into());
        assert_eq!(call.args[0], Num::new(10.0).unwrap().into());
    }

    #[test]
    fn expr_var() {
        let (i, expr) = Expr::parse(b"a").unwrap();
        assert_eq!(expr, Ident::new(b"a").unwrap().into());
    }

    #[test]
    fn expr_lit_num() {
        let (i, expr) = Expr::parse(b"10.0").unwrap();
        assert_eq!(expr, Num::new(10.0).unwrap().into());
        let (i, expr) = Expr::parse(b"10000").unwrap();
        assert_eq!(expr, Num::new(10000.0).unwrap().into());
    }

    #[test]
    fn expr_lit_str() {
        let (i, expr) = Expr::parse(b"'abc'").unwrap();
        assert_eq!(expr, Str{content: (*b"abc").into()}.into());
        let (i, expr) = Expr::parse(b"\"abc\"").unwrap();
        assert_eq!(expr, Str{content: (*b"abc").into()}.into());
    }

    // TODO: tests for tables, functions, calls and binops
    
    #[test]
    fn expr_subexpr_ident() {
        let (i, expr) = Expr::parse(b"(name)").unwrap();
        assert_eq!(expr, Expr::SubExpr(SubExpr(Ident::new(b"name").unwrap().into()).into()));
    }

}
