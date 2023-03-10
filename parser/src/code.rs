use derive_more::{From, TryInto};
use nom::{bytes::complete::tag, character::complete::one_of, combinator::opt, sequence::preceded, branch::alt};

use crate::{
    alt_from, consume_til_eol,
    utils::{self, ws},
    Buf, Call, Expr, Ident, Parseable, R, lhs::Function,
};

#[derive(From, TryInto, Clone, Debug, PartialEq)]
pub enum Statement {
    Call(Call),
    Decl(Decl),
    Return(Return),
}

impl Parseable for Statement {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        alt_from!(Return, Decl, Call)(i)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<Expr>,
}

impl Parseable for Return {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        let (i, _) = ws(tag("return"))(i)?;
        let (i, text) = ws(consume_til_eol)(i)?;
        let (i, value) = if text.is_empty() {
            (i, None)
        } else {
            Expr::parse(text).map(|(i, expr)| (i, Some(expr)))?
        };
        Ok((i, Return { value }))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    pub is_local: bool,
    pub name: Ident,
    pub value: Option<Expr>,
}

impl Parseable for Decl {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Decl> {
        let (i, is_local) = opt(utils::ws(tag("local")))(i)?;
        let normal_decl = |i| {
            let (i, name) = utils::ws(Ident::parse)(i)?;
            let (i, _) = utils::ws(tag("="))(i)?;
            let (i, expr) = consume_til_eol(i)?;
            let (_, expr) = utils::ws(Expr::parse)(expr)?;
            Ok((i, (name, expr)))
        };
        fn func_decl<'a>(i: Buf<'a>) -> R<'a, (Ident, Expr)> {
            let (i, name) = preceded(tag("function"), ws(Ident::parse))(i)?;
            let rest: Vec<u8> = [b"function".as_slice(), i].into_iter().flatten().copied().collect();
            let (afterfunc, func) = Function::parse(&rest).map_err(|err| err.map_input(|_| i))?;
            let bytes_consumed = afterfunc.as_ptr() as usize - rest.as_ptr() as usize;
            let i = &i[bytes_consumed - b"function".len()..];
            Ok((i, (name, func.into())))
        }
        let (i, (name, expr)) = alt((func_decl, normal_decl))(i)?;
        Ok((
            i,
            Decl {
                name,
                is_local: is_local.is_some(),
                value: Some(expr),
            },
        ))
    }
}

#[cfg(test)]
mod test {
    use crate::lhs::{Literal, Num};

    use super::*;

    #[test]
    fn return_empty() {
        let ret = Return::parse(b"return").unwrap().1;
        assert!(ret.value.is_none());
    }

    #[test]
    fn return_empty_terminated() {
        let ret = Return::parse(b"return ;").unwrap().1;
        assert!(ret.value.is_none());
    }

    #[test]
    fn return_value() {
        let ret = Return::parse(b"return 10").unwrap().1;
        assert_eq!(ret.value, Some(Num::new(10.0).unwrap().into()));
    }

    #[test]
    fn local_decl() {
        let (buf, decl) = Decl::parse(b"local a = 10;").unwrap();
        assert!(buf.len() == 0);
        assert!(decl.is_local);
        assert!(decl.name == b"a");
    }

    #[test]
    fn local_decl_func() {
        let (buf, decl) = Decl::parse(b"local function a() end").unwrap();
        assert_eq!(buf.len(), 0);
        assert!(decl.is_local);
        assert_eq!(decl.name, b"a");
        assert!(matches!(decl.value, Some(Expr::Literal(Literal::Function(_)))));
    }

    #[test]
    fn local_decl_func_many() {
        let (buf, decl) = Decl::parse(b"local function a(b, c) end").unwrap();
        assert_eq!(buf.len(), 0);
        assert!(decl.is_local);
        assert_eq!(decl.name, b"a");
        assert!(matches!(decl.value, Some(Expr::Literal(Literal::Function(_)))));
    }

    #[test]
    fn global_decl() {
        let (buf, decl) = Decl::parse(b"a = 10;").unwrap();
        assert!(buf.len() == 0);
        assert!(!decl.is_local);
        assert!(decl.name == b"a");
    }
    #[test]
    fn decl_end_line() {
        let (buf, _decl) = Decl::parse(b"a = 10;\n").unwrap();
        assert!(buf == b"\n");
    }
    #[test]
    fn decl_end_simicolon() {
        let (buf, _decl) = Decl::parse(b"a = 10\n;").unwrap();
        assert!(buf == b";");
    }

    #[test]
    fn decl_end_eof() {
        let (buf, _decl) = Decl::parse(b"a = 10").unwrap();
        assert!(buf.len() == 0);
    }
    #[test]
    fn decl_local_ident_fail() {
        let r = Decl::parse(b"local = 10");
        r.unwrap_err();
    }
    #[test]
    fn decl_missing_ident() {
        let r = Decl::parse(b" = 10");
        r.unwrap_err();
    }
}
