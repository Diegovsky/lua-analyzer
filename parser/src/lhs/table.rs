use derive_more::From;
use nom::Parser;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, multispace0, one_of};
use nom::combinator::{map, opt};
use nom::multi::{many0, separated_list0};
use nom::sequence::{delimited, terminated};

use crate::{Buf, Expr, Parseable, Ident, R};

use crate::utils::{self, ws, lnws};

use super::Str;

#[derive(Clone, Debug, PartialEq)]
pub struct Tbl {
    dict: Vec<(Expr, Expr)>,
    list: Vec<Expr>,
}

#[derive(From)]
enum TableItem {
    Dict(Expr, Expr),
    List(Expr),
}

impl Parseable for Tbl {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Tbl> {
        let dictdecl = |i| {
            let dictkey = delimited(tag("["), ws(Expr::parse), tag("]"));
            let ident_keystring = Ident::parse.map(|ident| ident.0.into());
            let (i, key) = ws(alt((dictkey, ident_keystring)))(i)?;
            let (i, _) = ws(tag("="))(i)?;
            let (i, expr) = ws(Expr::parse)(i)?;
            Ok((i, TableItem::Dict(key, expr)))
        };
        let listdecl = Expr::parse.map(TableItem::List);
        let decl = alt((dictdecl, listdecl));
        let (i, body) = ws(delimited(
            char('{'),
            many0(terminated(lnws(decl), ws(opt(one_of(";,"))))),
            char('}'),
        ))(i)?;

        let list: Vec<Expr> = body
            .iter()
            .filter_map(|val| {
                if let TableItem::List(e) = val {
                    Some(e)
                } else {
                    None
                }
            })
            .cloned()
            .collect();
        let dict = body
            .into_iter()
            .filter_map(|val| {
                if let TableItem::Dict(key, e) = val {
                    Some((key, e))
                } else {
                    None
                }
            })
            .collect();
        Ok((i, Tbl { list, dict }))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lhs::{Num, Literal, Str};

    impl From<&str> for Expr {
        fn from(value: &str) -> Self {
            Expr::Literal(Literal::Str(Str{content: value.as_bytes().to_vec()}))
        }
    }

    #[test]
    fn empty_table() {
        Tbl::parse(b"{}").unwrap();
    }

    #[test]
    fn list_table() {
        let (_, tbl) = Tbl::parse(b"{ 1, 2, 3 }").unwrap();
        let list: Vec<f64> = tbl
            .list
            .into_iter()
            .map(|expr| Num::try_from(expr).unwrap().into())
            .collect();
        assert_eq!(list[0], 1.0);
        assert_eq!(list[1], 2.0);
        assert_eq!(list[2], 3.0);
        assert!(tbl.dict.is_empty())
    }

    #[test]
    fn list_table_trailing() {
        let (_, tbl) = Tbl::parse(b"{ 1, 2, 3, }").unwrap();
        let list: Vec<f64> = tbl
            .list
            .into_iter()
            .map(|expr| Num::try_from(expr).unwrap().into())
            .collect();
        assert_eq!(list[0], 1.0);
        assert_eq!(list[1], 2.0);
        assert_eq!(list[2], 3.0);
        assert!(tbl.dict.is_empty())
    }

    #[test]
    fn list_table_trailing_mixed() {
        let (_, tbl) = Tbl::parse(b"{ 1; 2, 3; }").unwrap();
        let list: Vec<f64> = tbl
            .list
            .into_iter()
            .map(|expr| Num::try_from(expr).unwrap().into())
            .collect();
        assert_eq!(list[0], 1.0);
        assert_eq!(list[1], 2.0);
        assert_eq!(list[2], 3.0);
        assert!(tbl.dict.is_empty())
    }

    #[test]
    fn list_table_trailing_semicolon() {
        let (_, tbl) = Tbl::parse(b"{ 1; 2; 3; }").unwrap();
        let list: Vec<f64> = tbl
            .list
            .into_iter()
            .map(|expr| Num::try_from(expr).unwrap().into())
            .collect();
        assert_eq!(list[0], 1.0);
        assert_eq!(list[1], 2.0);
        assert_eq!(list[2], 3.0);
        assert!(tbl.dict.is_empty())
    }

    #[test]
    fn list_table_single() {
        let (_, tbl) = Tbl::parse(b"{ 1 }").unwrap();
        let list: Vec<f64> = tbl
            .list
            .into_iter()
            .map(|expr| Num::try_from(expr).unwrap())
            .map(|num| num.into() )
            .collect();
        assert_eq!(list[0], 1.0);
        assert!(tbl.dict.is_empty())
    }

    #[test]
    fn dict_table() {
        let (_, tbl) = Tbl::parse(b"{ a = 1 }").unwrap();
        let key = Expr::from("a");
        let value = tbl.dict.iter().find(|a| a.0 == key ).map(|a| &a.1).unwrap();
        let value = Num::try_from(value.clone()).unwrap();
        assert_eq!(<Num as Into<f64>>::into(value), 1.0);
        assert!(tbl.list.is_empty());
    }
}
