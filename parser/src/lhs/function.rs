use nom::{
    bytes::complete::tag,
    multi::{many0, separated_list0},
    Parser,
};

use crate::{code::Statement, utils::{ws, paren_comma_list, lnws}, *};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub args: Vec<Ident>,
    pub body: Vec<Statement>,
}

impl Parseable for Function {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Self> {
        let parse_body = separated_list0(eol, ws(Statement::parse));
        let (i, (args, body)) = delimited(
            tag("function"),
            lnws(paren_comma_list(Ident::parse))
                .and(lnws(parse_body)),
            lnws(tag("end"))
        )(i)?;
        Ok((i, Function { args, body }))
    }
}

#[cfg(test)]
mod test {
    use crate::lhs::Num;

    use super::*;

    #[test]
    fn func_zero_arg() {
        let (i, func) = Function::parse(b"function () end").unwrap();
        assert!(func.args.is_empty());
        assert!(func.body.is_empty());
    }

    #[test]
    fn func_one_arg() {
        let (i, func) = Function::parse(b"function(a) end").unwrap();
        assert_eq!(func.args, &[Ident::new(b"a").unwrap()]);
        assert!(func.body.is_empty());
    }

    #[test]
    fn func_multi_arg() {
        let (i, func) = Function::parse(b"function(a, b, c) end").unwrap();
        assert_eq!(func.args[0], Ident::new(b"a").unwrap());
        assert_eq!(func.args[1], Ident::new(b"b").unwrap());
        assert_eq!(func.args[2], Ident::new(b"c").unwrap());
        assert!(func.body.is_empty());
    }

    #[test]
    fn func_body_zero_arg() {
        let (i, func) = Function::parse(b"function() local a = 10; end").unwrap();
        assert!(func.args.is_empty());
        assert_eq!(func.body[0], Decl {
            is_local: true,name: Ident::new(b"a").unwrap(), value: Some(Num::new(10.0).unwrap().into())
        }.into());
    }
}
