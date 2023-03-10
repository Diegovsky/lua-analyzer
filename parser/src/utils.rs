
use nom::branch::Alt;
use nom::bytes::complete::tag;
use nom::character::complete::{space0, multispace0};
use nom::combinator::map;
use nom::error::ParseError;
use nom::multi::separated_list0;
use nom::sequence::delimited;
use nom::{AsChar, IResult, InputTakeAtPosition, Parser, UnspecializedInput, InputTake, InputIter, InputLength, Compare};

use crate::{R, Buf, Parseable};

pub fn wrap<'a, F: 'a, W: 'a, I: 'a, O, E: ParseError<I>>(
    inner: F,
    sep: W,
) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    W: Parser<I, I, E> + Clone,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    delimited(sep.clone(), inner, sep)
}

pub fn paren_comma_list<'a, F: 'a, I: 'a, O: 'a, E: ParseError<I> + 'a>(parser: F) -> impl (FnMut(I) -> Result<(I, Vec<O>), nom::Err<E>>) + 'a
where F: Parser<I, O, E>,
      I: InputTakeAtPosition + InputTake + InputIter + InputLength + Compare<&'a str> + Clone,
      <I as InputTakeAtPosition>::Item: AsChar + Clone,
      <I as InputIter>::Item: AsChar,
{
    delimited(
        tag("("),
        separated_list0(ws(tag(",")), parser),
        tag(")"),
        )
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, F: 'a, I: 'a, O, E: ParseError<I>>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    wrap(inner, space0)
}

pub fn lnws<'a, F: 'a, I: 'a, O, E: ParseError<I>>(inner: F) -> impl FnMut(I) -> IResult<I, O, E>
where
    F: Parser<I, O, E>,
    I: InputTakeAtPosition,
    <I as InputTakeAtPosition>::Item: AsChar + Clone,
{
    wrap(inner, multispace0)
}

pub(crate) fn from_parseable<'a, T, I>() -> impl FnMut(Buf<'a>) -> R<I> where T: Parseable + Into<I>, I: 'a {
    move |i| {
        /* let text = format!("{}::parse", std::any::type_name::<T>());
        println!("Calling {}", text); */
        map(T::parse, T::into)(i)//.map_err(|e| {println!("{} Failed", text); e})
    }
}

#[macro_export]
macro_rules! alt_from {
    ($($t:ty),*) => {
        ::nom::branch::alt(($(crate::utils::from_parseable::<$t, _>()),*)) };
}

#[cfg(test)]
mod test {
    use nom::bytes::complete::tag;

    use super::ws;

    #[test]
    fn strip_sides() {
        let r = tag::<_, _, ()>("a");
        let (_, cap) = ws(r)("  a  ").unwrap();
        assert_eq!(cap, "a")
    }
    #[test]
    fn no_strip() {
        let r = tag::<_, _, ()>("a");
        let (_, cap) = ws(r)("a").unwrap();
        assert_eq!(cap, "a")
    }
    #[test]
    fn strip_inner_fail() {
        let r = tag::<_, _, ()>("a");
        let (_, cap) = ws(r)("a").unwrap();
        assert_ne!(cap, "b")
    }
}
