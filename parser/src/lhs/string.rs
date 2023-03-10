use nom::{sequence::delimited, bytes::complete::tag, combinator::verify, branch::alt, InputIter};

use crate::{Parseable, Buf, R, OwnedBuf, BufElement};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Str {
    //delimiter: Buf<'a>,
    pub(crate) content: OwnedBuf
}

impl Str {
    pub fn bytes(&self) -> &[u8] {
        &self.content.as_bytes()
    }
}

impl PartialEq<&str> for Str {
    fn eq(&self, other: &&str) -> bool {
        self.content.eq(*other)
    }
}

impl PartialEq<&[u8]> for Str {
    fn eq(&self, other: &&[u8]) -> bool {
        self.bytes().eq(*other)
    }
}

impl Parseable for Str {
    fn parse<'a>(i: Buf<'a>) -> R<'a, Str> {
        fn take_until_escaped<'a>(seq: BufElement) -> impl FnMut(Buf<'a>) -> R<'a> {
            let mut is_escaped = false;
            move |i| {
                let index = i
                    .iter_elements()
                    .enumerate()
                    .find_map(|(i, x)| {
                        if x == seq && !is_escaped {
                            return Some(i);
                        }
                        is_escaped = x == '\\';
                        None
                    })
                    .ok_or(nom::Err::Error(nom::error::make_error(
                        i,
                        nom::error::ErrorKind::TakeUntil,
                    )))?;
                Ok((&i[index..], &i[..index]))
            }
        }
        fn parse_str<'a>(del: &'a str) -> impl FnMut(Buf<'a>) -> R<'a, Str> {
            move |i| {
                let (i, content) = delimited(
                    tag(del),
                    verify(take_until_escaped(del.chars().nth(0).unwrap()), |slc: &str| {
                        !slc.contains('\n')
                    }),
                    tag(del),
                )(i)?;
                Ok((
                    i,
                    Str {
                        //delimiter: del.as_bytes(),
                        content: content.to_string(),
                    },
                ))
            }
        }
        let parse_squote = parse_str("'");
        let parse_dquote = parse_str("\"");
        alt((parse_dquote, parse_squote))(i)
    }
}
#[cfg(test)]
impl<const N: usize> PartialEq<&[u8; N]> for Str {
    fn eq(&self, other: &&[u8; N]) -> bool {
        self.eq(&other.as_slice())
    } 
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn string_squote() {
        let (i, st) = Str::parse("'hello'").unwrap();
        assert_eq!(st, "hello");
        assert!(i.is_empty())
    }
    #[test]
    fn string_dquote() {
        let (i, st) = Str::parse("\"hello\"").unwrap();
        assert!(st == "hello");
        assert!(i.is_empty())
    }

    #[test]
    fn string_squote_esc() {
        let original = "'hello\\''";
        let (i, st) = Str::parse(original).unwrap();
        assert_eq!(st, "hello\\'");
        assert!(i.is_empty())
    }
    #[test]
    fn string_dquote_esc() {
        let original = r#""hello\"""#;
        let (i, st) = Str::parse(original).unwrap();
        assert_eq!(st, "hello\\\"");
        assert!(i.is_empty())
    }

    #[test]
    fn string_squote_endline_fail() {
        let original = "'hello\n\n'";
        let r = Str::parse(original);
        r.unwrap_err();
    }

    #[test]
    fn string_dquote_endline_fail() {
        let original = r#""hello

            ""#;
        let r = Str::parse(original);
        r.unwrap_err();
    }

    #[test]
    fn string_squote_endline() {
        let original = "'hello\\n\\n'";
        let r = Str::parse(original);
        r.unwrap();
    }

    #[test]
    fn string_dquote_endline() {
        let original = r#""hello\n\n""#;
        let r = Str::parse(original);
        r.unwrap();
    }
}

