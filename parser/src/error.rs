use std::{fmt::{Debug, Display}, error::Error as StdError};

use derive_more::From;
use nom::error::ParseError;

use crate::Buf;

#[derive(From)]
pub enum Error<'a> {
    Nom(nom::error::Error<Buf<'a>>),
    Message(String),
    // Chained{ current: Box<Self>, caused_by: Box<Self> }
}

impl Error<'_>{
    pub fn msg(msg: impl Into<String>) -> Self {
        Self::Message(msg.into())
    }
}

impl<'a> Error<'a>{
    pub fn replace_input<'b: 'a>(self, new_input: Buf<'b>) -> Error<'b> where Self: 'a {
        match self {
            Error::Nom(nom) => Error::Nom(nom::error::Error::from_error_kind(new_input, nom.code)),
            Error::Message(e) => Error::Message(e),
            // Error::Chained { current, caused_by } => Error::Chained { current: Box::new(current.replace_input(new_input)), caused_by }
        }
    }
}

impl std::fmt::Debug for Error<'_>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Nom(err) => {
                let fmted =
                if true {
                    let input: &&[u8] = unsafe { std::mem::transmute(&err.input) };
                    format!("\"{}\"", std::str::from_utf8(input).unwrap())
                } else {
                   format!("{:?}", err.input)
                };
                let err = nom::error::Error::from_error_kind(fmted.as_str(), err.code);
                Display::fmt(&err, f)
            },
            Error::Message(msg) => write!(f, "Parse Error: {:?}", msg),
            // Error::Chained{ current, caused_by } => { write!(f, "Error: {}\n\tCaused by: {}", current, caused_by) }
        }
    }
}

impl std::fmt::Display for Error<'_>{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl StdError for Error<'_>{ }

impl<'a> ParseError<Buf<'a>> for Error<'a>{
    fn from_error_kind(input: Buf<'a>, kind: nom::error::ErrorKind) -> Self {
        Self::Nom(nom::error::Error::from_error_kind(input, kind))
    }

    fn append(input: Buf<'_>, kind: nom::error::ErrorKind, other: Self) -> Self {
        other
        // Self::Chained{ current: Box::new(Self::from_error_kind(input, kind)), caused_by: Box::new(other) }
    }
}
