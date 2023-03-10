use std::fmt::{Debug, Display};

use derive_more::From;
use nom::error::ParseError;

#[derive(Debug, From)]
pub enum Error<I> {
    Nom(nom::error::Error<I>),
    Message(String),
    Bunch(Box<Self>, Box<Self>),
    Other(Box<dyn std::error::Error>)
}

impl<I> Error<I> {
    pub fn msg(msg: impl Into<String>) -> Self {
        Self::Message(msg.into())
    } 
}

impl<I> std::fmt::Display for Error<I> where I: Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Nom(err) => Display::fmt(err, f),
            Error::Message(msg) => write!(f, "Parse Error: {:?}", msg),
            Error::Other(err) => write!(f, "Parse Error: {}", err),
            Error::Bunch(x, tail) => { write!(f, "Error: {}\n\tCaused by: {}", x, tail) }
        }
    }
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        Self::Nom(nom::error::Error::from_error_kind(input, kind))
    }

    fn append(input: I, kind: nom::error::ErrorKind, other: Self) -> Self {
        Self::Bunch(Box::new(Self::from_error_kind(input, kind)), Box::new(other))
    }
}
