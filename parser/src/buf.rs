use std::{iter::{Enumerate, Copied}, slice::Iter};

use bstr::BStr;
use derive_more::*;
use nom::InputIter;

#[derive(Debug, From, Deref, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Buf<'a>(&'a BStr);

impl<'a> AsRef<[u8]> for Buf<'a> {
    fn as_ref(&self) -> &[u8] {
        self.0
    }
}

impl<'a> PartialEq<[u8]> for Buf<'a>  {
   fn eq(&self, other: &[u8]) -> bool {
        self.as_ref().eq(other)
    } 
}

impl<'a> PartialEq<str> for Buf<'a>  {
   fn eq(&self, other: &str) -> bool {
        self.as_ref().eq(other.as_bytes())
    } 
}

impl<'a> nom::InputIter for Buf<'a> {
    type Item = u8;

    type Iter = Enumerate<<&'a [u8] as InputIter>::IterElem>;

    type IterElem = Copied<Iter<'a, u8>>;

    fn iter_indices(&self) -> Self::Iter {
        self.as_ref().iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.as_ref().iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
  where
    P: Fn(Self::Item) -> bool {
        self.as_ref().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.as_ref().slice_index(count)
    }
}
