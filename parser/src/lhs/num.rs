use nom::number::complete::recognize_float;

use crate::{Buf, Parseable, R};

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Num(f64);

impl Parseable for Num {
    fn parse(i: Buf<'_>) -> R<'_, Num> {
        let (i, num) = recognize_float(i)?;
        Ok((
            i,
            Num(std::str::from_utf8(num).unwrap().parse::<f64>().unwrap()),
        ))
    }
}



impl std::hash::Hash for Num {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.to_bits());
    }
}

impl Eq for Num {}

#[derive(Debug, Clone, Copy)]
pub struct NonUniqueNumberError;

impl Num {
    pub fn new(f: f64) -> Result<Self, NonUniqueNumberError> {
        if f.is_finite() {
            Ok(Self(f))
        } else {
            Err(NonUniqueNumberError)
        }
    }
    pub fn value(&self) -> f64 {
        self.0
    }
}

impl<'a> TryFrom<f64> for Num {
    type Error = NonUniqueNumberError;
    fn try_from(value: f64) -> Result<Self, Self::Error> {
        Self::new(value)
    }
}

impl<'a> Into<f64> for Num {
    fn into(self) -> f64 {
        self.0
    }
}

#[test]
fn num_float() {
    let (i, num) = Num::parse(b"10.0").unwrap();
    assert!(num.value() == 10.0);
    assert!(i.is_empty())
}

#[test]
fn num_int() {
    let (i, num) = Num::parse(b"10").unwrap();
    assert!(num.value() == 10.0);
    assert!(i.is_empty())
}
