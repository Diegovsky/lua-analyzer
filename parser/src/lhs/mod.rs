pub mod num;
pub mod table;
pub mod function;
pub mod string;

use derive_more::{From, TryInto};
pub use num::Num;
pub use table::Tbl;
pub use function::Function;
pub use string::Str;

use crate::{Parseable, alt_from, Expr};

macro_rules! impl_expr_conv {
    ($($ty:ty),*) => {
        $(
        impl std::convert::TryFrom<Expr> for $ty {
            type Error = &'static str;
            fn try_from(value: Expr) -> Result<Self, Self::Error> {
                match value {
                    Expr::Literal(val) => <$ty>::try_from(val),
                    _ => Err(concat!(stringify!($ty), " is a literal")),
                }
            }
        }

        impl std::convert::From<$ty> for Expr {
            fn from(value: $ty) -> Self {
                Expr::Literal(value.into())
            }
        }
        )*
    };
}

impl_expr_conv!(Str, Num, Tbl, Function);

#[derive(From, TryInto, Clone, Debug, PartialEq)]
pub enum Literal {
    Num(Num),
    Str(Str),
    Tbl(Tbl),
    Function(Function)
}

impl Parseable for Literal {
    fn parse<'a>(i: crate::Buf<'a>) -> crate::R<'a, Self> {
        alt_from!(Num, Str, Tbl, Function)(i)
    }
}
