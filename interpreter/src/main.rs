

use std::borrow::Cow;
use std::ops::Deref;
use std::{error::Error, collections::HashMap};

use derive_more::{From, AsRef};
use parser::{Ident, Expr};
use parser::code::Statement;
use parser::lhs::{Num, Literal};

type R<T = ()> = anyhow::Result<T>;

#[derive(Debug, Clone, Default)]
struct Str(ecow::EcoString);

impl Deref for Str {
    type Target = str;
    fn deref(&self) -> &str {
        self.0.as_ref() 
    }
}

impl Str {
    fn new() -> Self {
        Self::default()
    }
    fn from_bytes(bytes: &[u8]) -> Self {
        Self(ecow::EcoString::from(std::str::from_utf8(bytes).unwrap()))
    }
}

#[derive(Debug, Clone, AsRef, From)]
struct Function(parser::lhs::Function);

#[derive(Debug, Clone, From)]
enum Value {
    Num(f64),
    Str(Str),
    Nil,
}

impl Value {
    fn from_lit(lit: Literal) -> Self {
        match lit {
            Literal::Num(num) => Self::Num(num.value()),
            Literal::Str(st) => Str::from_bytes(st.bytes()).into(),
            Literal::Tbl(_) => todo!(),
            Literal::Function(_) => todo!(),
        }
    }

    fn get_number(&self) -> Option<f64> {
        match self {
            Value::Num(num) => Some(*num),
            Value::Str(st) => st.parse().ok(),
            _ => None
        }
    }
}

#[derive(Debug, Clone, Default)]
struct State {
    vars: HashMap<Ident, Value>,
    stack: Vec<Value>
}

impl State {
    fn new() -> Self { Default::default() }
    fn assign(&mut self, key: Ident, value: Value) {
        self.vars.insert(key, value);
    }
    fn get(&self, key: &Ident) -> &Value {
        static NIL: Value = Value::Nil;
        self.vars.get(key).unwrap_or(&NIL)
    }
    fn value_of(&self, expr: Expr) -> Cow<Value> {
        match expr {
            Expr::Var(ident) => Cow::Borrowed(self.get(&ident)),
            Expr::Literal(lit) => Cow::Owned(Value::from_lit(lit)),
        }
    }
}


fn execute(statement: Statement, state: &mut State) {
    match statement {
        Statement::Decl(decl)=> if let Some(value) = decl.value { state.assign(decl.name, state.value_of(value).into_owned()) },
        Statement::Return(_) => todo!(),
        Statement::BinaryOp(op) => {
            let lhs = state.value_of(op.lhs).get_number().unwrap();
            let rhs = state.value_of(op.rhs).get_number().unwrap();
            state.stack.push((lhs + rhs).into())
        },
    }
}

fn main() -> R {
    let code = include_bytes!("../../main.lua");
    let parsed = parser::parse(code)?;
    let mut state = State::new();
    for x in parsed.1 {
        execute(x, &mut state);
    }
    println!("{:?}", state.stack.last().unwrap());
    Ok(())
}
