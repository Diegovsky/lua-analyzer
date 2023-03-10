use std::borrow::Cow;
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::{collections::HashMap, error::Error};

use derive_more::{AsRef, From};
use parser::code::Statement;
use parser::lhs::{Literal, Num};
use parser::{Expr, Ident};

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
struct LuaFunction(parser::lhs::Function);

impl LuaFunction {
    fn call(&self, state: &State, args: &[Value]) -> R<Vec<Value>> {
        let mut stack = State::new_with_upvalues(state.clone());
        // assign arguments' values
        for (ident, value) in self.0.args.iter().zip(args.iter()) {
            stack.assign(ident.clone(), value.clone());
        }
        Ok(stack.execute(&self.0.body))
    }
}

#[derive(Clone)]
struct NativeFunction {
    cb: Rc<RefCell<dyn FnMut(&State, &[Value]) -> R<Vec<Value>>>>,
}

impl NativeFunction {
    pub fn new(cb: impl FnMut(&State, &[Value]) -> R<Vec<Value>> + 'static) -> Self {
        Self {
            cb: Rc::new(RefCell::new(cb)),
        }
    }
    pub fn call(&self, state: &State, args: &[Value]) -> R<Vec<Value>> {
        self.cb.borrow_mut()(state, args)
    }
}

#[derive(Debug, Clone, From)]
enum Function {
    NativeFunction(NativeFunction),
    LuaFunction(LuaFunction),
}

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NativeFunction({:p})", self.cb)
    }
}

#[derive(Debug, Clone, From, Default)]
enum Value {
    Num(f64),
    Str(Str),
    #[from(ignore)]
    Function(Function),
    #[default]
    Nil,
}

impl<T> From<T> for Value where T: Into<Function> {
    fn from(value: T) -> Self {
        Value::Function(value.into())
    }
}

impl Value {
    fn from_lit(lit: Literal) -> Self {
        match lit {
            Literal::Num(num) => Self::Num(num.value()),
            Literal::Str(st) => Str::from_bytes(st.bytes()).into(),
            Literal::Tbl(_) => todo!(),
            Literal::Function(func) => Self::Function(LuaFunction::from(func).into()),
        }
    }

    fn or_else(self, cb: impl FnOnce() -> Self) -> Self {
        if let Self::Nil = self {
            cb()
        } else {
            self
        }
    }

    fn or(self, value: Self) -> Self {
        self.or_else(move || value)
    }

    fn get_number(&self) -> Option<f64> {
        match self {
            Value::Num(num) => Some(*num),
            Value::Str(st) => st.parse().ok(),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Default)]
struct State {
    vars: HashMap<Ident, Value>,
    upvalues: Option<Box<State>>,
}

impl State {
    fn new() -> Self {
       Default::default()
    }
    fn new_with_upvalues(upvalues: State) -> Self {
        Self {
            upvalues: Some(Box::new(upvalues)), ..Default::default()
        }
    }
    fn assign(&mut self, key: Ident, value: Value) {
        self.vars.insert(key, value);
    }
    fn set(&mut self, key: impl AsRef<[u8]>, value: impl Into<Value>) {
        self.vars.insert(Ident::new(key.as_ref()).unwrap(), value.into());
    }
    fn get(&self, key: &Ident) -> &Value {
        self.vars.get(key).unwrap_or_else(|| {
            self.upvalues
                .as_ref()
                .map(|upvalues| upvalues.get(&key))
                .unwrap_or(&Value::Nil)
        })
    }
    fn execute_call(&self, call: &parser::Call) -> R<Vec<Value>> {
        if let Value::Function(func) = self.value_of(&call.function).as_ref() {
            let args: Vec<Value> = call.args.iter().map(|arg| self.value_of(arg).into_owned()).collect();
            let ret = match func {
                Function::LuaFunction(luafn) => luafn.call(self, &args),
                Function::NativeFunction(natfn) => natfn.call(self, &args)
            };
            Ok(ret.unwrap())
        } else {todo!()}
    }
    fn value_of(&self, expr: &Expr) -> Cow<Value> {
        match expr {
            Expr::Var(ident) => Cow::Borrowed(self.get(&ident)),
            Expr::Literal(lit) => Cow::Owned(Value::from_lit(lit.clone())),
            Expr::BinaryOp(op) => {
                let lhs = self.value_of(&op.lhs).get_number().unwrap();
                let rhs = self.value_of(&op.rhs).get_number().unwrap();
                return Cow::Owned((lhs + rhs).into());
            }
            Expr::Call(call) => Cow::Owned(self.execute_call(&call).unwrap().remove(0)),
            Expr::SubExpr(sub) => self.value_of(&sub.0),
        }
    }
    fn value_of_option(&self, expr: Option<&Expr>) -> Cow<Value> {
        match expr {
            Some(expr) => self.value_of(expr),
            None => Cow::Owned(Value::Nil),
        }
    }
    fn execute<'a>(&mut self, statements: impl IntoIterator<Item = &'a Statement>) -> Vec<Value> {
        for stmt in statements {
            match stmt {
                Statement::Decl(decl) => {
                    if let Some(value) = &decl.value {
                        self.assign(decl.name.clone(), self.value_of(value).into_owned())
                    }
                }
                Statement::Return(ret) => return vec![self.value_of_option(ret.value.as_ref()).into_owned()],
                Statement::Call(call) => { self.execute_call(call).unwrap(); },
            }
        }
        vec![Value::Nil]
    }
}


fn main() -> R {
    let code = include_bytes!("../../main.lua");
    let parsed = parser::parse(code)?;
    println!("REST: {:?}", parser::parse(parsed.0));
    let mut state = State::new();
    state.set("print", NativeFunction::new(|_state, args| {
        println!("{:?}", args[0]);
        Ok(vec![ Value::Nil ])
    }));
    let ret = state.execute(&parsed.1);
    Ok(())
}
