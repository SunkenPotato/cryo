use crate::atom;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Semi;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Assign;

atom!(Semi, ";");
atom!(Assign, "=");
atom! {
    pub enum Keyword {
        #("let")
        Let,
        #("mut")
        Mut
    }
}

atom! {
    pub enum Operator {
        #("+")
        Add,
        #("-")
        Sub,
        #("*")
        Mul,
        #("/")
        Div,
        #("%")
        Rem
    }
}
