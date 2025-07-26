//! Atoms.
//!
//! Atoms are tokens which are constant and only may be parsed from one specific input.

use crate::atom;

atom!(
    /// The left curly parenthesis.
    LCurly, "{"
);
atom!(
    /// The right curly parenthesis.
    RCurly, "}"
);
atom!(
    /// The left parenthesis.
    LParen, "("
);
atom!(
    /// The right parenthesis.
    RParen, ")"
);
atom!(
    /// A semicolon.
    Semi, ";"
);
atom!(
    /// The assign token.
    Assign, "="
);
atom!(
    /// A comma.
    Comma, ","
);
atom!(
    /// A colon.
    Colon, ":"
);

atom!(
    /// A dot.
    Dot, "."
);

atom! {
    /// A keyword.
    ///
    /// Keywords are reserved tokens used to indicate behaviour or declare constructs.
    pub enum Keyword {
        /// The `let` keyword. Used to declare bindings.
        #("let")
        Let,
        /// The `mut` keyword. Used to declare mutability on bindings.
        #("mut")
        Mut,
        /// The `if` keyword. Used to indicate a conditional block.
        #("if")
        If,
        /// The `else` keyword. Used to define the operation to be executed should an if-statement not be valid.
        #("else")
        Else,
        /// The `struct` keyword.
        #("struct")
        Struct,
        /// The `fun` keyword. Used to define functions.
        #("fun")
        Fun
    }
}

atom! {
    /// Operators for binary expressions.
    pub enum Operator {
        /// The addition operator (`+`).
        #("+")
        Add,
        /// The subtraction operator (`-`).
        #("-")
        Sub,
        /// The multiplication operator (`*`).
        #("*")
        Mul,
        /// The division operator (`/`).
        #("/")
        Div,
        /// The remainder operator (`%`).
        #("%")
        Rem,
        /// The equality operator (`==`).
        #("==")
        Eq,
        /// The inequality operator (`!=`).
        #("!=")
        NotEq
    }
}
