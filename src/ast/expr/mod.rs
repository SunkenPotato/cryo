use literal::Literal;

pub mod literal;

pub enum Expr {
    Literal(Literal),
}
