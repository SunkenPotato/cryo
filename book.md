# Cryo Specification

## Grammar
```ebnf
Ascii ::= /* ASCII definition */;
Colon ::= ":";
DoubleColon ::= "::";
Comma ::= ",";
LParen ::= "(";
RParen ::= ")";
LCurly ::= "{";
RCurly ::= "}";
Semi ::= ";";
Equal ::= "=";
AssertEq ::= "==";
AssetNotEq ::= "!=";
Minus ::= "-";
Plus ::= "+";
Star ::= "*";
Slash ::= "/";
Backslash ::= "\";
Percent ::= "%";
Bang ::= "!";
Underscore ::= "_";
Dot ::= ".";
Quote ::= """;
Digit ::= "0".."9";

LoopKw ::= "loop";
ContinueKw ::= "continue";
BreakKw ::= "break";
ReturnKw ::= "return";
LetKw ::= "let";
MutKw ::= "mut";
PublicKw ::= "pub";
StructKw ::= "struct";

Ident ::= ("a".."z" | "A..Z") ("a".."z" | "A".."Z" | Digit | Underscore)*;
Type ::= Path; // Preliminary until proper types are introduced.

IntegerLiteral ::= (Minus)* (Digit | (Digit Underscore Digit))+;
StringChar ::= Ascii | EscapeChar;
EscapedChar ::= Backslash EscapeChar;
EscapeChar ::= "0" | Quote | "t" | "n";
StringLiteral ::= Quote (StringChar)* Quote;

Literal ::= IntegerLiteral | StringLiteral;

Path ::= Ident (DoubleColon Path)?;

BlockExpr ::= LCurly (Stmt)* (Expr)? RCurly;
Loop ::= LoopKw BlockExpr;
Continue ::= ContinueKw;
Break ::= BreakKw (Expr)?;
Return ::= ReturnKw (Expr)?;

BinaryOperator ::= Plus | Minus | Star | Slash | AssertEq | AssertNotEq | Dot;

BinaryExpr ::= Expr BinaryOperator Expr;

BaseExpr ::= Literal | Path | BlockExpr | Loop | Continue | Break | Return;

Expr ::= BinaryExpr | BaseExpr;

TypedIdent ::= Ident Colon Type;

Stmt ::= Binding | ExprSemi | Item;
ExprSemi ::= Expr Semi;
Binding ::= LetKw (MutKw)? TypedIdent Equal ExprSemi;

Visibility ::= PublicKw?; // until other visibilities are introduced.
Item ::= Visibility (StructDef | EnumDef | FnDef | UnionDef);

StructDef ::= StructKw StructBody Semi;
StructBody ::= Ident (TupleBody | NamedBody)?;
TupleBody ::= LParen ((Type Comma)* (Type)?) RParen;
NamedBody ::= LParen ((TypedBinding Comma)* (TypedBinding)?),* RParen;