# Cryo Specification

general type syntax:
type ident, not ident: type


## Grammar
```ebnf
Ascii ::= /* ASCII definition */;
Colon ::= ":";
DoubleColon ::= "::";
Comma = ",";
LParen = "(";
RParen = ")";
LCurly = "{";
RCurly = "}";
Semi = ";";
Equal = "=";
AssertEq = "==";
AssetNotEq = "!=";
Minus = "-";
Plus = "+";
Star = "*";
Slash = "/";
Backslash = "\";
Percent = "%";
Bang = "!";
Underscore = "_";
Dot = ".";
Quote = """;
Digit = "0".."9";

LoopKw ::= "loop";
ContinueKw ::= "continue";
BreakKw ::= "break";
ReturnKw ::= "return";
StoreKw ::= "store";
MutKw ::= "mut";
PublicKw ::= "pub";
RecordKw ::= "record";
ProtocolKw ::= "protocol";
ImplKw ::= "impl";
OnKw ::= "on";
ConstKw ::= "const";
StaticKw ::= "static";

Ident ::= ("a".."z" | "A..Z") ("a".."z" | "A".."Z" | Digit | Underscore)*;
Type ::= Path; // Preliminary until proper types are introduced.

IntegerLiteral ::= (Minus)* (Digit | (Digit Underscore Digit))+; (* --20_30 *)
StringChar ::= Ascii | EscapeChar; (* \n, a *)
EscapedChar ::= Backslash EscapeChar; (* \n *)
EscapeChar ::= "0" | Quote | "t" | "n"; (* n *)
StringLiteral ::= Quote (StringChar)* Quote; (* "hello, world\n" *)

Literal ::= IntegerLiteral | StringLiteral;

Path ::= Ident (DoubleColon Path)?; (* super::num::sqrt *)

BlockExpr ::= LCurly (Stmt)* (Expr)? RCurly; (* { 5 } *)
Loop ::= LoopKw BlockExpr; (* loop {} *)
Continue ::= ContinueKw; (* continue *)
Break ::= BreakKw (Expr)?; (* break / break 5 *)
Return ::= ReturnKw (Expr)?; (* return 5 *)

BinaryOperator ::= Plus | Minus | Star | Slash | AssertEq | AssertNotEq | Dot;

BinaryExpr ::= Expr BinaryOperator Expr; (* 5 + 5 *)

BaseExpr ::= Literal | Path | BlockExpr | Loop | Continue | Break | Return;

Expr ::= BinaryExpr | BaseExpr;

TypedIdent ::= Ident Colon Type; (* a: i32 *)
OptionallyTypedIdent ::= Ident (Colon Type)?; (* a: i32 / a *)

Stmt ::= Binding | ExprSemi | Item;
ExprSemi ::= Expr Semi; (* 5; *)
Binding ::= StoreKw MutKw? OptionallyTypedIdent Equal ExprSemi; (* store x: i32 = 5; / store x = 5; *)

Visibility ::= PublicKw?; // until other visibilities are introduced.
Item ::= VisItem | ImplBlock;
VisItem ::= (Visibility (RecordDef | EnumDef | FnDef | UnionDef | ConstDef | StaticDef));

RecordDef ::= RecordKw Ident LParen (TypedBinding),* RParen Semi; (* record Point(x: f32, y: f32); *)
EnumDef ::= EnumKw Ident RCurly (RecordBody),* RCurly Semi; (* enum Error { IoError(error: Error), AppError(error: AppError) } *)
UnionDef ::= UnionKw LCurly (TypedBinding),* RCurly Semi; (* union Quaternion { vec4: Vec4, points: [f32; 4] } *)
FnDef ::= FnKw ConstKw? Type Ident LParen (TypedIdent),* RParen BlockExpr; (* const fn i32 add(lhs: i32, rhs: i32) { lhs + rhs } *)
ConstDef ::= ConstKw TypedIdent Equal ExprSemi; (* const GRID_SIZE: u32 = 32; *) (* currently not a thing *)
StaticDef ::= StaticKw TypedIdent Equal ExprSemi; (* static LOGGER: Logger = Logger::new(); *)

ImplBlock ::= ImplKw (ProtoImpl | TypeImpl) ImplBody; (* impl proto Send on i32 {} *)
TypeImpl ::= Type; (* i32 *)
ProtoImpl ::= ProtocolKw Type OnKw Type; (* proto Send on i32 *)

ImplKw ::= LCurly (ImplBodyMember)* RCurly; (* { fn i32 add(self, rhs: i32) { self + rhs } const MAX: i32 = 35536; } *)
ImplBodyMember ::= FnDef | ConstDef;
```
