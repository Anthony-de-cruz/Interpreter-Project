# Interpreter-Project

### BNF

```
// Grammar 0 in BNF: ( Original )
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")"


// Grammar 1 in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <NR> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")"


// Grammar 2 in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <NR> <Topt> | "/" <NR> <Topt> | "%" <NR> <Topt> | <empty>
// <P>        ::= <NR> <Popt>                 
// <Popt>     ::= "^" <NR> <Popt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")"


// Grammar 3 in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <U> <Popt>
// <Popt>     ::= "^" <U> <Popt> | <empty>
// <U>        ::= "-" <U> | <NR>
// <NR>       ::= "Num" <value> | "(" <E> ")"


// Grammar 4 in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <U> <Popt>
// <Popt>     ::= "^" <U> <Popt> | <empty>
// <U>        ::= "-" <U> | <NM>
// <NM>       ::= <IN> | <FL> | "(" <E> ")"
// <IN>       ::= <digit>+
// <FL>       ::= <digit>+ "." <digit>+


// Grammar 5 in BNF: ( Current )
//
// STATEMENTS
// <STA>    ::= ( <ASN> | <PLT> | <PTR> ) ";"
//            | <STA> <STA>
//            | <empty>
// <ASN>    ::= "let" <SYM> "=" <NM>
// <PLT>    ::= "plot" <NM> <PLTopt>
// <PLTopt> ::= "," <NM> <PLopt> | <empty>
// <PRT>    ::= "print" <NM>
// <SYM>    ::= <alpha+>
//
// EXPRESSIONS
// <E>    ::= <T> <Eopt>
// <Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>    ::= <P> <Topt>
// <Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>    ::= <U> <Popt>
// <Popt> ::= "^" <U> <Popt> | <empty>
// <U>    ::= "-" <U> | <NM>
// <NM>   ::= <IN> | <FL> | <SYM> | "(" <E> ")"
// <IN>   ::= <digit+>
// <FL>   ::= <digit+> "." <digit+>


// STATEMENTS
// STA    -> Statement
// ASN    -> Assignment
// PLT    -> Plot
// PLTopt -> Plot
// SYM    -> Symbol
//
// EXPRESSIONS
// E      -> Expression
// Eopt   -> Expression/Optional
// T      -> Term
// Topt   -> Term/Optional
// P      -> Power
// Popt   -> Power/Optional
// U      -> Unary
// NM     -> Number
// IN     -> Integer
// FL     -> Floating Point
// CX     -> Complex Number
```
