# Interpreter-Project

### BNF History

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


// Grammar 5 in BNF:
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


// Grammar 6 in BNF: ( Current )
//
// STATEMENTS
// <PROG>  ::= <STA> <PROG> | <empty>
// <STA>   ::= <WHL> | <IF> | <ASN> | <PLT> | <PRT>
// <WHL>   ::= "while" <BE> "{" <PROG> "}"
// <IF>    ::= "if" <BE> "{" <PROG> "}"
// <ASN>   ::= "let" <SYM> "=" <BE> ";" | "func" <SYM> "=" <BE> ";"
// <PLT>   ::= "plot" <BE> ";"
// <PRT>   ::= "print" <BE> ";" | <BE> ";"              // Print top level expressions.
// <SYM>   ::= <alpha+>
//
// EXPRESSIONS
// <BE>    ::= <BU> <BEopt>
// <BEopt> ::= "and" <BU> <BEopt> | "or" <BU> <BEopt> | <empty>
// <BU>    ::= "!" <BU> | <BT>
// <BT>    ::= <E> <BTopt>
// <BTopt> ::= "==" <E> <BTopt> | "!=" <E> <BTopt> | ">" <E> <BTopt> | "<" <E> <BTopt> | <empty>
// <E>     ::= <T> <Eopt>
// <Eopt>  ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>     ::= <P> <Topt>
// <Topt>  ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>     ::= <U> <Popt>
// <Popt>  ::= "^" <U> <Popt> | <empty>
// <U>     ::= "-" <U> | <NM>
// <NM>    ::= <VL> | <SYM> | "(" <BE> ")"            // Where SYM is defined in symbol table.
// <VL>    ::= <IN> | <FL>                            // Separate literal value simplifies implementation.
// <IN>    ::= <digit+>
// <FL>    ::= <digit+> "." <digit+>

//// KEY
//// STATEMENTS
// PROG  -> Program
// STA   -> Statement
// ASN   -> Variable/Function Assignment
// PLT   -> Plot
// PTR   -> Print
// SYM   -> Symbol
//// EXPRESSIONS
// BE    -> Boolean Expression
// BEopt -> Boolean Expression/Optional
// BU    -> Boolean Unary
// BT    -> Boolean Term
// E     -> Expression
// Eopt  -> Expression/Optional
// T     -> Term
// Topt  -> Term/Optional
// P     -> Power
// Popt  -> Power/Optional
// U     -> Unary
// NM    -> Number
// VL    -> Value
// IN    -> Integer
// FL    -> Floating Point
```
