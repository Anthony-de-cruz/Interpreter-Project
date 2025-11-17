// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report
//
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

// Updated Grammar 6 in BNF: ( TBC )
//
// STATEMENTS
// <STA>    ::= ( <ASN> | <DEF> | <CAL> ) ";"
//            | <STA> <STA>
//            | <empty>
// <ASN>    ::= "let" <SYM> "=" <E>                                // Where not reserved variable.
//            | <SYM> "++"                                         // Where var exists in symbol table.
//            | <SYM> "--"                                         // Where var exists in symbol table.
// <DEF>    ::= "def" <SYM> "(" <DEFopt> ")" "{" <STA> "}"         // Where not reserved.
// <DEFopt> ::= <SYM> <DEFopt> | "," <DEFopt> | <empty>            // Where not reserved.
// <CLL>    ::= <SYM> "(" <CLLopt> ")"                             // Where def exists in symbol table.
// <CLLopt> ::= <NM> <CLLopt> | "," <CLLopt> | <empty>
// <SYM>    ::= <alpha+>
//
// EXPRESSIONS
// <E>      ::= <T> <Eopt>
// <Eopt>   ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>      ::= <P> <Topt>
// <Topt>   ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>      ::= <U> <Popt>
// <Popt>   ::= "^" <U> <Popt> | <empty>
// <U>      ::= "-" <U> | <NM>
// <NM>     ::= <IN>
//            | <FL>
//            | <SYM>                                              // Where var exists in symbol table. 
//            | <CLL>                                              // Where def exists in symbol table. 
//            | "(" <E> ")"                  
// <IN>     ::= <digit+>
// <FL>     ::= <digit+> "." <digit+>

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
// CX     -> Complex Number           // TBC

module Interpreter

open System

// AST nodes.
type E    = T * Eopt
and  Eopt = Add of T * Eopt | Sub of T * Eopt | EmptyE
and  T    = P * Topt
and  Topt = Mul of P * Topt | Div of P * Topt | Mod of P * Topt | EmptyT
and  P    = U * Popt
and  Popt = Pwr of U * Popt | EmptyP
and  U    = Neg of U | NM of NM 
and  NM   = VL of VL | E of E
and  VL   = Int of int | Flt of float

// Lexer tokens.
type Token = AddT | SubT | MulT | DivT | ModT | PwrT | EqlT | LparT | RparT | CmaT | SemiT 
           | IntT of int | FltT of float | SymT of string

// Reserved symbols.
// Todo - Consider adding a keyword type to the terminal union.
let assignSymbol = "let"
let printSymbol = "print"
let plotSymbol = "plot"
let reservedSymbols = Set.ofList [ assignSymbol; printSymbol; plotSymbol ]

// Utility functions for string / character handling
let str2lst s = [for c in s -> c]
let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c
let isAlpha c = Char.IsLetter c
let intVal (c:char) = int (int c - int '0')

exception SyntaxError of string
exception RuntimeError of string

// Scans digits to form an int, keeping track of lexer position
let rec scInt(iStr, iVal, pos) = 
    match iStr with
    | c :: tail when isDigit c -> scInt(tail, 10*iVal+(intVal c), pos + 1)
    | _ -> (iStr, iVal, pos)

// Scans digits and decimal points to form numbers, keeping track of lexer position
let scNum(iStr, iVal, startingPos) =
    let iStr, whole, wholePos = scInt(iStr, iVal, startingPos)
    match iStr with
    | '.'::c::tail when isDigit c -> 
        let rest, fractional, fractionalPos = scInt(tail, intVal c, wholePos + 2)
        // Find the positional difference between '.' and our fractional scan.
        let fractional = (float fractional / 10.0 ** (float fractionalPos - (float wholePos + 1.0)))
        (rest, FltT (float whole + fractional), fractionalPos)
    | '.' :: _ -> $"Missing fractional digit @ position {wholePos}" |> SyntaxError |> raise
    | _ -> (iStr, IntT whole, wholePos)

// Scan for a string of alphabet chars.
let rec scAlpha(iStr: char list, iSymbol: string, pos: int) =
    match iStr with
    | c :: tail when isAlpha c -> scAlpha(tail, iSymbol + string c, pos + 1)
    | _ -> (iStr, iSymbol, pos)

// Throw away chars until newline.
let rec scComment(iStr: char list) =
    match iStr with
    | [] -> []
    | '\n'::tail -> tail
    | _ ::tail -> scComment(tail)

let lexer input =
    let rec scan input line pos =
        match input with
        | [] -> []
        | '+'::tail -> AddT  :: scan tail line (pos + 1)
        | '^'::tail -> PwrT  :: scan tail line (pos + 1)
        | '-'::tail -> SubT  :: scan tail line (pos + 1)
        | '*'::tail -> MulT  :: scan tail line (pos + 1)
        | '/'::tail -> DivT  :: scan tail line (pos + 1)
        | '%'::tail -> ModT  :: scan tail line (pos + 1)
        | '='::tail -> EqlT  :: scan tail line (pos + 1)
        | '('::tail -> LparT :: scan tail line (pos + 1)
        | ')'::tail -> RparT :: scan tail line (pos + 1)
        | ','::tail -> CmaT  :: scan tail line (pos + 1)
        | ';'::tail -> SemiT :: scan tail (line + 1) 0
        | '#'::tail -> let tail' = scComment(tail)
                       scan tail' line pos
        | c :: tail when isBlank c -> scan tail line (pos + 1)
        | c :: tail when isDigit c -> let iStr, iVal, pos = scNum(tail, intVal c, pos + 1)
                                      iVal :: scan iStr line (pos + 1)
        | c :: tail when isAlpha c -> let iStr, iSymbol, pos = scAlpha(tail, string c, pos + 1)
                                      SymT iSymbol :: scan iStr line (pos + 1)
        | c :: _ ->  $"Bad character: {c} @ {line}{pos}" |> SyntaxError |> raise 
    scan (str2lst input) 0 0

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()


/// Parse tokens and construct an expression AST.
let buildExpr
    (tList: Token list)
    (symbolTable: Map<string, NM>)
    : E * Token list =
    // Parse expressions.
    // <E>    ::= <T> <Eopt>
    let rec parseE tList : E * Token list =
        let u, tail = parseT tList
        let popt, tail' = parseEopt tail
        (u, popt), tail'
    // <Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    and parseEopt tList : Eopt * Token list = 
        match tList with
        | AddT :: tail ->
            let t, tail' = parseT tail
            let eopt, tail'' = parseEopt tail'
            Add (t, eopt), tail''
        | SubT :: tail ->
            let t, tail' = parseT tail
            let eopt, tail'' = parseEopt tail'
            Sub (t, eopt), tail''
        | _ -> EmptyE, tList

    // Parse terms.
    // <T>    ::= <P> <Topt>
    and parseT tList : T * Token list =
        let u, tail = parseP tList
        let popt, tail' = parseTopt tail
        (u, popt), tail'
    // <Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
    and parseTopt tList : Topt * Token list =
        match tList with
        | MulT :: tail ->
            let p, tail' = parseP tail
            let topt, tail'' = parseTopt tail'
            Mul (p, topt), tail''
        | DivT :: tail ->
            let p, tail' = parseP tail
            let topt, tail'' = parseTopt tail'
            Div (p, topt), tail''
        | ModT :: tail ->
            let p, tail' = parseP tail
            let topt, tail'' = parseTopt tail'
            Div (p, topt), tail''
        | _ -> EmptyT, tList

    // Parse powers.
    // <P>    ::= <U> <Popt>
    and parseP tList: P * Token list = 
        let u, tail = parseU tList
        let popt, tail' = parsePopt tail
        (u, popt), tail'
    // <Popt> ::= "^" <U> <Popt> | <empty>
    and parsePopt tList : Popt * Token list =
        match tList with
        | PwrT :: tail ->
            let u, tail' = parseU tail
            let popt, tail'' = parsePopt tail'
            Pwr (u, popt), tail''
        | _ -> EmptyP, tList

    // Parse unary.
    // <U>    ::= "-" <U> | <NM>
    and parseU tList : U * Token list =
        match tList with
        | SubT :: tail ->
            let u, tail' = parseU tail
            Neg u, tail'
        | _ ->
            let nm, tail = parseNM tList
            NM nm, tail

    // Parse numbers.
    // <NM>   ::= <IN> | <FL> | <SYM> | "(" <E> ")"
    and parseNM tList : NM * Token list=
        match tList with
        | SymT name :: tail ->
            if name |> symbolTable.ContainsKey then symbolTable[name], tail
            else $"Undefined symbol \"{name}\"" |> SyntaxError |> raise
        | IntT n :: tail -> VL (Int n), tail
        | FltT n :: tail -> VL (Flt n), tail
        | LparT :: tail ->
            let e, tail' = parseE tail
            match tail' with
            | RparT :: tail'' -> E e, tail''
            | _ ->  "Missing Parentheses \")\"" |> SyntaxError |> raise
        // Any missing operands eventually trickle down to here.
        | _ ->  "Missing Operand" |> SyntaxError |> raise 
    parseE tList

/// Typecast any IN values to FL values if either is a FL for binops.
let promote lhs rhs : VL * VL =
    match lhs, rhs with
    | Int lhs, Int rhs -> Int lhs, Int rhs
    | Int lhs, Flt rhs -> Flt (float lhs), Flt rhs
    | Flt lhs, Int rhs -> Flt lhs, Flt (float rhs)
    | Flt lhs, Flt rhs -> Flt lhs, Flt rhs

/// Evaluate expressions.
//let evalExpr
//    (expr: E)
//    (symbolTable: Map<string, NM>)
//    : E = 
//    // Evaluate expressions.
//    // <E>    ::= <T> <Eopt>
//    let rec evalE ((t, eopt): E) : E =
//        let t' = evalT t
//        evalEopt eopt
//    // <Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
//    and evalEopt (eopt: Eopt) (t: T): E = 
//        let thing = 5
//        
// 
//       //match eopt with
//       //| Add -> let tList', tVal = T tail
//       //        match promoteNum value tVal with
//       //        | Int value', Int tVal' -> evalEopt(tList', Int (value' + tVal'))
//       //        | Flt value', Flt tVal' -> evalEopt(tList', Flt (value' + tVal'))
//       //        | _ -> "Bad evaluation: Cannot ADD different types" |> RuntimeError |> raise 
//       //| Sub -> let tList', tVal = T tail
//       //                 match promoteNum value tVal with
//       //                 | Int value', Int tVal' -> evalEopt(tList', Int (value' - tVal'))
//       //                 | Flt value', Flt tVal' -> evalEopt(tList', Flt (value' - tVal'))
//       //                 | _ -> "Bad evaluation: Cannot SUB different types" |> RuntimeError |> raise
//       //| EmptyE -> t
// 
//    // Evaluate terms.
//    // <T>    ::= <P> <Topt>
//    and evalT ((p, topt): T): T = 
//        let _ = evalP p
//    // <Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
//    and evalTopt (tList, value) =
//        match tList with
//        | Mul :: tail -> let tList', tVal = evalP tail
//                         match promoteNum value tVal with
//                         | Int value', Int tVal' -> Topt(tList', Int (value' * tVal'))
//                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' * tVal'))
//                         | _ -> "Bad evaluation: Cannot MUL different types" |> RuntimeError |> raise 
//        | Div :: tail -> let tList', tVal = evalP tail
//                         match promoteNum value tVal with
//                         // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
//                         | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
//                         | Int value', Int tVal' -> Topt(tList', Flt ((float value') / (float tVal'))) // Force a promotion.
//                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' / tVal'))
//                         | _ -> "Bad evaluation: Cannot DIV different types" |> RuntimeError |> raise 
//        | Mod :: tail -> let tList', tVal = evalP tail
//                         match promoteNum value tVal with
//                         // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
//                         | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
//                         | Int value', Int tVal' -> Topt(tList', Int (value' % tVal'))
//                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' % tVal'))
//                         | _ -> "Bad evaluation: Cannot MOD different types" |> RuntimeError |> raise 
//        | _ -> (tList, value)
// 
//    // Parse powers.
//    // <P>    ::= <U> <Popt>
//    and evalP tList = (evalU >> evalPopt) tList
//    // <Popt> ::= "^" <U> <Popt> | <empty>
//    and evalPopt (tList, value) =
//        match tList with
//        | Pwr :: tail -> let tList', tVal = evalU tail
//                         match promoteNum value tVal with
//                         | Int value', Int tVal' -> evalPopt(tList', Int (pown value' tVal'))
//                         | Flt value', Flt tVal' -> evalPopt(tList', Flt (value' ** tVal'))
//                         | _ -> "Bad evaluation: Cannot POW different types" |> RuntimeError |> raise 
//        | _ -> (tList, value)
// 
//    // Parse unary.
//    // <U>    ::= "-" <U> | <NM>
//    and evalU tList =
//        match tList with
//        | Sub :: tail -> let tList', tVal = evalU tail
//                         match tVal with
//                         | Int tVal' -> (tList', Int -tVal')
//                         | Flt tVal' -> (tList', Flt -tVal')
//        | _ -> evalNM tList
// 
//    // Parse numbers.
//    // <NM>   ::= <IN> | <FL> | <SYM> | "(" <E> ")"
//    and evalNM tList = // Superset of IN, FL, CX
//        match tList with 
//        | Num value :: tail -> (tail, value)
//        | Sym name :: tail ->
//            if name |> symbolTable.ContainsKey then (tail, symbolTable[name])
//            else $"Bad token: undefined symbol \"{name}\"" |> RuntimeError |> raise
//        | Lpar :: tail -> let tList', tVal = E tail
//                          match tList' with 
//                          | Rpar :: tail -> (tail, tVal)
//                          | _ -> "Bad token: Missing Parentheses" |> SyntaxError |> raise
//        | _ -> "Bad token: Missing Operand" |> SyntaxError |> raise
//    evalE expr


[<EntryPoint>]
let main _  =
     Console.WriteLine("Simple Interpreter")
     let stdOut = new System.IO.StringWriter();
     let input:string = getInputString()
     let tList = lexer input
     let nm, tList' = buildExpr tList Map.empty
     0

///// Parse expression tokens.
//let parseExpr
//    (tList: terminal list)
//    (symbolTable: Map<string, NM>)
//    : terminal list =
//    // >> is forward function composition operator: let inline (>>) f g x = g(f x)
//    let rec E tList = (T >> Eopt) tList         
//    and Eopt tList = 
//        match tList with
//        | Add :: tail -> (T >> Eopt) tail
//        | Sub :: tail -> (T >> Eopt) tail
//        | _ -> tList
//    and T tList = (P >> Topt) tList
//    and Topt tList =
//        match tList with
//        | Mul :: tail -> (P >> Topt) tail
//        | Div :: tail -> (P >> Topt) tail
//        | Mod :: tail -> (P >> Topt) tail
//        | _ -> tList
//    and P tList = (U >> Popt) tList
//    and Popt tList =
//        match tList with
//        | Pwr :: tail -> (U >> Popt) tail
//        | _ -> tList
//    and U tList =
//        match tList with
//        | Sub :: tail ->
//            let tail' = U tail
//            tail'
//        | _ -> NM tList
//    and NM tList =
//        match tList with
//        | Sym name :: tail ->
//            if name |> symbolTable.ContainsKey then tail
//            else $"Bad token: undefined symbol \"{name}\"" |> SyntaxError |> raise
//        | Num _ :: tail -> tail
//        | Lpar :: tail -> match E tail with 
//                          | Rpar :: tail -> tail
//                          | _ ->  "Bad token: Missing Parentheses \")\"" |> SyntaxError |> raise
//        | _ ->  "Bad token: Missing Operand" |> SyntaxError |> raise 
//    E tList


///// Parse statement tokens.
//let parseStat
//    (tList: terminal list) 
//    (symbolTable: Map<string, NM>) 
//    : terminal list * Map<string, NM> =
//    let rec STA tList symbolTable =
//        match tList with
//        | [] -> (tList, symbolTable)
//        | Sym s :: tail when s = assignSymbol -> ASN tail symbolTable
//        | Sym s :: tail when s = plotSymbol -> PLT tail symbolTable
//        | Sym s :: tail when s = printSymbol -> PRT tail symbolTable
//        //| Sym s :: _ -> $"Undefined statement '{s}'" |> SyntaxError |> raise
//        //| _ :: _ -> $"Bad token: Expecting statement symbol" |> SyntaxError |> raise
//        | _ :: _ -> PRT tList symbolTable // Print top level expressions.
    
//    and ASN tList symbolTable =
//        match tList with
//        | Sym name :: Eql :: tail ->
//            if Set.contains name reservedSymbols then
//                $"Attempted to define reserved symbol '{name}' as a variable" |> SyntaxError |> raise 
//            let tail' = parseExpr tail symbolTable
//            match tail' with
//            | Semi :: tail'' ->
//                // Set all variables to 0 since they aren't being truely evaluated.
//                let symbolTable' = Map.add name (Flt 0) symbolTable
//                STA tail'' symbolTable'
//            | _ -> "Expected ';' after assignment" |> SyntaxError |> raise
//        | _ -> "Expected symbol and '=' after 'let'" |> SyntaxError |> raise
    
//    and PLT tList symbolTable =
//        let tail = parseExpr tList symbolTable
//        PLTopt tail symbolTable

//    and PLTopt tList symbolTable =
//        match tList with
//        | Cma :: tail ->
//            let tail' = parseExpr tail symbolTable
//            PLTopt tail' symbolTable
//        | Semi :: tail ->
//            STA tail symbolTable
//        | _ -> "Expected ',' or ';' in plot statement" |> SyntaxError |> raise
    
//    and PRT tList symbolTable =
//        let tail = parseExpr tList symbolTable
//        match tail with
//        | Semi :: tail' -> STA tail' symbolTable
//        | _ -> "Expected ';' after print statement" |> SyntaxError |> raise
    
//    STA tList symbolTable

///// Parse and evaluate expressions.
//let parseNevalExpr
//    (tList: terminal list) // Token list
//    (symbolTable: Map<string, NM>) // Symbol table
//    : terminal list * NM = 
//    let rec E tList = (T >> Eopt) tList
//    and Eopt (tList, value) = 
//        match tList with
//        | Add :: tail -> let tList', tVal = T tail
//                         match promoteNum value tVal with
//                         | Int value', Int tVal' -> Eopt(tList', Int (value' + tVal'))
//                         | Flt value', Flt tVal' -> Eopt(tList', Flt (value' + tVal'))
//                         | _ -> "Bad evaluation: Cannot ADD different types" |> RuntimeError |> raise 
//        | Sub :: tail -> let tList', tVal = T tail
//                         match promoteNum value tVal with
//                         | Int value', Int tVal' -> Eopt(tList', Int (value' - tVal'))
//                         | Flt value', Flt tVal' -> Eopt(tList', Flt (value' - tVal'))
//                         | _ -> "Bad evaluation: Cannot SUB different types" |> RuntimeError |> raise
//        | _ -> (tList, value)
//    and T tList = (P >> Topt) tList
//    and Topt (tList, value) =
//        match tList with
//        | Mul :: tail -> let tList', tVal = P tail
//                         match promoteNum value tVal with
//                         | Int value', Int tVal' -> Topt(tList', Int (value' * tVal'))
//                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' * tVal'))
//                         | _ -> "Bad evaluation: Cannot MUL different types" |> RuntimeError |> raise 
//        | Div :: tail -> let tList', tVal = P tail
//                         match promoteNum value tVal with
//                         // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
//                         | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
//                         | Int value', Int tVal' -> Topt(tList', Flt ((float value') / (float tVal'))) // Force a promotion.
//                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' / tVal'))
//                         | _ -> "Bad evaluation: Cannot DIV different types" |> RuntimeError |> raise 
//        | Mod :: tail -> let tList', tVal = P tail
//                         match promoteNum value tVal with
//                         // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
//                         | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
//                         | Int value', Int tVal' -> Topt(tList', Int (value' % tVal'))
//                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' % tVal'))
//                         | _ -> "Bad evaluation: Cannot MOD different types" |> RuntimeError |> raise 
//        | _ -> (tList, value)
//    and P tList = (U >> Popt) tList
//    and Popt (tList, value) =
//        match tList with
//        | Pwr :: tail -> let tList', tVal = U tail
//                         match promoteNum value tVal with
//                         | Int value', Int tVal' -> Popt(tList', Int (pown value' tVal'))
//                         | Flt value', Flt tVal' -> Popt(tList', Flt (value' ** tVal'))
//                         | _ -> "Bad evaluation: Cannot POW different types" |> RuntimeError |> raise 
//        | _ -> (tList, value)
//    and U tList =
//        match tList with
//        | Sub :: tail -> let tList', tVal = U tail
//                         match tVal with
//                         | Int tVal' -> (tList', Int -tVal')
//                         | Flt tVal' -> (tList', Flt -tVal')
//        | _ -> NM tList
//    and NM tList = // Superset of IN, FL, CX
//        match tList with 
//        | Num value :: tail -> (tail, value)
//        | Sym name :: tail ->
//            if name |> symbolTable.ContainsKey then (tail, symbolTable[name])
//            else $"Bad token: undefined symbol \"{name}\"" |> RuntimeError |> raise
//        | Lpar :: tail -> let tList', tVal = E tail
//                          match tList' with 
//                          | Rpar :: tail -> (tail, tVal)
//                          | _ -> "Bad token: Missing Parentheses" |> SyntaxError |> raise
//        | _ -> "Bad token: Missing Operand" |> SyntaxError |> raise
//    E tList


///// Parse and evaluate statements.
//let parseNevalStat
//    (tList: terminal list) 
//    (symbolTable: Map<string, NM>) 
//    (stdOut: System.IO.StringWriter)
//    : terminal list * Map<string, NM> * NM list list =
//    let rec STA tList symbolTable plotTable =
//        match tList with
//        | [] -> (tList, symbolTable, plotTable)
//        | Sym s :: tail when s = assignSymbol -> ASN tail symbolTable plotTable
//        | Sym s :: tail when s = plotSymbol -> PLT tail symbolTable plotTable
//        | Sym s :: tail when s = printSymbol -> PRT tail symbolTable plotTable
//        //| Sym s :: _ -> $"Undefined statement '{s}'" |> SyntaxError |> raise
//        //| _ :: _ -> $"Bad token: Expecting statement symbol" |> SyntaxError |> raise
//        | _ :: _ -> PRT tList symbolTable plotTable // Print top level expressions.
    
//    and ASN tList symbolTable plotTable =
//        match tList with
//        |  Sym name :: Eql :: tail ->
//            let tail', value = parseNevalExpr tail symbolTable
//            match tail' with
//            | Semi :: tail'' ->
//                let symbolTable' = Map.add name value symbolTable
//                STA tail'' symbolTable' plotTable
//            | _ -> "Expected ';' after assignment" |> SyntaxError |> raise
//        | _ -> "Expected symbol and '=' after 'let'" |> SyntaxError |> raise
    
//    and PLT tList symbolTable plotTable =
//        let tail, value = parseNevalExpr tList symbolTable
//        PLTopt tail symbolTable plotTable [value]
    
//    and PLTopt tList symbolTable plotTable polynomial =
//        match tList with
//        | Cma :: tail ->
//            let tail', value = parseNevalExpr tail symbolTable
//            PLTopt tail' symbolTable plotTable (polynomial @ [value])
//        | Semi :: tail ->
//            STA tail symbolTable (plotTable @ [polynomial])
//        | _ -> "Expected ',' or ';' in plot statement" |> SyntaxError |> raise
    
//    and PRT tList symbolTable plotTable =
//        let tail, value = parseNevalExpr tList symbolTable
//        match tail with
//        | Semi :: tail' ->
//            let timeStr = DateTime.Now.ToString("hh:mm:ss")
//            stdOut.WriteLine($"{timeStr} | {value}")
//            STA tail' symbolTable plotTable
//        | _ -> "Expected ';' after print statement" |> SyntaxError |> raise
    
//    STA tList symbolTable []


///// Wrapper function for parseNexec to help C# interop.
//let parseNevalStatCSharp
//    (tList: terminal list) 
//    (symbolTable: Map<string, NM>) 
//    (stdOut: System.IO.StringWriter)
//    : Map<string, NM> * NM array array =
//    let _, symbolTable', plotTable = parseNevalStat tList symbolTable stdOut
//    // Convert plotTable to arrays rather than lists.
//    (symbolTable', plotTable |> List.map List.toArray |> List.toArray)
 

///// Prints token list
//let rec printTList (lst:list<terminal>) : list<string> = 
//    match lst with
//    head::tail -> Console.Write("{0} ",head.ToString())
//                  printTList tail
//    | [] -> Console.Write("EOL\n")
//            []


//[<EntryPoint>]
//let main _  =
//    Console.WriteLine("Simple Interpreter")
//    let stdOut = new System.IO.StringWriter();
//    let input:string = getInputString()
//    let oList = lexer input
//    parseStat oList Map.empty |> ignore
//    printTList oList |> ignore
//    let _, symbolTable', plotTable = parseNevalStat oList Map.empty stdOut
//    Console.WriteLine($"Symbol Table: {symbolTable'}")
//    Console.WriteLine($"Plot Table: {plotTable}")
//    Console.WriteLine(stdOut.ToString())
//    0
