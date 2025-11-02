// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report


module Interpreter

open System

// Superset of CX, FT, IN
type number =  Int of int | Flt of float

// Tokens recognised by lexer
type terminal = 
    Add | Sub | Mul | Div | Mod | Pwr | Eql | Lpar | Rpar | Cma | Semi | Num of number | Sym of string

// Utility functions for string / character handling
let str2lst s = [for c in s -> c]
let isBlank c = Char.IsWhiteSpace c
let isDigit c = Char.IsDigit c
let isAlpha c = Char.IsLetter c
let intVal (c:char) = int (int c - int '0')

exception LexError of string
exception ParseError of string

// Scans digits to form an int, keeping track of lexer position
let rec scInt(iStr, iVal, pos) = 
    match iStr with
    c :: tail when isDigit c -> scInt(tail, 10*iVal+(intVal c), pos + 1)
    | _ -> (iStr, iVal, pos)

// Scans digits and decimal points to form numbers, keeping track of lexer position
let scNum(iStr, iVal, startingPos) =
    let iStr, whole, wholePos = scInt(iStr, iVal, startingPos)
    match iStr with
    '.'::c::tail when isDigit c -> 
        let rest, fractional, fractionalPos = scInt(tail, intVal c, wholePos + 2)
        // Find the positional difference between '.' and our fractional scan.
        let fractional = (float fractional / 10.0 ** (float fractionalPos - (float wholePos + 1.0)))
        (rest, Flt (float whole + fractional), fractionalPos)
    | '.' :: _ -> $"Missing fractional digit @ position {wholePos}" |> LexError |> raise
    | _ -> (iStr, Int whole, wholePos)
    
let rec scAlpha(iStr: char list, iSymbol: string, pos: int) =
    match iStr with
    | c :: tail when isAlpha c -> scAlpha(tail, iSymbol + string c, pos + 1)
    | _ -> (iStr, iSymbol, pos)

let lexer input =
    let rec scan input line pos =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail line (pos + 1)
        | '^'::tail -> Pwr :: scan tail line (pos + 1)
        | '-'::tail -> Sub :: scan tail line (pos + 1)
        | '*'::tail -> Mul :: scan tail line (pos + 1)
        | '/'::tail -> Div :: scan tail line (pos + 1)
        | '%'::tail -> Mod :: scan tail line (pos + 1)
        | '='::tail -> Eql :: scan tail line (pos + 1)
        | '('::tail -> Lpar:: scan tail line (pos + 1)
        | ')'::tail -> Rpar:: scan tail line (pos + 1)
        | ','::tail -> Cma :: scan tail line (pos + 1)
        | ';'::tail -> Semi:: scan tail (line + 1) 0
        | c :: tail when isBlank c -> scan tail line (pos + 1)
        | c :: tail when isDigit c -> let iStr, iVal, pos = scNum(tail, intVal c, pos + 1)
                                      Num iVal :: scan iStr line (pos + 1)
        | c :: tail when isAlpha c -> let iStr, iSymbol, pos = scAlpha(tail, string c, pos + 1)
                                      Sym iSymbol :: scan iStr line (pos + 1)
        | c :: _ ->  $"Bad character: {c} @ {line}{pos}" |> LexError |> raise 
    scan (str2lst input) 0 0

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

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
// CX     -> Complex Number

// Parser
// >> is forward function composition operator: let inline (>>) f g x = g(f x)
let parser tList =
    let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (P >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (P >> Topt) tail
        | Div :: tail -> (P >> Topt) tail
        | Mod :: tail -> (P >> Topt) tail
        | _ -> tList
    and P tList = (U >> Popt) tList
    and Popt tList =
        match tList with
        | Pwr :: tail -> (U >> Popt) tail
        | _ -> tList
    and U tList =
        match tList with
        | Sub :: tail ->
            let tail' = U tail
            tail'
        | _ -> NM tList
    and NM tList =
        match tList with 
        | Num _ :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ ->  "Bad token: Missing Parentheses \")\"" |> ParseError |> raise
        | _ ->  "Bad token: Missing Operand" |> ParseError |> raise 
    E tList
    

/// <summary>
/// Typecast any IN values to FL values if either is a FL for binops.
/// </summary>
/// <param name="lhs">The first number.</param>
/// <param name="rhs">The second number.</param>
/// <returns>(Typecasted lhs * Typecasted rhs)</returns>
let promoteNum (lhs, rhs) =
    match lhs, rhs with
    | Int lhs, Int rhs -> (Int lhs, Int rhs)
    | Int lhs, Flt rhs -> (Flt (float lhs), Flt rhs)
    | Flt lhs, Int rhs -> (Flt lhs, Flt (float rhs))
    | Flt lhs, Flt rhs -> (Flt lhs, Flt rhs)
    

// Parser and evaluator combined into one
let parseNeval
    (tList: terminal list)
    (symbolTable: Map<string, number>)
    : terminal list * number = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let tList', tVal = T tail
                         match promoteNum(value, tVal) with
                         | Int value', Int tVal' -> Eopt(tList', Int (value' + tVal'))
                         | Flt value', Flt tVal' -> Eopt(tList', Flt (value' + tVal'))
                         | _ -> "Bad evaluation: Cannot ADD different types" |> ParseError |> raise 
        | Sub :: tail -> let tList', tVal = T tail
                         match promoteNum (value, tVal) with
                         | Int value', Int tVal' -> Eopt(tList', Int (value' - tVal'))
                         | Flt value', Flt tVal' -> Eopt(tList', Flt (value' - tVal'))
                         | _ -> "Bad evaluation: Cannot SUB different types" |> ParseError |> raise
        | _ -> (tList, value)
    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> let tList', tVal = P tail
                         match promoteNum(value, tVal) with
                         | Int value', Int tVal' -> Topt(tList', Int (value' * tVal'))
                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' * tVal'))
                         | _ -> "Bad evaluation: Cannot MUL different types" |> ParseError |> raise 
        | Div :: tail -> let tList', tVal = P tail
                         match promoteNum (value, tVal) with
                         // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
                         | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
                         | Int value', Int tVal' -> Topt(tList', Flt ((float value') / (float tVal'))) // Force a promotion.
                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' / tVal'))
                         | _ -> "Bad evaluation: Cannot DIV different types" |> ParseError |> raise 
        | Mod :: tail -> let tList', tVal = P tail
                         // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
                         match promoteNum (value, tVal) with
                         | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
                         | Int value', Int tVal' -> Topt(tList', Int (value' % tVal'))
                         | Flt value', Flt tVal' -> Topt(tList', Flt (value' % tVal'))
                         | _ -> "Bad evaluation: Cannot MOD different types" |> ParseError |> raise 
        | _ -> (tList, value)
    and P tList = (U >> Popt) tList
    and Popt (tList, value) =
        match tList with
        | Pwr :: tail -> let tList', tVal = U tail
                         match promoteNum(value, tVal) with
                         | Int value', Int tVal' -> Popt(tList', Int (pown value' tVal'))
                         | Flt value', Flt tVal' -> Popt(tList', Flt (value' ** tVal'))
                         | _ -> "Bad evaluation: Cannot POW different types" |> ParseError |> raise 
        | _ -> (tList, value)
    and U tList =
        match tList with
        | Sub :: tail -> let tList', tVal = U tail
                         match tVal with
                         | Int tVal' -> (tList', Int -tVal')
                         | Flt tVal' -> (tList', Flt -tVal')
        | _ -> NM tList
    and NM tList = // Superset of IN, FL, CX
        match tList with 
        | Num value :: tail -> (tail, value)
        | Sym name :: tail -> (tail, symbolTable[name])
        | Lpar :: tail -> let tList', tVal = E tail
                          match tList' with 
                          | Rpar :: tail -> (tail, tVal)
                          | _ -> "Bad token: Missing Parentheses" |> ParseError |> raise
        | _ -> "Bad token: Missing Operand" |> ParseError |> raise
    E tList


let parseNexec 
    (tList: terminal list) 
    (symbolTable: Map<string, number>) 
    : terminal list * Map<string, number> * number list list =
    let rec STA tList symbolTable plotTable =
        match tList with
        | [] -> (tList, symbolTable, plotTable)
        | Sym "let" :: _ -> ASN tList symbolTable plotTable
        | Sym "plot" :: _ -> PLT tList symbolTable plotTable
        | Sym "print" :: _ -> PRT tList symbolTable plotTable
        | _ -> (tList, symbolTable, plotTable)
    
    and ASN tList symbolTable plotTable =
        match tList with
        | Sym "let" :: Sym name :: Eql :: tail ->
            let tail', value = parseNeval tail symbolTable
            match tail' with
            | Semi :: tail'' ->
                let symbolTable' = Map.add name value symbolTable
                STA tail'' symbolTable' plotTable
            | _ -> "Expected ';' after assignment" |> ParseError |> raise
        | _ -> "Expected symbol and '=' after 'let'" |> ParseError |> raise
    
    and PLT tList symbolTable plotTable =
        match tList with
        | Sym "plot" :: tail ->
            let tail', value = parseNeval tail symbolTable
            PLTopt tail' symbolTable plotTable [value]
        | _ -> "Expected 'plot' statement" |> ParseError |> raise
    
    and PLTopt tList symbolTable plotTable polynomial =
        match tList with
        | Cma :: tail ->
            let tail', value = parseNeval tail symbolTable
            PLTopt tail' symbolTable plotTable (polynomial @ [value])
        | Semi :: tail ->
            STA tail symbolTable (plotTable @ [polynomial])
        | _ -> "Expected ',' or ';' in plot statement" |> ParseError |> raise
    
    and PRT tList symbolTable plotTable =
        match tList with
        | Sym "print" :: tail ->
            let tail', value = parseNeval tail symbolTable
            match tail' with
            | Semi :: tail'' ->
                let timeStr = DateTime.Now.ToString("hh:mm:ss")
                Console.WriteLine($"{timeStr}| {value}")
                STA tail'' symbolTable plotTable
            | _ -> "Expected ';' after print statement" |> ParseError |> raise
        | _ -> "Expected 'print' statement" |> ParseError |> raise
    
    STA tList symbolTable []

 
 // Prints token list
let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
    | [] -> Console.Write("EOL\n")
            []


[<EntryPoint>]
let main _  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let _ = printTList oList
    let symbolTable: Map<string, number> = Map.empty
    let _, symbolTable', plotTable = parseNexec oList symbolTable 
    Console.WriteLine($"Symbol Table: {symbolTable'}")
    Console.WriteLine($"Plot Table: {plotTable}")
    //let Out = parseNeval oList
    //Console.WriteLine("Result = {0}", snd Out)
    0
