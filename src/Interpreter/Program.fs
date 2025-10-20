// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report


module Interpreter

open System

// Tokens recognised by lexer
type terminal = 
    Add | Sub | Mul | Div | Mod | Pwr | Lpar | Rpar | Num of int | Flt of float

// Utility functions for string / character handling
let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let intVal (c:char) = (int)((int)c - (int)'0')

exception LexError of string
exception ParseError of string

// Scans digits to form an int 
let rec scInt(iStr, iVal) = 
    match iStr with
    c :: tail when isdigit c -> scInt(tail, 10*iVal+(intVal c))
    | _ -> (iStr, iVal)

// Converts input string into tokens 
let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '^'::tail -> Pwr :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '%'::tail -> Mod :: scan tail
        | '('::tail -> Lpar:: scan tail
        | ')'::tail -> Rpar:: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> let (iStr, iVal) = scInt(tail, intVal c) 
                                      Num iVal :: scan iStr
        | c ->  $"Bad character: {c}" |> LexError |> raise 
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Updated Grammar 3 in BNF: ( Current )
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <U> <Popt>
// <Popt>     ::= "^" <U> <Popt> | <empty>
// <U>        ::= "-" <U> | <NR>
// <NR>       ::= "Num" <value> | "(" <E> ")"

// Updated Grammar 4 in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>          // Divide should then handle FL differently????
// <P>        ::= <U> <Popt>
// <Popt>     ::= "^" <U> <Popt> | <empty>
// <U>        ::= "-" <U> | <FL>
// <FL>       ::= <NR> "." "Num" <value> | <NR>
// <NR>       ::= "Num" <value> | "(" <E> ")"

// E    -> Expression
// Eopt -> Expression/Optional
// T    -> Term
// Topt -> Term/Optional
// P    -> Power
// Popt -> Power/Optional
// U    -> Unary
// FL   -> Floating Point
// NR   -> Number -- Terminal

// Parser
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
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ ->  "Bad token: Missing Parentheses \")\"" |> ParseError |> raise
        | _ ->  "Bad token: Missing Operand" |> ParseError |> raise 
    E tList

// Parser and evaluator combined into one
let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value + tval)
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value - tval)
        | _ -> (tList, value)
    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = P tail
                         Topt (tLst, value * tval)
        | Div :: tail -> let (tLst, tval) = P tail
                         Topt (tLst, value / tval)
        | Mod :: tail -> let (tLst, tval) = P tail
                         Topt (tLst, value % tval)
        | _ -> (tList, value)
    and P tList = (U >> Popt) tList
    and Popt (tList, value) =
        match tList with
        | Pwr :: tail -> let(tLst, tval) = U tail
                         Popt (tLst, pown value tval) // When switching to floating point potentially add **
        | _ -> (tList, value)
    and U tList =
        match tList with
        | Sub :: tail ->
            let (tLst, tval) = U tail
            (tLst, -tval)
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> "Bad token: Missing Parentheses" |> ParseError |> raise
        | _ -> "Bad token: Missing Operand" |> ParseError |> raise
    E tList

 // Prints token list
let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
                  
    | [] -> Console.Write("EOL\n")
            []


[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList;
    let pList = printTList (parser oList)
    let Out = parseNeval oList
    Console.WriteLine("Result = {0}", snd Out)
    0
