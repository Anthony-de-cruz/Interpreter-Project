// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report



module Interpreter

open System

// Tokens recognised by lexer
type terminal = 
    Add | Sub | Mul | Div | Mod | Pwr | Lpar | Rpar | Num of int

// Utility functions for string / character handling
let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let lexError = System.Exception("Lexer error")
let intVal (c:char) = (int)((int)c - (int)'0')
let parseError = System.Exception("Parser error")

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
        | _ -> raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

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

// Updated Grammar 3 in BNF: ( Current )
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <U> <Topt>
// <Topt>     ::= "*" <U> <Topt> | "/" <U> <Topt> | "%" <U> <Topt> | <empty>
// <U>        ::= "-" <U> | <P>
// <P>        ::= <NR> <Popt>
// <Popt>     ::= "^" <NR> <Popt> | <empty>
// <NR>       ::= "Num" <value> | "(" <E> ")"

// E    -> Expression
// Eopt -> Expression/Optional
// T    -> Term
// Topt -> Term/Optional
// U    -> Unary
// P    -> Power
// Popt -> Power/Optional
// NR   -> Number -- Terminal

// Parser
let parser tList = 
    let rec E tList = (T >> Eopt) tList         // >> is forward function composition operator: let inline (>>) f g x = g(f x)
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (U >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (U >> Topt) tail
        | Div :: tail -> (U >> Topt) tail
        | Mod :: tail -> (U >> Topt) tail
        | _ -> tList
    and U tList =
        match tList with
        | Sub :: tail ->
            let tail' = U tail
            tail'
        | _ -> P tList
    and P tList = (NR >> Popt) tList
    and Popt tList =
        match tList with
        | Pwr :: tail -> (NR >> Popt) tail
        | _ -> tList
    and NR tList =
        match tList with 
        | Num value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError
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
    and T tList = (U >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = U tail
                         Topt (tLst, value * tval)
        | Div :: tail -> let (tLst, tval) = U tail
                         Topt (tLst, value / tval)
        | Mod :: tail -> let (tLst, tval) = U tail
                         Topt (tLst, value % tval)
        | _ -> (tList, value)
    and U tList =
        match tList with
        | Sub :: tail ->
            let (tLst, tval) = U tail
            (tLst, -tval)
        | _ -> P tList
    and P tList = (NR >> Popt) tList
    and Popt (tList, value) =
        match tList with
        | Pwr :: tail -> let(tLst, tval) = NR tail
                         Popt (tLst, pown value tval) // When switching to floating point potentially add **
        | _ -> (tList, value)           
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | _ -> raise parseError
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
