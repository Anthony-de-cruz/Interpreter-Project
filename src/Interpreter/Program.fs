// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

// Grammar in BNF: ( Current )
// STATEMENTS
// <PROG>  ::= <STA> <PROG> | <empty>
// <STA>   ::= <WHL> | <IF> | <ASN> | <PLT> | <PRT>
// <WHL>   ::= "while" <BE> "{" <PROG> "}"
// <IF>    ::= "if" <BE> "{" <PROG> "}"
// <ASN>   ::= "let" <SYM> "=" <E> ";"
// <PLT>   ::= "plot" <E> ";"
// <PRT>   ::= "print" <E> ";" | <E> ";"              // Print top level expressions.
// <SYM>   ::= <alpha+>
//
// BOOLEAN EXPRESSIONS
// <BE>    ::= <BT> <BEopt>
// <BEopt> ::= "and" <BT> <BEopt> | "or" <BT> <BEopt> | <empty>
// <BT>    ::= <E> "==" <E> | <E> "!=" <E> | <E> ">" <E> | <E> "<" <E> | "!" <BE> | "(" <BE> ")"
//
// EXPRESSIONS
// <E>     ::= <T> <Eopt>
// <Eopt>  ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>     ::= <P> <Topt>
// <Topt>  ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>     ::= <U> <Popt>
// <Popt>  ::= "^" <U> <Popt> | <empty>
// <U>     ::= "-" <U> | <NM>
// <NM>    ::= <VL> | <SYM> | "(" <E> ")"             // Where SYM is defined in symbol table.
// <VL>    ::= <IN> | <FL>                            // Separate literal value simplifies implementation.
// <IN>    ::= <digit+>
// <FL>    ::= <digit+> "." <digit+>

//// KEY
//// STATEMENTS
// PROG  -> Program
// STA   -> Statement
// ASN   -> Assignment
// PLT   -> Plot
// PTR   -> Print
// SYM   -> Symbol
//// BOOLEAN EXPRESSIONS
// BE    -> Boolean Expression
// BEopt -> Boolean Expression/Optional
// BT    -> Boolean Term
//// EXPRESSIONS
// E     -> Expression
// Eopt  -> Expression/Optional
// T     -> Term
// Topt  -> Term/Optional
// P     -> Power
// Popt  -> Power/Optional
// U     -> Unary
// NM    -> Number
// IN    -> Integer
// FL    -> Floating Point

module Interpreter

open System

// AST NODES.
// Statements.
type PROG  = Stat of STA * PROG | EmptyPROG
and  STA   = While of WHL | If of IF | Assign of ASN | Plot of PLT | Print of PRT
and  WHL   = BE * PROG
and  IF    = BE * PROG
and  ASN   = String * E
and  PLT   = E
and  PRT   = E
// Boolean Expressions.
and  BE    = BT * BEopt
and  BEopt = And of BT * BEopt | Or of BT * BEopt | EmptyBE
and  BT    = Eql of E * E | NEql of E * E | Grt of E * E | Lss of E * E | Not of BE | BE of BE
// Expressions.
and  E     = T * Eopt
and  Eopt  = Add of T * Eopt | Sub of T * Eopt | EmptyE
and  T     = P * Topt
and  Topt  = Mul of P * Topt | Div of P * Topt | Mod of P * Topt | EmptyT
and  P     = U * Popt
and  Popt  = Pwr of U * Popt | EmptyP
and  U     = Neg of U | NM of NM 
and  NM    = Val of VL | Sym of string | E of E
and  VL    = Int of int | Flt of float

// Lexer tokens.
type Token = AddT | SubT | MulT | DivT | ModT | PwrT // Maths operators
             | EqlT | NEqlT | GrtT | LssT | NotT // Boolean operators
             | LparT | RparT | LcurlT | RcurlT | SetT | CmaT | SemiT // Structure
             | IntT of int | FltT of float | SymT of string // Values

// Reserved symbols.
// Todo - Consider adding a keyword type to the terminal union.
let loopSymbol = "while"
let ifSymbol = "if"
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
        | '='::'='::tail -> EqlT :: scan tail line (pos + 2)
        | '!'::'='::tail -> NEqlT :: scan tail line (pos + 2)
        | '>'::tail -> GrtT :: scan tail line (pos + 1)
        | '<'::tail -> LssT :: scan tail line (pos + 1)
        | '!'::tail -> NotT :: scan tail line (pos + 1)
        | '='::tail -> SetT :: scan tail line (pos + 1)
        | '('::tail -> LparT :: scan tail line (pos + 1)
        | ')'::tail -> RparT :: scan tail line (pos + 1)
        | '{'::tail -> LcurlT :: scan tail line (pos + 1)
        | '}'::tail -> RcurlT :: scan tail line (pos + 1)
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
    (symbolTable: Map<string, VL>)
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
    // <NM>    ::= <VL> | <SYM> | "(" <E> ")"
    and parseNM tList : NM * Token list =
        match tList with
        | SymT name :: tail ->
            if name |> symbolTable.ContainsKey then (Sym name), tail
            else $"Undefined symbol \"{name}\"" |> SyntaxError |> raise
        | LparT :: tail ->
            let e, tail' = parseE tail
            match tail' with
            | RparT :: tail'' -> E e, tail''
            | _ ->  "Missing Parentheses \")\"" |> SyntaxError |> raise
        | _ ->
            let vl, tail = parseVL tList
            Val vl, tail

    // Parses values.
    // <VL>    ::= <IN> | <FL>
    and parseVL tList : VL * Token list =
        match tList with
        | IntT n :: tail -> (Int n), tail
        | FltT n :: tail -> (Flt n), tail
        | _ ->  "Missing Operand" |> SyntaxError |> raise
        // Any missing operands eventually trickle down to here.
    parseE tList

//let buildBoolExpr
//   (tList: Token list)
//   (symbolTable: Map<string, NM>)

/// Typecast any IN values to FL values if either is a FL for binops.
let promote lhs rhs : VL * VL =
    match lhs, rhs with
    | Int lhs, Int rhs -> Int lhs, Int rhs
    | Int lhs, Flt rhs -> Flt (float lhs), Flt rhs
    | Flt lhs, Int rhs -> Flt lhs, Flt (float rhs)
    | Flt lhs, Flt rhs -> Flt lhs, Flt rhs

// Evaluate expressions.
let evalExpr
    (expr: E)
    (symbolTable: Map<string, VL>)
    : VL = 
    // Evaluate expressions.
    // <E>    ::= <T> <Eopt>
    let rec evalE ((t, eopt): E, symbolTable: Map<string, VL>) : VL =
        let tVal = evalT t symbolTable // Evaluate LHS term.
        evalEopt eopt tVal symbolTable
    // <Eopt> ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
    and evalEopt (eopt: Eopt) (lhsT: VL) (symbolTable: Map<string, VL>): VL = 
       match eopt with
       | Add (rhsT, eopt') ->
           let rhsT' = evalT rhsT symbolTable // Evaluate RHS term; Evaluate next Eopt.
           match promote lhsT rhsT' with
           | Int lhs, Int rhs -> evalEopt eopt' (Int (lhs + rhs)) symbolTable
           | Flt lhs, Flt rhs -> evalEopt eopt' (Flt (lhs + rhs)) symbolTable
           | _ -> "Bad evaluation: Cannot ADD different types" |> RuntimeError |> raise 
       | Sub (rhsT, eopt') ->
           let rhsT' = evalT rhsT symbolTable // Evaluate RHS term; Evaluate next Eopt.
           match promote lhsT rhsT' with
           | Int lhs, Int rhs -> evalEopt eopt' (Int (lhs - rhs)) symbolTable
           | Flt lhs, Flt rhs -> evalEopt eopt' (Flt (lhs - rhs)) symbolTable
           | _ -> "Bad evaluation: Cannot ADD different types" |> RuntimeError |> raise 
       | EmptyE -> lhsT

    // Evaluate terms.
    // <T>    ::= <P> <Topt>
    and evalT ((p, topt): T) (symbolTable: Map<string, VL>) : VL =
        let pVal = evalP p symbolTable // Evaluate LHS power.
        evalTopt topt pVal symbolTable
    // <Topt> ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
    and evalTopt (topt: Topt) (lhsP: VL) (symbolTable: Map<string, VL>): VL = 
       match topt with
       | Mul (rhsP, topt') ->
           let rhsP' = evalP rhsP symbolTable // Evaluate RHS power; Evaluate next Topt.
           match promote lhsP rhsP' with
           | Int lhs, Int rhs -> evalTopt topt' (Int (lhs * rhs)) symbolTable
           | Flt lhs, Flt rhs -> evalTopt topt' (Flt (lhs * rhs)) symbolTable
           | _ -> "Bad evaluation: Cannot MUL different types" |> RuntimeError |> raise 
       | Div (rhsP, topt') ->
           let rhsP' = evalP rhsP symbolTable // Evaluate RHS power; Evaluate next Topt.
           match promote lhsP rhsP' with
           // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
           | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
           | Int lhs, Int rhs -> evalTopt topt' (Flt ((float lhs) / (float rhs))) symbolTable // Force a promotion.
           | Flt lhs, Flt rhs -> evalTopt topt' (Flt (lhs / rhs)) symbolTable
           | _ -> "Bad evaluation: Cannot DIV different types" |> RuntimeError |> raise 
       | Mod (rhsP, topt') ->
           let rhsP' = evalP rhsP symbolTable // Evaluate RHS power; Evaluate next Topt.
           match promote lhsP rhsP' with
           // F# evaluates floating point division by 0 as infinity, so we must manually catch this.
           | Int _, Int 0 | Flt _, Flt 0.0 -> "Attempted to divide by zero" |> DivideByZeroException |> raise 
           | Int lhs, Int rhs -> evalTopt topt' (Int (lhs % rhs)) symbolTable
           | Flt lhs, Flt rhs -> evalTopt topt' (Flt (lhs % rhs)) symbolTable
           | _ -> "Bad evaluation: Cannot MOD different types" |> RuntimeError |> raise 
       | EmptyT -> lhsP

    // Evaluate powers.
    // <P>    ::= <U> <Popt>
    and evalP ((u, popt): P) (symbolTable: Map<string, VL>) : VL =
        let uVal = evalU u symbolTable // Evaluate LHS unary.
        evalPopt popt uVal symbolTable
    // <Popt> ::= "^" <U> <Popt> | <empty>
    and evalPopt (popt: Popt) (lhsU: VL) (symbolTable: Map<string, VL>): VL = 
       match popt with
        | Pwr (rhsU, popt') ->
            let rhsU' = evalU rhsU symbolTable // Evaluate RHS unary; Evaluate next Topt.
            match promote lhsU rhsU' with
            | Int lhs, Int rhs -> evalPopt popt' (Int (pown lhs rhs)) symbolTable
            | Flt lhs, Flt rhs -> evalPopt popt' (Flt (lhs ** rhs)) symbolTable
            | _ -> "Bad evaluation: Cannot POW different types" |> RuntimeError |> raise 
        | EmptyP -> lhsU

    // Evaluate unary.
    // <U>    ::= "-" <U> | <NM>
    and evalU (u: U) (symbolTable: Map<string, VL>) : VL =
        match u with
        | Neg u' -> match evalU u' symbolTable with // Evaluate next unary.
                    | Int uVal' -> Int -uVal'
                    | Flt uVal' -> Flt -uVal'
        | NM nm -> evalNMvl nm symbolTable // Evaluate final number.

    // Evaluate numbers and value literals.
    // <NM>    ::= <VL> | <SYM> | "(" <E> ")"
    // <VL>    ::= <IN> | <FL>
    // <IN>    ::= <digit+>
    // <FL>    ::= <digit+> "." <digit+>
    and evalNMvl (nm: NM) (symbolTable: Map<string, VL>) : VL =
        match nm with 
        | Val value -> value // Evaluate VL
        | Sym name -> 
            if name |> symbolTable.ContainsKey then evalNMvl (Val symbolTable[name]) symbolTable
            // This should be impossible. Undefined symbol should be caught during AST build.
            else $"Undefined symbol \"{name}\"" |> SyntaxError |> raise
        | E expr -> evalE (expr, symbolTable)
    evalE (expr, symbolTable)

// Parse tokens and construct a program AST.
let buildProgram
    (tList: Token list)
    (symbolTable: Map<string, VL>)
    : PROG * Token list * Map<string, VL> =

    // Parse program.
    // <PROG>  ::= <STA> <PROG> | <empty>
    let rec parsePROG (tList: Token list) (symbolTable: Map<string, VL>) : PROG * Token list * Map<string, VL> =
        match tList with
        | [] -> (EmptyPROG, tList, symbolTable) // EOF.
        | _ :: _ -> // Parse current statement; parse next statement.
            let sta, tList', symbolTable' = parseSTA tList symbolTable
            let nextProg, tList'', symbolTable'' = parsePROG tList' symbolTable' 
            Stat (sta, nextProg), tList'', symbolTable''

    // Parse statements.
    // <STA>   ::= <WHL> | <IF> | <ASN> | <PLT> | <PRT>
    and parseSTA (tList: Token list) (symbolTable: Map<string, VL>) : STA * Token list * Map<string, VL> =
        match tList with
        | SymT s :: tail when s = loopSymbol -> // Parse loop statement.
            let whl, tail', symbolTable' = parseWHL tail symbolTable
            (While whl), tail', symbolTable'
        | SymT s :: tail when s = ifSymbol -> // Parse if statement.
            let ifs, tail', symbolTable' = parseIF tail symbolTable
            (If ifs), tail', symbolTable'
        | SymT s :: tail when s = assignSymbol -> // Parse assignment statement.
            let asn, tail', symbolTable' = parseASN tail symbolTable
            (Assign asn), tail', symbolTable'
        | SymT s :: tail when s = plotSymbol -> // Parse plot statement.
            let plt, tail' = parsePLT tail symbolTable 
            (Plot plt), tail', symbolTable
        | SymT s :: tail when s = printSymbol -> // Parse print statement.
            let prt, tail' = parsePLT tail symbolTable 
            (Print prt), tail', symbolTable
        | SymT s :: _ -> $"Undefined statement '{s}'" |> SyntaxError |> raise
        | _ :: tail -> // Parse top level expressions as a print statement.
            let prt, tail' = parsePLT tail symbolTable 
            (Print prt), tail', symbolTable
        | [] -> "Bad token: Expecting statement symbol" |> SyntaxError |> raise
        
    // Parse loops.
    // <WHL>   ::= "while" <BE> "{" <PROG> "}"
    and parseWHL (tList: Token list) (symbolTable: Map<string, VL>) : WHL * Token list * Map<string, VL> =
        match tList with
        | _ -> "Loops not implemented." |> SyntaxError |> raise
    
    // Parse ifs.
    // <IF>    ::= "if" <BE> "{" <PROG> "}"
    and parseIF (tList: Token list) (symbolTable: Map<string, VL>) : IF * Token list * Map<string, VL> = 
        match tList with
        | _ -> "Ifs not implemented." |> SyntaxError |> raise

    // Parse assignments.
    // <ASN>    ::= "let" <SYM> "=" <NM>
    and parseASN (tList: Token list) (symbolTable: Map<string, VL>) : ASN * Token list * Map<string, VL> = 
        match tList with
        | SymT name :: SetT :: tail ->
            let expr, tail' = buildExpr tail symbolTable
            match tail' with
            | SemiT :: tail'' ->
                let symbolTable' = Map.add name (Int 0) symbolTable // Init an empty value.
                (name, expr), tail'', symbolTable'
            | _ -> "Expected ';' after assignment" |> SyntaxError |> raise
        | _ -> "Expected symbol and '=' after 'let'" |> SyntaxError |> raise

    // Parse plots.
    // <PLT>   ::= "plot" <E> ";"
    and parsePLT (tList: Token list) (symbolTable: Map<string, VL>) : PLT * Token list =
        let expr, tail = buildExpr tList symbolTable
        match tail with
        | SemiT :: tail' ->
            expr, tail'
        | _ -> "Expected ';' after plot" |> SyntaxError |> raise
    
    // Parse prints.
    // <PRT>   ::= "print" <E> ";" | <E> ";"
    and parsePRT (tList: Token list) (symbolTable: Map<string, VL>) : PRT * Token list * Map<string, VL> =
        match tList with
        | _ -> "Not implemented." |> SyntaxError |> raise

    parsePROG tList symbolTable

// Execute constructed ASTs.
let executeProgram
    (program: PROG)
    (symbolTable: Map<string, VL>)
    (stdOut: System.IO.StringWriter)
    : Map<string, VL> * VL list list =
    
    // Execute program.
    // <PROG>  ::= <STA> <PROG> | <empty>
    let rec execPROG (prog: PROG) (symbolTable: Map<string, VL>) (plotTable: VL list list) : Map<string, VL> * VL list list =
        match prog with
        | Stat (sta, nextProg) -> // Execute current statement; execute next statement.
            let symbolTable', plotTable' = execSTA sta symbolTable plotTable
            execPROG nextProg symbolTable' plotTable' 
        | EmptyPROG -> // EOF
            symbolTable, plotTable
    
    // Execute statements.
    // <STA>   ::= <WHL> | <IF> | <ASN> | <PLT> | <PRT>
    and execSTA (sta: STA) (symbolTable: Map<string, VL>) (plotTable: VL list list) : Map<string, VL> * VL list list =
        match sta with
        | While whl -> // Execute loop.
            execWHL whl symbolTable plotTable
        | If ifs -> // Execute conditional.
            execIF ifs symbolTable plotTable
        | Assign asn -> // Execute assignment.
            (execASN asn symbolTable), plotTable
        | Plot plt -> // Execute plot.
            symbolTable, (execPLT plt symbolTable plotTable)
        | Print prt -> // Execute assignment.
            (execPRT prt symbolTable)
            symbolTable, plotTable

    // Execute loops.
    // <WHL>   ::= "while" <BE> "{" <PROG> "}"
    and execWHL ((be, prog): WHL) (symbolTable: Map<string, VL>) (plotTable: VL list list) : Map<string, VL> * VL list list =
        "Loops not implemented." |> SyntaxError |> raise

    // Execute ifs.
    // <IF>    ::= "if" <BE> "{" <PROG> "}"
    and execIF ((be, prog): IF) (symbolTable: Map<string, VL>) (plotTable: VL list list) : Map<string, VL> * VL list list =
        "Ifs not implemented." |> SyntaxError |> raise

    // Execute assignments.
    // <ASN>    ::= "let" <SYM> "=" <NM>
    and execASN ((symbol, e): ASN) (symbolTable: Map<string, VL>) : Map<string, VL> =
         let value = evalExpr e symbolTable
         Map.add symbol value symbolTable // Evaluate expression and add to symbol table.

    // Execute plots.
    // <PLT>   ::= "plot" <E> ";"
    and execPLT (e: PLT) (symbolTable: Map<string, VL>) (plotTable: VL list list) : VL list list =
        let value = evalExpr e symbolTable
        // Todo calculate actual points.
        plotTable @ [[value]] // Evaluate expression and add to plot table.
        
    // Execute prints.
    // <PRT>   ::= "print" <E> ";" | <E> ";"
    and execPRT (e: PRT) (symbolTable: Map<string, VL>) : unit =
        let value = evalExpr e symbolTable
        let timeStr = DateTime.Now.ToString("hh:mm:ss")
        stdOut.WriteLine($"{timeStr} | {value}") // Evaluate and write to IO writer.
    execPROG program symbolTable []

/// Prints token list
let rec printTList (lst:list<Token>) : list<string> = 
    match lst with
    head::tail -> Console.Write("{0} ",head.ToString())
                  printTList tail
    | [] -> Console.Write("EOL\n")
            []

[<EntryPoint>]
let main _  =
     Console.WriteLine("Simple Interpreter")
     let stdOut = new System.IO.StringWriter()
     let input:string = getInputString()
     let tList = lexer input // Lex input.
     printTList tList |> ignore
     let ast, _, _ = buildProgram tList Map.empty // Build AST from lexed input.
     let symbolTable, plotTable = executeProgram ast Map.empty stdOut // Execute AST.
     Console.WriteLine($"Final Symbol Table: {symbolTable}")
     Console.WriteLine($"Plot Table: {plotTable}")
     Console.WriteLine(stdOut.ToString())
     0
