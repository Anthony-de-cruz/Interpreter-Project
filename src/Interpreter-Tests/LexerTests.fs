module LexerTests

open Xunit
open Interpreter

[<Fact>]
let ``Basic Binop Lexing Valid`` () =
    [
        ("3 + 5", [terminal.Num(number.Int(3)); terminal.Add; terminal.Num(number.Int(5))])
        ("1 * 2", [terminal.Num(number.Int(1)); terminal.Mul; terminal.Num(number.Int(2))])
        ("3 ^ 8", [terminal.Num(number.Int(3)); terminal.Pwr; terminal.Num(number.Int(8))])
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        Assert.Equal<terminal>(expectedResult, lexer testCase)
    )
    
[<Fact>]
let ``Basic Symbol Lexing Invalid`` () =
    [
        "3 £ 4"
        "5 5 :"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            lexer testCase
            |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )

[<Fact>]
let ``Basic Floating Point Lexing Valid`` () =
    [
        ("3.8", [terminal.Num(number.Flt(3.8))])
        ("3.008", [terminal.Num(number.Flt(3.008))])
        ("3.811", [terminal.Num(number.Flt(3.811))])
        ("0.811", [terminal.Num(number.Flt(0.811))])
        ("100.001", [terminal.Num(number.Flt(100.001))])
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        Assert.Equal<terminal>(expectedResult, lexer testCase)
    )
    
[<Fact>]
let ``Basic Floating Point Lexing Invalid`` () =
    [
        "7."
        ".7"
        "5 .2"
        "2. 5"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            lexer testCase
            |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )
    
[<Fact>]
let ``Basic Symbol Lexing Valid`` () =
    [
        ("x", [terminal.Sym("x")])
        ("y", [terminal.Sym("y")])
        ("varname", [terminal.Sym("varname")])
        ("myvar", [terminal.Sym("myvar")])
        ("3 + variable", [terminal.Num(number.Int(3)); terminal.Add; terminal.Sym("variable")])
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        Assert.Equal<terminal>(expectedResult, lexer testCase)
    )
