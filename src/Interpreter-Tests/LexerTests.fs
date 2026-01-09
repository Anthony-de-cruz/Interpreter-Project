module LexerTests

open Xunit
open Interpreter

[<Fact>]
let ``Basic Binop Lexing Valid`` () =
    [
        ("3 + 5", [IntT 3; AddT; IntT 5])
        ("1 * 2", [IntT 1; MulT; IntT 2])
        ("3 ^ 8", [IntT 3; PwrT; IntT 8])
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        Assert.Equal<Token>(expectedResult, lexer testCase)
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
        ("3.8", [FltT 3.8])
        ("3.008", [FltT 3.008])
        ("3.811", [FltT 3.811])
        ("0.811", [FltT 0.811])
        ("100.001", [FltT 100.001])
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        Assert.Equal<Token>(expectedResult, lexer testCase)
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
        ("x", [SymT "x"])
        ("y", [SymT "y"])
        ("varname", [SymT "varname"])
        ("myvar", [SymT "myvar"])
        ("3 + variable", [IntT 3; AddT; SymT "variable"])
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        Assert.Equal<Token>(expectedResult, lexer testCase)
    )
