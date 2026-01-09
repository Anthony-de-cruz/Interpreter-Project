module ParserTests

open Xunit
open Interpreter

// To satisfy floating point arithmetic accuracy requirements, cases will be tested against F# computed values.
// E.g: "5 / 3" = 5 / 3 rather than "5 / 3" = 1.666666...

[<Fact>]
let ``Brief Test Conditions B1`` () =
    // Expressions.
    [
        ("5*3+(2*3-2)/2+6", Int 23)
        ("9-3-2", Int 4)
        ("10/3", Int 3)
        ("10/3.0", Flt (10.0 / 3.0))
        ("10%3", Int 1)
        ("10--2", Int 12)
        ("-2+10", Int 8)
        ("3*5^(-1+3)-2^2*-3", Int 87)
        ("-3^2", Int 9)
        ("-7%3", Int -1)
        ("2*3^2", Int 18)
        ("3*5^(-1+3)-2^-2*-3", Int 75)
        ("3*5^(-1+3)-2.0^-2*-3", Flt 75.750)
        ("(((3*2--2)))", Int 8)
        ("-((3*5-2*3))", Int -9)
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let ex, _ = buildExpr (lexer testCase) Map.empty
        Assert.Equal<VL>(expectedResult, (evalExpr ex Map.empty))
    )
    [
        "(((3*2--2))"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            buildExpr (lexer testCase) Map.empty |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )
    
    // Statements.
    [
        ("let x = 3; let y = (2*x)-x^2*5;", Int -39)
        ("let x = 3; let y = (2*x)-x^2*5/2;", Int -16) // Fails due to promotion to float in div.
        ("let x = 3; let y = (2*x)-x^2*(5/2);", Int -12) // Fails due to promotion to float in div.
        ("let x = 3; let y = (2*x)-x^2*5/2.0;", Flt -16.5)
        ("let x = 3; let y = (2*x)-x^2*5%2;", Int 5)
        ("let x = 3; let y = (2*x)-x^2*(5%2);", Int -3)
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let prog, _, _ = buildProgram (lexer testCase) Map.empty
        let symbolTable, _ = executeProgram prog Map.empty (new System.IO.StringWriter())
        Assert.Equal<NM>((Val expectedResult), symbolTable["y"])
    )

[<Fact>]
let ``Addition Valid`` () =
    [
        ("5 + 3", Int 8)
        ("200 + 13 + 45", Int 258)
        ("3 + 1.1", Flt 4.1)
        ("3.256 + 1.59", Flt 4.846)
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let ex, _ = buildExpr (lexer testCase) Map.empty
        Assert.Equal<VL>(expectedResult, (evalExpr ex Map.empty))
    )

[<Fact>]
let ``Addition Invalid`` () =
    [
        "+"
        "+ 3"
        "3 +"
        "3 + 5 +"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            buildExpr (lexer testCase) Map.empty |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )

[<Fact>]
let ``Multiplication Valid`` () =
    [
        ("3 * 3", Int 9)
        ("8 * 4 * 3", Int 96)
        ("3 * 1.1", Flt (float 3 * 1.1))
        ("3.256 * 1.59", Flt 5.17704)
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let ex, _ = buildExpr (lexer testCase) Map.empty
        Assert.Equal<VL>(expectedResult, (evalExpr ex Map.empty))
    )
    
[<Fact>]
let ``Multiplication Invalid`` () =
    [
        "*"
        "* 3"
        "3 *"
        "3 * 5 *"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            buildExpr (lexer testCase) Map.empty |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )

[<Fact>]
let ``Division Valid`` () =
    [
        ("6 / 3", Int 2)
        ("5 / 3.0", Flt (5.0 / 3.0)) 
        ("12 / 3 / 2", Int 2)
        ("3.2 / 2", Flt 1.6)
        ("3.4 / 2.3", Flt (3.4 / 2.3))
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let ex, _ = buildExpr (lexer testCase) Map.empty
        Assert.Equal<VL>(expectedResult, (evalExpr ex Map.empty))
    )

[<Fact>]
let ``Division Invalid`` () =
    [
        "/"
        "/ 3"
        "3 /"
        "3 / 5 /"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            buildExpr (lexer testCase) Map.empty |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )

    [
        "3 / 0"
        "3 / 0.0"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            let e, _ = buildExpr (lexer testCase) Map.empty
            evalExpr e Map.empty |> ignore
        |> Assert.Throws<System.DivideByZeroException>
        |> ignore
    )

[<Fact>]
let ``Modulus Valid`` () =
    [
        ("6 % 3", Int 0)
        ("5 % 3", Int 2) 
        ("19 % 5 % 3", Int 1)
        ("3.2 % 2", Flt (3.2 % float 2))
        ("3.4 % 2.3", Flt 1.1)
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let ex, _ = buildExpr (lexer testCase) Map.empty
        Assert.Equal<VL>(expectedResult, (evalExpr ex Map.empty))
    )

[<Fact>]
let ``Modulus Invalid`` () =
    [
        "%"
        "% 3"
        "3 %"
        "3 % 5 %"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            buildExpr (lexer testCase) Map.empty |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )
    
    [
        "3 % 0"
        "3 % 0.0"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            let e, _ = buildExpr (lexer testCase) Map.empty
            evalExpr e Map.empty |> ignore
        |> Assert.Throws<System.DivideByZeroException>
        |> ignore
    )

[<Fact>]
let ``Power Valid`` () =
    [
        ("5 ^ 2", Int 25)
        ("5 ^ 2 ^ 2", Int 625)
        ("2 ^ 1.1", Flt (2 ** 1.1))
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let ex, _ = buildExpr (lexer testCase) Map.empty
        Assert.Equal<VL>(expectedResult, (evalExpr ex Map.empty))
    )
  
[<Fact>]
let ``Power Invalid`` () =
    [
        "^"
        "^ 3"
        "3 ^"
        "3 ^ 5 ^"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            buildExpr (lexer testCase) Map.empty |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )
  
[<Fact>]
let ``Unary Valid`` () =
    [
        ("-2", Int -2)
        ("5 -- 2", Int 7)
        ("5 +- 2", Int 3)
        ("5 +-- 3", Int 8)
        ("2 ^- 2", Int 0)
        ("2 ^-- 2", Int 4)
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let ex, _ = buildExpr (lexer testCase) Map.empty
        Assert.Equal<VL>(expectedResult, (evalExpr ex Map.empty))
    )
 
[<Fact>]
let ``Unary Invalid`` () =
    [
        "-"
        "5 ++- 3"
        "2 ^ -"
        "2--+--2"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            buildExpr (lexer testCase) Map.empty |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )
