module ParserTests

open Xunit
open Interpreter

// To satisfy floating point arithmetic accuracy requirements, cases will be tested against F# computed values.
// E.g: "5 / 3" = 5 / 3 rather than "5 / 3" = 1.666666...

[<Fact>]
let ``Addition Valid`` () =
    [
        ("5 + 3", Int 8)
        ("200 + 13 + 45", Int 258)
        ("3 + 1.1", Flt 4.1)
        ("3.256 + 1.59", Flt 4.846)
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let lexed = lexer testCase
        parseExpr lexed |> ignore
        Assert.Equal<number>(expectedResult, parseNevalExpr lexed Map.empty |> snd)
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
            lexer testCase
            |> parseExpr <| Map.empty
            |> ignore
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
        let lexed = lexer testCase
        parseExpr lexed |> ignore
        Assert.Equal<number>(expectedResult, parseNevalExpr lexed Map.empty |> snd)
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
            lexer testCase
            |> parseExpr <| Map.empty
            |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )

[<Fact>]
let ``Division Valid`` () =
    [
        ("6 / 3", Flt 2.0)
        ("5 / 3", Flt (5.0 / 3.0)) 
        ("12 / 3 / 2", Flt 2.0)
        ("3.2 / 2", Flt 1.6)
        ("3.4 / 2.3", Flt (3.4 / 2.3))
    ]
    |> List.iter (fun (testCase, expectedResult) ->
        let lexed = lexer testCase
        parseExpr lexed |> ignore
        Assert.Equal<number>(expectedResult, parseNevalExpr lexed Map.empty |> snd)
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
            lexer testCase
            |> parseExpr <| Map.empty
            |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )

    [
        "3 / 0"
        "3 / 0.0"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            lexer testCase
            |> (fun lexed -> parseNevalExpr lexed Map.empty)
            |> ignore
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
        let lexed = lexer testCase
        parseExpr lexed |> ignore
        Assert.Equal<number>(expectedResult, parseNevalExpr lexed Map.empty |> snd)
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
            lexer testCase
            |> parseExpr <| Map.empty
            |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )
    
    [
        "3 % 0"
        "3 % 0.0"
    ]
    |> List.iter (fun testCase ->
        fun () ->
            lexer testCase
            |> (fun lexed -> parseNevalExpr lexed Map.empty)
            |> ignore
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
        let lexed = lexer testCase
        parseExpr lexed |> ignore
        Assert.Equal<number>(expectedResult, parseNevalExpr lexed Map.empty |> snd)
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
            lexer testCase
            |> parseExpr <| Map.empty
            |> ignore
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
        let lexed = lexer testCase
        parseExpr lexed |> ignore
        Assert.Equal<number>(expectedResult, parseNevalExpr lexed Map.empty |> snd)
    )
    
[<Fact>]
let ``Unary Invalid`` () =
    [
        "-"
        "5 ++- 3"
        "2 ^ -"
        "2--+--2" // Do we care to make this a valid case?
    ]
    |> List.iter (fun testCase ->
        fun () ->
            lexer testCase
            |> parseExpr <| Map.empty
            |> ignore
        |> Assert.Throws<SyntaxError>
        |> ignore
    )
