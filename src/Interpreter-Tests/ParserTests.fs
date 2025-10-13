module ParserTests

open Xunit

[<Fact>]
let ``Basic Integer Addition Valid`` () =
    Interpreter.lexer "5 + 3"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 8
    |> Assert.True
    
    Interpreter.lexer "200 + 13 + 45"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 258
    |> Assert.True

[<Fact>]
let ``Basic Integer Addition Invalid`` () =
    fun () ->
        Interpreter.lexer "+"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore

    fun () ->
        Interpreter.lexer "5 +"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore

    fun () ->
        Interpreter.lexer "5 + 3 +"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore

[<Fact>]
let ``Basic Integer Multiplication Valid`` () =
    Interpreter.lexer "3 * 3"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 9
    |> Assert.True
    
    Interpreter.lexer "8 * 4 * 3"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 96
    |> Assert.True

[<Fact>]
let ``Basic Integer Division Valid`` () =
    Interpreter.lexer "6 / 3"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 2
    |> Assert.True
    
    Interpreter.lexer "12 / 3 / 2"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 2
    |> Assert.True

[<Fact>]
let ``Basic Integer Division Invalid`` () =
    fun () ->
        Interpreter.lexer "/"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore
    
    fun () ->
        Interpreter.lexer "3 /"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore
    
    fun () ->
        Interpreter.lexer "/ 3"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore
    
    fun () ->
        Interpreter.lexer "3 / 0"
        |> fun o ->
            Interpreter.parser o
            |> ignore
            Interpreter.parseNeval o 
            |> ignore
    |> Assert.Throws<System.DivideByZeroException>
    |> ignore
    
[<Fact>]
let ``Basic Power implementation valid`` () =
    Interpreter.lexer "5 ^ 2"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 25
    |> Assert.True
    
    Interpreter.lexer "5 ^ 2 ^ 2"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 625
    |> Assert.True
    
[<Fact>]
let ``Basic Power implementation Invalid`` () =
    fun () ->
        Interpreter.lexer "^"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore

    fun () ->
        Interpreter.lexer "5 ^"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore

    fun () ->
        Interpreter.lexer "5 ^ 3 ^"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore
    
[<Fact>]
let ``Basic Unary Operator Valid`` () =
    Interpreter.lexer "-2"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = -2
    |> Assert.True
    
    Interpreter.lexer "5 -- 2"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 7
    |> Assert.True
    
    Interpreter.lexer "5 +- 2"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 3
    |> Assert.True
    
    Interpreter.lexer "5 +-- 3"
    |> fun o ->
        Interpreter.parser o |> ignore
        Interpreter.parseNeval o
    |> snd = 8
    |> Assert.True
    
[<Fact>]
let ``Basic Unary Operator Invalid`` () =
    fun () ->
        Interpreter.lexer "-"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore
    
    fun () ->
        Interpreter.lexer "5 ++- 3"
        |> Interpreter.parser
        |> ignore
    |> Assert.Throws<Interpreter.ParseError>
    |> ignore
