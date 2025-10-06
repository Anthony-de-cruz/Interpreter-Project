module ParserTests

open Xunit

[<Fact>]
let ``Basic Integer Addition Valid`` () =
    let oList = Interpreter.lexer "5 + 3"
    Interpreter.parser oList |> ignore
    let Out = Interpreter.parseNeval oList
    Assert.True(8 = snd Out)
    
[<Fact>]
let ``Basic Integer Addition Invalid`` () =
    Assert.Throws<System.Exception>(fun () ->
        Interpreter.lexer "5 +"
        |> Interpreter.parser
        |> ignore
    ) |> ignore 
    Assert.Throws<System.Exception>(fun () ->
        Interpreter.lexer "5 + 3 +"
        |> Interpreter.parser
        |> ignore
    ) |> ignore 
    Assert.Throws<System.Exception>(fun () ->
        Interpreter.lexer "+"
        |> Interpreter.parser
        |> ignore
    ) |> ignore 
    
[<Fact>]
let ``Basic Integer Multiplication Valid`` () =
    Interpreter.lexer "3 * 3"
    |> fun o -> Interpreter.parser o |> ignore; Interpreter.parseNeval o
    |> snd = 9
    |> Assert.True
    
  // let oList = Interpreter.lexer "3 * 3"
  // Interpreter.parser oList |> ignore
  // let Out = Interpreter.parseNeval oList
  // Assert.True(9 = snd Out)
    
[<Fact>]
let ``Basic Integer Division Valid`` () =
    let oList = Interpreter.lexer "6 / 3"
    Interpreter.parser oList |> ignore
    let Out = Interpreter.parseNeval oList
    Assert.True(2 = snd Out)
