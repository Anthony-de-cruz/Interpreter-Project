module Tests

open Xunit

[<Fact>]
let ``Basic Integer Addition Valid`` () =
    let oList = Interpreter.lexer "5 + 3"
    Interpreter.parser oList |> ignore
    let Out = Interpreter.parseNeval oList
    Assert.True(8 = snd Out)
    
[<Fact>]
let ``Basic Integer Addition Invalid`` () =
    let oList1 = Interpreter.lexer "5 +"
    Assert.Throws<System.Exception>(fun () -> Interpreter.parser oList1 |> ignore) |> ignore
    
    let oList0 = Interpreter.lexer "5 + 3 +"
    Assert.Throws<System.Exception>(fun () -> Interpreter.parser oList0 |> ignore) |> ignore
    
    let oList1 = Interpreter.lexer "+"
    Assert.Throws<System.Exception>(fun () -> Interpreter.parser oList1 |> ignore) |> ignore
    
[<Fact>]
let ``Basic Integer Multiplication Valid`` () =
    let oList = Interpreter.lexer "3 * 3"
    Interpreter.parser oList |> ignore
    let Out = Interpreter.parseNeval oList
    Assert.True(9 = snd Out)
    
[<Fact>]
let ``Basic Integer Division Valid`` () =
    let oList = Interpreter.lexer "6 / 3"
    Interpreter.parser oList |> ignore
    let Out = Interpreter.parseNeval oList
    Assert.True(2 = snd Out)
