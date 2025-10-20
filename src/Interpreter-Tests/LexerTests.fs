module LexerTests

open Xunit

[<Fact>]
let ``Basic Symbol Lexing Valid`` () =
    Interpreter.lexer "5 + 3" = [Interpreter.terminal.Num(5); Interpreter.terminal.Add; Interpreter.terminal.Num(3)]
    |> Assert.True
    
    Interpreter.lexer "1 * 2" = [Interpreter.terminal.Num(1); Interpreter.terminal.Mul; Interpreter.terminal.Num(2)]
    |> Assert.True
    
    Interpreter.lexer "3 ^ 8" = [Interpreter.terminal.Num(3); Interpreter.terminal.Pwr; Interpreter.terminal.Num(8)]
    |> Assert.True
    
[<Fact>]
let ``Basic Symbol Lexing Invalid`` () =
    fun () -> Interpreter.lexer "3 £ 4"
              |> ignore
    |> Assert.Throws<Interpreter.LexError>
    |> ignore
    
