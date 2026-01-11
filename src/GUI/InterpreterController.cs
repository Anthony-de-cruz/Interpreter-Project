using System.IO;
using Microsoft.FSharp.Collections;

namespace GUI;

/// <summary>
/// Wrapper for the F# <see cref="Interpreter"/>.
/// Currently static as this is far too simple to justify state.
/// </summary>
public static class InterpreterController
{

    /// <summary>
    /// Execute a given program and generate a plot sequence table, writing any output to <see cref="stdOut"/>.
    /// </summary>
    /// <param name="program">The program to be executed.</param>
    /// <param name="plotRangeMin">The lower range to be plotted.</param>
    /// <param name="plotRangeMax">The upper range to be plotted.</param>
    /// <param name="plotStep">The step to increment when plotting points on the range.</param>
    /// <param name="stdOut">The output stream to write to.</param>
    /// <returns>The generated set of plot point sequences.</returns>
    /// <exception cref="Interpreter.SyntaxError"></exception>
    /// <exception cref="Interpreter.RuntimeError"></exception>
    public static double[][] Execute(
        string program,
        double plotRangeMin,
        double plotRangeMax,
        double plotStep,
        StringWriter stdOut)
    {
        // Run interpreter.
        FSharpList<Interpreter.Terminal> lexed = Interpreter.lexer(program);
        Interpreter.PROG ast = Interpreter.buildProgram(
            lexed,
            MapModule.Empty<string, Interpreter.NM>().Add("x",  Interpreter.NM.NewVal(Interpreter.VL.NewFlt(0)))).Item1;
        FSharpList<Microsoft.FSharp.Collections.FSharpList<Interpreter.VL>> fsPlotTable = Interpreter.executeProgram(
            ast,
            MapModule.Empty<string, Interpreter.NM>().Add("x",  Interpreter.NM.NewVal(Interpreter.VL.NewFlt(0))),
            stdOut,
            plotRangeMin,
            plotRangeMax,
            plotStep).Item2;

        // Convert F# array to C# array.
        double[][] plotArray = new double[fsPlotTable.Length][];
        for (int i = 0; i < fsPlotTable.Length; i++)
        {
            plotArray[i] = new double[fsPlotTable[i].Length];
            for (int j = 0; j < fsPlotTable[i].Length; j++)
            {
                if (fsPlotTable[i][j].IsInt)
                    plotArray[i][j] = ((Interpreter.VL.Int)fsPlotTable[i][j]).Item;
                else if (fsPlotTable[i][j].IsFlt)
                    plotArray[i][j] = ((Interpreter.VL.Flt)fsPlotTable[i][j]).Item;
            }
        }
        return plotArray;
    }
}