using System.Text;
using System.Windows;
using System.Windows.Media;
using Microsoft.FSharp.Collections;

namespace GUI;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow : Window
{
    /// <summary>
    /// Construct <see cref="MainWindow"/>.
    /// </summary>
    public MainWindow()
    {
        InitializeComponent();
    }

    /// <summary>
    /// Handle expression text change.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void exprTextBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
    {
        calculateButton.IsEnabled = exprTextBox.Text.Length > 0;
    }

    /// <summary>
    /// Handle calculate button click.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void exprButton_Click(object sender, RoutedEventArgs e)
    {
        FSharpList<Interpreter.terminal> lexed;
        int? result;
        try
        {
            lexed = Interpreter.lexer(exprTextBox.Text);
            Interpreter.parser(lexed);
            (_, result) = Interpreter.parseNeval(lexed);
            
        }
        // Todo - Add new exception types to Interpreter to give better feedback.
        catch (Exception ex)
        {
            outputTextBlock.Foreground = Brushes.Red;
            outputTextBlock.Text = ex.ToString();
            return;
        }

        // Todo - Investigate why these values return nullable.
        if (result is null)
            throw new Exception("WHAT");
        
        StringBuilder terminalListBuilder = new StringBuilder();
        foreach (Interpreter.terminal i in lexed)
            terminalListBuilder.Append($"{i} ");
        
        outputTextBlock.Foreground = Brushes.Black;
        outputTextBlock.Text = $"{terminalListBuilder}= {result}";
    }
}