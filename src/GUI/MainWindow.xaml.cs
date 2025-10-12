using System.Text;
using System.Windows;
using System.Windows.Media;
using Microsoft.FSharp.Collections;

namespace GUI;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow : Window // Partial as other part generated from XAML
{
    /// <summary>
    /// Construct <see cref="MainWindow"/>.
    /// </summary>
    public MainWindow()
    {
        InitializeComponent();
    }

    /// <summary>
    /// Handle tutorial button click.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void HelpTutorialButton_Click(object sender, RoutedEventArgs e)
    {
        new TutorialWindow
        {
            Owner = this // Ensures window is attached to main window
        }.ShowDialog(); // Make window modal (must close before returning to mainwindow)
    }

    /// <summary>
    /// Handle expression text change.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void ExprTextBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
    {
        CalculateButton.IsEnabled = ExprTextBox.Text.Length > 0; // Enables calculate if there is text (no accidental entries)
    }

    /// <summary>
    /// Handle calculate button click.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void ExprButton_Click(object sender, RoutedEventArgs e) 
    {
        FSharpList<Interpreter.terminal> lexed;
        int result;
        try
        {
            lexed = Interpreter.lexer(ExprTextBox.Text); // Lex tokens
            Interpreter.parser(lexed); // Parse tokens
            (_, result) = Interpreter.parseNeval(lexed); // Parse and Evaluate tokens
        }
        // Todo - Add new exception types to Interpreter to give better feedback.
        catch (Exception ex)
        {
            OutputTextBox.Foreground = Brushes.Red;
            OutputTextBox.Text = ex.ToString();
            return;
        }
        StringBuilder terminalListBuilder = new StringBuilder();
        foreach (Interpreter.terminal i in lexed)
            terminalListBuilder.Append($"{i} "); // Build lexed tokens into string

        OutputTextBox.Foreground = Brushes.Black;
        OutputTextBox.Text = $"{terminalListBuilder}= {result}"; // Display calculation and result
    }
}