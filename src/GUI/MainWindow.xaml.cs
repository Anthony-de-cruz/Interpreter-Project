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
    /// Handle tutorial button click.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void HelpTutorialButton_Click(object sender, RoutedEventArgs e)
    {
        new TutorialWindow
        {
            Owner = this
        }.ShowDialog();
    }

    /// <summary>
    /// Handle expression text change.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void ExprTextBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
    {
        CalculateButton.IsEnabled = ExprTextBox.Text.Length > 0;
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
            lexed = Interpreter.lexer(ExprTextBox.Text);
            Interpreter.parser(lexed);
            (_, result) = Interpreter.parseNeval(lexed);
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
            terminalListBuilder.Append($"{i} ");

        OutputTextBox.Foreground = Brushes.Black;
        OutputTextBox.Text = $"{terminalListBuilder}= {result}";
    }
}