using Microsoft.FSharp.Collections;
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Series;
using OxyPlot.Wpf;
using System.Text;
using System.Windows;
using System.Windows.Media;

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
    /// Handles plotting
    /// </summary>
    private void plot(float[] plotTable)
    {
        var model = new PlotModel();

        // Add horizontal lines for each value
        for (int i = 0; i < plotTable.Length; i++)
        {
            float yValue = plotTable[i];
            var lineSeries = new LineSeries
            {
                Title = $"y = {yValue}"
            };

            // Add X-axis (centered at 0)
            model.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Bottom,
                Minimum = -100,
                Maximum = 100,
                Title = "X",
                AxislineStyle = LineStyle.Solid,
                AxislineThickness = 2,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineColor = OxyColor.FromRgb(200, 200, 200),
                MinorGridlineColor = OxyColor.FromRgb(230, 230, 230)
            });

            // Add Y-axis (centered at 0)
            model.Axes.Add(new LinearAxis
            {
                Position = AxisPosition.Left,
                Minimum = -100,
                Maximum = 100,
                Title = "Y",
                AxislineStyle = LineStyle.Solid,
                AxislineThickness = 2,
                MajorGridlineStyle = LineStyle.Solid,
                MinorGridlineStyle = LineStyle.Dot,
                MajorGridlineColor = OxyColor.FromRgb(200, 200, 200),
                MinorGridlineColor = OxyColor.FromRgb(230, 230, 230)
            });

            lineSeries.Points.Add(new DataPoint(-100, yValue));
            lineSeries.Points.Add(new DataPoint(100, yValue));

            model.Series.Add(lineSeries);
        }

        PlotView.Model = model;
    }

    /// <summary>
    /// Handle calculate button click.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void ExprButton_Click(object sender, RoutedEventArgs e)
    {
        FSharpList<Interpreter.terminal> lexed;
        FSharpList<Interpreter.number> plotTable;
        try
        {
            lexed = Interpreter.lexer(ExprTextBox.Text);
            //Interpreter.parser(lexed);
            (_, plotTable) = Interpreter.parseNexec(
                lexed, MapModule.Empty<string, Interpreter.number>());
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

        float[] plotArray = new float[plotTable.Length];
        for (int i = 0; i < plotTable.Length; i++)
        {
            if (plotTable[i].IsInt)
                plotArray[i] = ((Interpreter.number.Int)plotTable[i]).Item;
            else if (plotTable[i].IsFlt)
                plotArray[i] = (float)((Interpreter.number.Flt)plotTable[i]).Item;
        }

        OutputTextBox.Foreground = Brushes.Black;
        //OutputTextBox.Text = $"{terminalListBuilder}= {result}"; // Display calculation and result

        plot(plotArray);
    }
}