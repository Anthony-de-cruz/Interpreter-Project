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
    private void plot(List<List<double>> coefficientLists)
    {
        var model = new PlotModel();

        // Plot range
        double xMin = -10.0;
        double xMax = 10.0;
        double deltaX = 0.1; // the step

        model.Axes.Add(new LinearAxis
        {
            Position = AxisPosition.Bottom,
            Minimum = xMin,
            Maximum = xMax,
            Title = "X",
            AxislineStyle = LineStyle.Solid,
            AxislineThickness = 2,
            MajorGridlineStyle = LineStyle.Solid,
            MinorGridlineStyle = LineStyle.Dot,
            MajorGridlineColor = OxyColor.FromRgb(200, 200, 200),
            MinorGridlineColor = OxyColor.FromRgb(230, 230, 230)
        });

        model.Axes.Add(new LinearAxis
        {
            Position = AxisPosition.Left,
            Title = "Y",
            Minimum = xMin,
            Maximum = xMax,
            AxislineStyle = LineStyle.Solid,
            AxislineThickness = 2,
            MajorGridlineStyle = LineStyle.Solid,
            MinorGridlineStyle = LineStyle.Dot,
            MajorGridlineColor = OxyColor.FromRgb(200, 200, 200),
            MinorGridlineColor = OxyColor.FromRgb(230, 230, 230)
        });

        foreach (var coeffs in coefficientLists)
        {
            var lineSeries = new LineSeries();
            
            for(double currentX = xMin; currentX <= xMax; currentX += deltaX)
            {
                double yVal = 0.0;

                // calculate y = a[0] + b[1]*x + c[2] * x^2 + ....
                for (int i = 0; i < coeffs.Count; i++)
                {

                    yVal += coeffs[i] * Math.Pow(currentX, i);
                }

                lineSeries.Points.Add(new DataPoint(currentX, yVal));
            }

            // Add entire line (all the points)
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
        /*
        FSharpList<Interpreter.terminal> lexed;
        FSharpList<Interpreter.number> plotTable;
        
        // parseExex returns a list of list of floats
        FSharpList<FSharpList<double>> fsharpPlotList;
        try
        {
            lexed = Interpreter.lexer(ExprTextBox.Text);
            //Interpreter.parser(lexed);
            (_, fsharpPlotList) = Interpreter.parseNexec(
                lexed, MapModule.Empty<string, Interpreter.number>());
        }
        // Todo - Add new exception types to Interpreter to give better feedback.
        catch (Exception ex)
        {
            OutputTextBox.Foreground = Brushes.Red;
            OutputTextBox.Text = ex.ToString();
            return;
        }

        // Convert fsharp to csharp list type
        var plotList = new List<List<double>>();
        foreach (var fsharpCoeffs in fsharpPlotList)
        {
            plotList.Add(new List<double>(fsharpCoeffs));
        }
        */

        var plotList = new List< List<double>>();

        plotList.Add(new List<double> { 5.0 });

        plotList.Add(new List<double> { 5.0, 2.0 });

        plotList.Add(new List<double> { 0.0, 0.0, 3.0 });

        OutputTextBox.Foreground = Brushes.Black;
        OutputTextBox.Text = $"Plotting {plotList.Count} dummy data"; // Display calculation and result

        plot(plotList);
    }
}