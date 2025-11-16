using Microsoft.FSharp.Collections;
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Legends;
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
        RunButton.IsEnabled = ExprTextBox.Text.Length > 0;
    }

    /// <summary>
    /// Handles plotting
    /// </summary>
    private void plot(float[][] polyCoefficients)
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

        foreach (var coeffs in polyCoefficients)
        {
            var lineSeries = new LineSeries();
            
            for(double currentX = xMin; currentX <= xMax; currentX += deltaX)
            {
                double yVal = 0.0;

                // calculate y = a[0] + b[1]*x + c[2] * x^2 + ....
                for (int i = 0; i < coeffs.Length; i++)
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
    private void RunButton_Click(object sender, RoutedEventArgs e)
    {
        System.IO.StringWriter stdOut = new();
        Interpreter.number[][] fsPlots;
        try
        {
            FSharpList<Interpreter.terminal>  lexed = Interpreter.lexer(ExprTextBox.Text);
            //Interpreter.parser(lexed);
            var result = Interpreter.parseNevalStatCSharp (lexed, MapModule.Empty<string, Interpreter.number>(), stdOut);
            fsPlots = result.Item2;
        }
        // Todo - Add new exception types to Interpreter to give better feedback.
        catch (Exception ex)
        {
            OutputTextBox.Foreground = Brushes.Red;
            OutputTextBox.Text = ex.ToString();
            return;
        }

        // Convert F# array to C# array.
        float[][] plotArray = new float[fsPlots.Length][];
        for (int i = 0; i < fsPlots.Length; i++)
        {
            plotArray[i] = new float[fsPlots[i].Length];
            for (int j = 0; j < fsPlots[i].Length; j++)
            {
                if (fsPlots[i][j].IsInt)
                    plotArray[i][j] = ((Interpreter.number.Int)fsPlots[i][j]).Item;
                else if (fsPlots[i][j].IsFlt)
                    plotArray[i][j] = (float)((Interpreter.number.Flt)fsPlots[i][j]).Item;
            }
        }

        // Display lines.
        StringBuilder plotStrBuilder = new();
        for (int i = 0; i < plotArray.Length; i++) {
            plotStrBuilder.Append($"{i}: y = ");

            var polynomial = plotArray[i];
            int nonZeroCoeffs = 0;
            for (int coeff = 0; coeff < polynomial.Length; coeff++) {
                if (polynomial[coeff] == 0)
                    continue;

                if (coeff == 0)
                    plotStrBuilder.Append($"{polynomial[coeff]}");
                else if (nonZeroCoeffs == 0)
                    plotStrBuilder.Append($"{polynomial[coeff]}x^{coeff}");
                else if (polynomial[coeff] < 0)
                    // Put a nicer looking "-".
                    plotStrBuilder.Append($" - {float.Abs(polynomial[coeff])}x^{coeff}");
                else
                    plotStrBuilder.Append($" + {polynomial[coeff]}x^{coeff}");

                nonZeroCoeffs++;
            }
            plotStrBuilder.AppendLine();
        }
        PlottingTextBox.Text = plotStrBuilder.ToString();

        // Display stdOut.
        OutputTextBox.Foreground = Brushes.Black;
        OutputTextBox.Text = stdOut.ToString();

        // Plot the polynomials.
        plot(plotArray);
    }

    private void OpenButton_Click(object sender, RoutedEventArgs e)
    {

    }

    private void SaveButton_Click(object sender, RoutedEventArgs e)
    {

    }
}