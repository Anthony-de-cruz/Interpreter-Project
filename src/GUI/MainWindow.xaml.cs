using Microsoft.FSharp.Collections;
using Microsoft.Win32;
using OxyPlot;
using OxyPlot.Axes;
using OxyPlot.Series;
using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Media;

namespace GUI;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow
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
        bool enabled = ExprTextBox.Text.Length > 0;
        RunButton.IsEnabled = enabled;
        SaveButton.IsEnabled = enabled;
    }

    /// <summary>
    /// Handles plotting
    /// </summary>
    private void Plot(float[][] polyCoefficients)
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
        StringWriter stdOut = new();
        FSharpList<FSharpList<Interpreter.VL>> fsPlotTable;
        try
        {
            FSharpList<Interpreter.Token> lexed = Interpreter.lexer(ExprTextBox.Text);
            Interpreter.PROG ast = Interpreter.buildProgram(
                lexed,
                MapModule.Empty<string, Interpreter.NM>()).Item1;
            fsPlotTable = Interpreter.executeProgram(
                ast,
                MapModule.Empty<string, Interpreter.NM>(),
                stdOut).Item2;
        }
        catch (Exception ex)
        {
            OutputTextBox.Foreground = Brushes.Red;
            OutputTextBox.Text = ex.ToString();
            return;
        }

        // Convert F# array to C# array.
        float[][] plotArray = new float[fsPlotTable.Length][];
        for (int i = 0; i < fsPlotTable.Length; i++)
        {
            plotArray[i] = new float[fsPlotTable[i].Length];
            for (int j = 0; j < fsPlotTable[i].Length; j++)
            {
                if (fsPlotTable[i][j].IsInt)
                    plotArray[i][j] = ((Interpreter.VL.Int)fsPlotTable[i][j]).Item;
                else if (fsPlotTable[i][j].IsFlt)
                    plotArray[i][j] = (float)((Interpreter.VL.Flt)fsPlotTable[i][j]).Item;
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
        Plot(plotArray);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void OpenButton_Click(object sender, RoutedEventArgs e)
    {
        var dialog = new OpenFileDialog
        {
            Title = "Open file",
            Filter = "Text files (*.txt)|*.txt|All files (*.*)|*.*",
            CheckFileExists = true,
            CheckPathExists = true
        };
        if (dialog.ShowDialog(this) != true) return;
        
        ExprTextBox.Text = File.ReadAllText(dialog.FileName);
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void SaveButton_Click(object sender, RoutedEventArgs e)
    {
        var dialog = new SaveFileDialog
        {
            Title = "Save file",
            Filter = "Text files (*.txt)|*.txt|All files (*.*)|*.*",
            DefaultExt = "txt"
        };
        if (dialog.ShowDialog(this) != true)
            return;
        
        try
        {
            File.WriteAllText(dialog.FileName, ExprTextBox.Text);
        }
        catch (Exception ex)
        {
            MessageBox.Show(
                $"Error saving file:\n{ex.Message}",
                "Save Error",
                MessageBoxButton.OK,
                MessageBoxImage.Error
            ); 
        }
    }
}