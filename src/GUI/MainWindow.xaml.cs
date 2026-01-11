using System.Globalization;
using Microsoft.Win32;
using OxyPlot;
using System.IO;
using System.Windows;
using System.Windows.Media;

namespace GUI;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow
{
    /// <summary>
    /// The lowest X value to be rendered.
    /// </summary>
    private double _minimumX = -10.0;

    /// <summary>
    /// The highest X value to be rendered.
    /// </summary>
    private double _maximumX = 10.0;

    /// <summary>
    /// The incremental step for line point plotting.
    /// </summary>
    private double _step = 0.1;

    /// <summary>
    /// Construct <see cref="MainWindow"/>.
    /// </summary>
    public MainWindow()
    {
        InitializeComponent();

        MinimumXTextBox.Text = _minimumX.ToString(CultureInfo.CurrentCulture);
        MaximumXTextBox.Text = _maximumX.ToString(CultureInfo.CurrentCulture);
        ResolutionTextBox.Text = (1/_step).ToString(CultureInfo.CurrentCulture);
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
    /// Handle calculate button click.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void RunButton_Click(object sender, RoutedEventArgs e)
    {
        if (ExprTextBox.Text.Length == 0) return;
        
        StringWriter stdOut = new StringWriter();
        double[][] plotArray;
        try
        {
            plotArray = InterpreterController.Execute(
                ExprTextBox.Text,
                _minimumX,
                _maximumX,
                _step,
                stdOut);
        }
        catch (Exception ex)
        {
            OutputTextBox.Foreground = Brushes.Red;
            OutputTextBox.Text = ex.ToString();
            return;
        }

        // Display stdOut.
        OutputTextBox.Foreground = Brushes.Black;
        OutputTextBox.Text = stdOut.ToString();

        // Plot the polynomials.
        PlotView.Model = InterferenceCheckbox.IsChecked == true
            ? PlotController.InterferencePlot(
                plotArray,
                _minimumX,
                _maximumX,
                _step) 
            : PlotController.IndividualPlot(
                plotArray,
                _minimumX,
                _maximumX,
                _step);
    }

    /// <summary>
    /// Read a text file to load in a new program.
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
    /// Save the current program to a text file.
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

    /// <summary>
    /// Clear the existing plot view.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void ClearButton_Click(object sender, RoutedEventArgs e)
    {
        PlotView.Model = new PlotModel();
    }

    /// <summary>
    /// Update plot range maximum.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void MinimumXTextBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
    {
        if (MinimumXTextBox.Text.Length > 0
            && Double.TryParse(MinimumXTextBox.Text, out double result))
            _minimumX = result;
    }

    /// <summary>
    /// Update plot range minimum.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void MaximumXTextBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
    {
        if (MaximumXTextBox.Text.Length > 0
            && Double.TryParse(MaximumXTextBox.Text, out double result))
            _maximumX = result;
    }

    /// <summary>
    /// Update plot resolution.
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void ResolutionTextBox_TextChanged(object sender, System.Windows.Controls.TextChangedEventArgs e)
    {
        if (ResolutionTextBox.Text.Length > 0
            && Double.TryParse(ResolutionTextBox.Text, out double result))
        {
            _step = 1 / result;
        }
    }
}