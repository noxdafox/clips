using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Threading;
using System.Windows.Threading;

namespace CLIPSIDE
  {
   public partial class AgendaBrowser : UserControl
     { 
      private MainWindow ide = null;

      public AgendaBrowser()
        {
         InitializeComponent();
        }

      public AgendaBrowser(MainWindow theMW) : this()
        {
         ide = theMW;
         if (ide != null)
           { 
            ide.dialog.StartCommandEvent += new StartCommandDelegate(StartCommandEventHandler); 
            ide.dialog.FinishCommandEvent += new FinishCommandDelegate(FinishCommandEventHandler); 
            UpdateButtonsDriver(ide.dialog.GetExecuting());
           }
        }

      private void StartCommandEventHandler()
        {
         UpdateButtons(true);
        }

      private void FinishCommandEventHandler()
        {
         UpdateButtons(false);
        }

      private void ResetClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.ReplaceCommand("(reset)\n"); }
        }

      private void RunClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.ReplaceCommand("(run)\n"); }
        }

      private void StepClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.ReplaceCommand("(run 1)\n"); }
        }

      private void HaltRulesClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.HaltRules(); }
        }

      public void DetachIDE()
        {
         if (ide != null)
           {
            ide.dialog.StartCommandEvent -= StartCommandEventHandler;
            ide.dialog.FinishCommandEvent -= FinishCommandEventHandler;
           }
        }

      /***********************/
      /* UpdateButtonsDriver */
      /***********************/
      private void UpdateButtonsDriver(
        bool isExecuting)
        {
         if (isExecuting)
           {
            ResetButton.IsEnabled = false;
            RunButton.IsEnabled = false;
            StepButton.IsEnabled = false;
            HaltRulesButton.IsEnabled = true;        
           }
         else
           {
            ResetButton.IsEnabled = true;
            RunButton.IsEnabled = true;
            StepButton.IsEnabled = true;
            HaltRulesButton.IsEnabled = false;        
           }
        }

      /*****************/
      /* UpdateButtons */
      /*****************/
      public void UpdateButtons(
        bool isExecuting)
        {
         Application.Current.Dispatcher.Invoke(DispatcherPriority.Normal,
                                                new Action(delegate { UpdateButtonsDriver(isExecuting); }));
        }
     }
  }
