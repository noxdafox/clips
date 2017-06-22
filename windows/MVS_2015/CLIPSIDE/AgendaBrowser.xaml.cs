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

using CLIPSNET;

namespace CLIPSIDE
  {
   public partial class AgendaBrowser : UserControl
     { 
      private MainWindow ide = null;
      private FocusStack focusStack = null;
      private Dictionary<Focus,Agenda> agendaMap = null;
      
      /*****************/
      /* AgendaBrowser */
      /*****************/
      public AgendaBrowser()
        {
         InitializeComponent();
        }

      public AgendaBrowser(MainWindow theMW) : this()
        {
         ide = theMW;
         if (ide != null)
           { 
            UpdateButtonsDriver(ide.dialog.GetExecuting());
           }
        }
        
      /****************/
      /* ResetClicked */
      /****************/
      private void ResetClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.ReplaceCommand("(reset)\n"); }
        }

      /**************/
      /* RunClicked */
      /**************/
      private void RunClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.ReplaceCommand("(run)\n"); }
        }

      /***************/
      /* StepClicked */
      /***************/
      private void StepClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.ReplaceCommand("(run 1)\n"); }
        }
              
      /********************/
      /* HaltRulesClicked */
      /********************/
      private void HaltRulesClicked(
        object sender,
        RoutedEventArgs e)
        {
         if (ide != null)
           { ide.dialog.HaltRules(); }
        }
        
      /*************/
      /* DetachIDE */
      /*************/
      public void DetachIDE()
        {
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

     /**************/
     /* UpdateData */
     /**************/
     public void UpdateData(
        FocusStack theFocusStack,
        Dictionary<Focus,Agenda> theAgendaMap)
        {
         Application.Current.Dispatcher.Invoke(DispatcherPriority.Normal,
                                                new Action(delegate { AssignData(theFocusStack,theAgendaMap); }));
        }

      /**************/
      /* AssignData */
      /**************/
      private void AssignData(
        FocusStack theFocusStack,
        Dictionary<Focus,Agenda> theAgendaMap)
        {
         focusStack = theFocusStack;
         agendaMap = theAgendaMap;

         focusStackDataGridView.ItemsSource = theFocusStack.GetStack();

         if (theFocusStack.Count == 0)
           { activationDataGridView.ItemsSource = null; }
         else
           { activationDataGridView.ItemsSource = theAgendaMap[theFocusStack.GetStack().First()].GetActivations(); }

         if (theFocusStack.Count != 0)
           {
            focusStackDataGridView.SelectedItem = theFocusStack.GetStack().First();

            if (theAgendaMap[theFocusStack.GetStack().First()].GetActivations().Count != 0)
              { 
               activationDataGridView.SelectedItem = theAgendaMap[theFocusStack.GetStack().First()].GetActivations().First(); 
              }
           }
        }

      /**********************/
      /* ModuleFocusChanged */
      /**********************/
      private void ModuleFocusChanged(object sender,SelectionChangedEventArgs e)
        {
         Focus theFocus;
         Agenda theAgenda;

         if (e.RemovedItems.Count != 1) return;
         if (e.AddedItems.Count != 1) return;

         theFocus = (Focus) e.AddedItems[0];
         theAgenda = agendaMap[theFocus];

         activationDataGridView.ItemsSource = theAgenda.GetActivations();
         if (theAgenda.GetActivations().Count != 0)
           { activationDataGridView.SelectedItem = theAgenda.GetActivations().First(); }
        }
      }
  }
