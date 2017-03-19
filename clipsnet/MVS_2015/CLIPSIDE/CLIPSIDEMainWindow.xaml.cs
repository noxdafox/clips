using System;
using System.Windows;
using System.Windows.Input;
using System.IO;
using Microsoft.Win32;

using System.Windows.Threading;
using System.ComponentModel;

namespace CLIPSIDE
  {
   public static class IDECommands
     {
      public static readonly RoutedCommand Clear = 
         new RoutedUICommand("Clear",
                             "Clear", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand LoadConstructs = 
         new RoutedUICommand("LoadConstructs",
                             "LoadConstructs", 
                             typeof(IDECommands),
                             new InputGestureCollection()
                               { new KeyGesture(Key.L,ModifierKeys.Control) }
                            );

      public static readonly RoutedCommand LoadBatch = 
         new RoutedUICommand("LoadBatch",
                             "LoadBatch", 
                             typeof(IDECommands),
                             new InputGestureCollection()
                               { new KeyGesture(Key.L,ModifierKeys.Control | ModifierKeys.Shift) }
                            );

      public static readonly RoutedCommand SetDirectory = 
         new RoutedUICommand("SetDirectory",
                             "SetDirectory", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand Reset = 
         new RoutedUICommand("Reset",
                             "Reset", 
                             typeof(IDECommands),
                             new InputGestureCollection()
                               { new KeyGesture(Key.R,ModifierKeys.Control) }
                            );

      public static readonly RoutedCommand Run = 
         new RoutedUICommand("Run",
                             "Run", 
                             typeof(IDECommands),
                             new InputGestureCollection()
                               { new KeyGesture(Key.R,ModifierKeys.Control | ModifierKeys.Shift) }
                            );

      public static readonly RoutedCommand HaltRules = 
         new RoutedUICommand("HaltRules",
                             "HaltRule", 
                             typeof(IDECommands),
                             new InputGestureCollection()
                               { new KeyGesture(Key.H,ModifierKeys.Control) }
                            );

      public static readonly RoutedCommand HaltExecution = 
         new RoutedUICommand("HaltExecution",
                             "HaltExecution", 
                             typeof(IDECommands),
                             new InputGestureCollection()
                               { new KeyGesture(Key.H,ModifierKeys.Control | ModifierKeys.Shift) }
                            );
     }

  public partial class MainWindow : Window
     {
      private IDEPreferences preferences;

      /**************/
      /* MainWindow */
      /**************/
      public MainWindow()
        {
         InitializeComponent();

         preferences = new IDEPreferences();

         String currentDirectory = preferences.GetCurrentDirectory();

         if (Directory.Exists(currentDirectory))
           { this.SetCurrentDirectory(currentDirectory); }
         else
           { this.SetCurrentDirectory(Directory.GetCurrentDirectory()); }
        }
        
      /**********/
      /* OnLoad */
      /**********/
      private void OnLoad(object sender, RoutedEventArgs e)
        {
         dialog.Focus();
        }

      /*************/
      /* OnClosing */
      /*************/
      private void OnClosing(object sender, CancelEventArgs e)
        {
         this.dialog.OnClosing();
        }
         
      /****************/
      /* Quit_OnClick */
      /****************/
      private void Quit_OnClick(object sender, RoutedEventArgs e)
        {
          Application.Current.Shutdown();
        }

      /*********************/
      /* Clear_CanExecute */
      /********************/
      private void Clear_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = false; }
         else
           { e.CanExecute = true; }
        }

      /******************/      
      /* Clear_Executed */
      /******************/      
      private void Clear_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         dialog.ReplaceCommand("(clear)\n");
        }
        
      /*****************************/
      /* LoadConstructs_CanExecute */
      /*****************************/
      private void LoadConstructs_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = false; }
         else
           { e.CanExecute = true; }
        }

      /***************************/      
      /* LoadConstructs_Executed */
      /***************************/      
      private void LoadConstructs_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenFileDialog openFileDialog = new OpenFileDialog();
         
         openFileDialog.Filter = "Constructs Files (*.clp)|*.clp|All Files (*.*)|*.*";

         String currentDirectory = preferences.GetCurrentDirectory();
         if (Directory.Exists(currentDirectory))
           { openFileDialog.InitialDirectory = currentDirectory; } 

         openFileDialog.Title = "Load Constructs";

         if (openFileDialog.ShowDialog() != true) return;

         String dirPath = Path.GetDirectoryName(openFileDialog.FileName);
         String fileName = Path.GetFileName(openFileDialog.FileName);

         preferences.SaveCurrentDirectory(dirPath);
         this.SetCurrentDirectory(dirPath);
           
         dialog.ReplaceCommand("(load \"" + fileName + "\")\n");
        }

      /************************/
      /* LoadBatch_CanExecute */
      /************************/
      private void LoadBatch_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = false; }
         else
           { e.CanExecute = true; }
        }

      /**********************/      
      /* LoadBatch_Executed */
      /**********************/      
      private void LoadBatch_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenFileDialog openFileDialog = new OpenFileDialog();
         
         openFileDialog.Filter = "Batch Files (*.bat, *.tst)|*.bat;*.tst|All Files (*.*)|*.*";

         String currentDirectory = preferences.GetCurrentDirectory();
         if (Directory.Exists(currentDirectory))
           { openFileDialog.InitialDirectory = currentDirectory; } 

         openFileDialog.Title = "Load Batch";

         if (openFileDialog.ShowDialog() != true) return;

         String dirPath = Path.GetDirectoryName(openFileDialog.FileName);
         String fileName = Path.GetFileName(openFileDialog.FileName);

         preferences.SaveCurrentDirectory(dirPath);
         this.SetCurrentDirectory(dirPath);
           
         dialog.ReplaceCommand("(batch \"" + fileName + "\")\n");
        }

      /***************************/
      /* SetDirectory_CanExecute */
      /***************************/
      private void SetDirectory_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = false; }
         else
           { e.CanExecute = true; }
        }

      /*************************/      
      /* SetDirectory_Executed */
      /*************************/      
      private void SetDirectory_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         var selectFolderDialog = new System.Windows.Forms.FolderBrowserDialog();

         String currentDirectory = preferences.GetCurrentDirectory();
         if (Directory.Exists(currentDirectory))
           { selectFolderDialog.SelectedPath = currentDirectory; } 

         selectFolderDialog.Description = "Set Directory...";

         if (selectFolderDialog.ShowDialog() != System.Windows.Forms.DialogResult.OK) return;

         preferences.SaveCurrentDirectory(selectFolderDialog.SelectedPath);
         this.SetCurrentDirectory(selectFolderDialog.SelectedPath);
        }

      /***********************/
      /* SetCurrentDirectory */
      /***********************/  
      public bool SetCurrentDirectory(
        String newDirectory)
        {
         bool dirChanged = dialog.ChangeDirectory(newDirectory);

         if (dirChanged == false)
           { 
            this.currentDirectoryLabel.Content = "Dir: " + newDirectory;
            return true;
           }
        
         return false;
        }     

      /********************/
      /* Reset_CanExecute */
      /********************/
      private void Reset_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = false; }
         else
           { e.CanExecute = true; }
        }

      /******************/      
      /* Reset_Executed */
      /******************/      
      private void Reset_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         dialog.ReplaceCommand("(reset)\n");
        }

      /******************/
      /* Run_CanExecute */
      /******************/
      private void Run_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = false; }
         else
           { e.CanExecute = true; }
        }

      /****************/      
      /* Run_Executed */
      /****************/      
      private void Run_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         dialog.ReplaceCommand("(run)\n");
        }

      /************************/
      /* HaltRules_CanExecute */
      /************************/
      private void HaltRules_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = true; }
         else
           { e.CanExecute = false; }
        }

      /**********************/      
      /* HaltRules_Executed */
      /**********************/      
      private void HaltRules_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         dialog.HaltRules();
        }

      /****************************/
      /* HaltExecution_CanExecute */
      /****************************/
      private void HaltExecution_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = true; }
         else
           { e.CanExecute = false; }
        }

      /**************************/      
      /* HaltExecution_Executed */
      /**************************/      
      private void HaltExecution_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         dialog.HaltExecution();
        }
     }
  }
