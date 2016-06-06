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

      public static readonly RoutedCommand Clear = 
         new RoutedUICommand("Clear",
                             "Clear", 
                             typeof(IDECommands)
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
           { this.SetDirectory(currentDirectory); }
         else
           { this.SetDirectory(Directory.GetCurrentDirectory()); }
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
         e.CanExecute = true;
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
         e.CanExecute = true;
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
         this.SetDirectory(dirPath);
           
         dialog.ReplaceCommand("(load \"" + fileName + "\")\n");
        }

      /************************/
      /* LoadBatch_CanExecute */
      /************************/
      private void LoadBatch_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
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
         this.SetDirectory(dirPath);
           
         dialog.ReplaceCommand("(batch \"" + fileName + "\")\n");
        }

      /****************/
      /* SetDirectory */
      /****************/  
      public bool SetDirectory(
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
     }
  }
