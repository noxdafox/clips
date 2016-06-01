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

      public static readonly RoutedCommand Clear = 
         new RoutedUICommand("Clear",
                             "Clear", 
                             typeof(IDECommands)
                            );
     }

  public partial class MainWindow : Window
     {
 
      /**************/
      /* MainWindow */
      /**************/
      public MainWindow()
        {
         InitializeComponent();
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
         if (openFileDialog.ShowDialog() == true)
           {
            String dirPath = Path.GetDirectoryName(openFileDialog.FileName);
            String fileName = Path.GetFileName(openFileDialog.FileName);

            Directory.SetCurrentDirectory(dirPath);
            dialog.ReplaceCommand("(load \"" + 
                                  fileName + 
                                  "\")\n");
           }
        }
  
      }
  }
