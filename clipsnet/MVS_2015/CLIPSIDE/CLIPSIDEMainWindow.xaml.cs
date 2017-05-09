using System;
using System.Windows;
using System.Windows.Input;
using System.IO;
using Microsoft.Win32;

using System.Windows.Threading;
using System.ComponentModel;

using CLIPSNET;

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

      public static readonly RoutedCommand AgendaBrowser = 
         new RoutedUICommand("AgendaBrowser",
                             "AgendaBrowser", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand FactBrowser = 
         new RoutedUICommand("FactBrowser",
                             "FactBrowser", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand InstanceBrowser = 
         new RoutedUICommand("InstanceBrowser",
                             "InstanceBrowser", 
                             typeof(IDECommands)
                            );
     }

  public partial class MainWindow : Window
     {
      private AgendaBrowserManager agendaBrowserManager;
      private IDEPreferences preferences;
      private int agendaCount = 1;
      private int factsCount = 1;
      private int instancesCount = 1;
      private int windowCount = 0;

      private class IDEPeriodicCallback : PeriodicCallback
        { 
         MainWindow mw;

         public IDEPeriodicCallback(
           MainWindow theMW)
           { 
            mw = theMW; 
           }

         public override void Callback()
           {
            mw.dialog.GetEnvironment().EnablePeriodicFunctions(false);
            mw.UpdateBrowsers();
           }
        }

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

         agendaBrowserManager = new AgendaBrowserManager(this);

         IDEPeriodicCallback theCB = new IDEPeriodicCallback(this);

         this.dialog.GetEnvironment().AddPeriodicCallback("IDECallback",0,theCB);
         this.dialog.GetEnvironment().EnablePeriodicFunctions(true);

         this.dialog.StartCommandEvent += new StartCommandDelegate(StartExecutionEventOccurred); 
         this.dialog.FinishCommandEvent += new FinishCommandDelegate(FinishExecutionEventOccurred); 
 
         System.Windows.Threading.DispatcherTimer dispatcherTimer = new System.Windows.Threading.DispatcherTimer();

         dispatcherTimer.Tick += IDEPeriodicTimer;
         dispatcherTimer.Interval = TimeSpan.FromMilliseconds(100);
         
         dispatcherTimer.Start();
        }

      public CLIPSNET.Environment GetEnvironment()
        {
         return this.dialog.GetEnvironment();
        }

      private void IDEPeriodicTimer(object sender, EventArgs e)
        {
         this.dialog.GetEnvironment().EnablePeriodicFunctions(true);
        }

      /*******************************/
      /* StartExecutionEventOccurred */
      /*******************************/
      private void StartExecutionEventOccurred()
        {
         agendaBrowserManager.UpdateAgendaBrowserButtons(true);
        }

      /********************************/
      /* FinishExecutionEventOccurred */
      /********************************/
      private void FinishExecutionEventOccurred()
        {
         agendaBrowserManager.UpdateAgendaBrowserButtons(false);
         UpdateBrowsers();
        }
        
      /******************/
      /* UpdateBrowsers */
      /******************/
      private void UpdateBrowsers()
        {
         if (this.dialog.GetEnvironment().GetAgendaChanged() ||
             this.dialog.GetEnvironment().GetFocusChanged())
           {
            this.dialog.GetEnvironment().SetAgendaChanged(false);
            this.dialog.GetEnvironment().SetFocusChanged(false);
            this.agendaBrowserManager.UpdateAllBrowsers();
           }
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

      /****************************/
      /* AgendaBrowser_CanExecute */
      /****************************/
      private void AgendaBrowser_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /**************************/      
      /* AgendaBrowser_Executed */
      /**************************/      
      private void AgendaBrowser_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         ClosableTab theTabItem = new ClosableTab();
         theTabItem.Title = "Agenda #" + agendaCount++;
         OpenTabItem(theTabItem);

         AgendaBrowser theBrowser = agendaBrowserManager.CreateBrowser();
         theTabItem.Content = theBrowser;
        }

      /**************************/
      /* FactBrowser_CanExecute */
      /**************************/
      private void FactBrowser_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /************************/      
      /* FactBrowser_Executed */
      /************************/      
      private void FactBrowser_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         ClosableTab theTabItem = new ClosableTab();
         theTabItem.Title = "Facts #" + factsCount++;
         OpenTabItem(theTabItem);
        }

      /******************************/
      /* InstanceBrowser_CanExecute */
      /******************************/
      private void InstanceBrowser_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /****************************/      
      /* InstanceBrowser_Executed */
      /****************************/      
      private void InstanceBrowser_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         ClosableTab theTabItem = new ClosableTab();
         theTabItem.Title = "Instances #" + instancesCount++;
         OpenTabItem(theTabItem);
        }

      /***************/
      /* OpenTabItem */
      /***************/
      public void OpenTabItem(
        ClosableTab theTabItem)
        {
         if (this.mainGrid.RowDefinitions[3].Height.Value == 0.0)
           { 
            this.mainGrid.RowDefinitions[1].Height = new GridLength(1,GridUnitType.Star);
            this.mainGrid.RowDefinitions[3].Height = new GridLength(1,GridUnitType.Star); 
           }

         theTabItem.TabClosedEvent += new TabClosedDelegate(CloseTabItem);
         this.debugTabControl.Items.Add(theTabItem);
   
         windowCount++;
         theTabItem.Focus();
        }
        
      /****************/
      /* CloseTabItem */
      /****************/
      public void CloseTabItem(
        ClosableTab theTabItem)
        {
         if (theTabItem.Content is AgendaBrowser)
           { 
            agendaBrowserManager.RemoveBrowser((AgendaBrowser)  theTabItem.Content);
            ((AgendaBrowser) theTabItem.Content).DetachIDE(); 
           }

         windowCount--;

         theTabItem.TabClosedEvent -= CloseTabItem;
         if (windowCount == 0)
           { 
            this.mainGrid.RowDefinitions[3].Height = new GridLength(0); 
            this.dialog.Focus();
           }
        }
     }
  }
