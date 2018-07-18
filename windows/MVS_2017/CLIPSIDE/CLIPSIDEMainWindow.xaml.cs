using System;
using System.Windows;
using System.Windows.Input;
using System.IO;
using Microsoft.Win32;

using System.Windows.Threading;
using System.ComponentModel;
using System.Collections.Generic;

using CLIPSNET;

namespace CLIPSIDE
  {
   public class ClearWindowFunction : UserFunction
     {
      MainWindow mw;

      public ClearWindowFunction(
        MainWindow theWindow) 
        {
         mw = theWindow;
        }

      public override PrimitiveValue Evaluate(List<PrimitiveValue> arguments)
        {
         Application.Current.Dispatcher.Invoke(DispatcherPriority.Normal,
                                               new Action(delegate { mw.dialog.Clear(); }));

         return new VoidValue();
        }
     }
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


      public static readonly RoutedCommand ClearScrollback = 
         new RoutedUICommand("ClearScrollback",
                             "ClearScrollback", 
                             typeof(IDECommands)
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

      public static readonly RoutedCommand CLIPSHomePage = 
         new RoutedUICommand("CLIPSHomePage",
                             "CLIPSHomePage", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand OnlineDocumentation = 
         new RoutedUICommand("OnlineDocumentation",
                             "OnlineDocumentation", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand OnlineExamples = 
         new RoutedUICommand("OnlineExamples",
                             "OnlineExamples", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand CLIPSExpertSystemGroup = 
         new RoutedUICommand("CLIPSExpertSystemGroup",
                             "CLIPSExpertSystemGroup", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand SourceForgeForums = 
         new RoutedUICommand("SourceForgeForums",
                             "SourceForgeForums", 
                             typeof(IDECommands)
                            );

      public static readonly RoutedCommand StackOverflowQA = 
         new RoutedUICommand("StackOverflowQA",
                             "StackOverflowQA", 
                             typeof(IDECommands)
                            );
     }

  public partial class MainWindow : Window
     {
      private AgendaBrowserManager agendaBrowserManager;
      private FactBrowserManager factBrowserManager;
      private InstanceBrowserManager instanceBrowserManager;
      private IDEPreferences preferences;
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
         factBrowserManager = new FactBrowserManager(this);
         instanceBrowserManager = new InstanceBrowserManager(this);

         IDEPeriodicCallback theCB = new IDEPeriodicCallback(this);

         this.dialog.GetEnvironment().AddPeriodicCallback("IDECallback",0,theCB);
         this.dialog.GetEnvironment().EnablePeriodicFunctions(true);
         this.dialog.GetEnvironment().AddUserFunction("clear-window","v",0,0,null,new ClearWindowFunction(this));

         this.dialog.StartCommandEvent += new StartCommandDelegate(StartExecutionEventOccurred); 
         this.dialog.FinishCommandEvent += new FinishCommandDelegate(FinishExecutionEventOccurred); 
 
         System.Windows.Threading.DispatcherTimer dispatcherTimer = new System.Windows.Threading.DispatcherTimer();

         dispatcherTimer.Tick += IDEPeriodicTimer;
         dispatcherTimer.Interval = TimeSpan.FromMilliseconds(500);
         
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

         if (this.dialog.GetEnvironment().GetFactListChanged())
           {
            this.dialog.GetEnvironment().SetFactListChanged(false);
            this.factBrowserManager.UpdateAllBrowsers();
           }

         if (this.dialog.GetEnvironment().GetInstancesChanged())
           {
            this.dialog.GetEnvironment().SetInstancesChanged(false);
            this.instanceBrowserManager.UpdateAllBrowsers();
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

         if (dirChanged == true)
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

      /******************************/
      /* ClearScrollback_CanExecute */
      /******************************/
      private void ClearScrollback_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         if (dialog.GetExecuting())
           { e.CanExecute = false; }
         else
           { e.CanExecute = true; }
        }

      /****************************/      
      /* ClearScrollback_Executed */
      /****************************/      
      private void ClearScrollback_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         this.dialog.Clear();
         this.GetEnvironment().PrintPrompt();
         this.GetEnvironment().Write(this.GetEnvironment().GetInputBuffer());
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
         if (agendaBrowserManager.BrowserCount() == 0)
           { 
            ClosableTab theTabItem = new ClosableTab();
            theTabItem.Title = "Agenda";
            OpenTabItem(theTabItem);

            AgendaBrowser theBrowser = agendaBrowserManager.CreateBrowser();
            theTabItem.Content = theBrowser;
           }
         else
           {
            foreach (ClosableTab theTabItem in this.debugTabControl.Items)
              {
               if (theTabItem.Content is AgendaBrowser)
                 {
                  AgendaBrowser theBrowser = theTabItem.Content as AgendaBrowser; 
                  if (agendaBrowserManager.ManagesBrowser(theBrowser))
                    {
                     OpenExistingTabItem(theTabItem);
                     return;
                    }
                 }
              }
           }
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
         if (factBrowserManager.BrowserCount() == 0)
           { 
            ClosableTab theTabItem = new ClosableTab();
            theTabItem.Title = "Facts";

            OpenTabItem(theTabItem);

            EntityBrowser theBrowser = factBrowserManager.CreateBrowser();
            theBrowser.entityDataGridView.Columns[0].Header = "Index";
            theBrowser.entityDataGridView.Columns[1].Header = "Template";
            theTabItem.Content = theBrowser;
           }
         else
           {
            foreach (ClosableTab theTabItem in this.debugTabControl.Items)
              {
               if (theTabItem.Content is EntityBrowser)
                 {
                  EntityBrowser theBrowser = theTabItem.Content as EntityBrowser; 
                  if (factBrowserManager.ManagesBrowser(theBrowser))
                    {
                     OpenExistingTabItem(theTabItem);
                     return;
                    }
                 }
              }
           }
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
         if (instanceBrowserManager.BrowserCount() == 0)
           { 
            ClosableTab theTabItem = new ClosableTab();
            theTabItem.Title = "Instances";
            OpenTabItem(theTabItem);

            EntityBrowser theBrowser = instanceBrowserManager.CreateBrowser();
            theBrowser.entityDataGridView.Columns[0].Header = "Name";
            theBrowser.entityDataGridView.Columns[1].Header = "Class";
            theTabItem.Content = theBrowser;
           }
         else
           {
            foreach (ClosableTab theTabItem in this.debugTabControl.Items)
              {
               if (theTabItem.Content is EntityBrowser)
                 {
                  EntityBrowser theBrowser = theTabItem.Content as EntityBrowser; 
                  if (instanceBrowserManager.ManagesBrowser(theBrowser))
                    {
                     OpenExistingTabItem(theTabItem);
                     return;
                    }
                 }
              }
           }
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

      /***********************/
      /* OpenExistingTabItem */
      /***********************/
      public void OpenExistingTabItem(
        ClosableTab theTabItem)
        {
         if (this.mainGrid.RowDefinitions[3].Height.Value == 0.0)
           { 
            this.mainGrid.RowDefinitions[1].Height = new GridLength(1,GridUnitType.Star);
            this.mainGrid.RowDefinitions[3].Height = new GridLength(1,GridUnitType.Star); 
           }

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
            agendaBrowserManager.RemoveBrowser((AgendaBrowser) theTabItem.Content);
            ((AgendaBrowser) theTabItem.Content).DetachIDE(); 
           }

         if (theTabItem.Content is EntityBrowser)
           {
            EntityBrowser theBrowser = theTabItem.Content as EntityBrowser;

            if (factBrowserManager.ManagesBrowser(theBrowser))
              { factBrowserManager.RemoveBrowser(theBrowser); }

            else if (instanceBrowserManager.ManagesBrowser(theBrowser))
              { instanceBrowserManager.RemoveBrowser(theBrowser); }

            theBrowser.DetachIDE(); 
           }

         windowCount--;

         theTabItem.TabClosedEvent -= CloseTabItem;
         if (windowCount == 0)
           { 
            this.mainGrid.RowDefinitions[3].Height = new GridLength(0); 
            this.dialog.Focus();
           }
        }

      /****************************/
      /* CLIPSHomePage_CanExecute */
      /****************************/
      private void CLIPSHomePage_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /**************************/      
      /* CLIPSHomePage_Executed */
      /**************************/      
      private void CLIPSHomePage_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenURL("http://www.clipsrules.net/");
        }

      /**********************************/
      /* OnlineDocumentation_CanExecute */
      /**********************************/
      private void OnlineDocumentation_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /********************************/      
      /* OnlineDocumentation_Executed */
      /********************************/      
      private void OnlineDocumentation_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenURL("http://www.clipsrules.net/?q=Documentation");
        }
        
      /*****************************/
      /* OnlineExamples_CanExecute */
      /*****************************/
      private void OnlineExamples_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /***************************/      
      /* OnlineExamples_Executed */
      /***************************/      
      private void OnlineExamples_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenURL("https://sourceforge.net/p/clipsrules/code/HEAD/tree/branches/64x/examples/");
        }
        
      /*************************************/
      /* CLIPSExpertSystemGroup_CanExecute */
      /*************************************/
      private void CLIPSExpertSystemGroup_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /***********************************/      
      /* CLIPSExpertSystemGroup_Executed */
      /***********************************/      
      private void CLIPSExpertSystemGroup_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenURL("http://groups.google.com/group/CLIPSESG/");
        }
        
      /********************************/
      /* SourceForgeForums_CanExecute */
      /********************************/
      private void SourceForgeForums_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /******************************/      
      /* SourceForgeForums_Executed */
      /******************************/      
      private void SourceForgeForums_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenURL("http://sourceforge.net/p/clipsrules/discussion");
        }
        
      /******************************/
      /* StackOverflowQA_CanExecute */
      /******************************/
      private void StackOverflowQA_CanExecute(
        object sender, 
        CanExecuteRoutedEventArgs e)
        {
         e.CanExecute = true;
        }

      /****************************/      
      /* StackOverflowQA_Executed */
      /****************************/      
      private void StackOverflowQA_Executed(
        object sender, 
        ExecutedRoutedEventArgs e)
        {
         OpenURL("http://stackoverflow.com/questions/tagged/clips");
        }

      /***********/      
      /* OpenURL */
      /***********/      
      private void OpenURL(
        String theURL)
        {
         try
           {
            System.Diagnostics.Process.Start(theURL);
           }
         catch (System.ComponentModel.Win32Exception noBrowser)
           {
            if (noBrowser.ErrorCode==-2147467259)
              { MessageBox.Show(noBrowser.Message); }
           }
         catch (System.Exception other)
           {
            MessageBox.Show(other.Message);
           }
        }
        
      /***************************/
      /* UpdateSubmenuWatchItems */
      /***************************/
      private void UpdateSubmenuWatchItems(
        object sender,
        RoutedEventArgs e)
        {
         ActivationsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("activations");
         CompilationsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("compilations");
         DeffunctionsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("deffunctions");
         FactsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("facts");
         FocusWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("focus");
         GenericFunctionsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("generic-functions");
         GlobalsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("globals");
         InstancesWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("instances");
         MessageHandlersWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("message-handlers");
         MessagesWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("messages");
         MethodsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("methods");
         RulesWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("rules");
         SlotsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("slots");
         StatisticsWatchMenuItem.IsChecked = this.dialog.GetEnvironment().GetWatchItem("statistics");
        }

      /***************/
      /* ToggleWatch */
      /***************/  
      private void ToggleWatch(
        String watchItem)
        {
         bool currentValue;
      
         currentValue = this.dialog.GetEnvironment().GetWatchItem(watchItem); 
         this.dialog.GetEnvironment().SetWatchItem(watchItem,! currentValue);     
        }
        
      /***************************/
      /* WatchActivationsClicked */
      /***************************/
      private void WatchActivationsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("activations");
        }        

      /****************************/
      /* WatchCompilationsClicked */
      /****************************/
      private void WatchCompilationsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("compilations");
        } 
               
      /****************************/
      /* WatchDeffunctionsClicked */
      /****************************/
      private void WatchDeffunctionsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("deffunctions");
        }
        
      /*********************/
      /* WatchFactsClicked */
      /*********************/
      private void WatchFactsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("facts");
        }        

      /*********************/
      /* WatchFocusClicked */
      /*********************/
      private void WatchFocusClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("focus");
        } 
               
      /********************************/
      /* WatchGenericFunctionsClicked */
      /********************************/
      private void WatchGenericFunctionsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("generic-functions");
        }
        
      /***********************/
      /* WatchGlobalsClicked */
      /***********************/
      private void WatchGlobalsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("globals");
        }        

      /*************************/
      /* WatchInstancesClicked */
      /*************************/
      private void WatchInstancesClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("instances");
        } 
               
      /*******************************/
      /* WatchMessageHandlersClicked */
      /*******************************/
      private void WatchMessageHandlersClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("message-handlers");
        }
        
      /************************/
      /* WatchMessagesClicked */
      /************************/
      private void WatchMessagesClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("messages");
        }        

      /***********************/
      /* WatchMethodsClicked */
      /***********************/
      private void WatchMethodsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("methods");
        } 
               
      /*********************/
      /* WatchRulesClicked */
      /*********************/
      private void WatchRulesClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("rules");
        } 

      /*********************/
      /* WatchSlotsClicked */
      /*********************/
      private void WatchSlotsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("slots");
        } 
               
      /**************************/
      /* WatchStatisticsClicked */
      /**************************/
      private void WatchStatisticsClicked(object sender, RoutedEventArgs e)
        {
         ToggleWatch("statistics");
        }

      /*******************/
      /* WatchAllClicked */
      /*******************/
      private void WatchAllClicked(object sender, RoutedEventArgs e)
        {
         CLIPSNET.Environment theEnv = this.dialog.GetEnvironment();

         theEnv.Watch("activations");
         theEnv.Watch("compilations");
         theEnv.Watch("deffunctions");
         theEnv.Watch("facts");
         theEnv.Watch("focus");
         theEnv.Watch("generic-functions");
         theEnv.Watch("globals");
         theEnv.Watch("instances");
         theEnv.Watch("message-handlers");
         theEnv.Watch("messages");
         theEnv.Watch("methods");
         theEnv.Watch("rules");
         theEnv.Watch("slots");
         theEnv.Watch("statistics");
        } 
               
      /********************/
      /* WatchNoneClicked */
      /********************/
      private void WatchNoneClicked(object sender, RoutedEventArgs e)
        {
         CLIPSNET.Environment theEnv = this.dialog.GetEnvironment();

         theEnv.Unwatch("activations");
         theEnv.Unwatch("compilations");
         theEnv.Unwatch("deffunctions");
         theEnv.Unwatch("facts");
         theEnv.Unwatch("focus");
         theEnv.Unwatch("generic-functions");
         theEnv.Unwatch("globals");
         theEnv.Unwatch("instances");
         theEnv.Unwatch("message-handlers");
         theEnv.Unwatch("messages");
         theEnv.Unwatch("methods");
         theEnv.Unwatch("rules");
         theEnv.Unwatch("slots");
         theEnv.Unwatch("statistics");
        }

      /*********************/
      /* AboutCLIPSClicked */
      /*********************/
      private void AboutCLIPSClicked(object sender, RoutedEventArgs e)
        {
         AboutBox theBox = new AboutBox();
         theBox.Owner = this;
         theBox.ShowDialog();
        }
     }
  }
