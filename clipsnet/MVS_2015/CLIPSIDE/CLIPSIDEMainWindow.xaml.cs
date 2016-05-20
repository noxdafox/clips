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

using System.Windows.Threading;
using System.ComponentModel;

namespace CLIPSIDE
  {
   public partial class MainWindow : Window
     {
      private BackgroundWorker autoBackgroundWorker = new BackgroundWorker();
      private BackgroundWorker animalBackgroundWorker = new BackgroundWorker();
      private CLIPSNET.Environment autoEnv = new CLIPSNET.Environment();
      private CLIPSNET.Environment animalEnv = new CLIPSNET.Environment();

      public void RunExample(
        BackgroundWorker worker,
        DoWorkEventArgs e,
        CLIPSNET.Environment theEnv)
        {
         Console.WriteLine("RunExample");
         theEnv.CommandLoop();
         /*
         theEnv.Reset();
         while (theEnv.Run(1) > 0)
           {
            if (worker.CancellationPending == true)
              { 
               e.Cancel = true; 
               return;
              }
           }
         */
        }

      public MainWindow()
        {
         InitializeComponent();
         /*
         this.autoBackgroundWorker.WorkerSupportsCancellation = true;
         this.autoBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(AutoDoWork);
         this.autoBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(AutoWorkCompleted);

         this.animalBackgroundWorker.WorkerSupportsCancellation = true;
         this.animalBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(AnimalDoWork);
         this.animalBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(AnimalWorkCompleted);
         Console.WriteLine("Program about to start");
         */
        }

      private void AutoDoWork(object sender, DoWorkEventArgs e)
         {
          Console.WriteLine("AutoDoWork");
          BackgroundWorker worker = sender as BackgroundWorker;
          RunExample(worker,e,autoEnv);
         }

      private void AnimalDoWork(object sender, DoWorkEventArgs e)
         {
          Console.WriteLine("AnimalDoWork");
          BackgroundWorker worker = sender as BackgroundWorker;
          RunExample(worker,e,animalEnv);
         }

      private void AutoWorkCompleted(object sender, RunWorkerCompletedEventArgs e)
         {
          Console.WriteLine("AutoWorkCompleted");
          if (e.Error != null)
            { MessageBox.Show(e.Error.Message); }
          else if (e.Cancelled)
            { /* Do Nothing */ }
         }

      private void AnimalWorkCompleted(object sender, RunWorkerCompletedEventArgs e)
         {
          Console.WriteLine("AnimalWorkCompleted");
          if (e.Error != null)
            { MessageBox.Show(e.Error.Message); }
          else if (e.Cancelled)
            { /* Do Nothing */ }
         }

      private void OnLoad(object sender, RoutedEventArgs e)
        {
         autoTextBox.Focus();
        }

      private void OnDragOver(object sender, DragEventArgs e)
         {
          Console.WriteLine("OnDragOver");
         }

      private void OnClosing(object sender, CancelEventArgs e)
        {
         Console.WriteLine("RouterMainWindow OnClosing");
         this.autoTextBox.OnClosing();
         /*
         autoBackgroundWorker.CancelAsync();
         while (autoBackgroundWorker.IsBusy)
           { this.DoEvents(); }
           */
         this.animalTextBox.OnClosing();
         /*
         animalBackgroundWorker.CancelAsync();
         while (animalBackgroundWorker.IsBusy)
           { this.DoEvents(); }
           */
         }

       /*******************************************************************************************/
       /* DoEvents: Allows events on the UI thread to be processed while waiting for termination. */
       /*    http://stackoverflow.com/questions/4502037/where-is-the-application-doevents-in-wpf  */
       /*******************************************************************************************/
       private void DoEvents()
         {
          Application.Current.Dispatcher.Invoke(DispatcherPriority.Background,
                                                new Action(delegate { }));
         }
      }
  }
