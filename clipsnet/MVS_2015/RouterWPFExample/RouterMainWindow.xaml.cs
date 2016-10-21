using System;
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

namespace RouterWPFExample
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
         theEnv.Reset();
         while (theEnv.Run(1) > 0)
           {
            if (worker.CancellationPending == true)
              { 
               e.Cancel = true; 
               return;
              }
           }
        }

      public MainWindow()
        {
         InitializeComponent();

         this.autoBackgroundWorker.WorkerSupportsCancellation = true;
         this.autoBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(AutoDoWork);
         this.autoBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(AutoWorkCompleted);

         this.animalBackgroundWorker.WorkerSupportsCancellation = true;
         this.animalBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(AnimalDoWork);
         this.animalBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(AnimalWorkCompleted);
        }

      private void AutoDoWork(object sender, DoWorkEventArgs e)
         {
          BackgroundWorker worker = sender as BackgroundWorker;
          RunExample(worker,e,autoEnv);
         }

      private void AnimalDoWork(object sender, DoWorkEventArgs e)
         {
          BackgroundWorker worker = sender as BackgroundWorker;
          RunExample(worker,e,animalEnv);
         }

      private void AutoWorkCompleted(object sender, RunWorkerCompletedEventArgs e)
         {
          if (e.Error != null)
            { MessageBox.Show(e.Error.Message); }
          else if (e.Cancelled)
            { /* Do Nothing */ }
          else
            { this.RestartAuto.IsEnabled = true; }
         }

      private void AnimalWorkCompleted(object sender, RunWorkerCompletedEventArgs e)
         {
          if (e.Error != null)
            { MessageBox.Show(e.Error.Message); }
          else if (e.Cancelled)
            { /* Do Nothing */ }
          else
            { this.RestartAnimal.IsEnabled = true; }
         }

      private void OnLoad(object sender, RoutedEventArgs e)
        {
         autoTextBox.AttachRouter(autoEnv,10);
         autoEnv.LoadFromResource("RouterWPFExample", "RouterWPFExample.auto.clp");
         autoEnv.LoadFromResource("RouterWPFExample", "RouterWPFExample.auto_en.clp");

         this.animalTextBox.AttachRouter(animalEnv, 10);
         animalEnv.LoadFromResource("RouterWPFExample", "RouterWPFExample.bcengine.clp");
         animalEnv.LoadFromResource("RouterWPFExample", "RouterWPFExample.animal.clp");
         animalEnv.LoadFromResource("RouterWPFExample", "RouterWPFExample.animal_en.clp");

         autoBackgroundWorker.RunWorkerAsync();
         animalBackgroundWorker.RunWorkerAsync();

         autoTextBox.Focus();
        }

      private void OnDragOver(object sender, DragEventArgs e)
         {
          Console.WriteLine("OnDragOver");
         }

      private void OnClosing(object sender, CancelEventArgs e)
        {
         autoTextBox.DetachRouter();
         this.autoTextBox.OnClosing();
         autoBackgroundWorker.CancelAsync();
         while (autoBackgroundWorker.IsBusy)
           { this.DoEvents(); }

         animalTextBox.DetachRouter();
         this.animalTextBox.OnClosing();
         animalBackgroundWorker.CancelAsync();
         while (animalBackgroundWorker.IsBusy)
           { this.DoEvents(); }
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

      private void RestartAutoClicked(object sender, RoutedEventArgs e)
         {
          this.RestartAuto.IsEnabled = false;
          this.autoTextBox.Clear();
          this.autoTextBox.Focus();
          autoBackgroundWorker.RunWorkerAsync();
         }

      private void RestartAnimalClicked(object sender, RoutedEventArgs e)
         {
          this.RestartAnimal.IsEnabled = false;
          this.animalTextBox.Clear();
          this.animalTextBox.Focus();
          animalBackgroundWorker.RunWorkerAsync();
         }
      }
  }
