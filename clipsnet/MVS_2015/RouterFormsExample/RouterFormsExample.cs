using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Threading;

using CLIPSNET;
using RouterFormsExample.Properties;

namespace RouterFormsExample
  {
   public partial class RouterFormsExample : Form
     {
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

      private void OnLoad(object sender, EventArgs e)
        {
         this.autoTextBox.AttachRouter(autoEnv,10);
         autoEnv.LoadFromResource("RouterFormsExample", "RouterFormsExample.auto.clp");
         autoEnv.LoadFromResource("RouterFormsExample", "RouterFormsExample.auto_en.clp");

         this.animalTextBox.AttachRouter(animalEnv, 10);
         animalEnv.LoadFromResource("RouterFormsExample", "RouterFormsExample.bcengine.clp");
         animalEnv.LoadFromResource("RouterFormsExample", "RouterFormsExample.animal.clp");
         animalEnv.LoadFromResource("RouterFormsExample", "RouterFormsExample.animal_en.clp");

         autoBackgroundWorker.RunWorkerAsync();
         animalBackgroundWorker.RunWorkerAsync();
        }

      public RouterFormsExample()
        {
         InitializeComponent();
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
            { this.RestartAuto.Enabled = true; }
         }

      private void AnimalWorkCompleted(object sender, RunWorkerCompletedEventArgs e)
         {
          if (e.Error != null)
            { MessageBox.Show(e.Error.Message); }
          else if (e.Cancelled)
            { /* Do Nothing */ }
          else
            { this.RestartAnimal.Enabled = true; }
         }

      private void RestartAutoClicked(object sender, EventArgs e)
         {
          this.RestartAuto.Enabled = false;
          this.autoTextBox.Clear();
          this.autoTextBox.Focus();
          autoBackgroundWorker.RunWorkerAsync();
         }

      private void RestartAnimalClicked(object sender, EventArgs e)
         {
          this.RestartAnimal.Enabled = false;
          this.animalTextBox.Clear();
          this.animalTextBox.Focus();
          animalBackgroundWorker.RunWorkerAsync();
         }

      private void OnClosing(object sender, FormClosingEventArgs e)
        {
         this.autoTextBox.OnClosing();
         autoBackgroundWorker.CancelAsync();
         while (autoBackgroundWorker.IsBusy)
           { Application.DoEvents(); }

         this.animalTextBox.OnClosing();
         animalBackgroundWorker.CancelAsync();
         while (animalBackgroundWorker.IsBusy)
           { Application.DoEvents(); }
        }
     }
  }
