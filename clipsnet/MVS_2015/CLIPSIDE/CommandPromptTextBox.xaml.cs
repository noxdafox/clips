using System;
using System.Collections.Generic;
using System.Linq;
using System.Media;
using System.Text;
using System.Threading;
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

using CLIPSNET;

namespace CLIPSIDE
  {
   /// <summary>
   /// Interaction logic for CommandPromptTextArea.xaml
   /// </summary>
   public partial class CommandPromptTextBox : RouterTextBox
     {
      private CLIPSNET.Environment clips;
      private String executingCommand;
      private BackgroundWorker commandBackgroundWorker = new BackgroundWorker();

      static readonly int DEFAULT_COMMAND_MAX = 25;

      private bool isExecuting = false;

      int oldDot = 0;

      private int maxCommandCount;
      private int currentCommandCount;
      private int currentCommand;

      List<String> commandHistory = new List<String>();
      
      /************************/
      /* CommandPromptTextBox */
      /************************/
      public CommandPromptTextBox() : this (new CLIPSNET.Environment())
        {
        }
        
      /************************/
      /* CommandPromptTextBox */
      /************************/
      public CommandPromptTextBox(
        CLIPSNET.Environment theEnv)
        {
         clips = theEnv;
         InitializeComponent();
         AttachRouter(clips,10);
         clips.PrintBanner();
         clips.PrintPrompt();

         maxCommandCount = DEFAULT_COMMAND_MAX;
         currentCommandCount = 1;
         currentCommand = 0;

         commandHistory = new List<String>(DEFAULT_COMMAND_MAX); 
         commandHistory.Add("");
        }
          
      /********************/
      /* OnPreviewKeyDown */
      /********************/
      protected override void OnPreviewKeyDown(KeyEventArgs e)
        {
         if (GetExecuting())
           { 
            base.OnPreviewKeyDown(e); 
            return;
           }

        if ((Keyboard.Modifiers & (ModifierKeys.Alt | ModifierKeys.Control | ModifierKeys.Windows)) != 0)
          { 
           base.OnPreviewKeyDown(e); 
           return; 
          }

         if (e.Key == Key.Up)
           {
            if ((currentCommand + 1) < commandHistory.Count)
              {
               if ((Keyboard.Modifiers & ModifierKeys.Shift) != 0)
                 { SwitchCommand(currentCommand,commandHistory.Count - 1); }
               else
                 { SwitchCommand(currentCommand,currentCommand + 1); }
              }
            e.Handled = true;
            return;
           }
         else if (e.Key == Key.Down)
           {
            if (currentCommand != 0)
              {
               if ((Keyboard.Modifiers & ModifierKeys.Shift) != 0)
                 { SwitchCommand(currentCommand,0); }
               else
                 { SwitchCommand(currentCommand,currentCommand - 1); }
              }
            e.Handled = true;
            return;
           }
         else if (e.Key == Key.Escape)
           {
            if ((Keyboard.Modifiers & ModifierKeys.Shift) == 0)
              { this.SelectionStart = this.Text.Length; }
            else
              { this.SelectionStart = this.Text.Length - clips.GetInputBuffer().Length; }
            this.SelectionLength = 0;
            e.Handled = true;
            return;
           }     
         else if ((e.Key == Key.Back) || (e.Key == Key.Delete))
           {
            ModifyCommand("",true);
            e.Handled = true;
            return;
           }
         else if (e.Key == Key.Space)
           {
            ModifyCommand(" ",false);
            e.Handled = true;
            return;
           }
         else if (e.Key == Key.Return)
           {
            ModifyCommand("\r",false);
            CommandCheck();
            e.Handled = true;
            return;
           }

         base.OnPreviewKeyDown(e); 
        }
   
      /***************/
      /* OnTextInput */
      /***************/
      protected override void OnTextInput(TextCompositionEventArgs e)
        {
         if (GetExecuting())
           { 
            base.OnTextInput(e); 
            return;
           }

         ModifyCommand(e.Text,false);
         CommandCheck();
         e.Handled = true;
        }
 
      /**********************/
      /* OnPreviewTextInput */
      /**********************/
      protected override void OnPreviewTextInput(TextCompositionEventArgs e)
        {
         if (GetExecuting())
           { return; }

         if (e.Text.Equals(")"))
           { BalanceParentheses(true); }
        }

      /************************/
      /* HasCuttableSelection */
      /************************/
      public bool HasCuttableSelection() // TBD synchronized
        {
         //if (GetExecuting())
         //  { return base.HasCuttableSelection(); }

         int textLength = this.Text.Length;
         int commandLength = (int) clips.GetInputBuffer().Length;  
         int lockedLength = textLength - commandLength;
      
         int left = this.SelectionStart;
         int right = left + this.SelectionLength;
      
         if (left == right) return false;
      
         if (left < lockedLength)
           { return false; }
      
         return true;
        }

      /*************************/
      /* HasPasteableSelection */
      /*************************/
      public bool HasPasteableSelection() // TBD synchronized
        {
         //if (GetExecuting())
         //  { return super.hasPasteableSelection(); }

         int textLength = this.Text.Length;
         int commandLength = (int) clips.GetInputBuffer().Length;  
         int lockedLength = textLength - commandLength;
      
         int left = this.SelectionStart;
            
         if (left < lockedLength)
           { return false; }
      
         return true;
        }

      /*****************/
      /* ModifyCommand */
      /*****************/
      protected void ModifyCommand(
        String replaceString,
        bool isDelete) 
        {
         int textLength = this.Text.Length;
         String oldCommand = clips.GetInputBuffer();
         int commandLength = (int) oldCommand.Length;  
         int lockedLength = textLength - commandLength;

         /*========================================*/
         /* Determine the left and right positions */
         /* of the current selection.              */
         /*========================================*/
            
         int left = this.SelectionStart;
         int right = left + this.SelectionLength;

         if (isDelete && (left == right) && (left > lockedLength))
           { left--; }
       
         /*************************************************/
         /* If the selection falls within text that can't */
         /* be modified (the output from prior commands), */
         /* then set the caret to the end of the command  */
         /* being edited and do nothing else.             */
         /*************************************************/
      
         if (left < lockedLength)
           {
            this.SelectionStart = textLength;
            this.SelectionLength = 0;
            return;
           }

         String newCommand = this.Text.Substring(lockedLength, left - lockedLength) + 
                             replaceString + 
                             this.Text.Substring(right);

         if ((right - left) != 0)
           { this.Text = this.Text.Remove(left,right - left).Insert(left, replaceString); }
         else
           { this.Text = this.Text.Insert(left, replaceString); }
      
         this.SelectionStart = left + replaceString.Length;
         this.SelectionLength = 0;

         clips.SetInputBuffer(newCommand);   
        }

      /**********************/
      /* BalanceParentheses */
      /**********************/
      public void BalanceParentheses(
        bool closingParenDetected) 
        {
         long commandLength;
         char characterToCheck;

         /*=======================================================*/
         /* Don't balance the parentheses if there is no command. */
         /*=======================================================*/
   
         commandLength = clips.GetInputBuffer().Length;
         if (commandLength <= 0) 
           { return; }

         /*=================================*/
         /* Retrieve the current selection. */
         /*=================================*/
          
         int selStart = this.SelectionStart;
         int selLength = this.SelectionLength;

         /*======================*/
         /* Where is the cursor? */
         /*======================*/

         int cursorLocation = this.CaretIndex;
         if (cursorLocation == 0) return;
          
         if (! closingParenDetected)
           {      
            cursorLocation--;

           /*===============================================*/
            /* What is the character at the cursor location? */
            /*===============================================*/

            characterToCheck = this.Text[cursorLocation];
      
            /*======================================*/
            /* We only balance a right parenthesis. */
            /*======================================*/
   
            if (characterToCheck != ')') return;
           }

         /*======================================================================*/
         /* The nesting depth will start at zero. Each time a ')' is encountered */
         /* the nesting depth is incremented by one and each time a '(' is       */
         /* encountered the nesting depth is decremented by one. If a '(' is     */
         /* encountered when the nesting depth is zero (the starting value), the */
         /* matching parenthesis has been found.                                 */
         /*======================================================================*/
   
         int nestingDepth = 0;

         /*==================================================*/
         /* Start looking for the matching left parenthesis. */
         /*==================================================*/

         while ((cursorLocation-- != 0) && (commandLength-- != 0)) 
           {
            characterToCheck = this.Text[cursorLocation];
            if (characterToCheck == '(') 
              {
               if (nestingDepth == 0) 
                 {
                  /*======================================*/
                  /* Select the matching left parenthesis */
                  /* and hide the carete.                 */
                  /*======================================*/
                  
                  //this.getCaret().setVisible(false);

                  this.Select(cursorLocation,1);
                  
                  /*========================================*/
                  /* Force an update to occur otherwise the */
                  /* changed selection won't be visible.    */
                  /*========================================*/
 
                  this.Dispatcher.Invoke(DispatcherPriority.Render,new Action(delegate { }));

                  /*============================================*/
                  /* Pause momentarily so the selected matching */
                  /* parenthesis can be observed.               */
                  /*============================================*/

                  Thread.Sleep(200);

                  /*===========================*/
                  /* Restore the selection and */
                  /* make the caret visible.   */
                  /*===========================*/

                  this.Select(selStart,selLength);
                 
                  //this.getCaret().setVisible(true);
		            
                  return;
                 }
               else
		           { nestingDepth--; }
              }
            else if (characterToCheck == ')') 
              { nestingDepth++; }
           }

         /*================================================*/
         /* Beep to indicate a matching ')' was not found. */
         /*================================================*/

         SystemSounds.Beep.Play();
        }

      /****************/
      /* CommandCheck */
      /****************/
      private void CommandCheck() 
        {
         if (clips.InputBufferContainsCommand())
           { 
            UpdateCommandHistory();
            ExecuteCommand();
           }
        }

      /****************/
      /* GetExecuting */
      /****************/
      public bool GetExecuting() 
        {
         return isExecuting;
        }

      /****************/
      /* SetExecuting */
      /****************/
      public void SetExecuting(
        bool value) 
        {
         isExecuting = value;
        }
 
      /********************/
      /* DoExecuteCommand */
      /********************/  
      private void DoExecuteCommand(
        String executingCommand)
        {
         /*
         Timer periodicTimer = new Timer();

         callExecutionCommandListeners(executingCommand,CommandExecutionEvent.START_EVENT);
         clips.addPeriodicCallback(periodicName,0,this);      
         periodicTimer.schedule(new PeriodicTask(),0,periodicTaskFrequency);
         */
         clips.CommandLoopOnceThenBatch(); 
         /*
         dumpOutput();
         setExecuting(false);
         periodicTimer.cancel();
         clips.removePeriodicCallback(periodicName);      
         callExecutionCommandListeners(executingCommand,CommandExecutionEvent.FINISH_EVENT);
         */
        }
        
      /*****************/
      /* CommandDoWork */
      /*****************/
      private void CommandDoWork(object sender, DoWorkEventArgs e)
         {
          DoExecuteCommand(executingCommand);
          //BackgroundWorker worker = sender as BackgroundWorker;

          //RunExample(worker,e,autoEnv);
         }
         
      /************************/
      /* CommandWorkCompleted */
      /************************/
      private void CommandWorkCompleted(object sender, RunWorkerCompletedEventArgs e)
         {
          if (e.Error != null)
            { MessageBox.Show(e.Error.Message); }
          else if (e.Cancelled)
            { /* Do Nothing */ }

          SetExecuting(false);
         }

      /******************/
      /* ExecuteCommand */
      /******************/  
      private void ExecuteCommand()
        {
         executingCommand = clips.GetInputBuffer();      
         clips.AppendToDribble(executingCommand + "\n");
         SetExecuting(true);

         this.commandBackgroundWorker.WorkerSupportsCancellation = true;
         this.commandBackgroundWorker.DoWork += new System.ComponentModel.DoWorkEventHandler(CommandDoWork);
         this.commandBackgroundWorker.RunWorkerCompleted += new System.ComponentModel.RunWorkerCompletedEventHandler(CommandWorkCompleted);

         commandBackgroundWorker.RunWorkerAsync();
        }

      /*******************/
      /* ChangeDirectory */
      /*******************/  
      public bool ChangeDirectory(
        String newDirectory)
        {
         return clips.ChangeDirectory(newDirectory);
        }     
  
      /************************/
      /* UpdateCommandHistory */
      /************************/  
      private void UpdateCommandHistory()
        {
         /*=================================================*/
         /* Replace the first command with the contents of  */
         /* the command string, up to but not including the */ 
         /* last carriage return which initiated execution  */
         /* of the command. Removing the last carriage      */
         /* will prevent the command from being immediately */
         /* executed when the command is recalled by the    */
         /* up/down arrow keys (i.e. the user must hit the  */
         /* final carriage return again to execute the      */
         /* recalled command).                              */
         /*=================================================*/

         String theCommand = clips.GetInputBuffer();
      
         int length = theCommand.Length;
         int i, lastCR;
   
         for (i = 0, lastCR = length; i < length; i++)
           {
            if (theCommand[i] == '\r')
              { lastCR = i; }
           }   

         commandHistory[0] = theCommand.Substring(0, lastCR);
      
         /*====================================================*/
         /* If this command is identical to the prior command, */
         /* don't add it to the command history.               */
         /*====================================================*/
    
         if ((commandHistory.Count > 1) &&
             (commandHistory[0].Equals(commandHistory[1])))
           {
            commandHistory[0] = "";
            currentCommand = 0;
            return;
           }

         /*=================================================*/
         /* Add a new empty command to the top of the stack */
         /* in preparation for the next user command.       */
         /*=================================================*/

         commandHistory.Insert(0,"");
         currentCommand = 0;
         currentCommandCount++;
            
         /*=============================================*/
         /* Remove commands at the end of the command   */
         /* history if the maximum number of remembered */
         /* commands is exceeded.                       */
         /*=============================================*/

         while (commandHistory.Count > maxCommandCount)
           {
            commandHistory.RemoveAt(maxCommandCount);
            currentCommandCount--;
           }
        }
     
      /*****************/
      /* SwitchCommand */
      /*****************/  
      private void SwitchCommand(
        int oldCommand,
        int newCommand)
        {
         /*=============================================*/
         /* Remove the current command from the window. */
         /*=============================================*/

         String theCommand = clips.GetInputBuffer();
      
         int length = theCommand.Length;

         this.Text = this.Text.Substring(0,this.Text.Length - length);

         /*==============================================*/
         /* Replace the old command with the contents of */
         /* the command string, which will now include   */
         /* any edits the user made.                     */
         /*==============================================*/
      
         commandHistory[oldCommand] = theCommand;
         
         /*======================*/
         /* Use the new command. */
         /*======================*/

         clips.SetInputBuffer(commandHistory[newCommand]);
         this.AppendText(commandHistory[newCommand]);

         this.SelectionStart = this.Text.Length;
         this.SelectionLength = 0;
      
         currentCommand = newCommand;
        }

      /******************/
      /* ReplaceCommand */
      /******************/  
      public void ReplaceCommand(
        String newCommand)
        {
        /*=============================================*/
         /* Remove the current command from the window. */
         /*=============================================*/

         String theCommand = clips.GetInputBuffer();
      
         int length = theCommand.Length;

         this.Text = this.Text.Substring(0,this.Text.Length - length);

         /*======================*/
         /* Use the new command. */
         /*======================*/
   
         clips.SetInputBuffer(newCommand);
         this.AppendText(newCommand);
               
         /*==========================*/
         /* Process the new command. */
         /*==========================*/
      
         CommandCheck();
        }
              
      /*******************/
      /* UpdateSelection */
      /*******************/
      protected override void UpdateSelection()
        {
         if (GetExecuting())
           { 
            base.UpdateSelection(); 
            return;
           }
          
         /*==============================================*/
         /* Attempting to move the caret outside of the  */
         /* text for the current command is not allowed. */
         /*==============================================*/
            
         if (this.SelectionLength == 0) 
           { 
            int tl = this.Text.Length;
            int il = (int) clips.GetInputBuffer().Length;
               
            if (this.SelectionStart < (tl - il))
              { 
               if (oldDot < (tl - il))
                 { this.SelectionStart = tl; }
               else
                 { this.SelectionStart = oldDot; }
              }

            this.SetCaretVisible(true);
           }

         /*======================================*/
         /* If text is selected, hide the caret. */
         /*======================================*/
            
         else
           { this.SetCaretVisible(false); }

         oldDot = this.SelectionStart;
        }        

      /*#################*/
      /* TextBox Methods */
      /*#################*/
      
      /*********/
      /* OnCut */
      /*********/
      protected override void OnCut(
        object sender,
        ExecutedRoutedEventArgs e) 
        {
         if (GetExecuting())
           { 
            base.OnCut(sender,e); 
            return;
           }
        
         if (! this.HasCuttableSelection())
           { 
            e.Handled = true;
            return; 
           }

         this.Copy();
         ModifyCommand("",true);
         e.Handled = true;
        } 

      /***********/
      /* OnPaste */
      /***********/
      protected override void OnPaste(
        object sender, 
        DataObjectPastingEventArgs e)
        {
         Console.WriteLine("CommandPromptTextBox OnPaste");

         if (GetExecuting())
           { 
            base.OnPaste(sender,e); 
            return;
           }

        if (! this.HasPasteableSelection())
          { 
           e.CancelCommand();
           return; 
          }
         
         bool isText = e.SourceDataObject.GetDataPresent(DataFormats.UnicodeText, true);
         if (! isText) 
           {
            e.CancelCommand();
            return;
           }

         String clipboardText = e.SourceDataObject.GetData(DataFormats.UnicodeText) as string;
         ModifyCommand(clipboardText, false);
         e.CancelCommand();
        }
     }
  }
