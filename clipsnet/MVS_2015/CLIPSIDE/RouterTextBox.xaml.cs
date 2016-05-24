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
using CLIPSNET;

namespace CLIPSIDE
  {
   public partial class RouterTextBox : TextBox
     {
      /*@@@@@@@@@@@@@@@@@@@@*/
      /* RouterThreadBridge */
      /*@@@@@@@@@@@@@@@@@@@@*/

      private class RouterThreadBridge
        {
         public bool charNeeded = false;
         public bool closed = false;
         public List<Byte> charList = new List<Byte>();
        }

      /*@@@@@@@@@@@@@@@@@@@@*/
      /* RouterThreadBridge */
      /*@@@@@@@@@@@@@@@@@@@@*/

      private class TextBoxRouter : Router
        {         
         delegate void AddTextCallback(string text);

         static int RouterTextBoxNameIndex = 0;
         private RouterTextBox m_RouterTextBox;
         public String routerName;
           
         /*****************/
         /* TextBoxRouter */
         /*****************/

         public TextBoxRouter(
           RouterTextBox theTextBox)
           {
            m_RouterTextBox = theTextBox;
            routerName = "RouterTextBox" + RouterTextBoxNameIndex++;
           }
           
         /*********/
         /* Query */
         /*********/

         public override bool Query(String logicalName)
            {
             if (logicalName.Equals(CLIPSNET.Router.STANDARD_OUTPUT) ||
                 logicalName.Equals(CLIPSNET.Router.STANDARD_INPUT) ||
                 logicalName.Equals(CLIPSNET.Router.DIALOG) ||
                 logicalName.Equals(CLIPSNET.Router.DISPLAY) ||
                 logicalName.Equals(CLIPSNET.Router.ERROR) ||
                 logicalName.Equals(CLIPSNET.Router.PROMPT) ||
                 logicalName.Equals(CLIPSNET.Router.TRACE) ||
                 logicalName.Equals(CLIPSNET.Router.WARNING))
               return true;
             else
               return false;
            }
         
         /***********/
         /* AddText */
         /***********/
         
         public void AddText(string text)
		     {
            /*=====================================================*/
            /* If we're attempting to modify the control from a    */
            /* different thread from the one that created it, then */
            /* we need to use Invoke to handle the modification.   */
            /*=====================================================*/

           if (! m_RouterTextBox.Dispatcher.CheckAccess())
			     {	
			      AddTextCallback d = new AddTextCallback(AddText);
               m_RouterTextBox.Dispatcher.Invoke(d, new object[] { text });
			     }
              
            /*===================================*/
            /* Otherwise the thread that created */
            /* it can process the modification.  */
            /*===================================*/

			   else
			     { 
               m_RouterTextBox.AppendText(text); 
               m_RouterTextBox.Select(m_RouterTextBox.Text.Length,m_RouterTextBox.Text.Length);
               m_RouterTextBox.ScrollToEnd();
              }
           }    
                
         /*********/
         /* Print */
         /*********/

         public override void Print(String logicalName, String printString)
           {
            this.AddText(printString); 
           }
       
         /********/
         /* Getc */
         /********/
         
         public override int Getc(String logicalName)
           {
            RouterThreadBridge theBridge = this.m_RouterTextBox.m_ThreadBridge;

            lock (theBridge)
              {
               if (theBridge.closed) 
                 {                   
                  this.m_RouterTextBox.attachedEnv.SetHaltExecution(true);
                  return -1; 
                 }

               if (theBridge.charList.Count == 0)
                 {
                  theBridge.charNeeded = true;
                  try
                    { 
                     Monitor.Wait(theBridge); 
                    }
                  catch (SynchronizationLockException e)
                    { Console.WriteLine(e); }
                  catch (ThreadInterruptedException e)
                    { Console.WriteLine(e); }
                 }

               theBridge.charNeeded = false;
               if (theBridge.closed) 
                 {
                  this.m_RouterTextBox.attachedEnv.SetHaltExecution(true);
                  return -1; 
                 }

               Byte theByte = theBridge.charList[0];
               theBridge.charList.RemoveAt(0);

               return theByte;
              }
           }
    
         /**********/
         /* Ungetc */
         /**********/

         public override int Ungetc(String logicalName,int theChar)
           {
            lock (this.m_RouterTextBox.m_ThreadBridge)
              {
               this.m_RouterTextBox.m_ThreadBridge.charList.Insert(0,(Byte) theChar);
              }
            return 0;
           }
        }

      /*@@@@@@@@@@@@@@@*/
      /* RouterTextBox */
      /*@@@@@@@@@@@@@@@*/
      
      private CLIPSNET.Environment attachedEnv;
      private TextBoxRouter m_TextBoxRouter;
      private RouterThreadBridge m_ThreadBridge;
      private bool selectionWasChanged = false;
      
      /*****************/
      /* RouterTextBox */
      /*****************/

      public RouterTextBox() : base()
        {
         m_TextBoxRouter = new TextBoxRouter(this);
         m_ThreadBridge = new RouterThreadBridge();
         this.AcceptsReturn = true;
         this.IsReadOnly = false;
         DataObject.AddPastingHandler(this,OnPaste);
         CommandBindings.Add(new CommandBinding(ApplicationCommands.Cut,OnCut)); 
         this.SelectionChanged += new RoutedEventHandler(TextBoxSelectionChanged);
         this.PreviewMouseLeftButtonUp += new MouseButtonEventHandler(TextBox_PreviewMouseLeftButtonUp);
        }
      
      /****************/
      /* AttachRouter */
      /****************/
      public void AttachRouter(
        CLIPSNET.Environment theEnv,
        int priority)
        {
         attachedEnv = theEnv;
         theEnv.AddRouter(m_TextBoxRouter.routerName,priority,m_TextBoxRouter);
        }
        
      /****************/
      /* DetachRouter */
      /****************/
      public void DetachRouter()
        {
        }
 
      /*************/
      /* OnClosing */
      /*************/
      public void OnClosing()
        {
         lock (m_ThreadBridge)
           {               
            m_ThreadBridge.closed = true;
            if (m_ThreadBridge.charNeeded)
              {
               m_ThreadBridge.charNeeded = false;
               Monitor.Pulse(m_ThreadBridge);
              }
           }
        }

      /**************/
      /* OnDragOver */
      /**************/
      protected override void OnDragOver(DragEventArgs e)
        {
         e.Effects = DragDropEffects.None;

         if (e.Data.GetDataPresent(DataFormats.UnicodeText))
           {
            bool isText = e.Data.GetDataPresent(DataFormats.UnicodeText, true);
            if (isText)
              { e.Effects = DragDropEffects.Copy; }
           }
        }

      /**********/
      /* OnDrop */
      /**********/
      protected override void OnDrop(DragEventArgs e)
        {
         e.Effects = DragDropEffects.None;

         if (e.Data.GetDataPresent(DataFormats.UnicodeText))
           {
            bool isText = e.Data.GetDataPresent(DataFormats.UnicodeText, true);
            if (isText)
              { e.Effects = DragDropEffects.Copy; }
           }

         base.OnDrop(e);
        }

      /*********/
      /* OnCut */
      /*********/
      protected virtual void OnCut(object sender, ExecutedRoutedEventArgs e) 
        {
         e.Handled = true; 
        } 

      /***********/
      /* OnPaste */
      /***********/
      protected virtual void OnPaste(object sender, DataObjectPastingEventArgs e)
        {
         Console.WriteLine("RouterTextBox OnPaste");
         bool isText = e.SourceDataObject.GetDataPresent(DataFormats.UnicodeText, true);
         if (! isText) return;

         lock (m_ThreadBridge)
           {
            if (m_ThreadBridge.charNeeded)
              { 
               String text = e.SourceDataObject.GetData(DataFormats.UnicodeText) as string;
               m_ThreadBridge.charList.AddRange(Encoding.UTF8.GetBytes(text));
               this.Select(this.Text.Length,this.Text.Length);
               this.ScrollToEnd();
               m_ThreadBridge.charNeeded = false;
               Monitor.Pulse(m_ThreadBridge);
              }
            else
              { e.CancelCommand(); }
           }
        }

      /********************/
      /* OnPreviewKeyDown */
      /********************/
      protected override void OnPreviewKeyDown(KeyEventArgs e)
        {
         if (e.Key == Key.Return)
           {
            lock (m_ThreadBridge)
              {
               if (m_ThreadBridge.charNeeded)
                 { 
                  m_ThreadBridge.charList.AddRange(Encoding.UTF8.GetBytes("\n"));
                  this.Select(this.Text.Length,this.Text.Length);
                  this.ScrollToEnd();
                  m_ThreadBridge.charNeeded = false;
                  Monitor.Pulse(m_ThreadBridge);
                 }
               else
                 { e.Handled = true; }
              }
           }
         else if (e.Key == Key.Space)
           {
            lock (m_ThreadBridge)
              {
               if (m_ThreadBridge.charNeeded)
                 { 
                  m_ThreadBridge.charList.AddRange(Encoding.UTF8.GetBytes(" "));
                  this.Select(this.Text.Length,this.Text.Length);
                  this.ScrollToEnd();
                  m_ThreadBridge.charNeeded = false;
                  Monitor.Pulse(m_ThreadBridge);
                 }
               else
                 { e.Handled = true; }
              }
           }
         else if ((e.Key == Key.Back) || (e.Key == Key.Delete))
           {            
            lock (m_ThreadBridge)
              {
               if ((m_ThreadBridge.charNeeded) && (attachedEnv.InputBufferCount() > 0))
                 {
                  m_ThreadBridge.charList.AddRange(Encoding.UTF8.GetBytes("\b"));
                  this.Select(this.Text.Length,this.Text.Length);
                  this.ScrollToEnd();
                  m_ThreadBridge.charNeeded = false;
                  Monitor.Pulse(m_ThreadBridge);
                 }
               else
                 { e.Handled = true; }
              }
           }

         base.OnPreviewKeyDown(e);
        }

      /***************/
      /* OnTextInput */
      /***************/
      protected override void OnTextInput(TextCompositionEventArgs e)
        {
         lock (m_ThreadBridge)
           {
            if (m_ThreadBridge.charNeeded)
              { 
               m_ThreadBridge.charList.AddRange(Encoding.UTF8.GetBytes(e.Text));
               this.Select(this.Text.Length,this.Text.Length);
               this.ScrollToEnd();
               m_ThreadBridge.charNeeded = false;
               Monitor.Pulse(m_ThreadBridge);
               base.OnTextInput(e);
              }
           }
        }

      /***************************/
      /* TextBoxSelectionChanged */
      /***************************/
      private void TextBoxSelectionChanged(object sender, RoutedEventArgs e)
        {
         if (System.Windows.Input.Mouse.LeftButton != MouseButtonState.Pressed)
           { UpdateSelection(); }
           else
           { selectionWasChanged = true; }
        }    
        
      /************************************/
      /* TextBox_PreviewMouseLeftButtonUp */
      /************************************/
      private void TextBox_PreviewMouseLeftButtonUp(object sender, MouseButtonEventArgs e)
        {
         if (selectionWasChanged)
           {
            selectionWasChanged = false; 
            UpdateSelection(); 
           }
        }    

      /*******************/
      /* UpdateSelection */
      /*******************/
      protected virtual void UpdateSelection()
        {
         /*==============================================*/
         /* Attempting to move the caret outside of the  */
         /* text for the current command is not allowed. */
         /*==============================================*/
            
         if (this.SelectionLength == 0) 
           { 
            int tl = this.Text.Length;
               
            if (this.SelectionStart < tl)
              { this.SelectionStart = tl; }

            this.SetCaretVisible(true); 
           }
              
         /*======================================*/
         /* If text is selected, hide the caret. */
         /*======================================*/
            
         else
           { this.SetCaretVisible(false); }
        }
      
      /*******************/
      /* SetCaretVisible */
      /*******************/
      protected void SetCaretVisible(
        bool value)
        {
         if (value)
           { this.CaretBrush = null; }
         else 
           { this.CaretBrush =  new SolidColorBrush(Color.FromArgb(0, 0, 0, 0)); }
        }  
     }
  }
