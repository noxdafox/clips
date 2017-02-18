package net.sf.clipsrules.jni;

import java.awt.*; 
import java.awt.event.*; 
import java.awt.datatransfer.*;
import java.awt.dnd.*;

import javax.swing.*;
import javax.swing.event.*; 

import java.lang.Thread;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class CommandPromptTextArea extends RouterTextArea
                                   implements PeriodicCallback
  {   
   private class PeriodicTask extends TimerTask 
     {
      public void run()
        {
         clips.setPeriodicCallbackEnabled(true);
        }
     }

   static final int periodicTaskFrequency = 200;
   
   static final int DEFAULT_COMMAND_MAX = 25;

   private boolean isExecuting = false;
   
   int oldDot = 0;
   
   int maxCommandCount;
   int currentCommandCount;
   int currentCommand;
   
   ArrayList<String> commandHistory;

   EventListenerList listenerList = new EventListenerList();
    
   private static int CommandPromptTextAreaIndex = 0;
   private String periodicName;
   
   private String executingCommand = "";
   private boolean paused = false;
          
   private ReentrantReadWriteLock pauseLock = new ReentrantReadWriteLock();
   
   /*************************/
   /* CommandPromptTextArea */
   /*************************/
   public CommandPromptTextArea(
     Environment theEnv) 
     {  
      super(theEnv);
      
      this.setPlainBorder();
      theEnv.printBanner();
      theEnv.printPrompt();
      theEnv.setInputBufferCount(0);
            
      this.getCaret().setVisible(true);
      this.addCaretListener(this);

      maxCommandCount = DEFAULT_COMMAND_MAX;
      currentCommandCount = 1;
      currentCommand = 0;

      commandHistory = new ArrayList<String>(DEFAULT_COMMAND_MAX); 
      commandHistory.add(new String(""));
      
      periodicName = "CPTAP" + CommandPromptTextAreaIndex++;    
     }  

   /*************/
   /* setPaused */
   /*************/
   public void setPaused(
      boolean value)
      {
       if (paused == value) return;
       
       paused = value;
       
       if (paused)
         { pauseLock.writeLock().lock(); }
       else
         { pauseLock.writeLock().unlock(); }       
      }

   /************/
   /* isPaused */
   /************/
   public boolean isPaused()
      {
       return paused;
      }
      
   /*******************************/
   /* addCommandExecutionListener */
   /*******************************/
   public void addCommandExecutionListener(
     CommandExecutionListener theListener) 
     {
      listenerList.add(CommandExecutionListener.class,theListener);
     }     

   /**********************************/
   /* removeCommandExecutionListener */
   /**********************************/
   public void removeCommandExecutionListener(
     CommandExecutionListener theListener) 
     {
      listenerList.remove(CommandExecutionListener.class,theListener);
     }     

   /*********************************/
   /* callExecutionCommandListeners */
   /*********************************/
   public void callExecutionCommandListeners(
     String command,
     String event)
     {
      Object[] listeners = listenerList.getListenerList();
      CommandExecutionEvent theEvent = null;
      
      for (int i = listeners.length-2; i>=0; i-=2) 
        {
         if (listeners[i] == CommandExecutionListener.class) 
           {
            if (theEvent == null)
              { theEvent = new CommandExecutionEvent(this,command,event); }
            ((CommandExecutionListener) listeners[i+1]).commandExecutionEventOccurred(theEvent);
           }
        }
     }
     
   /**************/
   /* keyPressed */
   /**************/
   @Override
   public void keyPressed(KeyEvent e) 
     {
      if (getExecuting())
        { 
         super.keyPressed(e); 
         return;
        }
        
      if ((e.getModifiers() & (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) return;

      int kc = e.getKeyCode();
      
      if ((kc == KeyEvent.VK_UP) || (kc == KeyEvent.VK_KP_UP))
        { 
         if ((currentCommand + 1) < commandHistory.size())
           {
            if ((e.getModifiers() & KeyEvent.SHIFT_MASK) != 0)
              { switchCommand(currentCommand,commandHistory.size() - 1); }
            else
              { switchCommand(currentCommand,currentCommand + 1); }
          }
         e.consume();
        }
      else if ((kc == KeyEvent.VK_DOWN) || (kc == KeyEvent.VK_KP_DOWN))
        { 
         if (currentCommand != 0)
          {
           if ((e.getModifiers() & KeyEvent.SHIFT_MASK) != 0)
             { switchCommand(currentCommand,0); }
           else
             { switchCommand(currentCommand,currentCommand - 1); }
          }
         e.consume();
        } 
      else if (kc == KeyEvent.VK_ESCAPE)
        {
         if ((e.getModifiers() & KeyEvent.SHIFT_MASK) == 0)
           { this.getCaret().setDot(this.getText().length()); }
         else
           { this.getCaret().setDot(this.getText().length() - clips.getInputBuffer().length()); }
         e.consume();
        }     
      else
        { super.keyPressed(e); }  
     }

   /************/
   /* keyTyped */
   /************/
   @Override
   public void keyTyped(KeyEvent e) 
     {      
      if (getExecuting())
        { 
         super.keyTyped(e); 
         return;
        }

      if ((e.getModifiers() & 
          (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) 
        { return; }
          
      char c = e.getKeyChar();
         
      if ((c == KeyEvent.VK_BACK_SPACE) ||
          (c == KeyEvent.VK_DELETE))
        { modifyCommand("",true); }
      else if (c == KeyEvent.VK_ESCAPE)
        { /* Do nothing */ }
      else 
        {
         modifyCommand(String.valueOf(c),false);
         commandCheck();
        }
                    
      e.consume();
     }
    
   /************************/
   /* hasCuttableSelection */
   /************************/
   public synchronized boolean hasCuttableSelection()
     {
      if (getExecuting())
        { return super.hasCuttableSelection(); }

      int textLength = this.getText().length();
      int commandLength = (int) clips.getInputBuffer().length();  
      int lockedLength = textLength - commandLength;
      
      int left = Math.min(this.getCaret().getDot(),this.getCaret().getMark());
      int right = Math.max(this.getCaret().getDot(),this.getCaret().getMark());
      
      if (left == right) return false;
      
      if (left < lockedLength)
        { return false; }
      
      return true;
     }

   /*************************/
   /* hasPasteableSelection */
   /*************************/
   public synchronized boolean hasPasteableSelection()
     {
      if (getExecuting())
        { return super.hasPasteableSelection(); }

      int textLength = this.getText().length();
      int commandLength = (int) clips.getInputBuffer().length();  
      int lockedLength = textLength - commandLength;
      
      int left = Math.min(this.getCaret().getDot(),this.getCaret().getMark());
            
      if (left < lockedLength)
        { return false; }
      
      return true;
     }
      
   /*****************/
   /* modifyCommand */
   /*****************/
   protected void modifyCommand(
     String replaceString,
     boolean isDelete) 
     {
      int textLength = this.getText().length();
      int commandLength = (int) clips.getInputBuffer().length();  
      int lockedLength = textLength - commandLength;

      /*========================================*/
      /* Determine the left and right positions */
      /* of the current selection.              */
      /*========================================*/
            
      int left = Math.min(this.getCaret().getDot(),this.getCaret().getMark());
      int right = Math.max(this.getCaret().getDot(),this.getCaret().getMark());
      
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
         this.getCaret().setDot(textLength); 
         return;
        }
                    
      String newCommand = this.getText().substring(lockedLength,left) + 
                          replaceString +
                          this.getText().substring(right);
      this.replaceRange(replaceString,left,right);
      
      clips.setInputBuffer(newCommand);   
            
      balanceParentheses();
     }
     
   /**********************/
   /* balanceParentheses */
   /**********************/
   public void balanceParentheses() 
     {
      long commandLength;
   
      /*=======================================================*/
      /* Don't balance the parentheses if there is no command. */
      /*=======================================================*/
   
      commandLength = clips.getInputBuffer().length();
      if (commandLength <= 0) 
        { return; }

      /*=================================*/
      /* Retrieve the current selection. */
      /*=================================*/
          
      int selStart = this.getSelectionStart();
      int selEnd = this.getSelectionEnd();

      /*======================*/
      /* Where is the cursor? */
      /*======================*/
    
      int cursorLocation = this.getCaretPosition();
   
      if (cursorLocation == 0) return;
   
      cursorLocation--;
      
      /*===============================================*/
      /* What is the character at the cursor location? */
      /*===============================================*/
    
      char characterToCheck = this.getText().charAt(cursorLocation);
      
      /*======================================*/
      /* We only balance a right parenthesis. */
      /*======================================*/
   
      if (characterToCheck != ')') return;

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
         characterToCheck = this.getText().charAt(cursorLocation);
         if (characterToCheck == '(') 
           {
            if (nestingDepth == 0) 
              {
               /*======================================*/
               /* Select the matching left parenthesis */
               /* and hide the caret.                  */
               /*======================================*/
               
               this.getCaret().setVisible(false);
               this.setSelectionStart(cursorLocation);
               this.setSelectionEnd(cursorLocation + 1);

               /*========================================*/
               /* Force an update to occur otherwise the */
               /* changed selection won't be visible.    */
               /*========================================*/
               
               this.update(this.getGraphics());

               /*============================================*/
               /* Pause momentarily so the selected matching */
               /* parenthesis can be observed.               */
               /*============================================*/
               
               try
		         { Thread.sleep(200); }
		       catch (Exception e)
		         { e.printStackTrace(); }

               /*===========================*/
               /* Restore the selection and */
               /* make the caret visible.   */
               /*===========================*/
               
               this.setSelectionStart(selStart);
               this.setSelectionEnd(selEnd);
               this.getCaret().setVisible(true);
		       
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

      Toolkit.getDefaultToolkit().beep();
     }
     
   /*********************/
   /* expandInputBuffer */
   /*********************/
   public void expandInputBuffer(
     char theChar)
     {
      String charString = String.valueOf(theChar);
      try
        {
         byte theBytes [] = charString.getBytes("UTF-8");
         for (int i = 0; i < theBytes.length; i++)
           { clips.expandInputBuffer(Byte.valueOf(theBytes[i]).intValue()); }
        }
      catch (Exception e)
        { e.printStackTrace(); }
     }
     
   /****************/
   /* commandCheck */
   /****************/
   private void commandCheck() 
     {
      if (clips.inputBufferContainsCommand())
        { 
         updateCommandHistory();
         executeCommand(); 
        }
     }
         
   /****************/
   /* getExecuting */
   /****************/
   public synchronized boolean getExecuting() 
     {
      return isExecuting;
     }

   /****************/
   /* setExecuting */
   /****************/
   public synchronized void setExecuting(
     boolean value) 
     {
      isExecuting = value;
     }
   
   /********************/
   /* doExecuteCommand */
   /********************/  
   private void doExecuteCommand(
     String executingCommand)
     {
      Timer periodicTimer = new Timer();

      clips.appendDribble(executingCommand);
      callExecutionCommandListeners(executingCommand,CommandExecutionEvent.START_EVENT);
      clips.addPeriodicCallback(periodicName,0,this);      
      periodicTimer.schedule(new PeriodicTask(),0,periodicTaskFrequency);
      clips.commandLoopOnceThenBatch(); 
      dumpOutput();
      setExecuting(false);
      periodicTimer.cancel();
      clips.removePeriodicCallback(periodicName);      
      callExecutionCommandListeners(executingCommand,CommandExecutionEvent.FINISH_EVENT);
     }
         
   /******************/
   /* executeCommand */
   /******************/  
   private void executeCommand()
     {
      executingCommand = clips.getInputBuffer();      
      setExecuting(true);

      Runnable runThread = 
         new Runnable()
           {
            public void run() 
              { doExecuteCommand(executingCommand); }
           };
      
      Thread executionThread = new Thread(runThread);
      
      executionThread.start();
     }

   /******************/
   /* doExecuteBatch */
   /******************/  
   private void doExecuteBatch()
     {
      Timer periodicTimer = new Timer();

      callExecutionCommandListeners("batch",CommandExecutionEvent.START_EVENT);
      clips.addPeriodicCallback(periodicName,0,this);      
      periodicTimer.schedule(new PeriodicTask(),0,periodicTaskFrequency);
      clips.commandLoopBatchDriver(); 
      dumpOutput();
      setExecuting(false);
      periodicTimer.cancel();
      clips.removePeriodicCallback(periodicName);      
      callExecutionCommandListeners("batch",CommandExecutionEvent.FINISH_EVENT);
     }
         
   /****************/
   /* executeBatch */
   /****************/  
   public void executeBatch()
     {
      setExecuting(true);

      Runnable runThread = 
         new Runnable()
           {
            public void run() 
              { doExecuteBatch(); }
           };
      
      Thread executionThread = new Thread(runThread);
      
      executionThread.start();
     }
   
   /************************/
   /* updateCommandHistory */
   /************************/  
   private void updateCommandHistory()
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

      String theCommand = clips.getInputBuffer();
      
      int length = theCommand.length();
      int i, lastCR;
   
      for (i = 0, lastCR = length; i < length; i++)
        {
         if (theCommand.charAt(i) == '\n')
           { lastCR = i; }
        }   

      commandHistory.set(0,theCommand.substring(0,lastCR));
      
      /*====================================================*/
      /* If this command is identical to the prior command, */
      /* don't add it to the command history.               */
      /*====================================================*/
    
      if ((commandHistory.size() > 1) &&
          (commandHistory.get(0).equals(commandHistory.get(1))))
        {
         commandHistory.set(0,new String(""));
         currentCommand = 0;
         return;
        }

      /*=================================================*/
      /* Add a new empty command to the top of the stack */
      /* in preparation for the next user command.       */
      /*=================================================*/

      commandHistory.add(0,new String(""));
      currentCommand = 0;
      currentCommandCount++;
            
      /*=============================================*/
      /* Remove commands at the end of the command   */
      /* history if the maximum number of remembered */
      /* commands is exceeded.                       */
      /*=============================================*/
   
      while (commandHistory.size() > maxCommandCount)
        {
         commandHistory.remove(maxCommandCount);
         currentCommandCount--;
        }
     }
     
   /*****************/
   /* switchCommand */
   /*****************/  
   private void switchCommand(
     int oldCommand,
     int newCommand)
     {
      /*=============================================*/
      /* Remove the current command from the window. */
      /*=============================================*/

      String theCommand = clips.getInputBuffer();
      
      int length = theCommand.length();
      
      this.replaceRange("",this.getText().length() - length,this.getText().length());

      /*==============================================*/
      /* Replace the old command with the contents of */
      /* the command string, which will now include   */
      /* any edits the user made.                     */
      /*==============================================*/
      
      commandHistory.set(oldCommand,theCommand);
         
      /*======================*/
      /* Use the new command. */
      /*======================*/
   
      clips.setInputBuffer(commandHistory.get(newCommand));
      this.append(commandHistory.get(newCommand));
      
      currentCommand = newCommand;
     }

   /******************/
   /* replaceCommand */
   /******************/  
   public void replaceCommand(
     String newCommand)
     {
      /*=============================================*/
      /* Remove the current command from the window. */
      /*=============================================*/

      String theCommand = clips.getInputBuffer();
      
      int length = theCommand.length();
      
      this.replaceRange("",this.getText().length() - length,this.getText().length());

      /*======================*/
      /* Use the new command. */
      /*======================*/
   
      clips.setInputBuffer(newCommand);
      this.append(newCommand);
      
      /*==========================*/
      /* Process the new command. */
      /*==========================*/
      
      commandCheck();
     }
  
   /*########################*/
   /* JTextComponent Methods */
   /*########################*/

   /*******/
   /* cut */
   /*******/
   @Override
   public void cut()
     {
      if (getExecuting())
        { 
         super.cut(); 
         return;
        }

      if (! this.hasCuttableSelection())
        { return; }
        
      this.copy();
      modifyCommand("",true);
     }
     
   /*********/
   /* paste */
   /*********/
   @Override
   public void paste()
     {
      if (getExecuting())
        { 
         super.paste(); 
         return;
        }
        
      if (! this.hasPasteableSelection())
        { return; }
        
      try
        {
         String clipboardText = (String) 
                                Toolkit.getDefaultToolkit()
                                       .getSystemClipboard()
                                       .getData(DataFlavor.stringFlavor); 

         modifyCommand(clipboardText,false);
        }
      catch (Exception e)
        { e.printStackTrace(); }
     }
     
   /*#######################*/
   /* CaretListener Methods */
   /*#######################*/

   /*********************/
   /* caretUpdateAction */
   /*********************/  
   @Override
   protected void caretUpdateAction(
     final int dot,
     final int mark) 
     {      
      if (getExecuting())
        { 
         super.caretUpdateAction(dot,mark); 
         return;
        }

      removeCaretListener(this);
              
      /*==============================================*/
      /* Attempting to move the caret outside of the  */
      /* text for the current command is not allowed. */
      /*==============================================*/
            
      if (dot == mark) 
        { 
         int tl = this.getText().length();
         int il = (int) clips.getInputBuffer().length();
               
         if (dot < (tl - il))
           { 
            if (oldDot < (tl - il))
              { this.getCaret().setDot(tl); }
            else
              { this.getCaret().setDot(oldDot); }
           }

         this.getCaret().setVisible(true);
        }
              
      /*======================================*/
      /* If text is selected, hide the caret. */
      /*======================================*/
            
      else
        { this.getCaret().setVisible(false); }
              
      oldDot = this.getCaret().getMark();
      
      addCaretListener(this);
     }  
       
   /*############################*/
   /* DropTargetListener Methods */
   /*############################*/
   
   /************************/
   /* dragTargetCaretStart */
   /************************/
   @Override
   protected int dragTargetCaretStart()
     {
      if (getExecuting())
        { return super.dragTargetCaretStart(); }

      return this.getText().length() - (int) clips.getInputBuffer().length();
     }
  
   /********/
   /* drop */
   /********/
   @Override
   public void drop(DropTargetDropEvent dtde) 
     { 
      if (getExecuting())
        { 
         super.drop(dtde); 
         return;
        }
        
      if ((dtde.getDropAction() & DnDConstants.ACTION_COPY_OR_MOVE) == 0) 
        {
         dtde.dropComplete(false);
         return; 
        }
        
      try 
        {
         Transferable tr = dtde.getTransferable();
         DataFlavor[] flavors = tr.getTransferDataFlavors();
         for (int i = 0; i < flavors.length; i++) 
           {
            if (flavors[i].equals(DataFlavor.stringFlavor)) 
              {
               dtde.acceptDrop(dtde.getDropAction());
               String dropText = (String) tr.getTransferData(flavors[i]);
               modifyCommand(dropText,false);
               this.requestFocus();
               dtde.dropComplete(true);
               return;
              }
           }

         dtde.rejectDrop();
        } 
      catch (Exception e) 
        {
         e.printStackTrace();
         dtde.rejectDrop();
        }
     }
     
   /*##########################*/
   /* PeriodicListener Methods */
   /*##########################*/
   
   /********************/
   /* periodicCallback */
   /********************/  
   public void periodicCallback()
     {
      /*=========================================================*/
      /* When the environment is paused, the writeLock is locked */
      /* so we won't be able to lock the readLock until the      */
      /* environment is not paused.                              */
      /*=========================================================*/
      
      pauseLock.readLock().lock(); 
      
      /*=============================================*/
      /* Once we've determined the environment is no */
      /* longer paused we can release the readLock.  */
      /*=============================================*/
      
      pauseLock.readLock().unlock(); 

      /*===========================================================*/
      /* Notify and CommandExecutionListeners of a periodic event. */
      /*===========================================================*/
      
      callExecutionCommandListeners(executingCommand,CommandExecutionEvent.PERIODIC_EVENT);
      
      /*==========================================================*/
      /* Disable periodic callbacks until the timer enables them  */
      /* again. This improves performance since the callback from */
      /* the native code is relatively expensive.                 */
      /*==========================================================*/
      
      clips.setPeriodicCallbackEnabled(false);
     }
  }
  
  
