package net.sf.clipsrules.jni;

import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.event.*;
import javax.swing.table.*;
import javax.swing.BorderFactory;
import javax.swing.text.DefaultCaret;

import java.awt.*; 
import java.awt.event.*; 
import java.awt.dnd.*;
import java.awt.datatransfer.*;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.lang.Byte;

public class RouterTextArea extends JTextArea
                            implements Router, KeyListener, CaretListener,
                                       FocusListener, DropTargetListener,
                                       ActionListener
  {   
   protected Environment clips;
   
   static final int bufferSize = 32768; 
   private static int TextAreaRouterNameIndex = 0;
   private String routerName;

   private boolean charNeeded = false;
   private List<Byte> charList = new ArrayList<Byte>();
   
   private StringBuffer outputBuffer = new StringBuffer(bufferSize);
   private int maxLines = 1000;
   
   private boolean hasFocus = false;
   
   private Timer dumpTimer = null;
   
   Border activeBorder;
   Border inactiveBorder;
   
   static final String dumpOutputAction = "dumpOutput";
    
   /******************/
   /* RouterTextArea */
   /******************/
   public RouterTextArea(
     Environment theEnv) 
     {  
      clips = theEnv;
      
      this.setEditable(false);
      this.setDragEnabled(true);
      this.addKeyListener(this);
      this.addFocusListener(this);
      
      this.setBorders(BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.white,3),
                                                          BorderFactory.createEmptyBorder(5,5,5,0)),
                      BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.darkGray,3),
                                                         BorderFactory.createEmptyBorder(5,5,5,0)));
      
      this.setFont(new Font("monospaced",Font.PLAIN,12));
      
      routerName = "JTextAreaRouter" + TextAreaRouterNameIndex++;
      clips.addRouter(this);
      
      this.setDropTarget(new DropTarget(this,this));

      ((DefaultCaret) this.getCaret()).setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE);
      this.addCaretListener(this);
     }  

   /*************/
   /* setBorder */
   /*************/
   public void setBorders(
     Border theInactiveBorder,
     Border theActiveBorder)
     {
      inactiveBorder = theInactiveBorder;
      activeBorder = theActiveBorder;
      if (this.hasFocus)
        { this.setBorder(activeBorder); }
      else
        { this.setBorder(inactiveBorder); }
     }
      
   /******************/
   /* setPlainBorder */
   /******************/
   public void setPlainBorder()
     {
      this.setBorders(BorderFactory.createEmptyBorder(5,5,5,0),
                      BorderFactory.createEmptyBorder(5,5,5,0));
     }
     
   /******************/
   /* getEnvironment */
   /******************/
   public Environment getEnvironment()
     {
      return clips;
     }
     
   /*########################*/
   /* Router Support Methods */
   /*########################*/
  
   /***************/
   /* appendChars */
   /***************/
   public synchronized boolean appendChars(
     String theString) 
     {
      if (! charNeeded) return false;
      
      try
        {
         byte theBytes [] = theString.getBytes("UTF-8");
         for (int i = 0; i < theBytes.length; i++)
           { charList.add(new Byte(theBytes[i])); }
        }
      catch (Exception e)
        { e.printStackTrace(); }
           
      charNeeded = false;
      notifyAll();
      return true;
     }   
     
   /************/
   /* pushChar */
   /************/
   public synchronized int pushChar(
     int theChar) 
     {
      charList.add(0,new Byte((byte) theChar));
      return theChar;
     }
     
   /****************/
   /* requestChar: */
   /****************/
   public synchronized int requestChar() 
     {
      /*====================================================*/
      /* If there's no input in the CLIPS internal buffer,  */
      /* but we have input remaining in the buffer for this */
      /* object, then clear this buffer since the remaining */
      /* input was extraneous for the last input request.   */
      /*====================================================*/

      if ((clips.getInputBufferCount() == 0) &&
          (charList.size() != 0))
        { charList.clear(); }
      
      if (charList.size() == 0)
        {
         charNeeded = true;
         while (charNeeded) 
           {
            try 
              { wait(); } 
            catch (InterruptedException e) 
              { e.printStackTrace(); }
           }
        }
        
      Byte theByte = charList.get(0);
      charList.remove(0);
      return theByte.intValue();
     }
     
   /***************/
   /* supplyChar: */
   /***************/
   public synchronized boolean supplyChar(
     char theChar) 
     {
      if (! charNeeded) return false;
      
      String charString = String.valueOf(theChar);
      try
        {
         byte theBytes [] = charString.getBytes("UTF-8");
         for (int i = 0; i < theBytes.length; i++)
           { charList.add(new Byte(theBytes[i])); }
        }
      catch (Exception e)
        { e.printStackTrace(); }
           
      charNeeded = false;
      notifyAll();
      return true;
     }
     
   /****************/
   /* hasSelection */
   /****************/
   public boolean hasSelection()
     {
      int left = Math.min(this.getCaret().getDot(),this.getCaret().getMark());
      int right = Math.max(this.getCaret().getDot(),this.getCaret().getMark());
      if (left == right) return false;
      return true;
     }
     
   /************************/
   /* hasCuttableSelection */
   /************************/
   public synchronized boolean hasCuttableSelection()
     {
      return false;
     }
     
   /*************************/
   /* hasPasteableSelection */
   /*************************/
   public synchronized boolean hasPasteableSelection()
     {
      if (! charNeeded) return false;
      
      int left = Math.min(getCaret().getDot(),getCaret().getMark());
      int right = Math.max(getCaret().getDot(),getCaret().getMark());
      
      if (left != right) return false;
      if (left != getText().length()) return false;
      
      return true;
     }
     
   /*########################*/
   /* JTextComponent Methods */
   /*########################*/

   /*********/
   /* paste */
   /*********/
   @Override
   public void paste()
     {
      if (! hasPasteableSelection())
        { return;}
      
      try
        {
         String clipboardText = (String) 
                                Toolkit.getDefaultToolkit()
                                       .getSystemClipboard()
                                       .getData(DataFlavor.stringFlavor); 
         if (appendChars(clipboardText))
            {
             dumpOutput();
        
             this.setCaretPosition(this.getText().length()); 
             this.append(clipboardText); 
            }
        }
      catch (Exception e)
        { e.printStackTrace(); }
     }

   /*################*/
   /* Router Methods */
   /*#################*/

   /****************/
   /* getPriority: */
   /****************/
   @Override
   public int getPriority()
     {
      return 10;
     }

   /************/
   /* getName: */
   /************/
   @Override
   public String getName()
     {
      return routerName;
     }

   /**********/
   /* query: */
   /**********/
   @Override
   public boolean query(
     String logName)
     {      
      if (logName.equals(STDOUT) ||
          logName.equals(STDIN) ||
          logName.equals(STDWRN) ||
          logName.equals(STDERR))
      
        { return true; }

      return false;
     }
     
   /****************/
   /* clearOutput: */
   /****************/
   private synchronized void clearOutput()
     {
      if (dumpTimer != null)
        { dumpTimer.stop(); }

      outputBuffer.setLength(0);
      outputBuffer.ensureCapacity(bufferSize);

      this.replaceRange("",0,this.getText().length());
      
      dumpTimer = null;
     }
     
   /**********/
   /* clear: */
   /**********/
   public void clear()
     {
      if (EventQueue.isDispatchThread())
        { 
         clearOutput();
         return; 
        }
        
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() { 
                                 clearOutput();
                                }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }

   /****************/
   /* createTimer: */
   /****************/
   private synchronized boolean createTimer(
     String printString)
     {
      outputBuffer.append(printString);
      
      if (dumpTimer == null)
        {
         dumpTimer = new Timer(100,this);
         dumpTimer.setRepeats(false);
         dumpTimer.setActionCommand(dumpOutputAction);
         dumpTimer.start();
         return true;
        }
        
      return false;
     }

   /***************/
   /* dumpOutput: */
   /***************/
   protected void dumpOutput()
     {
      if (outputBuffer.length() == 0)
        { return; }

      if (EventQueue.isDispatchThread())
        { 
         checkTimer();
         return; 
        }
        
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() { 
                                 checkTimer();
                                }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }
     
   /**********/
   /* write: */
   /**********/
   @Override
   public void write(
     String logName,
     String printString)
     {
      createTimer(printString);
     }

   /******************/
   /* checkLineCount */
   /******************/
   public void checkLineCount()
     {
      if (this.getLineCount() > maxLines)
        {
         try
           {
            int beginOffset = this.getLineStartOffset(0);
            int endOffset = this.getLineStartOffset(this.getLineCount() - maxLines);
            this.replaceRange("",beginOffset,endOffset);
           }
         catch (Exception e)
           { e.printStackTrace(); }
        }
     }

   /*********/
   /* read: */
   /*********/
   @Override
   public int read(
     String routerName)
     {
      return requestChar();
     }

   /***********/
   /* unread: */
   /***********/
   @Override
   public int unread(
     String routerName,
     int theChar)
     {
      return pushChar(theChar);
     }

   /*********/
   /* exit: */
   /*********/
   @Override
   public void exit(
     boolean failure)
     {      
     }
     
   /*#####################*/
   /* KeyListener Methods */
   /*#####################*/
   
   /**************/
   /* keyPressed */
   /**************/     
   @Override
   public void keyPressed(KeyEvent e) 
     {
      char theChar = e.getKeyChar();
      if ((theChar == KeyEvent.VK_BACK_SPACE) ||
          (theChar == KeyEvent.VK_DELETE))
        { e.consume(); }
     }

   /***************/
   /* keyReleased */
   /***************/     
   @Override
   public void keyReleased(KeyEvent e) 
     { 
      char theChar = e.getKeyChar();
      if ((theChar == KeyEvent.VK_BACK_SPACE) ||
          (theChar == KeyEvent.VK_DELETE))
        { e.consume(); }
     }

   /************/
   /* keyTyped */
   /************/
   @Override
   public void keyTyped(KeyEvent e) 
     {
      if ((e.getModifiers() & 
         (KeyEvent.ALT_MASK | KeyEvent.CTRL_MASK | KeyEvent.META_MASK)) != 0) 
        { return; }

      char theChar = e.getKeyChar();

      if ((theChar == KeyEvent.VK_BACK_SPACE) ||
          (theChar == KeyEvent.VK_DELETE))
        {
         if (clips.getInputBufferCount() == 0)
           {
            e.consume();
            return;
           }
        }
      
      dumpOutput();      
      if (supplyChar(theChar))
        {
         if ((theChar == KeyEvent.VK_BACK_SPACE) ||
             (theChar == KeyEvent.VK_DELETE))
           {
            this.replaceRange("",this.getText().length() - 1,this.getText().length()); 
           }
         else
           { this.append(String.valueOf(theChar)); }
         e.consume();
        } 
     }
 
   /*########################*/
   /* ActionListener Methods */
   /*########################*/

   /***************/
   /* checkTimer: */
   /***************/
   private synchronized boolean checkTimer()
     {
      if (dumpTimer != null)
        { dumpTimer.stop(); }

      this.append(outputBuffer.toString());
      checkLineCount();
      outputBuffer.setLength(0);
      outputBuffer.ensureCapacity(bufferSize);
      
      dumpTimer = null;
              
      return true;
     }

   /*******************/
   /* actionPerformed */
   /*******************/
   public void actionPerformed(ActionEvent e) 
     {
      if (e.getActionCommand().equals(dumpOutputAction))
        { checkTimer(); }
     }
      
   /*#######################*/
   /* FocusListener Methods */
   /*#######################*/

   /***************/
   /* focusGained */
   /***************/
   @Override
   public void focusGained(FocusEvent e)
     {
      hasFocus = true;
      this.setBorder(activeBorder);
      this.getCaret().setVisible(true);
     }

   /*************/
   /* focusLost */
   /*************/
   @Override
   public void focusLost(FocusEvent e)
     {
      hasFocus = false;
      this.setBorder(inactiveBorder);
      this.getCaret().setVisible(false);
     }

   /*############################*/
   /* DropTargetListener Methods */
   /*############################*/
   
   /*************/
   /* dragEnter */
   /*************/
   @Override
   public void dragEnter(DropTargetDragEvent dtde) 
     {
      boolean acceptedDrag = true;
      dragUnderFeedback(dtde,acceptedDrag);
     }
      
   /*************/
   /* dragOver */
   /*************/
   @Override
   public void dragOver(DropTargetDragEvent dtde) 
     {
      boolean acceptedDrag = acceptOrRejectDrag(dtde);
      dragUnderFeedback(dtde,acceptedDrag);
     }
     
   /*************/
   /* dragExit */
   /*************/ 
   @Override
   public void dragExit(DropTargetEvent dte) 
     {
      dragUnderFeedback(null, false);
     }
       
   /*********************/
   /* dropActionChanged */
   /*********************/
   @Override
   public void dropActionChanged(DropTargetDragEvent dtde)   
     { 
      boolean acceptedDrag = acceptOrRejectDrag(dtde);
      dragUnderFeedback(dtde,acceptedDrag);
     }
      
   /********/
   /* drop */
   /********/
   @Override
   public void drop(DropTargetDropEvent dtde) 
     { 
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
               if (appendChars(dropText))
                 {
                  dumpOutput();
                  this.setCaretPosition(this.getText().length()); 
                  this.append(dropText); 
                 }
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
     
   /*********************/
   /* dragUnderFeedback */
   /*********************/
   protected void dragUnderFeedback(
     DropTargetDragEvent dtde,
     boolean acceptedDrag) 
     {
      if ((dtde != null) && acceptedDrag) 
        {
         Point location = dtde.getLocation();
         this.getCaret().setVisible(true);
         this.setCaretPosition(this.viewToModel(location));
        } 
      else 
        { this.getCaret().setVisible(false); }
     }

   /*********************/
   /* checkTransferType */
   /*********************/
   protected boolean checkTransferType(
     DropTargetDragEvent dtde)
     {
      if (dtde.isDataFlavorSupported(DataFlavor.stringFlavor))
        { return true; }
        
      return false;
     }
     
   /************************/
   /* dragTargetCaretStart */
   /************************/
   protected int dragTargetCaretStart()
     {
      return this.getText().length();
     }
     
   /**********************/
   /* dragTargetCaretEnd */
   /**********************/
   protected int dragTargetCaretEnd()
     {
      return this.getText().length();
     }
     
   /**********************/
   /* acceptOrRejectDrag */
   /**********************/
   protected boolean acceptOrRejectDrag(
     DropTargetDragEvent dtde) 
     {
      int dropAction = dtde.getDropAction();
      int sourceActions = dtde.getSourceActions();
      
      Point location = dtde.getLocation();
      int caretPosition = this.viewToModel(location);
            
      /*===============================================*/
      /* Drag is only valid in a specific caret range. */
      /*===============================================*/
      
      if ((caretPosition < dragTargetCaretStart()) ||
          (caretPosition > dragTargetCaretEnd()))
        {
         dtde.rejectDrag(); 
         return false;
        }
        
      /*====================================*/
      /* Reject anything that's not a valid */
      /* type (in this case a text string). */
      /*====================================*/
      
      if (! checkTransferType(dtde))
        { 
         dtde.rejectDrag(); 
         return false;
        }
      
      /*===========================*/
      /* Only copy and move source */
      /* actions are acceptable.   */
      /*===========================*/
      
      if ((sourceActions & DnDConstants.ACTION_COPY_OR_MOVE) == 0)
        { 
         dtde.rejectDrag(); 
         return false;
        }
        
      if ((dropAction & DnDConstants.ACTION_COPY_OR_MOVE) == 0) 
        {
         dtde.rejectDrag();
         return false;
        } 
      
      /*==================*/
      /* Accept the drag. */
      /*==================*/
      
      dtde.acceptDrag(dropAction);
      return true;
     }
     
   /*#######################*/
   /* CaretListener Methods */
   /*#######################*/

   /***************/
   /* caretUpdate */
   /***************/  
   public void caretUpdate(
     final CaretEvent ce) 
     {
      if (EventQueue.isDispatchThread())
        { 
         caretUpdateAction(ce.getDot(), ce.getMark()); 
         return;
        }
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() 
                 { RouterTextArea.this.caretUpdateAction(ce.getDot(),ce.getMark()); }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }

   /*********************/
   /* caretUpdateAction */
   /*********************/  
   protected void caretUpdateAction(
     final int dot,
     final int mark) 
     {
      /*==============================================*/
      /* Attempting to move the caret outside of the  */
      /* text for the current command is not allowed. */
      /*==============================================*/
            
      if (dot == mark) 
        { 
         int tl = this.getText().length();
               
         if (dot < tl)
           { this.getCaret().setDot(tl); }

         if (hasFocus)
           { this.getCaret().setVisible(true); }
        }
              
      /*======================================*/
      /* If text is selected, hide the caret. */
      /*======================================*/
            
      else
        { this.getCaret().setVisible(false); }
     }    
  }
