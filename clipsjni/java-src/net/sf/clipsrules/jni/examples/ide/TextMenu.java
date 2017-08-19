package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.Toolkit;

import javax.swing.AbstractAction;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JTextArea;
import javax.swing.KeyStroke;

/*
import java.awt.Rectangle;
import javax.swing.text.BadLocationException;
*/

import net.sf.clipsrules.jni.*;

public class TextMenu extends JMenu 
                   implements MenuListener
  {  
   private TextFrame textFrame = null;
   private CLIPSIDE ide = null;

   private JMenuItem jmiLoadSelection;
   private JMenuItem jmiBatchSelection;
   private JMenuItem jmiLoadBuffer;
   private JMenuItem jmiBalance;
   private JMenuItem jmiComment;
   private JMenuItem jmiUncomment;

   private LoadSelectionAction loadSelectionAction;
   private BatchSelectionAction batchSelectionAction;
   private LoadBufferAction loadBufferAction;
   private BalanceAction balanceAction;
   private CommentAction commentAction;
   private UncommentAction uncommentAction;

   /************/
   /* TextMenu */
   /************/
   TextMenu(
     CLIPSIDE theIDE)
     {  
      super("Text");
        
      ide = theIDE;
      
      addMenuListener(this);
      
      /*==================================*/
      /* Get KeyStrokes for accelerators. */
      /*==================================*/

      KeyStroke loadSelection = KeyStroke.getKeyStroke(KeyEvent.VK_K,KeyEvent.CTRL_MASK);
      KeyStroke batchSelection = KeyStroke.getKeyStroke(KeyEvent.VK_K,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);
      KeyStroke balance = KeyStroke.getKeyStroke(KeyEvent.VK_B,KeyEvent.CTRL_MASK);

      /*================*/
      /* Setup actions. */
      /*================*/
       
      loadSelectionAction = new LoadSelectionAction("Load Selection");
      batchSelectionAction = new BatchSelectionAction("Batch Selection");
      loadBufferAction = new LoadBufferAction("Load Buffer");
      balanceAction = new BalanceAction("Balance");
      commentAction = new CommentAction("Comment");
      uncommentAction = new UncommentAction("Uncomment");

      /*=================*/
      /* Add menu items. */
      /*=================*/

      jmiLoadSelection = new JMenuItem(loadSelectionAction);
      jmiLoadSelection.setAccelerator(loadSelection);
      add(jmiLoadSelection);

      jmiBatchSelection = new JMenuItem(batchSelectionAction);
      jmiBatchSelection.setAccelerator(batchSelection);
      add(jmiBatchSelection);

      jmiLoadBuffer = new JMenuItem(loadBufferAction);
      add(jmiLoadBuffer);

      addSeparator();

      jmiBalance = new JMenuItem(balanceAction);
      jmiBalance.setAccelerator(balance);
      add(jmiBalance);

      jmiComment = new JMenuItem(commentAction);
      add(jmiComment);

      jmiUncomment = new JMenuItem(uncommentAction);
      add(jmiUncomment);
     }  

   /****************/
   /* setTextFrame */
   /****************/
   public void setTextFrame(
     TextFrame theFrame)
     {
      textFrame = theFrame;
     }
     
   /*################*/
   /* Action Methods */
   /*################*/
     
   /*****************/
   /* loadSelection */
   /*****************/  
   private void loadSelection()
     {
      if (textFrame == null)
        { return; }

      Environment clips = ide.getEnvironment();
      DialogFrame dialog = ide.getDialogWindow();
      
      if (dialog.isExecuting())
        { return; }
      
      JTextArea theTextArea = textFrame.getTextArea();
      String loadString = theTextArea.getSelectedText();
      if (loadString == null) return;
      
      ide.selectDialogWindow();
      
      clips.flushInputBuffer();
      clips.print(Router.STDOUT,"Loading Selection...\n");

      clips.loadFromStringWithOutput(loadString);

      clips.printPrompt();
     }
   
   /******************/
   /* batchSelection */
   /******************/  
   private void batchSelection()
     {
      if (textFrame == null)
        { return; }
       
      Environment clips = ide.getEnvironment();
      DialogFrame dialog = ide.getDialogWindow();
     
      if (dialog.isExecuting())
        { return; }
       
      JTextArea theTextArea = textFrame.getTextArea();
      String batchString = theTextArea.getSelectedText();
      if (batchString == null) return;

      ide.selectDialogWindow();

      clips.openStringBatch("batchtext",batchString,false); 
      ide.executeBatch();
     }
     
   /**************/
   /* loadBuffer */
   /**************/  
   private void loadBuffer()
     {
      if (textFrame == null)
        { return; }
        
      Environment clips = ide.getEnvironment();
      DialogFrame dialog = ide.getDialogWindow();
      
      if (dialog.isExecuting())
        { return; }
       
      JTextArea theTextArea = textFrame.getTextArea();
      String loadString = theTextArea.getText();
      
      ide.selectDialogWindow();

      clips.flushInputBuffer();
      clips.print(Router.STDOUT,"Loading Buffer...\n");
      clips.loadFromStringWithOutput(loadString);
      clips.printPrompt();
     }
     
   /***********/
   /* balance */
   /***********/  
   private void balance()
     {
      int leftMiddle, rightMiddle, textLength;
      char characterToCheck;
      int count, leftCount, rightCount;
      int i;
      boolean endReached;

      if (textFrame == null)
        { return; }

      JTextArea theTextArea = textFrame.getTextArea();
      String theText = theTextArea.getText();
      
      /*====================================*/
      /* Can't balance if there is no text. */
      /*====================================*/
   
      if (theText.length() == 0)
        {
         Toolkit.getDefaultToolkit().beep(); 
         return;
        }
       
      /*=============================================================*/
      /* Get information about the current selection to be balanced. */
      /*=============================================================*/

      leftMiddle = theTextArea.getSelectionStart();
      rightMiddle = theTextArea.getSelectionEnd();
      textLength = theText.length(); 

      /*===================================*/
      /* If the selection is empty then... */
      /*===================================*/

      if (leftMiddle == rightMiddle)
        {
         /*============================================*/
         /* If '(' is to the right of the cursor, then */
         /* all balancing should occur to the right.   */
         /*============================================*/

         if ((leftMiddle < textLength) && (theText.charAt(leftMiddle) == '('))
           { balanceIt(theText,leftMiddle,leftMiddle + 1,1,0,textLength); }

         /*================================================*/
         /* Else if ')' is to the left of the cursor, then */
         /* all balancing should occur to the left.        */
         /*================================================*/

         else if ((leftMiddle > 0) ? (theText.charAt(leftMiddle - 1) == ')') : false)
           { 
            if (leftMiddle < 2)
              {
               Toolkit.getDefaultToolkit().beep();
               return;
              }

            balanceIt(theText,leftMiddle - 2,leftMiddle,0,-1,textLength); 
           }

         /*====================================================*/
         /* Else balancing occurs to left and right of cursor. */
         /*====================================================*/

         else
           { balanceIt(theText,leftMiddle - 1,rightMiddle,0,0,textLength); }
        }
     
      /*===================================================*/
      /* Otherwise the selection is non-empty therefore... */
      /*===================================================*/

      else
        {
         /*===============================================*/
         /* Determine the number of right parentheses ')' */
         /* that need to be balanced from the left side.  */
         /*===============================================*/

         count = 0;
         leftCount = 0;

         for (i = leftMiddle ; i < rightMiddle ; i++)
           {
            characterToCheck = theText.charAt(i);

            if (characterToCheck == '(') count++;
            else if (characterToCheck == ')') count--;
            if (count < leftCount) leftCount = count;
           }

         /*===============================================*/
         /* Determine the number of left parentheses '('  */
         /* that need to be balanced from the right side. */
         /*===============================================*/

         count = 0;
         rightCount = 0;

         for (endReached = false, i = rightMiddle - 1 ; ! endReached ;)
           {
            characterToCheck = theText.charAt(i);

            if (characterToCheck == '(') count++;
            else if (characterToCheck == ')') count--;
            if (count > rightCount) rightCount = count;
         
            if (i == leftMiddle) endReached = true;
            else i--;
           }

         /*==============================================*/
         /* Balance to the left and right of the cursor. */
         /*==============================================*/

         balanceIt(theText,((leftMiddle == 0) ? 0 : leftMiddle - 1),
                   rightMiddle,leftCount,rightCount,textLength); 
        }
     }

   /********************************************************/
   /* balanceIt: Balances a selection of text by extending */ 
   /* it to the left and right until the number of left    */
   /* and right parentheses is balanced.                   */
   /********************************************************/
   private void balanceIt(
     String theText,
     int leftMiddle,
     int rightMiddle,
     int leftCount,
     int rightCount,
     int textLength)
     {
      char characterToCheck;
      boolean beginningReached = false;
      
      /*==========================================================*/
      /* Balance the left side of the text by moving left and up. */
      /*==========================================================*/

      while (leftCount <= 0)
        {
         if (beginningReached)
           {
            Toolkit.getDefaultToolkit().beep();
            return;
           }
        
         characterToCheck = theText.charAt(leftMiddle);
      
         if (characterToCheck == '(') leftCount++;
         else if (characterToCheck == ')') leftCount--;
      
         if (leftCount <= 0)
           {
            if (leftMiddle > 0) leftMiddle--;
            else beginningReached = true;
           }
        }
     
      /*==============================================================*/
      /* Balance the right side of the text by moving right and down. */
      /*==============================================================*/

      while (rightCount >= 0)
        {
         if (rightMiddle >= textLength)
           {
            Toolkit.getDefaultToolkit().beep();
            return;
           }

         characterToCheck = theText.charAt(rightMiddle);

         if (characterToCheck == '(') rightCount++;
         else if (characterToCheck == ')') rightCount--;

         rightMiddle++;  
        }
     
      /*=============================================*/
      /* Set the current selection to balanced text. */
      /*=============================================*/
         
      JTextArea theTextArea = textFrame.getTextArea();
      theTextArea.setCaretPosition(leftMiddle);
      theTextArea.moveCaretPosition(rightMiddle);

      /*=====================================*/
      /* Make sure the selection is visible. */
      /*=====================================*/
/*
      try
        {
         Rectangle rect = theTextArea.modelToView(leftMiddle);
         theTextArea.scrollRectToVisible(rect);
        }
      catch (BadLocationException e)
        { e.printStackTrace(); }
*/
     }

   /***********/
   /* comment */
   /***********/  
   private void comment()
     {
      if (textFrame == null)
        { return; }

      JTextArea theTextArea = textFrame.getTextArea();
      
      int startLine, endLine;
      int startOffset, endOffset;
      
      int start = theTextArea.getSelectionStart();
      int end = theTextArea.getSelectionEnd();
            
      try
        {
         startLine = theTextArea.getLineOfOffset(start);
         endLine = theTextArea.getLineOfOffset(end);
         startOffset = theTextArea.getLineStartOffset(startLine);
         endOffset = theTextArea.getLineEndOffset(endLine);
        }
      catch (Exception e)
        { 
         e.printStackTrace();
         return; 
        }
       
      StringBuilder sb; 
       
      try
        {
         String subText = theTextArea.getText(startOffset,endOffset-startOffset);
         sb = new StringBuilder(subText);
         for (int i = endLine; i >= startLine; i--)
           {
            int replaceOffset = theTextArea.getLineStartOffset(i) - startOffset;
            sb.insert(replaceOffset,';');
           }
        }
      catch (Exception e)
        { 
         e.printStackTrace();
         return;
        }
         
      theTextArea.replaceRange(sb.toString(),startOffset,endOffset);
      theTextArea.setCaretPosition(startOffset);
      theTextArea.moveCaretPosition(startOffset+sb.length()-1);
     }

   /*************/
   /* uncomment */
   /*************/  
   private void uncomment()
     {
      if (textFrame == null)
        { return; }

      JTextArea theTextArea = textFrame.getTextArea();
      
      int startLine, endLine;
      int startOffset, endOffset;
      
      int start = theTextArea.getSelectionStart();
      int end = theTextArea.getSelectionEnd();
            
      try
        {
         startLine = theTextArea.getLineOfOffset(start);
         endLine = theTextArea.getLineOfOffset(end);
         startOffset = theTextArea.getLineStartOffset(startLine);
         endOffset = theTextArea.getLineEndOffset(endLine);
        }
      catch (Exception e)
        {  
         e.printStackTrace();
         return;
        }
       
      StringBuilder sb; 
       
      try
        {
         String subText = theTextArea.getText(startOffset,endOffset-startOffset);
         sb = new StringBuilder(subText);
         for (int i = endLine; i >= startLine; i--)
           {
            int replaceOffset = theTextArea.getLineStartOffset(i) - startOffset;
            if (sb.charAt(replaceOffset) ==';')
              { sb.deleteCharAt(replaceOffset); }
           }
        }
      catch (Exception e)
        {
         e.printStackTrace();
         return; 
        }
       
      if (sb.length() == (endOffset-startOffset))
        { return; }
           
      theTextArea.replaceRange(sb.toString(),startOffset,endOffset);
      theTextArea.setCaretPosition(startOffset);
      theTextArea.moveCaretPosition(startOffset+sb.length()-1); 
     }
  
   /*######################*/
   /* MenuListener Methods */
   /*######################*/
   
   /****************/
   /* menuCanceled */
   /****************/  
   public void menuCanceled(MenuEvent e)
     {
     }
     
   /****************/
   /* menuSelected */
   /****************/  
   public void menuSelected(MenuEvent e)
     {
      DialogFrame dialog = ide.getDialogWindow();

      if (textFrame == null)
        {
         jmiLoadSelection.setEnabled(false);
         jmiBatchSelection.setEnabled(false);   
         jmiLoadBuffer.setEnabled(false);
         jmiBalance.setEnabled(false);
         jmiComment.setEnabled(false);
         jmiUncomment.setEnabled(false);
         return;
        }

      if (textFrame.hasSelection())
        {
         if (dialog.isExecuting())
           {
            jmiLoadSelection.setEnabled(false);
            jmiBatchSelection.setEnabled(false); 
           }  
         else
           {
            jmiLoadSelection.setEnabled(true);
            jmiBatchSelection.setEnabled(true); 
           }  
        }
      else
        {
         jmiLoadSelection.setEnabled(false);
         jmiBatchSelection.setEnabled(false);   
        }
        
      if (dialog.isExecuting())
        { jmiLoadBuffer.setEnabled(false); }
      else
        { jmiLoadBuffer.setEnabled(true); }
        
      jmiBalance.setEnabled(true);
      jmiComment.setEnabled(true);
      jmiUncomment.setEnabled(true);
     }   
     
   /******************/
   /* menuDeselected */
   /******************/  
   public void menuDeselected(MenuEvent e)
     {
     }
     
   /*#####################*/
   /* LoadSelectionAction */
   /*#####################*/
   class LoadSelectionAction extends AbstractAction 
     {
      public LoadSelectionAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         loadSelection();     
        }
     }

   /*######################*/
   /* BatchSelectionAction */
   /*######################*/
   class BatchSelectionAction extends AbstractAction 
     {
      public BatchSelectionAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         batchSelection();     
        }
     }

   /*##################*/
   /* LoadBufferAction */
   /*##################*/
   class LoadBufferAction extends AbstractAction 
     {
      public LoadBufferAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         loadBuffer();     
        }
     }

   /*###############*/
   /* BalanceAction */
   /*###############*/
   class BalanceAction extends AbstractAction 
     {
      public BalanceAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         balance();     
        }
     }

   /*###############*/
   /* CommentAction */
   /*###############*/
   class CommentAction extends AbstractAction 
     {
      public CommentAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         comment();     
        }
     }

   /*#################*/
   /* UncommentAction */
   /*#################*/
   class UncommentAction extends AbstractAction 
     {
      public UncommentAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         uncomment();     
        }
     }
  }
