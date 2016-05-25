package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.Toolkit;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JTextArea;
/*
import java.awt.Rectangle;
import javax.swing.text.BadLocationException;
*/
import net.sf.clipsrules.jni.*;

public class TextMenu extends JMenu 
                   implements ActionListener, MenuListener
  {  
   private static final String loadSelectionAction = "LoadSelection";
   private static final String batchSelectionAction = "BatchSelection";
   private static final String loadBufferAction = "LoadBuffer";
   private static final String balanceAction = "Balance";
   private static final String commentAction = "Comment";
   private static final String uncommentAction = "Uncomment";

   private JMenuItem jmiLoadSelection = null;
   private JMenuItem jmiBatchSelection = null;   
   private JMenuItem jmiLoadBuffer = null;
   private JMenuItem jmiBalance = null;
   private JMenuItem jmiComment = null;
   private JMenuItem jmiUncomment = null;
   
   private TextFrame textFrame = null;
   private CLIPSIDE ide = null;

   /************/
   /* TextMenu */
   /************/
   TextMenu(
     CLIPSIDE theIDE)
     {  
      super("Text");
        
      ide = theIDE;
      
      addMenuListener(this);
      
      jmiLoadSelection = new JMenuItem("Load Selection");
      jmiLoadSelection.setActionCommand(loadSelectionAction);
      jmiLoadSelection.addActionListener(this);
      add(jmiLoadSelection);

      jmiBatchSelection = new JMenuItem("Batch Selection");
      jmiBatchSelection.setActionCommand(batchSelectionAction);
      jmiBatchSelection.addActionListener(this);
      add(jmiBatchSelection);

      jmiLoadBuffer = new JMenuItem("Load Buffer");
      jmiLoadBuffer.setActionCommand(loadBufferAction);
      jmiLoadBuffer.addActionListener(this);
      add(jmiLoadBuffer);

      addSeparator();

      jmiBalance = new JMenuItem("Balance");
      jmiBalance.setActionCommand(balanceAction);
      jmiBalance.addActionListener(this);
      add(jmiBalance);

      jmiComment = new JMenuItem("Comment");
      jmiComment.setActionCommand(commentAction);
      jmiComment.addActionListener(this);
      add(jmiComment);

      jmiUncomment = new JMenuItem("Uncomment");
      jmiUncomment.setActionCommand(uncommentAction);
      jmiUncomment.addActionListener(this);
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

   /*********************/
   /* onActionPerformed */
   /*********************/  
   public void onActionPerformed(
     ActionEvent ae) throws Exception 
     {     
      if (ae.getActionCommand().equals(loadSelectionAction))  
        { loadSelection(); }
      else if (ae.getActionCommand().equals(batchSelectionAction))  
        { batchSelection(); }
      else if (ae.getActionCommand().equals(loadBufferAction))  
        { loadBuffer(); }
      else if (ae.getActionCommand().equals(balanceAction))  
        { balance(); }
      else if (ae.getActionCommand().equals(commentAction))  
        { comment(); }
      else if (ae.getActionCommand().equals(uncommentAction))  
        { uncomment(); }
     }
     
   /*****************/
   /* loadSelection */
   /*****************/  
   public void loadSelection()
     {
      if (textFrame == null)
        { return; }

      Environment clips = ide.getEnvironment();
        
      JTextArea theTextArea = textFrame.getTextArea();
      String loadString = theTextArea.getSelectedText();
      
      clips.flushInputBuffer();
      clips.printRouter(Router.STANDARD_OUTPUT,"Loading Selection...\n");
      clips.loadFromStringWithOutput(loadString);
      clips.printPrompt();
     }
   
   /******************/
   /* batchSelection */
   /******************/  
   public void batchSelection()
     {
      if (textFrame == null)
        { return; }
       
      Environment clips = ide.getEnvironment();
       
      JTextArea theTextArea = textFrame.getTextArea();
      String batchString = theTextArea.getSelectedText();
      clips.openStringBatch("batchtext",batchString,false); 
      ide.executeBatch();
     }
     
   /**************/
   /* loadBuffer */
   /**************/  
   public void loadBuffer()
     {
      if (textFrame == null)
        { return; }
        
      Environment clips = ide.getEnvironment();
       
      JTextArea theTextArea = textFrame.getTextArea();
      String loadString = theTextArea.getText();
      
      clips.flushInputBuffer();
      clips.printRouter(Router.STANDARD_OUTPUT,"Loading Buffer...\n");
      clips.loadFromStringWithOutput(loadString);
      clips.printPrompt();
     }
     
   /***********/
   /* balance */
   /***********/  
   public void balance()
     {
      int leftMiddle, rightMiddle, textLength;
      char characterToCheck;
      int count, leftCount, rightCount;
      int i;
      boolean endReached;

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
   public void balanceIt(
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
   public void comment()
     {
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
   public void uncomment()
     {
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

   /*########################*/
   /* ActionListener Methods */
   /*########################*/

   /*******************/
   /* actionPerformed */
   /*******************/  
   public void actionPerformed(
     ActionEvent ae) 
     {
      try
        { onActionPerformed(ae); }
      catch (Exception e)
        { e.printStackTrace(); }
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
         jmiLoadSelection.setEnabled(true);
         jmiBatchSelection.setEnabled(true);   
        }
      else
        {
         jmiLoadSelection.setEnabled(false);
         jmiBatchSelection.setEnabled(false);   
        }
        
      jmiLoadBuffer.setEnabled(true);
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
  }
