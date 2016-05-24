package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JTextArea;

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
        { return; }
       
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
        { return; }
         
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
        { return; }
       
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
        { return; }
       
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
