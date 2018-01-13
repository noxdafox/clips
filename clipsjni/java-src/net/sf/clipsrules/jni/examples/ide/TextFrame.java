package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.table.*;
import javax.swing.border.*; 
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.IOException;
import java.util.ArrayList;
import javax.swing.undo.AbstractUndoableEdit;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;
import javax.swing.event.UndoableEditListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.PlainDocument;

import javax.swing.BorderFactory;

public class TextFrame extends JInternalFrame 
                    implements DocumentListener, KeyListener
  {
   private JTextArea textArea;
   private static int untitledCount = 1;
   private boolean changed = false;
   private TextUndoManager textAreaUndo;
   private File textFile;

   /*************/
   /* TextFrame */
   /*************/
   TextFrame(
     File theFile)
     {  
      super("",true,true,true,true);
      
      textFile = theFile;
      
      if (theFile == null)
        { setTitle("Untitled #" + untitledCount++); }
      else
        { setTitle(theFile.getName()); }
        
      /*===================================*/
      /* Create a new JFrame container and */
      /* assign a layout manager to it.    */
      /*===================================*/

      this.getContentPane().setLayout(new BoxLayout(this.getContentPane(),BoxLayout.Y_AXIS));
      
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      this.setSize(600,400);  
      this.setMinimumSize(new Dimension(550,250));
      
      /*===========================================*/
      /* The close button closes just the browser. */
      /*===========================================*/
     
      this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);  

      /*==========================*/
      /* Create the status panel. */
      /*==========================*/

      JPanel statusPanel = new JPanel(); 
      statusPanel.setPreferredSize(new Dimension(600,40));
      
      this.getContentPane().add(statusPanel); 
      
      /*=============================*/
      /* Create the text field area. */
      /*=============================*/

      try
        { 
         textArea = new JTextArea(); 
         textAreaUndo = new TextUndoManager();
         textArea.setDocument(new TextUndoPlainDocument(textAreaUndo));
         textArea.setFont(new Font("monospaced",Font.PLAIN,12));
        }
      catch (Exception e)
        { 
         e.printStackTrace();
         return;
        }       
        
      textArea.setBorder(BorderFactory.createEmptyBorder(5,5,5,0));
      //textArea.addKeyListener(this);
      setUpKeys();

      /*=======================================*/
      /* Put the text area into a scroll pane. */
      /*=======================================*/

      JScrollPane textPane = new JScrollPane(textArea);
      textPane.setPreferredSize(new Dimension(600,360));
      textPane.setViewportBorder(BorderFactory.createEmptyBorder(0,0,2,0));
      
      /*========================================*/
      /* Add the scroll pane to the main frame. */
      /*========================================*/
      
      this.getContentPane().add(textPane); 

      /*========================*/
      /* Read the file content. */
      /*========================*/
      
      if (theFile != null)
        { readContent(theFile); }

      /*============================*/
      /* Set the document listener. */
      /*============================*/
      
      textArea.getDocument().addDocumentListener(this);

      /*===================================================*/
      /* Override copy/paste for the CommandPromptTextArea */
      /* so that we can define our own menu accelerators.  */
      /*===================================================*/

      KeyStroke cut = KeyStroke.getKeyStroke(KeyEvent.VK_X,KeyEvent.CTRL_MASK);
      KeyStroke copy = KeyStroke.getKeyStroke(KeyEvent.VK_C,KeyEvent.CTRL_MASK);
      KeyStroke paste = KeyStroke.getKeyStroke(KeyEvent.VK_V,KeyEvent.CTRL_MASK);
      InputMap map = textArea.getInputMap();
      map.put(cut,"none");
      map.put(copy,"none");
      map.put(paste,"none");
        
      /*====================*/
      /* Display the frame. */
      /*====================*/

      this.pack();
     }  
     
   /*************/
   /* setUpKeys */
   /*************/
   public void setUpKeys()
     {
      int condition = WHEN_FOCUSED;  

      InputMap inputMap = textArea.getInputMap(condition);
      ActionMap actionMap = textArea.getActionMap();
      
      KeyStroke rparenStroke = KeyStroke.getKeyStroke(')');
      
      inputMap.put(rparenStroke,rparenStroke.toString());

      actionMap.put(rparenStroke.toString(), new AbstractAction() 
        {
         @Override
         public void actionPerformed(ActionEvent ae)
           {
            MatchParenthesis();
            textArea.replaceRange(")",textArea.getSelectionStart(),textArea.getSelectionEnd());
           }
      });
     }

   /********************/
   /* MatchParenthesis */
   /********************/
   public void MatchParenthesis() 
     {
      int nestingDepth = 0;
      char characterToCheck;
      int selStart = textArea.getSelectionStart();
      int selEnd = textArea.getSelectionEnd();
      int cursorLocation = Math.min(textArea.getCaret().getDot(),
                                    textArea.getCaret().getMark());
                                       
      while ((cursorLocation--) > 0)
        {
         characterToCheck = textArea.getText().charAt(cursorLocation);
            
         if (characterToCheck == '(') 
           {
            if (nestingDepth == 0) 
              {
               /*======================================*/
               /* Select the matching left parenthesis */
               /* and hide the caret.                  */
               /*======================================*/

               textArea.getCaret().setVisible(false);
               textArea.setSelectionStart(cursorLocation);
               textArea.setSelectionEnd(cursorLocation + 1);

               /*========================================*/
               /* Force an update to occur otherwise the */
               /* changed selection won't be visible.    */
               /*========================================*/

               textArea.update(textArea.getGraphics());

               /*============================================*/
               /* Pause momentarily so the selected matching */
               /* parenthesis can be observed.               */
               /*============================================*/
    
               try
	             { Thread.sleep(200); }
	           catch (Exception ex)
	             { ex.printStackTrace(); }

               /*===========================*/
               /* Restore the selection and */
               /* make the caret visible.   */
               /*===========================*/
              
               textArea.setSelectionStart(selStart);
               textArea.setSelectionEnd(selEnd);
               textArea.getCaret().setVisible(true);
	      	   return;
	          }
            else
	          { nestingDepth--; }
	       }
         else if (characterToCheck == ')') 
           { nestingDepth++; }
        }
           
      Toolkit.getDefaultToolkit().beep();                                
     }    
       
   /***************/
   /* readContent */
   /***************/
   public void readContent(
     File theFile)
     {
      FileReader reader = null;
      try 
        {
         reader = new FileReader(theFile);
         textArea.read(reader,null);
        }
      catch (IOException e)
        {
         JOptionPane.showMessageDialog(getParent(),
                                       e.getMessage(),"ERROR", 
                                       JOptionPane.ERROR_MESSAGE);
        }
      finally
        {
         if (reader != null)
           {
            try 
              { reader.close(); } 
            catch (IOException e) 
              {}
           }
        }
     }
     
   /****************/
   /* writeContent */
   /****************/
   public void writeContent(
     File theFile)
     {
      BufferedWriter writer = null;
      try 
        { 
         writer = new BufferedWriter(new FileWriter(theFile));
         textArea.write(writer);
         changed = false;
        }
      catch (IOException e)
        {
         JOptionPane.showMessageDialog(getParent(),
                                       e.getMessage(),"ERROR", 
                                       JOptionPane.ERROR_MESSAGE);
        }
      finally 
        {
         if (writer != null)
           {
            try 
              { writer.close(); } 
            catch (IOException e) 
              {}
           }
        }
     }
     
   /******************/
   /* closeTextFrame */
   /******************/  
   public void closeTextFrame(
     IDEPreferences preferences)
     {
      if (! changed) 
        {
         setDefaultCloseOperation(DISPOSE_ON_CLOSE);
         return;
        }
      
      int confirmResult = JOptionPane.showConfirmDialog(this,
           "Do you want to save the changes made to the document \"" +
            this.getTitle() +"\"?",
            null,
            JOptionPane.YES_NO_CANCEL_OPTION);
          
      if (confirmResult == JOptionPane.YES_OPTION)
        { 
         saveTextFrame(preferences);
         setDefaultCloseOperation(DISPOSE_ON_CLOSE); 
        }
      else if (confirmResult == JOptionPane.NO_OPTION)
        { setDefaultCloseOperation(DISPOSE_ON_CLOSE); }
      else if (confirmResult == JOptionPane.CANCEL_OPTION)
        { setDefaultCloseOperation(DO_NOTHING_ON_CLOSE); }
      else if (confirmResult == JOptionPane.CLOSED_OPTION)
        { setDefaultCloseOperation(DO_NOTHING_ON_CLOSE); }
     }
     
   /*******************/
   /* saveAsTextFrame */
   /*******************/  
   public void saveAsTextFrame(
     IDEPreferences preferences)
     {
      final JFileChooser fc = new JFileChooser();

      File currentDirectory = preferences.getCurrentDirectory();
      if (currentDirectory != null)
        { fc.setCurrentDirectory(currentDirectory); }

      if (textFile != null)
        { fc.setSelectedFile(textFile); }
      else
        { fc.setSelectedFile(new File(this.getTitle() + ".clp")); }
        
      int returnVal = fc.showSaveDialog(this);
      
      if (returnVal != JFileChooser.APPROVE_OPTION) return;
      
      File selectedFile = fc.getSelectedFile();
      if (selectedFile == null) return;
            
      if (selectedFile.exists()) 
        {
         int confirmationResult = JOptionPane.showConfirmDialog(this,"Replace existing file?");
         if (confirmationResult != JOptionPane.YES_OPTION) return;
        }
        
      setTitle(selectedFile.getName()); 
        
      currentDirectory = fc.getCurrentDirectory();
      preferences.setCurrentDirectory(currentDirectory);
      textFile = selectedFile;  
      
      writeContent(textFile);
     }

   /*****************/
   /* saveTextFrame */
   /*****************/  
   public void saveTextFrame(
     IDEPreferences preferences)
     {
      if (textFile == null)
        {
         saveAsTextFrame(preferences);
         return;
        }
        
      writeContent(textFile);
     }
     
   /****************/
   /* hasSelection */
   /****************/
   public boolean hasSelection()
     {
      int left = Math.min(textArea.getCaret().getDot(),textArea.getCaret().getMark());
      int right = Math.max(textArea.getCaret().getDot(),textArea.getCaret().getMark());
      if (left == right) return false;
      return true;
     }

   /***********/
   /* canUndo */
   /***********/
   public boolean canUndo()
     {
      return textAreaUndo.canUndo();
     }
     
   /***********/
   /* canRedo */
   /***********/
   public boolean canRedo()
     {
      return textAreaUndo.canRedo();     
     }

   /*************/
   /* isChanged */
   /*************/
   public boolean isChanged()
     {
      return changed;    
     }

   /***********/
   /* canSave */
   /***********/
   public boolean canSave()
     {
      if (textFile == null) return true;
      else return changed;    
     }

   /********/
   /* undo */
   /********/     
   public void undo()
     {
      if (! textAreaUndo.canUndo()) return;
      try 
        { textAreaUndo.undo(); }
      catch (CannotUndoException e) 
        { e.printStackTrace(); }
     }

   /********/
   /* redo */
   /********/     
   public void redo()
     {
      if (! textAreaUndo.canRedo()) return;
      try 
        { textAreaUndo.redo(); }
      catch (CannotUndoException e) 
        { e.printStackTrace(); }
     }
            
   /***************/
   /* getTextArea */
   /***************/
   public JTextArea getTextArea()
     { return textArea; }
          
   /*******/
   /* cut */
   /*******/
   public void cut()
     {
      textArea.cut();
     }

   /********/
   /* copy */
   /********/
   public void copy()
     {
      textArea.copy(); 
     }

   /*********/
   /* paste */
   /*********/
   public void paste()
     { 
      textArea.paste(); 
     }
     
   /****************/
   /* insertUpdate */
   /****************/
   public void insertUpdate(DocumentEvent e)
     {
      changed = true;
     }
  
   /****************/
   /* removeUpdate */
   /****************/
   public void removeUpdate(DocumentEvent e)
     {
      changed = true;
     }
  
   /*****************/
   /* changedUpdate */
   /*****************/
   public void changedUpdate(DocumentEvent e)
     {      
      changed = true;
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
     }

   /***************/
   /* keyReleased */
   /***************/     
   @Override
   public void keyReleased(KeyEvent e) 
     { 
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
          
      if (e.getKeyChar() == '\n')
        {
         return;
        }

      if (e.getKeyChar() == ')') 
        { MatchParenthesis(); }    
     }
     
   /*##################*/
   /* TextCompoundEdit */
   /*##################*/
   class TextCompoundEdit extends CompoundEdit
     {
      private boolean isUnDone = false;

      /*************/
      /* getLength */
      /*************/
      public int getLength()
        {
         return edits.size();
        }

      /********/
      /* undo */
      /********/
      public void undo() throws CannotUndoException
        {
         super.undo();
         isUnDone = true;
        }

      /********/
      /* redo */
      /********/
      public void redo() throws CannotUndoException
        {
         super.redo();
         isUnDone = false;
        }

      /***********/
      /* canUndo */
      /***********/
      public boolean canUndo()
        {
         return (edits.size() > 0) && (! isUnDone);
        }

      /***********/
      /* canRedo */
      /***********/
      public boolean canRedo()
        {
         return (edits.size() > 0) && isUnDone;
        }
     }
     
   /*#################*/
   /* TextUndoManager */
   /*#################*/
   class TextUndoManager extends AbstractUndoableEdit 
                         implements UndoableEditListener
     {     
      private String lastEditName = null;
      private int lastStart = 0;
      private ArrayList<TextCompoundEdit> edits = new ArrayList<TextCompoundEdit>();
      private TextCompoundEdit current;
      private int pointer = -1;
      private int groupIndex = 0;
      private String groupName = null;
       
      /************************/
      /* undoableEditHappened */
      /************************/
      public void undoableEditHappened(
        UndoableEditEvent e)
        {
         boolean isNeedStart = false;
         UndoableEdit edit = e.getEdit();

         if (! (edit instanceof AbstractDocument.DefaultDocumentEvent))
           { return; }

         AbstractDocument.DefaultDocumentEvent event = (AbstractDocument.DefaultDocumentEvent) edit;
                 
         int start = event.getOffset();

         String editName;

         /*============================================*/
         /* If an explicit group name is not present,  */
         /* use the INSERT/REMOVE name from the event. */
         /*============================================*/
                 
         if (groupName != null)
           { editName = groupName; }
         else
           { editName = event.getType().toString(); }

         /*============================*/
         /* Create a new compound edit */
         /* for the very first edit.   */
         /*============================*/
         
         if (current == null)
           { isNeedStart = true; }

         /*============================*/
         /* Create a new compound edit */
         /* for a different operation. */
         /*============================*/
         
         else if ((lastEditName == null) || 
                  (! lastEditName.equals(editName)))
           { isNeedStart = true; }

         /*================================================*/
         /* Only group continuous single character inserts */
         /* and deletes. Create a new edit if the user has */
         /* moved the caret from its prior position.       */
         /*================================================*/

         else if (groupName == null)
           {            
            if ((event.getType() == DocumentEvent.EventType.INSERT) &&
                     (start != (lastStart + 1)))
              { isNeedStart = true; }
            else if ((event.getType() == DocumentEvent.EventType.REMOVE) &&
                     (start != (lastStart - 1)))
              { isNeedStart = true; }
           }
         
         /*=========================================*/
         /* Adding a new edit will clear all of the */
         /* redos forward of the current position.  */
         /*=========================================*/
         
         while (pointer < edits.size() - 1)
           {
            edits.remove(edits.size() - 1);
            isNeedStart = true;
           }
                       
         /*===================*/
         /* Add the new edit. */
         /*===================*/
              
         if (isNeedStart)
           { createCompoundEdit(); }
 
         current.addEdit(edit);

         /*=====================================*/
         /* Remember prior state for next edit. */
         /*=====================================*/
         
         lastEditName = editName;
         lastStart = start;
        }
        
      /*********************/
      /* startEditGrouping */
      /*********************/
      public void startEditGrouping()
        {
         groupName = "Group-" + groupIndex++;
        }

      /********************/
      /* stopEditGrouping */
      /********************/
      public void stopEditGrouping()
        {
         groupName = null;
        }
     
      /**********************/
      /* createCompoundEdit */
      /**********************/
      private void createCompoundEdit()
        {
         if (current == null)
           { current = new TextCompoundEdit(); }
         else if (current.getLength() > 0)
           { current = new TextCompoundEdit(); }

         edits.add(current);
         pointer++;
        }

      /********/
      /* undo */
      /********/ 
      public void undo() throws CannotUndoException
        {
         if (! canUndo())
           { throw new CannotUndoException(); }
 
         TextCompoundEdit u = edits.get(pointer);
         u.undo();
         pointer--;
        }
 
      /********/
      /* redo */
      /********/
      public void redo() throws CannotUndoException
        {
         if (! canRedo())
           { throw new CannotUndoException(); }
 
         pointer++;
         TextCompoundEdit u = edits.get(pointer);
         u.redo();
        }
 
      /***********/
      /* canUndo */
      /***********/
      public boolean canUndo()
        { 
         return pointer >= 0; 
        }

      /***********/
      /* canRedo */
      /***********/
      public boolean canRedo()
        {
         return (edits.size() > 0) && (pointer < (edits.size() - 1));
        }
     }
     
   /*#######################*/
   /* TextUndoPlainDocument */
   /*#######################*/
   class TextUndoPlainDocument extends PlainDocument 
     {    
      private TextUndoManager undoManager;

      /*************************/
      /* TextUndoPlainDocument */
      /*************************/
      TextUndoPlainDocument(
        TextUndoManager theManager)
        {
         super();
         undoManager = theManager;
         this.addUndoableEditListener(undoManager);
        }

      /***********/
      /* replace */
      /***********/
      @Override 
      public void replace(
        int offset, 
        int length,
        String text, 
        AttributeSet attrs) throws BadLocationException
        {
         if (length == 0)
           { super.replace(offset,length,text,attrs); }
         else
           {
            undoManager.startEditGrouping();
            super.replace(offset,length,text,attrs); 
            undoManager.stopEditGrouping();
           }
        }
     }
  }
