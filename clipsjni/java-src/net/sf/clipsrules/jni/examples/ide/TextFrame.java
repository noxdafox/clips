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
import java.io.IOException;
import javax.swing.undo.UndoManager;
import javax.swing.event.UndoableEditListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.text.PlainDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.AttributeSet;

import javax.swing.BorderFactory;

public class TextFrame extends JInternalFrame 
                    implements DocumentListener
  {
   private JTextArea textArea;
   private static int untitledCount = 1;
   private boolean changed = false;
   private UndoManager textAreaUndo;
   
   /*************/
   /* TextFrame */
   /*************/
   TextFrame(
     File theFile)
     {  
      super("",true,true,true,true);
      
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
         textArea.setDocument(new CustomUndoPlainDocument());
         textArea.setFont(new Font("monospaced",Font.PLAIN,12));
        }
      catch (Exception e)
        { 
         e.printStackTrace();
         return;
        }       
        
      textArea.setBorder(BorderFactory.createEmptyBorder(5,5,5,0));

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

      /***************************/
      /* Create the undoManager. */
      /***************************/
       
      textAreaUndo = new UndoManager();
      textArea.getDocument().addUndoableEditListener(new UndoableEditListener() 
        {
         @Override
         public void undoableEditHappened(UndoableEditEvent e) 
           {
            textAreaUndo.addEdit(e.getEdit());
           }
        });
        
      /*====================*/
      /* Display the frame. */
      /*====================*/

      this.pack();
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
                                       e.getMessage(), "ERROR", 
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
     
   /*#########################*/
   /* CustomUndoPlainDocument */
   /*#########################*/
   class CustomUndoPlainDocument extends PlainDocument 
     {
      private CompoundEdit compoundEdit;
  
      @Override 
      protected void fireUndoableEditUpdate(UndoableEditEvent e) 
        {
         if (compoundEdit == null) 
           { super.fireUndoableEditUpdate(e); } 
         else
           { compoundEdit.addEdit(e.getEdit()); }
        }
        
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
            compoundEdit = new CompoundEdit();
            super.fireUndoableEditUpdate(new UndoableEditEvent(this,compoundEdit));
            super.replace(offset, length, text, attrs);
            compoundEdit.end();
            compoundEdit = null;
           }
        }
     }
  }
