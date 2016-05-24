package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*;
import javax.swing.table.*;
import javax.swing.border.*; 
import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.BorderFactory;

public class TextFrame extends JInternalFrame
  {
   private JTextArea textArea;
   private static int untitledCount = 1;
   
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
  }