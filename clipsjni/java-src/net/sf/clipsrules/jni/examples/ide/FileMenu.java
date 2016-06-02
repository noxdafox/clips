package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import java.io.File;

import java.util.prefs.Preferences;

import javax.swing.AbstractAction;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.JDesktopPane;
import javax.swing.JFileChooser;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

public class FileMenu extends JMenu 
                      implements MenuListener
  {  
   private NewAction newAction;
   private OpenAction openAction;
   private SaveAction saveAction;
   private SaveAsAction saveAsAction;
   private QuitAction quitAction;
   private CLIPSIDE ide;

   /************/
   /* FileMenu */
   /************/
   FileMenu(
     CLIPSIDE theIDE)
     {  
      super("File");
             
      ide = theIDE;
      
      addMenuListener(this);

      /*==================================*/
      /* Get KeyStrokes for accelerators. */
      /*==================================*/

      KeyStroke newDoc = KeyStroke.getKeyStroke(KeyEvent.VK_N,KeyEvent.CTRL_MASK);
      KeyStroke openDoc = KeyStroke.getKeyStroke(KeyEvent.VK_O,KeyEvent.CTRL_MASK);
      KeyStroke saveDoc = KeyStroke.getKeyStroke(KeyEvent.VK_S,KeyEvent.CTRL_MASK);
      KeyStroke saveAsDoc = KeyStroke.getKeyStroke(KeyEvent.VK_S,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);
      KeyStroke quitIDE = KeyStroke.getKeyStroke(KeyEvent.VK_Q,KeyEvent.CTRL_MASK);

      /*================*/
      /* Setup actions. */
      /*================*/
       
      newAction = new NewAction("New");
      openAction = new OpenAction("Open...");
      saveAction = new SaveAction("Save");
      saveAsAction = new SaveAsAction("Save As...");
      quitAction = new QuitAction("Quit CLIPS IDE");

      /*=================*/
      /* Add menu items. */
      /*=================*/
      
      JMenuItem jmiNew = new JMenuItem(newAction);
      jmiNew.setAccelerator(newDoc);
      this.add(jmiNew);

      JMenuItem jmiOpen = new JMenuItem(openAction);
      jmiOpen.setAccelerator(openDoc);
      this.add(jmiOpen);

      JMenuItem jmiSave = new JMenuItem(saveAction);
      jmiSave.setAccelerator(saveDoc);
      this.add(jmiSave);

      JMenuItem jmiSaveAs = new JMenuItem(saveAsAction);
      jmiSaveAs.setAccelerator(saveAsDoc);
      this.add(jmiSaveAs);

      JMenuItem jmiQuitIDE = new JMenuItem(quitAction);
      jmiQuitIDE.setAccelerator(quitIDE);
      this.add(jmiQuitIDE);
     }

   /*************/
   /* NewAction */
   /*************/
   class NewAction extends AbstractAction 
     {
      public NewAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         newTextFile(null);      
        }
     }

   /**************/
   /* OpenAction */
   /**************/
   class OpenAction extends AbstractAction 
     {
      public OpenAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         final JFileChooser fc = new JFileChooser();

         File currentDirectory = ide.getPreferences().getCurrentDirectory();

         if (currentDirectory != null)
           { fc.setCurrentDirectory(currentDirectory); }

         int returnVal = fc.showOpenDialog(ide);
      
         if (returnVal != JFileChooser.APPROVE_OPTION) return;
      
         File file = fc.getSelectedFile();
         if (file == null) return;
            
         currentDirectory = fc.getCurrentDirectory();
         ide.getPreferences().setCurrentDirectory(currentDirectory);
      
         if (ide.getDialogWindow().setDirectory(currentDirectory))
           { ide.getPreferences().saveCurrentDirectory(currentDirectory); }
         
         newTextFile(file);      
        }
     }

   /**************/
   /* SaveAction */
   /**************/
   class SaveAction extends AbstractAction 
     {
      public SaveAction(
        String text)
        {
         super(text);
        }
        
      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
         
         if (theFrame instanceof TextFrame)
           {
            TextFrame theTextFrame = (TextFrame) theFrame;
            
            theTextFrame.saveTextFrame(ide.getPreferences());
           }
        }
     }
     
   /****************/
   /* SaveAsAction */
   /****************/
   class SaveAsAction extends AbstractAction 
     {
      public SaveAsAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();

         if (theFrame instanceof TextFrame)
           {
            TextFrame theTextFrame = (TextFrame) theFrame;
            
            theTextFrame.saveAsTextFrame(ide.getPreferences());
           }
        }
     }

   /**************/
   /* QuitAction */
   /**************/
   class QuitAction extends AbstractAction 
     {
      public QuitAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         System.exit(0);
        }
     }
   
   /***************/
   /* newTextFile */
   /***************/  
   public void newTextFile(
     File theFile)
     {
      TextFrame theFrame = new TextFrame(theFile);
      
      theFrame.addInternalFrameListener(ide);
      
      ide.getPlacer().placeInternalFrame(theFrame);
      
      ide.getDesktopPane().add(theFrame);
      theFrame.setVisible(true);
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
      JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
     
      if ((theFrame == null) || theFrame.isIcon())
        {
         saveAction.setEnabled(false);
         saveAsAction.setEnabled(false);
         return;
        }
       
      if (theFrame instanceof DialogFrame)
        {
         saveAction.setEnabled(false);
         saveAsAction.setEnabled(false);
        }
      else if (theFrame instanceof TextFrame)
        {
         TextFrame theTextFrame = (TextFrame) theFrame;
         
         saveAsAction.setEnabled(true);

         if (theTextFrame.canSave())
           { saveAction.setEnabled(true); }
         else
           { saveAction.setEnabled(false); }
        }
      else
        {
         saveAction.setEnabled(false);
         saveAsAction.setEnabled(false);
        }
     }
   
   /******************/
   /* menuDeselected */
   /******************/  
   public void menuDeselected(MenuEvent e)
     {
     }
  }  
