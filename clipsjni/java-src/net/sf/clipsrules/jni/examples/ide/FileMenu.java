package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowEvent;
import java.awt.print.PageFormat;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

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
   private CLIPSIDE ide;
   
   private PrinterJob job;
   private PageFormat format;

   private NewAction newAction;
   private OpenAction openAction;
   private CloseAction closeAction;
   private SaveAction saveAction;
   private SaveAsAction saveAsAction;
   private PageSetupAction pageSetupAction;
   private PrintAction printAction;
   private QuitAction quitAction;

   private JMenuItem jmiNew;
   private JMenuItem jmiOpen;
   private JMenuItem jmiClose;
   private JMenuItem jmiSave;
   private JMenuItem jmiSaveAs;
   private JMenuItem jmiPageSetup;
   private JMenuItem jmiPrint;
   private JMenuItem jmiQuitIDE;

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
      KeyStroke closeDoc = KeyStroke.getKeyStroke(KeyEvent.VK_W,KeyEvent.CTRL_MASK);
      KeyStroke saveDoc = KeyStroke.getKeyStroke(KeyEvent.VK_S,KeyEvent.CTRL_MASK);
      KeyStroke saveAsDoc = KeyStroke.getKeyStroke(KeyEvent.VK_S,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);
      KeyStroke quitIDE = KeyStroke.getKeyStroke(KeyEvent.VK_Q,KeyEvent.CTRL_MASK);

      /*================*/
      /* Setup actions. */
      /*================*/
       
      newAction = new NewAction("New");
      openAction = new OpenAction("Open...");
      closeAction = new CloseAction("Close");
      saveAction = new SaveAction("Save");
      saveAsAction = new SaveAsAction("Save As...");
      pageSetupAction = new PageSetupAction("Page Setup...");
      printAction = new PrintAction("Print...");
      quitAction = new QuitAction("Quit CLIPS IDE");

      /*=================*/
      /* Add menu items. */
      /*=================*/
      
      jmiNew = new JMenuItem(newAction);
      jmiNew.setAccelerator(newDoc);
      this.add(jmiNew);

      jmiOpen = new JMenuItem(openAction);
      jmiOpen.setAccelerator(openDoc);
      this.add(jmiOpen);

      this.addSeparator();

      jmiClose = new JMenuItem(closeAction);
      jmiClose.setAccelerator(closeDoc);
      this.add(jmiClose);

      jmiSave = new JMenuItem(saveAction);
      jmiSave.setAccelerator(saveDoc);
      this.add(jmiSave);

      jmiSaveAs = new JMenuItem(saveAsAction);
      jmiSaveAs.setAccelerator(saveAsDoc);
      this.add(jmiSaveAs);
      
      this.addSeparator();

      jmiPageSetup = new JMenuItem(pageSetupAction);
      this.add(jmiPageSetup);

      jmiPrint = new JMenuItem(printAction);
      this.add(jmiPrint);

      this.addSeparator();

      jmiQuitIDE = new JMenuItem(quitAction);
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

   /***************/
   /* CloseAction */
   /***************/
   class CloseAction extends AbstractAction 
     {
      public CloseAction(
        String text)
        {
         super(text);
        }
        
      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
         
         theFrame.doDefaultCloseAction();
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

   /*******************/
   /* PageSetupAction */
   /*******************/
   class PageSetupAction extends AbstractAction 
     {
      public PageSetupAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         job = PrinterJob.getPrinterJob();
         format = job.pageDialog(job.defaultPage());    
        }
     }

   /***************/
   /* PrintAction */
   /***************/
   class PrintAction extends AbstractAction 
     {
      public PrintAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         job = PrinterJob.getPrinterJob();

         if (job.printDialog())
           {
            try
              { job.print(); } 
            catch (PrinterException err)
              { err.printStackTrace(); }
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
         WindowEvent windowClosing = new WindowEvent(ide,WindowEvent.WINDOW_CLOSING);
	     ide.dispatchEvent(windowClosing);
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
      for (int i = 0; i < this.getItemCount(); i++)
        { 
         JMenuItem theItem = this.getItem(i);
         if (theItem != null) theItem.setEnabled(true);
        }
     }
   
   /****************/
   /* menuSelected */
   /****************/  
   public void menuSelected(MenuEvent e)
     {
      JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
     
      if ((theFrame == null) || theFrame.isIcon())
        {
         jmiClose.setEnabled(false);
         jmiSave.setEnabled(false);
         jmiSaveAs.setEnabled(false);
         return;
        }
       
      if (theFrame instanceof DialogFrame)
        {
         jmiClose.setEnabled(false);
         jmiSave.setEnabled(false);
         jmiSaveAs.setEnabled(false);
        }
      else if (theFrame instanceof TextFrame)
        {
         TextFrame theTextFrame = (TextFrame) theFrame;
         
         jmiClose.setEnabled(true);
         jmiSaveAs.setEnabled(true);

         if (theTextFrame.canSave())
           { jmiSave.setEnabled(true); }
         else
           { jmiSave.setEnabled(false); }
        }
      else
        {
         jmiClose.setEnabled(true);
         jmiSave.setEnabled(false);
         jmiSaveAs.setEnabled(false);
        }
     }
   
   /******************/
   /* menuDeselected */
   /******************/  
   public void menuDeselected(MenuEvent e)
     {
      for (int i = 0; i < this.getItemCount(); i++)
        { 
         JMenuItem theItem = this.getItem(i);
         if (theItem != null) theItem.setEnabled(true);
        }
     }
  }  
