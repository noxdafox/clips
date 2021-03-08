package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.Font;

import java.io.File;

import java.util.prefs.Preferences;
import java.util.HashMap;

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

public class EditMenu extends JMenu 
                      implements MenuListener
  {  
   private UndoAction undoAction;
   private RedoAction redoAction;
   private CutAction cutAction;
   private CopyAction copyAction;
   private PasteAction pasteAction;
   private SetFontsAction setFontsAction;
   
   private JMenuItem jmiUndo;
   private JMenuItem jmiRedo;
   private JMenuItem jmiCut;
   private JMenuItem jmiCopy;
   private JMenuItem jmiPaste;
   private JMenuItem jmiSetFonts;

   private CLIPSIDE ide;

   /************/
   /* EditMenu */
   /************/
   EditMenu(
     CLIPSIDE theIDE)
     {  
      super("Edit");
             
      ide = theIDE;
      
      addMenuListener(this);

      /*==================================*/
      /* Get KeyStrokes for accelerators. */
      /*==================================*/

      KeyStroke undo = KeyStroke.getKeyStroke(KeyEvent.VK_Z,KeyEvent.CTRL_DOWN_MASK);
      KeyStroke redo = KeyStroke.getKeyStroke(KeyEvent.VK_Z,KeyEvent.CTRL_DOWN_MASK | KeyEvent.SHIFT_DOWN_MASK);

      KeyStroke cut = KeyStroke.getKeyStroke(KeyEvent.VK_X,KeyEvent.CTRL_DOWN_MASK);
      KeyStroke copy = KeyStroke.getKeyStroke(KeyEvent.VK_C,KeyEvent.CTRL_DOWN_MASK);
      KeyStroke paste = KeyStroke.getKeyStroke(KeyEvent.VK_V,KeyEvent.CTRL_DOWN_MASK);

      /*================*/
      /* Setup actions. */
      /*================*/
       
      undoAction = new UndoAction("Undo");
      redoAction = new RedoAction("Redo");
      cutAction = new CutAction("Cut");
      copyAction = new CopyAction("Copy");
      pasteAction = new PasteAction("Paste");
      setFontsAction = new SetFontsAction("Set Fonts...");

      /*=================*/
      /* Add menu items. */
      /*=================*/
      
      jmiUndo = new JMenuItem(undoAction);
      jmiUndo.setAccelerator(undo);
      this.add(jmiUndo);
      
      jmiRedo = new JMenuItem(redoAction);
      jmiRedo.setAccelerator(redo);
      this.add(jmiRedo);

      this.addSeparator();
      
      jmiCut = new JMenuItem(cutAction);
      jmiCut.setAccelerator(cut);
      this.add(jmiCut);

      jmiCopy = new JMenuItem(copyAction);
      jmiCopy.setAccelerator(copy);
      this.add(jmiCopy);

      jmiPaste = new JMenuItem(pasteAction);
      jmiPaste.setAccelerator(paste);      
      this.add(jmiPaste);

      this.addSeparator();
      jmiSetFonts = new JMenuItem(setFontsAction);
      this.add(jmiSetFonts);
     }

   /**************/
   /* UndoAction */
   /**************/
   class UndoAction extends AbstractAction 
     {
      public UndoAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
         
         if ((theFrame == null) || theFrame.isIcon())
           { /* Do Nothing */ }
         else if (theFrame instanceof TextFrame)
           {
            TextFrame theTextFrame = (TextFrame) theFrame;
            
            if (theTextFrame.canUndo())
              { theTextFrame.undo(); }
           }
        }
     }

   /**************/
   /* RedoAction */
   /**************/
   class RedoAction extends AbstractAction 
     {
      public RedoAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
         if ((theFrame == null) || theFrame.isIcon())
           { /* Do Nothing */ }
         else if (theFrame instanceof TextFrame)
           {
            TextFrame theTextFrame = (TextFrame) theFrame;
            if (theTextFrame.canRedo())
              { theTextFrame.redo(); } 
           }
        }
     }

   /**************/
   /* CutAction */
   /**************/
   class CutAction extends AbstractAction 
     {
      public CutAction(
        String text)
        {
         super(text);
        }
        
      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
         if ((theFrame == null) || theFrame.isIcon())
           { /* Do Nothing */ }
         else if (theFrame instanceof DialogFrame)
           {
            DialogFrame theDialogFrame = (DialogFrame) theFrame;
            if (theDialogFrame.hasCuttableSelection())
              { theDialogFrame.cut(); }
           }
         else if (theFrame instanceof TextFrame)
           {
            TextFrame theTextFrame = (TextFrame) theFrame;
            if (theTextFrame.hasSelection())
              { theTextFrame.cut(); } 
           }
        }
     }
     
   /**************/
   /* CopyAction */
   /**************/
   class CopyAction extends AbstractAction 
     {
      public CopyAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
         if ((theFrame == null) || theFrame.isIcon())
           { /* Do Nothing */ }
         else if (theFrame instanceof DialogFrame)
           {
            DialogFrame theDialogFrame = (DialogFrame) theFrame;
            if (theDialogFrame.hasSelection())
              { theDialogFrame.copy(); } 
           }
         else if (theFrame instanceof TextFrame)
           {
            TextFrame theTextFrame = (TextFrame) theFrame;
            
            if (theTextFrame.hasSelection())
              { theTextFrame.copy(); } 
           }
        }
     }

   /***************/
   /* PasteAction */
   /***************/
   class PasteAction extends AbstractAction 
     {
      public PasteAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         JInternalFrame theFrame = ide.getDesktopPane().getSelectedFrame();
         if ((theFrame == null) || theFrame.isIcon())
           { /* Do Nothing */ }
         else if (theFrame instanceof DialogFrame)
           {
            DialogFrame theDialogFrame = (DialogFrame) theFrame;
            if (theDialogFrame.hasPasteableSelection())
              { theDialogFrame.paste(); }
           }
         else if (theFrame instanceof TextFrame)
           {
            TextFrame theTextFrame = (TextFrame) theFrame;
            theTextFrame.paste(); 
           }
        }
     }

   /******************/
   /* SetFontsAction */
   /******************/
   class SetFontsAction extends AbstractAction 
     {
      public SetFontsAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         HashMap<String,Font> theFonts;
         IDEPreferences preferences = ide.getPreferences();
         Font dialogFont = preferences.getDialogFont();
         Font editorFont = preferences.getEditorFont();
         Font browserFont = preferences.getBrowserFont();

         theFonts = new HashMap<String,Font>();
         
         theFonts.put("dialog",dialogFont);
         theFonts.put("editor",editorFont);
         theFonts.put("browser",browserFont);
         
         FontPreferencesDialog theDialog = new FontPreferencesDialog(ide,"Font Preferences",theFonts);
         theDialog.showDialog(theFonts);         
         
         dialogFont = theFonts.get("dialog");
         editorFont = theFonts.get("editor");
         browserFont = theFonts.get("browser");
         
         preferences.setDialogFont(dialogFont);
         preferences.saveDialogFont(dialogFont);
         ide.getDialogWindow().setFont(dialogFont);
         
         preferences.setEditorFont(editorFont);
         preferences.saveEditorFont(editorFont);
         ide.assignEditorFont(editorFont);
         
         preferences.setBrowserFont(browserFont);
         preferences.saveBrowserFont(browserFont);
         ide.assignBrowserFont(browserFont);
         
         // TBD set the font for the editing windows
         // TBD construct inspector
        }
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
         jmiUndo.setEnabled(false);
         jmiRedo.setEnabled(false);
         jmiCut.setEnabled(false);
         jmiCopy.setEnabled(false);
         jmiPaste.setEnabled(false);
         return;
        }
       
      if (theFrame instanceof DialogFrame)
        {
         DialogFrame theDialogFrame = (DialogFrame) theFrame;
         
         jmiUndo.setEnabled(false);
         jmiRedo.setEnabled(false);
         
         if (theDialogFrame.hasCuttableSelection())
           { jmiCut.setEnabled(true); }
         else
           { jmiCut.setEnabled(false); }

         if (theDialogFrame.hasSelection())
           { jmiCopy.setEnabled(true); }
         else
           { jmiCopy.setEnabled(false); }

         if (theDialogFrame.hasPasteableSelection())
           { jmiPaste.setEnabled(true); }
         else
           { jmiPaste.setEnabled(false); }
        }
      else if (theFrame instanceof TextFrame)
        {
         TextFrame theTextFrame = (TextFrame) theFrame;
         
         if (theTextFrame.canUndo())
           { jmiUndo.setEnabled(true); }
         else
           { jmiUndo.setEnabled(false); }
     
         if (theTextFrame.canRedo())
           { jmiRedo.setEnabled(true); }
         else
           { jmiRedo.setEnabled(false); }
     
         if (theTextFrame.hasSelection())
           { 
            jmiCopy.setEnabled(true); 
            jmiCut.setEnabled(true); 
           }
         else
           { 
            jmiCopy.setEnabled(false);
            jmiCut.setEnabled(false);
           }
         
         jmiPaste.setEnabled(true);
        }
      else
        {
         jmiUndo.setEnabled(false);
         jmiRedo.setEnabled(false);
         jmiCut.setEnabled(false);
         jmiCopy.setEnabled(false);
         jmiPaste.setEnabled(false);
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
