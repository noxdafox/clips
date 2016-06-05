package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import java.io.File;

import java.util.prefs.Preferences;

import javax.swing.AbstractAction;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

public class EnvironmentMenu extends JMenu 
                          implements MenuListener
  {  
   private DialogFrame dialogWindow;

   private ClearAction clearAction;
   private LoadConstructsAction loadConstructsAction;
   private LoadBatchAction loadBatchAction;
   private SetDirectoryAction setDirectoryAction;
   
   private ResetAction resetAction;
   private RunAction runAction;
   private HaltRulesAction haltRulesAction;
   private HaltExecutionAction haltExecutionAction;

   private ClearScrollbackAction clearScrollbackAction;

   private JMenuItem jmiClear = null;
   private JMenuItem jmiLoadConstructs = null;
   private JMenuItem jmiLoadBatch = null;
   private JMenuItem jmiSetDirectory = null;
   
   private JMenuItem jmiReset = null;
   private JMenuItem jmiRun = null;
   private JMenuItem jmiHaltRules = null;
   private JMenuItem jmiHaltExecution = null;
   
   private JMenuItem jmiClearScrollback = null;
   
   private IDEPreferences preferences;

   /*******************/
   /* EnvironmentMenu */
   /*******************/
   EnvironmentMenu(
     IDEPreferences thePreferences)
     {  
      super("Environment");
       
      preferences = thePreferences;
      
      addMenuListener(this);

      /*==================================*/
      /* Get KeyStrokes for accelerators. */
      /*==================================*/

      KeyStroke loadConstructs = KeyStroke.getKeyStroke(KeyEvent.VK_L,KeyEvent.CTRL_MASK);
      KeyStroke loadBatch = KeyStroke.getKeyStroke(KeyEvent.VK_L,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);
     
      KeyStroke reset = KeyStroke.getKeyStroke(KeyEvent.VK_R,KeyEvent.CTRL_MASK);
      KeyStroke run = KeyStroke.getKeyStroke(KeyEvent.VK_R,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);
      KeyStroke haltRules = KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD,KeyEvent.CTRL_MASK);
      KeyStroke haltExecution = KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);

      /*================*/
      /* Setup actions. */
      /*================*/
       
      clearAction = new ClearAction("Clear");
      loadConstructsAction = new LoadConstructsAction("Load Constructs...");
      loadBatchAction = new LoadBatchAction("Load Batch...");
      setDirectoryAction = new SetDirectoryAction("Set Directory...");
      
      resetAction = new ResetAction("Reset");
      runAction = new RunAction("Run");
      haltRulesAction = new HaltRulesAction("Halt Rules");
      haltExecutionAction = new HaltExecutionAction("Halt Execution");

      clearScrollbackAction = new ClearScrollbackAction("Clear Scrollback");
      
      /*=================*/
      /* Add menu items. */
      /*=================*/

      jmiClear = new JMenuItem(clearAction);
      add(jmiClear);
 
      jmiLoadConstructs = new JMenuItem(loadConstructsAction);
      jmiLoadConstructs.setAccelerator(loadConstructs);
      add(jmiLoadConstructs);

      jmiLoadBatch = new JMenuItem(loadBatchAction);
      jmiLoadBatch.setAccelerator(loadBatch);
      add(jmiLoadBatch);

      jmiSetDirectory = new JMenuItem(setDirectoryAction);
      add(jmiSetDirectory);

      addSeparator();

      jmiReset = new JMenuItem(resetAction);
      jmiReset.setAccelerator(reset);
      add(jmiReset);

      jmiRun = new JMenuItem(runAction); 
      jmiRun.setAccelerator(run);
      add(jmiRun);

      jmiHaltRules = new JMenuItem(haltRulesAction); 
      jmiHaltRules.setAccelerator(haltRules);
      add(jmiHaltRules);

      jmiHaltExecution = new JMenuItem(haltExecutionAction); 
      jmiHaltExecution.setAccelerator(haltExecution);
      add(jmiHaltExecution);

      addSeparator();
      
      jmiClearScrollback = new JMenuItem(clearScrollbackAction);
      add(jmiClearScrollback);
     }
       
   /*************/
   /* setDialog */
   /*************/
   public void setDialog(
     DialogFrame theDialog)
     {  
      dialogWindow = theDialog;
     }

   /******************/
   /* loadConstructs */
   /******************/  
   private void loadConstructs()
     {
      final JFileChooser fc = new JFileChooser();
      
      FileNameExtensionFilter filter 
         = new FileNameExtensionFilter("Constructs File","clp");
      fc.setFileFilter(filter);

      File currentDirectory = preferences.getCurrentDirectory();
      if (currentDirectory != null)
        { fc.setCurrentDirectory(currentDirectory); }

      fc.setApproveButtonText("Load");
      fc.setDialogTitle("Load Constructs");
      
      int returnVal = fc.showOpenDialog(this);
      
      if (returnVal != JFileChooser.APPROVE_OPTION) return;
      
      File file = fc.getSelectedFile();
            
      currentDirectory = fc.getCurrentDirectory();
      preferences.setCurrentDirectory(currentDirectory);

      if (dialogWindow.setDirectory(currentDirectory))
        { 
         dialogWindow.replaceCommand("(load \"" + 
                                     file.getName() + 
                                     "\")\n");
         preferences.saveCurrentDirectory(currentDirectory);
        }
      else
        {
         dialogWindow.replaceCommand("(load \"" + 
                                     file.getAbsolutePath() + 
                                     "\")\n");
        }
     }
     
   /*************/
   /* loadBatch */
   /*************/  
   private void loadBatch()
     {
      final JFileChooser fc = new JFileChooser();

      FileNameExtensionFilter filter 
         = new FileNameExtensionFilter("Batch File","bat","tst");
      fc.setFileFilter(filter);

      File currentDirectory = preferences.getCurrentDirectory();
      if (currentDirectory != null)
        { fc.setCurrentDirectory(currentDirectory); }

      fc.setApproveButtonText("Load");
      fc.setDialogTitle("Load Batch");
      
      int returnVal = fc.showOpenDialog(this);
      
      if (returnVal != JFileChooser.APPROVE_OPTION) return;
      
      File file = fc.getSelectedFile();
            
      currentDirectory = fc.getCurrentDirectory();
      preferences.setCurrentDirectory(currentDirectory);

      if (dialogWindow.setDirectory(currentDirectory))
        { 
         dialogWindow.replaceCommand("(batch \"" + 
                                     file.getName() + 
                                     "\")\n");
         preferences.saveCurrentDirectory(currentDirectory);
        }
      else
        {
         dialogWindow.replaceCommand("(batch \"" + 
                                     file.getAbsolutePath() + 
                                     "\")\n");
        }
     }

   /****************/
   /* setDirectory */
   /****************/  
   private void setDirectory()
     {
      final JFileChooser fc = new JFileChooser();

      File currentDirectory = preferences.getCurrentDirectory();
      if (currentDirectory != null)
        { fc.setCurrentDirectory(currentDirectory); }
        
      fc.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      fc.setApproveButtonText("Set");
      fc.setDialogTitle("Set Directory");
      
      int returnVal = fc.showOpenDialog(this);
      
      if (returnVal != JFileChooser.APPROVE_OPTION) return;
      
      File file = fc.getSelectedFile();
            
      currentDirectory = file.getAbsoluteFile();
      preferences.setCurrentDirectory(currentDirectory);
      
      if (dialogWindow.setDirectory(currentDirectory))
        { preferences.saveCurrentDirectory(currentDirectory); }
     }
         
   /*########################*/
   /* AbstractAction Classes */
   /*########################*/

   /***************/
   /* ClearAction */
   /***************/
   class ClearAction extends AbstractAction 
     {
      public ClearAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (! dialogWindow.isExecuting())
           { dialogWindow.replaceCommand("(clear)\n"); }
        }
     }

   /************************/
   /* LoadConstructsAction */
   /************************/
   class LoadConstructsAction extends AbstractAction 
     {
      public LoadConstructsAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (! dialogWindow.isExecuting())
           { loadConstructs(); }
        }
     }

   /*******************/
   /* LoadBatchAction */
   /*******************/
   class LoadBatchAction extends AbstractAction 
     {
      public LoadBatchAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (! dialogWindow.isExecuting())
           { loadBatch(); }
        }
     }

   /**********************/
   /* SetDirectoryAction */
   /**********************/
   class SetDirectoryAction extends AbstractAction 
     {
      public SetDirectoryAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (! dialogWindow.isExecuting())
           { setDirectory(); }
        }
     }

   /***************/
   /* ResetAction */
   /***************/
   class ResetAction extends AbstractAction 
     {
      public ResetAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (! dialogWindow.isExecuting())
           { dialogWindow.replaceCommand("(reset)\n"); }
        }
     }
     
   /*************/
   /* RunAction */
   /*************/
   class RunAction extends AbstractAction 
     {
      public RunAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (! dialogWindow.isExecuting())
           { dialogWindow.replaceCommand("(run)\n"); }
        }
     }

   /*******************/
   /* HaltRulesAction */
   /*******************/
   class HaltRulesAction extends AbstractAction 
     {
      public HaltRulesAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (dialogWindow.isExecuting())
           { dialogWindow.haltRules(); }
        }
     }

   /***********************/
   /* HaltExecutionAction */
   /***********************/
   class HaltExecutionAction extends AbstractAction 
     {
      public HaltExecutionAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (dialogWindow.isExecuting())
           { dialogWindow.haltExecution(); }
        }
     }
     
   /*************************/
   /* ClearScrollbackAction */
   /*************************/
   class ClearScrollbackAction extends AbstractAction 
     {
      public ClearScrollbackAction(
        String text)
        {
         super(text);
        }

      @Override
      public void actionPerformed(
        ActionEvent e)
        {
         if (! dialogWindow.isExecuting())
           { dialogWindow.clearScrollback(); }
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
      if (dialogWindow.isExecuting())
        {
         jmiClear.setEnabled(false);
         jmiLoadConstructs.setEnabled(false);
         jmiLoadBatch.setEnabled(false);
         jmiSetDirectory.setEnabled(false);
         jmiReset.setEnabled(false);
         jmiRun.setEnabled(false);
         jmiHaltRules.setEnabled(true);
         jmiHaltExecution.setEnabled(true);
         jmiClearScrollback.setEnabled(false);
        }
      else
        {
         jmiClear.setEnabled(true);
         jmiLoadConstructs.setEnabled(true);
         jmiLoadBatch.setEnabled(true);
         jmiSetDirectory.setEnabled(true);
         jmiReset.setEnabled(true);
         jmiRun.setEnabled(true);
         jmiHaltRules.setEnabled(false);
         jmiHaltExecution.setEnabled(false);
         jmiClearScrollback.setEnabled(true);
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
