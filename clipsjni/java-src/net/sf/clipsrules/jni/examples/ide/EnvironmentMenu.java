package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;

import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import javax.swing.JFileChooser;
import java.io.File;

import javax.swing.filechooser.FileNameExtensionFilter;

import javax.swing.KeyStroke;
import java.awt.event.KeyEvent;

import java.util.prefs.Preferences;

/*
import net.sf.clipsrules.jni.*;
*/
public class EnvironmentMenu extends JMenu 
                          implements ActionListener, MenuListener
  {  
   private DialogFrame dialogWindow;

   private static final String clearAction = "Clear";
   private static final String loadConstructsAction = "LoadConstructs";
   private static final String loadBatchAction = "LoadBatch";
   private static final String setDirectoryAction = "SetDirectory";
   private static final String resetAction = "Reset";
   private static final String runAction = "Run";
   private static final String stepAction = "Step";
   private static final String haltRulesAction = "HaltRules";
   private static final String haltExecutionAction = "HaltExecution";
   private static final String clearScrollbackAction = "ClearScrollback";

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

      KeyStroke loadConstructs = KeyStroke.getKeyStroke(KeyEvent.VK_L,KeyEvent.CTRL_MASK);
      KeyStroke loadBatch = KeyStroke.getKeyStroke(KeyEvent.VK_L,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);
     
      KeyStroke reset = KeyStroke.getKeyStroke(KeyEvent.VK_R,KeyEvent.CTRL_MASK);
      KeyStroke run = KeyStroke.getKeyStroke(KeyEvent.VK_R,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);
      KeyStroke haltRules = KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD,KeyEvent.CTRL_MASK);
      KeyStroke haltExecution = KeyStroke.getKeyStroke(KeyEvent.VK_PERIOD,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);

      jmiClear = new JMenuItem("Clear");
      jmiClear.setActionCommand(clearAction);
      jmiClear.addActionListener(this);
      add(jmiClear);
 
      jmiLoadConstructs = new JMenuItem("Load Constructs...");
      jmiLoadConstructs.setActionCommand(loadConstructsAction);
      jmiLoadConstructs.setAccelerator(loadConstructs);
      jmiLoadConstructs.addActionListener(this);
      add(jmiLoadConstructs);

      jmiLoadBatch = new JMenuItem("Load Batch...");
      jmiLoadBatch.setActionCommand(loadBatchAction);
      jmiLoadBatch.setAccelerator(loadBatch);
      jmiLoadBatch.addActionListener(this);
      add(jmiLoadBatch);

      jmiSetDirectory = new JMenuItem("Set Directory...");
      jmiSetDirectory.setActionCommand(setDirectoryAction);
      jmiSetDirectory.addActionListener(this);
      add(jmiSetDirectory);

      addSeparator();

      jmiReset = new JMenuItem("Reset");
      jmiReset.setActionCommand(resetAction);
      jmiReset.setAccelerator(reset);
      jmiReset.addActionListener(this);
      add(jmiReset);

      jmiRun = new JMenuItem("Run"); 
      jmiRun.setActionCommand(runAction);
      jmiRun.setAccelerator(run);
      jmiRun.addActionListener(this);
      add(jmiRun);

      jmiHaltRules = new JMenuItem("Halt Rules"); 
      jmiHaltRules.setActionCommand(haltRulesAction);
      jmiHaltRules.setAccelerator(haltRules);
      jmiHaltRules.addActionListener(this);
      add(jmiHaltRules);

      jmiHaltExecution = new JMenuItem("Halt Execution"); 
      jmiHaltExecution.setActionCommand(haltExecutionAction);
      jmiHaltExecution.setAccelerator(haltExecution);
      jmiHaltExecution.addActionListener(this);
      add(jmiHaltExecution);

      addSeparator();
      
      jmiClearScrollback = new JMenuItem("Clear Scrollback");
      jmiClearScrollback.setActionCommand(clearScrollbackAction);
      jmiClearScrollback.addActionListener(this);
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
     
   /*################*/
   /* Action Methods */
   /*################*/

   /*********************/
   /* onActionPerformed */
   /*********************/  
   public void onActionPerformed(
     ActionEvent ae) throws Exception 
     {     
      if (ae.getActionCommand().equals(clearAction))  
        { clear(); }
      else if (ae.getActionCommand().equals(loadConstructsAction))  
        { loadConstructs(); }
      else if (ae.getActionCommand().equals(loadBatchAction))  
        { loadBatch(); }
      else if (ae.getActionCommand().equals(setDirectoryAction))  
        { setDirectory(); }
      else if (ae.getActionCommand().equals(resetAction))
        { reset(); }
      else if (ae.getActionCommand().equals(runAction))
        { run(); }
      else if (ae.getActionCommand().equals(stepAction))
        { step(); }
      else if (ae.getActionCommand().equals(haltRulesAction))
        { haltRules(); }
      else if (ae.getActionCommand().equals(haltExecutionAction))  
        { haltExecution(); }
      else if (ae.getActionCommand().equals(clearScrollbackAction))  
        { clearScrollback(); }
     }     
     
   /******************/
   /* loadConstructs */
   /******************/  
   public void loadConstructs()
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
   public void loadBatch()
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
   public void setDirectory()
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
     
   /*********/
   /* clear */
   /*********/  
   public void clear()
     {
      dialogWindow.replaceCommand("(clear)\n");
     }
     
   /*********/
   /* reset */
   /*********/  
   public void reset()
     {
      dialogWindow.replaceCommand("(reset)\n");
     }

   /*******/
   /* run */
   /*******/  
   public void run()
     {
      dialogWindow.replaceCommand("(run)\n");
     }

   /********/
   /* step */
   /********/  
   public void step()
     {
      dialogWindow.replaceCommand("(run 1)\n");
     }

   /*************/
   /* haltRules */
   /*************/  
   public void haltRules()
     {
      dialogWindow.haltRules();
     }

   /*****************/
   /* haltExecution */
   /*****************/  
   public void haltExecution()
     {
      dialogWindow.haltExecution();
     }
     
   /*******************/
   /* clearScrollback */
   /*******************/  
   public void clearScrollback()
     {
      dialogWindow.clearScrollback();
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
     }
  }  
