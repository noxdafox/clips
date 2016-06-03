package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*; 
import javax.swing.event.*;

import java.awt.*; 
import java.awt.event.*; 

import java.util.List; 
import java.util.Iterator;

import java.io.File;

import net.sf.clipsrules.jni.*;

public class CLIPSIDE extends JFrame 
                   implements ActionListener, CommandExecutionListener,
                              MenuListener, InternalFrameListener
  {  
   private static final String windowProperty = "windowProperty";
   private static final String menuItemProperty = "menuItemProperty";
   static final String selectWindowAction = "SelectWindow";
   
   private FileMenu jmFile;
   private TextMenu jmText;
   private EnvironmentMenu jmEnvironment;
   private JMenu jmWindow;

   private IDEPreferences preferences;
   
   private JMenuItem jmiUndo;
   private JMenuItem jmiRedo;
   private JMenuItem jmiCut;
   private JMenuItem jmiCopy;
   private JMenuItem jmiPaste;

   private JMenuItem jmiAgendaBrowser;
   private JMenuItem jmiFactBrowser;
   private JMenuItem jmiInstanceBrowser;
   private JMenuItem jmiConstructInspector;
   
   private Environment clips;
   private ConstructInspectorFrame constructInspector;
   private FramePlacer placer;
   private AgendaBrowserManager agendaBrowserManager;
   private FactBrowserManager factBrowserManager;
   private InstanceBrowserManager instanceBrowserManager;
   private DialogFrame dialogWindow;
   private JDesktopPane ideDesktopPane;
   
   /************/
   /* CLIPSIDE */
   /************/
   CLIPSIDE()
     {  
      super("CLIPS IDE");

      /*===================================*/
      /* Use cross platform look and feel. */
      /*===================================*/

      try  
        { UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName()); }
      catch (Exception e)
        { e.printStackTrace(); }
     
      preferences = new IDEPreferences();
      
      clips = new Environment();
      
      /*======================*/
      /* Create the menu bar. */
      /*======================*/

      createMenuBar(clips);

      /*====================*/
      /* Display the frame. */
      /*====================*/
      
      ideDesktopPane = new JDesktopPane();
      ideDesktopPane.setDesktopManager(new BoundsDesktopManager());
      add(ideDesktopPane);
      
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      setLocation(50,50);
      Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      setSize((int) (screenSize.width * 0.80),(int) (screenSize.height * 0.80));
      setVisible(true);
      
      placer = new FramePlacer(ideDesktopPane);
      
      /*****************************/
      /* Create the dialog window. */
      /*****************************/
      
      createDialogWindow(clips);
      dialogWindow.addCommandExecutionListener(this);
      jmEnvironment.setDialog(dialogWindow);
      
      /********************************/
      /* Create the browser managers. */
      /********************************/
      
      agendaBrowserManager = new AgendaBrowserManager(this);
      factBrowserManager = new FactBrowserManager(this);
      instanceBrowserManager = new InstanceBrowserManager(this);

      /*========================================*/
      /* Create the clear-window user function. */
      /*========================================*/
      
      clips.addUserFunction("clear-window","00",
            new UserFunction()
              {
               public PrimitiveValue evaluate(List<PrimitiveValue> arguments)
                 {
                  dialogWindow.clearScrollback();
                  return null;
                 }
              });
            
      /*==================================*/
      /* Add some example user functions. */
      /*==================================*/

      UserFunctionExamples.addUserFunctions(clips);
     }  

   /******************/
   /* getPreferences */
   /******************/  
   public IDEPreferences getPreferences()
     {
      return preferences;
     }

   /******************/
   /* getEnvironment */
   /******************/  
   public Environment getEnvironment()
     {
      return clips;
     }
     
   /*******************/
   /* getDialogWindow */
   /*******************/  
   public DialogFrame getDialogWindow()
     {
      return dialogWindow;
     }
     
   /******************/
   /* getDesktopPane */
   /******************/  
   public JDesktopPane getDesktopPane()
     {
      return ideDesktopPane;
     }

   /*************************/
   /* getConstructInspector */
   /*************************/  
   public ConstructInspectorFrame getConstructInspector()
     {
      return constructInspector;
     }
     
   /*************/
   /* getPlacer */
   /*************/  
   public FramePlacer getPlacer()
     {
      return placer;
     }
     
   /**********************/
   /* createDialogWindow */
   /**********************/  
   public void createDialogWindow(
     Environment theEnvironment)
     {
      dialogWindow = new DialogFrame(theEnvironment,preferences.getCurrentDirectory());
      dialogWindow.addInternalFrameListener(this);
      
      placer.placeInternalFrame(dialogWindow);
      
      ideDesktopPane.add(dialogWindow);
      dialogWindow.setVisible(true);
     }
     
   /**********************/
   /* selectDialogWindow */
   /**********************/  
   public void selectDialogWindow()
     {
      try
        { dialogWindow.setSelected(true); }
      catch (Exception e) 
        { e.printStackTrace(); }
     }
          
   /********/
   /* main */
   /********/  
   public static void main(String args[])
     {  
      CLIPSIDE ide = new CLIPSIDE();  
     } 
   
   /*********************************/
   /* commandExecutionEventOccurred */
   /*********************************/  
   public void commandExecutionEventOccurred(
     CommandExecutionEvent theEvent)
     {    
      if (theEvent.getExecutionEvent().equals(CommandExecutionEvent.START_EVENT))
        { agendaBrowserManager.updateAgendaBrowserButtons(true); }
     
      if (theEvent.getExecutionEvent().equals(CommandExecutionEvent.FINISH_EVENT))
        { agendaBrowserManager.updateAgendaBrowserButtons(false); }
     
      if (theEvent.getExecutionEvent().equals(CommandExecutionEvent.PERIODIC_EVENT) ||
          theEvent.getExecutionEvent().equals(CommandExecutionEvent.FINISH_EVENT))
        {
         if (dialogWindow.getEnvironment().getAgendaChanged())
           {
            dialogWindow.getEnvironment().setAgendaChanged(false);
            agendaBrowserManager.updateAllBrowsers();
           } 

         if (dialogWindow.getEnvironment().getFactListChanged())
           {
            dialogWindow.getEnvironment().setFactListChanged(false);
            factBrowserManager.updateAllBrowsers();
           } 

         if (dialogWindow.getEnvironment().getInstancesChanged())
           {
            dialogWindow.getEnvironment().setInstancesChanged(false);
            instanceBrowserManager.updateAllBrowsers();
           } 
        }
     }  

   /****************/
   /* executeBatch */
   /****************/
   public void executeBatch()
     {
      dialogWindow.executeBatch();
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
      JInternalFrame theFrame = ideDesktopPane.getSelectedFrame();

      if (ae.getSource() == jmiUndo)
        { 
          if ((theFrame == null) || theFrame.isIcon())
            { /* Do Nothing */ }
          else if (theFrame instanceof TextFrame)
            {
             TextFrame theTextFrame = (TextFrame) theFrame;
             theTextFrame.undo(); 
            }
        }
      else if (ae.getSource() == jmiRedo)
        { 
          if ((theFrame == null) || theFrame.isIcon())
            { /* Do Nothing */ }
          else if (theFrame instanceof TextFrame)
            {
             TextFrame theTextFrame = (TextFrame) theFrame;
             theTextFrame.redo(); 
            }
        }
      else if (ae.getSource() == jmiCut)
        { 
          if ((theFrame == null) || theFrame.isIcon())
            { /* Do Nothing */ }
          else if (theFrame instanceof DialogFrame)
            {
             DialogFrame theDialogFrame = (DialogFrame) theFrame;
             theDialogFrame.cut(); 
            }
          else if (theFrame instanceof TextFrame)
            {
             TextFrame theTextFrame = (TextFrame) theFrame;
             theTextFrame.cut(); 
            }
        }
      else if (ae.getSource() == jmiCopy)  
        { 
          if ((theFrame == null) || theFrame.isIcon())
            { /* Do Nothing */ }
          else if (theFrame instanceof DialogFrame)
            {
             DialogFrame theDialogFrame = (DialogFrame) theFrame;
             theDialogFrame.copy(); 
            }
          else if (theFrame instanceof TextFrame)
            {
             TextFrame theTextFrame = (TextFrame) theFrame;
             theTextFrame.copy(); 
            }
        }
      else if (ae.getSource() == jmiPaste)  
        { 
          if ((theFrame == null) || theFrame.isIcon())
            { /* Do Nothing */ }
          else if (theFrame instanceof DialogFrame)
            {
             DialogFrame theDialogFrame = (DialogFrame) theFrame;
             theDialogFrame.paste(); 
            }
          else if (theFrame instanceof TextFrame)
            {
             TextFrame theTextFrame = (TextFrame) theFrame;
             theTextFrame.paste(); 
            }
        }
      else if (ae.getSource() == jmiAgendaBrowser)  
        { agendaBrowserManager.createBrowser(); }
      else if (ae.getSource() == jmiFactBrowser)  
        { factBrowserManager.createBrowser(); }
      else if (ae.getSource() == jmiInstanceBrowser)  
        { instanceBrowserManager.createBrowser();  }
      else if (ae.getSource() == jmiConstructInspector)  
        { constructInspector(); }
      else if (ae.getActionCommand().equals(selectWindowAction))  
        { selectWindow(ae.getSource()); }
     }
               
   /**********************/
   /* constructInspector */
   /**********************/  
   public void constructInspector()
     {
      String theText = "";
      
      if (constructInspector != null) return;

      JInternalFrame selectedPane = ideDesktopPane.getSelectedFrame();
      
      if (selectedPane instanceof AgendaBrowserFrame)
        { theText = agendaBrowserManager.browserSelectionText((AgendaBrowserFrame) selectedPane); } 
      else if (selectedPane instanceof EntityBrowserFrame)
        {
         EntityBrowserFrame theEntityFrame = (EntityBrowserFrame) selectedPane;
      
         if (theEntityFrame.getEntityName().equals(factBrowserManager.ENTITY_NAME))
           { theText = factBrowserManager.browserSelectionText(theEntityFrame); }
         else if (theEntityFrame.getEntityName().equals(instanceBrowserManager.ENTITY_NAME))
           { theText = instanceBrowserManager.browserSelectionText(theEntityFrame); }
        }
      
      constructInspector = new ConstructInspectorFrame();

      constructInspector.setText(theText); 

      constructInspector.addInternalFrameListener(this);
             
      constructInspector.setLocation(10,10);
      
      ideDesktopPane.add(constructInspector,JLayeredPane.PALETTE_LAYER);

      constructInspector.setVisible(true);
     }
           
   /****************/
   /* selectWindow */
   /****************/  
   public void selectWindow(
     Object source)
     {
      JCheckBoxMenuItem jmiWindow = (JCheckBoxMenuItem) source;
      JInternalFrame theFrame = (JInternalFrame) jmiWindow.getClientProperty(windowProperty);

      if (theFrame.isSelected())
        {
         jmiWindow.setState(true);
         return;
        }

      try
        { theFrame.setSelected(true); }
      catch (Exception e) 
        { e.printStackTrace(); }
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
      if (constructInspector == null)
        { jmiConstructInspector.setEnabled(true); }
      else
        { jmiConstructInspector.setEnabled(false); }

      JInternalFrame theFrame = ideDesktopPane.getSelectedFrame();
     
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
     }
          
   /*###############################*/
   /* InternalFrameListener Methods */
   /*###############################*/
     
   /************************/
   /* internalFrameClosing */
   /************************/
   public void internalFrameClosing(
     InternalFrameEvent e)
     {
     }

   /***********************/
   /* internalFrameClosed */
   /***********************/
   public void internalFrameClosed(
     InternalFrameEvent e) 
     {
      JInternalFrame theFrame = e.getInternalFrame();
      theFrame.removeInternalFrameListener(this);

      if (theFrame instanceof ConstructInspectorFrame)
        { 
         constructInspector = null;
         return; 
        }

      if (theFrame instanceof TextFrame)
        { jmText.setTextFrame(null); }
      else if (theFrame instanceof AgendaBrowserFrame)
        { agendaBrowserManager.removeBrowser((AgendaBrowserFrame) theFrame); }
      else if (theFrame instanceof EntityBrowserFrame)
        {
         EntityBrowserFrame theEntityFrame = (EntityBrowserFrame) theFrame;
         
         if (theEntityFrame.getEntityName().equals(factBrowserManager.ENTITY_NAME))
           { factBrowserManager.removeBrowser(theEntityFrame); }
         else if (theEntityFrame.getEntityName().equals(instanceBrowserManager.ENTITY_NAME))
           { instanceBrowserManager.removeBrowser(theEntityFrame); }
        }

      JCheckBoxMenuItem jmiWindow = (JCheckBoxMenuItem) theFrame.getClientProperty(menuItemProperty);
      
      theFrame.putClientProperty(menuItemProperty,null);
      jmiWindow.putClientProperty(windowProperty,null);
      
      jmWindow.remove(jmiWindow);
     }

   /***********************/
   /* internalFrameOpened */
   /***********************/
   public void internalFrameOpened(
     InternalFrameEvent e)
     {
      JInternalFrame theFrame = e.getInternalFrame();
      
      if (theFrame instanceof ConstructInspectorFrame)
        { return; }
        
      if (theFrame instanceof TextFrame)
        { jmText.setTextFrame((TextFrame) theFrame); }
      else
        { jmText.setTextFrame(null); }
        
      JCheckBoxMenuItem jmiWindow = new JCheckBoxMenuItem(theFrame.getTitle());
      jmiWindow.setState(false);      
      jmiWindow.putClientProperty(windowProperty,theFrame);
      jmiWindow.addActionListener(this);
      jmiWindow.setActionCommand(selectWindowAction);
      jmWindow.add(jmiWindow);
      theFrame.putClientProperty(menuItemProperty,jmiWindow);
     }

   /**************************/
   /* internalFrameIconified */
   /**************************/
   public void internalFrameIconified(
     InternalFrameEvent e)
     {
      JInternalFrame theFrame = e.getInternalFrame();

      if (theFrame instanceof ConstructInspectorFrame)
        { return; }
        
      if (theFrame instanceof TextFrame)
        { jmText.setTextFrame(null); }

      JCheckBoxMenuItem jmiWindow = (JCheckBoxMenuItem) theFrame.getClientProperty(menuItemProperty);
      jmiWindow.setState(false);
     }

   /****************************/
   /* internalFrameDeiconified */
   /****************************/
   public void internalFrameDeiconified(
     InternalFrameEvent e)
     {
      JInternalFrame theFrame = e.getInternalFrame();

      if (theFrame instanceof ConstructInspectorFrame)
        { return; }
        
      if (theFrame instanceof TextFrame)
        { jmText.setTextFrame((TextFrame) theFrame); }
      else
        { jmText.setTextFrame(null); }

      JCheckBoxMenuItem jmiWindow = (JCheckBoxMenuItem) theFrame.getClientProperty(menuItemProperty);
      jmiWindow.setState(true);
     }

   /**************************/
   /* internalFrameActivated */
   /**************************/
   public void internalFrameActivated(
     InternalFrameEvent e)
     {
      JInternalFrame theFrame = e.getInternalFrame();
      
      if (theFrame instanceof ConstructInspectorFrame)
        { return; }
        
      if (theFrame instanceof TextFrame)
        { jmText.setTextFrame((TextFrame) theFrame); }
      else
        { jmText.setTextFrame(null); }

      if (constructInspector != null)
        {
         if (theFrame instanceof AgendaBrowserFrame)
           { 
            String theText = agendaBrowserManager.browserSelectionText((AgendaBrowserFrame) theFrame); 
            constructInspector.setText(theText); 
           }
         else if (theFrame instanceof EntityBrowserFrame)
           {
            EntityBrowserFrame theEntityFrame = (EntityBrowserFrame) theFrame;
         
            if (theEntityFrame.getEntityName().equals(factBrowserManager.ENTITY_NAME))
              { 
               String theText = factBrowserManager.browserSelectionText(theEntityFrame); 
               constructInspector.setText(theText); 
              }
            else if (theEntityFrame.getEntityName().equals(instanceBrowserManager.ENTITY_NAME))
              { 
               String theText = instanceBrowserManager.browserSelectionText(theEntityFrame); 
               constructInspector.setText(theText); 
              }
           }
        } 

      JCheckBoxMenuItem jmiWindow = (JCheckBoxMenuItem) theFrame.getClientProperty(menuItemProperty);
      jmiWindow.setState(true);
     }

   /****************************/
   /* internalFrameDeactivated */
   /****************************/
   public void internalFrameDeactivated(
     InternalFrameEvent e) 
     {
      JInternalFrame theFrame = e.getInternalFrame();

      if (theFrame instanceof ConstructInspectorFrame)
        { return; }

      if (theFrame instanceof TextFrame)
        { jmText.setTextFrame(null); }

      JCheckBoxMenuItem jmiWindow = (JCheckBoxMenuItem) theFrame.getClientProperty(menuItemProperty);
      jmiWindow.setState(false);
     }
     
   /*#####################*/
   /* GUI Building Methods*/
   /*#####################*/
     
   /*****************/
   /* createMenuBar */
   /*****************/
   private void createMenuBar(
     Environment theEnvironment)
     {
      /*==================================*/
      /* Get KeyStrokes for accelerators. */
      /*==================================*/

      KeyStroke undo = KeyStroke.getKeyStroke(KeyEvent.VK_Z,KeyEvent.CTRL_MASK);
      KeyStroke redo = KeyStroke.getKeyStroke(KeyEvent.VK_Z,KeyEvent.CTRL_MASK | KeyEvent.SHIFT_MASK);

      KeyStroke cut = KeyStroke.getKeyStroke(KeyEvent.VK_X,KeyEvent.CTRL_MASK);
      KeyStroke copy = KeyStroke.getKeyStroke(KeyEvent.VK_C,KeyEvent.CTRL_MASK);
      KeyStroke paste = KeyStroke.getKeyStroke(KeyEvent.VK_V,KeyEvent.CTRL_MASK);

      /*======================*/
      /* Create the menu bar. */
      /*======================*/
      
      JMenuBar jmb = new JMenuBar();

      /*===========*/
      /* File menu */
      /*===========*/
        
      jmFile = new FileMenu(this);
      jmb.add(jmFile);
      
      /*===========*/
      /* Edit menu */
      /*===========*/
         
      JMenu jmEdit = new JMenu("Edit");
      jmEdit.addMenuListener(this);

      jmiUndo = new JMenuItem("Undo");
      jmiUndo.addActionListener(this);
      jmiUndo.setAccelerator(undo);
      jmEdit.add(jmiUndo);
      
      jmiRedo = new JMenuItem("Redo");
      jmiRedo.addActionListener(this);
      jmiRedo.setAccelerator(redo);
      jmEdit.add(jmiRedo);

      jmEdit.addSeparator();
      
      jmiCut = new JMenuItem("Cut");
      jmiCut.addActionListener(this);
      jmiCut.setAccelerator(cut);
      jmEdit.add(jmiCut);

      jmiCopy = new JMenuItem("Copy");
      jmiCopy.addActionListener(this);
      jmiCopy.setAccelerator(copy);
      jmEdit.add(jmiCopy);

      jmiPaste = new JMenuItem("Paste");
      jmiPaste.addActionListener(this);      
      jmiPaste.setAccelerator(paste);      
      jmEdit.add(jmiPaste);
      
      jmb.add(jmEdit);
      
      /*===========*/
      /* Text menu */
      /*===========*/

      jmText = new TextMenu(this);
      jmb.add(jmText);

      /*==================*/
      /* Environment menu */
      /*==================*/
        
      jmEnvironment = new EnvironmentMenu(preferences);
      jmb.add(jmEnvironment);

      /*============*/
      /* Debug menu */
      /*============*/
         
      JMenu jmDebug = new JMenu("Debug");
      jmDebug.addMenuListener(this);

      /*===============*/
      /* Watch submenu */
      /*===============*/

      jmDebug.add(new WatchMenu(theEnvironment));

      /*==========*/
      /* Browsers */
      /*==========*/
      
      jmDebug.addSeparator();

      jmiAgendaBrowser = new JMenuItem("Agenda Browser"); 
      jmiAgendaBrowser.addActionListener(this);
      jmDebug.add(jmiAgendaBrowser);

      jmiFactBrowser = new JMenuItem("Fact Browser"); 
      jmiFactBrowser.addActionListener(this);
      jmDebug.add(jmiFactBrowser);

      jmiInstanceBrowser = new JMenuItem("Instance Browser"); 
      jmiInstanceBrowser.addActionListener(this);
      jmDebug.add(jmiInstanceBrowser);

      jmiConstructInspector = new JMenuItem("Construct Inspector"); 
      jmiConstructInspector.addActionListener(this);
      jmDebug.add(jmiConstructInspector);

      jmb.add(jmDebug);
      
      /*=============*/
      /* Window menu */
      /*=============*/
         
      jmWindow = new JMenu("Window");
      jmWindow.addMenuListener(this);
      jmb.add(jmWindow);
      
      /*===========*/
      /* Help menu */
      /*===========*/

      jmb.add(new HelpMenu());

      /*===================*/
      /* Add the menu bar. */
      /*===================*/
             
      this.setJMenuBar(jmb);
     }   
  }
