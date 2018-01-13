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
   private EditMenu jmEdit;
   private TextMenu jmText;
   private EnvironmentMenu jmEnvironment;
   private JMenu jmWindow;

   private IDEPreferences preferences;

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
      
      setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

      createWindowClosingAdaptor();
              
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
      
      clips.addUserFunction("clear-window","v",0,0,null,
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

   /******************************/
   /* createWindowClosingAdaptor */
   /******************************/  
   private void createWindowClosingAdaptor()
     {
      addWindowListener(new WindowAdapter() 
        {
         public void windowClosing(WindowEvent we)
           {
            int i, count, changeCount = 0;
                        
            count = jmWindow.getItemCount();
            for (i = 0; i < count; i++)
              {
               JCheckBoxMenuItem jmiWindow = (JCheckBoxMenuItem) jmWindow.getItem(i);
               JInternalFrame theFrame = (JInternalFrame) jmiWindow.getClientProperty(windowProperty);

               if (theFrame instanceof TextFrame)
                 { 
                  TextFrame theTextFrame = (TextFrame) theFrame;
         
                  if (theTextFrame.isChanged())
                    { changeCount++; }
                 }
              }
            
            if (changeCount == 1)
              {
               int confirmResult = JOptionPane.showConfirmDialog(null,
                  "You have a document with unsaved changes. Do you want to quit?",
                  null,
                  JOptionPane.YES_NO_OPTION);

               if (confirmResult == JOptionPane.YES_OPTION)
                 {
                  dispose();
                  System.exit(0);
                 }
              }
            else if (changeCount > 1)
              {
               int confirmResult = JOptionPane.showConfirmDialog(null,
                  "You have " + changeCount + " documents with unsaved changes. Do you want to quit?",
                  null,
                  JOptionPane.YES_NO_OPTION);
                  
               if (confirmResult == JOptionPane.YES_OPTION)
                 {
                  dispose();
                  System.exit(0);
                 }
              }
            else
              { 
               dispose();
               System.exit(0);
              }  
           }
        });
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
         if (dialogWindow.getEnvironment().getAgendaChanged() ||
             dialogWindow.getEnvironment().getFocusChanged())
           {
            dialogWindow.getEnvironment().setAgendaChanged(false);
            dialogWindow.getEnvironment().setFocusChanged(false);
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
      if (ae.getSource() == jmiAgendaBrowser)  
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
        { return; }
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
      JInternalFrame theFrame = e.getInternalFrame();

      if (theFrame instanceof TextFrame)
        { 
         TextFrame theTextFrame = (TextFrame) theFrame;
         
         theTextFrame.closeTextFrame(preferences);
        }
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
      
      jmEdit = new EditMenu(this);
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

      jmb.add(new HelpMenu(this));

      /*===================*/
      /* Add the menu bar. */
      /*===================*/
             
      this.setJMenuBar(jmb);
     }   
  }
