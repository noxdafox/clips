package net.sf.clipsrules.jni.examples.ide;

import java.awt.EventQueue;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import javax.swing.event.InternalFrameListener;
import javax.swing.JDesktopPane;
import javax.swing.SwingUtilities;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.BitSet;

import net.sf.clipsrules.jni.*;

public class FactBrowserManager implements ActionListener
  {  
   private List<EntityBrowserFrame> browsers = new ArrayList<EntityBrowserFrame>();
   private List<Module> modules;
   private List<FactInstance> entities;
   private HashMap<Long,BitSet> scopes;
   private CLIPSIDE ide;
   public static final String ENTITY_NAME = "Fact";
   private static int entityCount = 1;

   /**********************/
   /* FactBrowserManager */
   /**********************/
   FactBrowserManager(
     CLIPSIDE theIDE)
     {  
      ide = theIDE;
      modules = new ArrayList<Module>();
      entities = new ArrayList<FactInstance>();
      scopes = new HashMap<Long,BitSet>();
     }
     
   /*****************/
   /* createBrowser */
   /*****************/  
   public void createBrowser()
     {
      EntityBrowserFrame frame = new EntityBrowserFrame(ENTITY_NAME,"Index","Template",entityCount++);
      frame.addInternalFrameListener(ide);
      frame.setActionTarget(this);
      browsers.add(frame);
            
      ide.getPlacer().placeInternalFrame(frame);
      
      ide.getDesktopPane().add(frame);

      frame.setVisible(true);
      
      if (! ide.getDialogWindow().isExecuting())
        { 
         if (browsers.size() == 1)
           { fetchData(); }
         assignData(frame); 
        }      
     }
     
   /*****************/
   /* removeBrowser */
   /*****************/  
   public void removeBrowser(
     EntityBrowserFrame theBrowser)
     {
      browsers.remove(theBrowser);
     }
     
   /*************/
   /* fetchData */
   /*************/
   private synchronized void fetchData()
     {
      modules = ide.getEnvironment().getModuleList();
      entities = ide.getEnvironment().getFactList();
      scopes = ide.getEnvironment().getFactScopes();
     }

   /**************/
   /* assignData */
   /**************/
   private synchronized void assignData(
     EntityBrowserFrame theBrowser)
     {
      theBrowser.assignData(modules,entities,scopes);
     }

   /******************/
   /* updateBrowser: */
   /******************/
   private void updateBrowser(
     final EntityBrowserFrame theBrowser)
     {
      if (EventQueue.isDispatchThread())
        { 
         assignData(theBrowser);
         return; 
        }
              
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() 
                { assignData(theBrowser); }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }  
  
   /**********************/
   /* updateAllBrowsers: */
   /**********************/
   public void updateAllBrowsers()
     {
      if (browsers.size() == 0) return;
      
      fetchData();
      
      for (Iterator itr = browsers.iterator(); itr.hasNext(); ) 
        { 
         EntityBrowserFrame theBrowser = (EntityBrowserFrame) itr.next();
         updateBrowser(theBrowser);
        }
     }
     
   /************************/
   /* browserSelectionText */
   /************************/  
   public String browserSelectionText(
    EntityBrowserFrame theFrame)
    {
     long constructPtr = theFrame.selectedEntityConstruct();
     if (constructPtr == -1) return "";
     return ide.getEnvironment().getDeftemplateText(constructPtr);
    }

   /********************/
   /* browserSelection */
   /********************/  
   public void browserSelection(
     ActionEvent ae)
     {
      ConstructInspectorFrame constructInspector = ide.getConstructInspector();
      
      if (constructInspector == null) return;
      
      EntityBrowserFrame theFrame = (EntityBrowserFrame) ae.getSource();
        
      constructInspector.setText(browserSelectionText(theFrame));
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
      if (ae.getActionCommand().equals(EntityBrowserFrame.BROWSER_SELECTION_ACTION)) 
        { browserSelection(ae); }
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
  }  
