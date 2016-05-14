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

public class InstanceBrowserManager implements ActionListener
  {  
   private List<InstanceBrowserFrame> instanceBrowsers = new ArrayList<InstanceBrowserFrame>();
   private List<Module> modules;
   private List<FactInstance> instances;
   private HashMap<Long,BitSet> scopes;
   private CLIPSIDE ide;
 
   /**************************/
   /* InstanceBrowserManager */
   /**************************/
   InstanceBrowserManager(
     CLIPSIDE theIDE)
     {  
      ide = theIDE;
      modules = new ArrayList<Module>();
      instances = new ArrayList<FactInstance>();
      scopes = new HashMap<Long,BitSet>();
     }
     
   /*************************/
   /* createInstanceBrowser */
   /*************************/  
   public void createInstanceBrowser()
     {
      InstanceBrowserFrame frame = new InstanceBrowserFrame();
      frame.addInternalFrameListener(ide);
      frame.setActionTarget(this);
      instanceBrowsers.add(frame);
            
      ide.getPlacer().placeInternalFrame(frame);
      
      ide.getDesktopPane().add(frame);

      frame.setVisible(true);
      
      if (! ide.getDialogWindow().isExecuting())
        { 
         if (instanceBrowsers.size() == 1)
           { fetchInstances(); }
         assignInstances(frame); 
        }      
     }
     
   /*************************/
   /* removeInstanceBrowser */
   /*************************/  
   public void removeInstanceBrowser(
     InstanceBrowserFrame theBrowser)
     {
      instanceBrowsers.remove(theBrowser);
     }
     
   /******************/
   /* fetchInstances */
   /******************/
   private synchronized void fetchInstances()
     {
      modules = ide.getEnvironment().getModuleList();
      instances = ide.getEnvironment().getInstanceList();
      scopes = ide.getEnvironment().getInstanceScopes();
     }

   /*******************/
   /* assignInstances */
   /*******************/
   private synchronized void assignInstances(
     InstanceBrowserFrame theBrowser)
     {
      theBrowser.assignData(modules,instances,scopes);
     }

   /**************************/
   /* updateInstanceBrowser: */
   /**************************/
   private void updateInstanceBrowser(
     InstanceBrowserFrame theBrowser)
     {
      if (EventQueue.isDispatchThread())
        { 
         assignInstances(theBrowser);
         return; 
        }
              
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() 
                { assignInstances(theBrowser); }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }  
  
   /******************************/
   /* updateAllInstanceBrowsers: */
   /******************************/
   public void updateAllInstanceBrowsers()
     {
      if (instanceBrowsers.size() == 0) return;
      
      fetchInstances();
      
      for (Iterator itr = instanceBrowsers.iterator(); itr.hasNext(); ) 
        { 
         InstanceBrowserFrame theBrowser = (InstanceBrowserFrame) itr.next();
         updateInstanceBrowser(theBrowser);
        }
     }
     
   /********************************/
   /* instanceBrowserSelectionText */
   /********************************/  
   public String instanceBrowserSelectionText(
    InstanceBrowserFrame theFrame)
    {
     long classPtr = theFrame.selectedInstanceDefclass();
     if (classPtr == -1) return "";
     return ide.getEnvironment().getDefclassText(classPtr);
    }

   /****************************/
   /* instanceBrowserSelection */
   /****************************/  
   public void instanceBrowserSelection(
     ActionEvent ae)
     {
      ConstructInspectorFrame constructInspector = ide.getConstructInspector();
      
      if (constructInspector == null) return;
      
      InstanceBrowserFrame theFrame = (InstanceBrowserFrame) ae.getSource();
        
      constructInspector.setText(instanceBrowserSelectionText(theFrame));
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
      if (ae.getActionCommand().equals(InstanceBrowserFrame.BROWSER_SELECTION_ACTION)) 
        { instanceBrowserSelection(ae); }
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
