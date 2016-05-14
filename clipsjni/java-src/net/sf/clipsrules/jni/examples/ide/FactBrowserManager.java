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
   private List<FactBrowserFrame> factBrowsers = new ArrayList<FactBrowserFrame>();
   private List<Module> modules;
   private List<FactInstance> facts;
   private HashMap<Long,BitSet> scopes;
   private CLIPSIDE ide;
 
   /**********************/
   /* FactBrowserManager */
   /**********************/
   FactBrowserManager(
     CLIPSIDE theIDE)
     {  
      ide = theIDE;
      modules = new ArrayList<Module>();
      facts = new ArrayList<FactInstance>();
      scopes = new HashMap<Long,BitSet>();
     }
     
   /*********************/
   /* createFactBrowser */
   /*********************/  
   public void createFactBrowser()
     {
      FactBrowserFrame frame = new FactBrowserFrame();
      frame.addInternalFrameListener(ide);
      frame.setActionTarget(this);
      factBrowsers.add(frame);
            
      ide.getPlacer().placeInternalFrame(frame);
      
      ide.getDesktopPane().add(frame);

      frame.setVisible(true);
      
      if (! ide.getDialogWindow().isExecuting())
        { 
         if (factBrowsers.size() == 1)
           { fetchFacts(); }
         assignFacts(frame); 
        }      
     }
     
   /*********************/
   /* removeFactBrowser */
   /*********************/  
   public void removeFactBrowser(
     FactBrowserFrame theBrowser)
     {
      factBrowsers.remove(theBrowser);
     }
     
   /**************/
   /* fetchFacts */
   /**************/
   private synchronized void fetchFacts()
     {
      modules = ide.getEnvironment().getModuleList();
      facts = ide.getEnvironment().getFactList();
      scopes = ide.getEnvironment().getFactScopes();
     }

   /***************/
   /* assignFacts */
   /***************/
   private synchronized void assignFacts(
     FactBrowserFrame theBrowser)
     {
      theBrowser.assignData(modules,facts,scopes);
     }

   /**********************/
   /* updateFactBrowser: */
   /**********************/
   private void updateFactBrowser(
     FactBrowserFrame theBrowser)
     {
      if (EventQueue.isDispatchThread())
        { 
         assignFacts(theBrowser);
         return; 
        }
              
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() 
                { assignFacts(theBrowser); }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }  
  
   /**************************/
   /* updateAllFactBrowsers: */
   /***************************/
   public void updateAllFactBrowsers()
     {
      if (factBrowsers.size() == 0) return;
      
      fetchFacts();
      
      for (Iterator itr = factBrowsers.iterator(); itr.hasNext(); ) 
        { 
         FactBrowserFrame theBrowser = (FactBrowserFrame) itr.next();
         updateFactBrowser(theBrowser);
        }
     }
     
   /****************************/
   /* factBrowserSelectionText */
   /****************************/  
   public String factBrowserSelectionText(
    FactBrowserFrame theFrame)
    {
     long templatePtr = theFrame.selectedFactDeftemplate();
     if (templatePtr == -1) return "";
     return ide.getEnvironment().getDeftemplateText(templatePtr);
    }

   /************************/
   /* factBrowserSelection */
   /************************/  
   public void factBrowserSelection(
     ActionEvent ae)
     {
      ConstructInspectorFrame constructInspector = ide.getConstructInspector();
      
      if (constructInspector == null) return;
      
      FactBrowserFrame theFrame = (FactBrowserFrame) ae.getSource();
        
      constructInspector.setText(factBrowserSelectionText(theFrame));
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
      if (ae.getActionCommand().equals(FactBrowserFrame.BROWSER_SELECTION_ACTION)) 
        { factBrowserSelection(ae); }
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
