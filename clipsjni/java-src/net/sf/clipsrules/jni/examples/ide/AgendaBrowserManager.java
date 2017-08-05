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

import net.sf.clipsrules.jni.*;

public class AgendaBrowserManager implements ActionListener
  {  
   private List<AgendaBrowserFrame> browsers = new ArrayList<AgendaBrowserFrame>();
   private FocusStack focusStack;
   private HashMap<Focus,Agenda> agendaMap;
   private CLIPSIDE ide;
 
   /************************/
   /* AgendaBrowserManager */
   /************************/
   AgendaBrowserManager(
     CLIPSIDE theIDE)
     {  
      ide = theIDE;
      focusStack = new FocusStack();
      agendaMap = new HashMap<Focus,Agenda>();
     }
     
   /*****************/
   /* createBrowser */
   /*****************/  
   public void createBrowser()
     {
      AgendaBrowserFrame frame = new AgendaBrowserFrame();
      frame.addInternalFrameListener(ide);
      frame.setActionTarget(this);
      browsers.add(frame);
      
      frame.updateButtons(ide.getDialogWindow().isExecuting());
      
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
     AgendaBrowserFrame theBrowser)
     {
      browsers.remove(theBrowser);
     }
     
   /*************/
   /* fetchData */
   /*************/
   private synchronized void fetchData()
     {
      focusStack = ide.getEnvironment().getFocusStack();
      agendaMap = new HashMap<Focus,Agenda>();

      for (Iterator itr = focusStack.iterator(); itr.hasNext(); ) 
        { 
         Focus theFocus = (Focus) itr.next();
         Agenda theAgenda = ide.getEnvironment().getAgenda(theFocus);
         agendaMap.put(theFocus,theAgenda);
        }
     }

   /**************/
   /* assignData */
   /**************/
   private synchronized void assignData(
     AgendaBrowserFrame theBrowser)
     {
      theBrowser.assignData(focusStack,agendaMap);
     }

   /******************/
   /* updateBrowser: */
   /******************/
   private void updateBrowser(
     final AgendaBrowserFrame theBrowser)
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
         AgendaBrowserFrame theBrowser = (AgendaBrowserFrame) itr.next();
         updateBrowser(theBrowser);
        }
     }

   /*******************************/
   /* updateAgendaBrowserButtons: */
   /*******************************/
   public void updateAgendaBrowserButtons(
     boolean isExecuting)
     {
      if (browsers.size() == 0) return;
            
      for (Iterator itr = browsers.iterator(); itr.hasNext(); ) 
        { 
         AgendaBrowserFrame theBrowser = (AgendaBrowserFrame) itr.next();
         theBrowser.updateButtons(isExecuting);
        }
     }
     
   /************************/
   /* browserSelectionText */
   /************************/  
   public String browserSelectionText(
    AgendaBrowserFrame theFrame)
    {
     String ruleName = theFrame.selectedActivationRule();
     if (ruleName == null)
       { return ""; }
     else
       { return ide.getEnvironment().getDefruleText(ruleName); }
    }

   /********************/
   /* browserSelection */
   /********************/  
   public void browserSelection(
     ActionEvent ae)
     {
      ConstructInspectorFrame constructInspector = ide.getConstructInspector();
      
      if (constructInspector == null) return;
      
      AgendaBrowserFrame theFrame = (AgendaBrowserFrame) ae.getSource();
        
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
      if (ae.getActionCommand().equals(AgendaBrowserFrame.RESET_ACTION))
        { reset(); }
      else if (ae.getActionCommand().equals(AgendaBrowserFrame.RUN_ACTION))
        { run(); }
      else if (ae.getActionCommand().equals(AgendaBrowserFrame.STEP_ACTION))
        { step(); }
      else if (ae.getActionCommand().equals(AgendaBrowserFrame.HALT_RULES_ACTION))
        { haltRules(); }
      else if (ae.getActionCommand().equals(AgendaBrowserFrame.BROWSER_SELECTION_ACTION)) 
        { browserSelection(ae); }
     }
     
   /*********/
   /* reset */
   /*********/  
   public void reset()
     {
      ide.getDialogWindow().replaceCommand("(reset)\n");
     }

   /*******/
   /* run */
   /*******/  
   public void run()
     {
      ide.getDialogWindow().replaceCommand("(run)\n");
     }

   /********/
   /* step */
   /********/  
   public void step()
     {
      ide.getDialogWindow().replaceCommand("(run 1)\n");
     }

   /*************/
   /* haltRules */
   /*************/  
   public void haltRules()
     {
      ide.getDialogWindow().haltRules();
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
