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
   private List<AgendaBrowserFrame> agendaBrowsers = new ArrayList<AgendaBrowserFrame>();
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
     
   /***********************/
   /* createAgendaBrowser */
   /***********************/  
   public void createAgendaBrowser()
     {
      AgendaBrowserFrame frame = new AgendaBrowserFrame();
      frame.addInternalFrameListener(ide);
      frame.setActionTarget(this);
      agendaBrowsers.add(frame);
      
      frame.updateButtons(ide.getDialogWindow().isExecuting());
      
      ide.getPlacer().placeInternalFrame(frame);
      
      ide.getDesktopPane().add(frame);

      frame.setVisible(true);
      
      if (! ide.getDialogWindow().isExecuting())
        { 
         if (agendaBrowsers.size() == 1)
           { fetchAgenda(); }
         assignAgenda(frame); 
        }      
     }
     
   /***********************/
   /* removeAgendaBrowser */
   /***********************/  
   public void removeAgendaBrowser(
     AgendaBrowserFrame theBrowser)
     {
      agendaBrowsers.remove(theBrowser);
     }
     
   /***************/
   /* fetchAgenda */
   /***************/
   private synchronized void fetchAgenda()
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

   /****************/
   /* assignAgenda */
   /****************/
   private synchronized void assignAgenda(
     AgendaBrowserFrame theBrowser)
     {
      theBrowser.assignData(focusStack,agendaMap);
     }

   /************************/
   /* updateAgendaBrowser: */
   /************************/
   private void updateAgendaBrowser(
     AgendaBrowserFrame theBrowser)
     {
      if (EventQueue.isDispatchThread())
        { 
         assignAgenda(theBrowser);
         return; 
        }
              
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() 
                { assignAgenda(theBrowser); }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }  
  
   /****************************/
   /* updateAllAgendaBrowsers: */
   /****************************/
   public void updateAllAgendaBrowsers()
     {
      if (agendaBrowsers.size() == 0) return;
      
      fetchAgenda();
      
      for (Iterator itr = agendaBrowsers.iterator(); itr.hasNext(); ) 
        { 
         AgendaBrowserFrame theBrowser = (AgendaBrowserFrame) itr.next();
         updateAgendaBrowser(theBrowser);
        }
     }

   /*******************************/
   /* updateAgendaBrowserButtons: */
   /*******************************/
   public void updateAgendaBrowserButtons(
     boolean isExecuting)
     {
      if (agendaBrowsers.size() == 0) return;
            
      for (Iterator itr = agendaBrowsers.iterator(); itr.hasNext(); ) 
        { 
         AgendaBrowserFrame theBrowser = (AgendaBrowserFrame) itr.next();
         theBrowser.updateButtons(isExecuting);
        }
     }
     
   /******************************/
   /* agendaBrowserSelectionText */
   /******************************/  
   public String agendaBrowserSelectionText(
    AgendaBrowserFrame theFrame)
    {
     String ruleName = theFrame.selectedActivationRule();
     if (ruleName == null)
       { return ""; }
     else
       { return ide.getEnvironment().getDefruleText(ruleName); }
    }

   /**************************/
   /* agendaBrowserSelection */
   /**************************/  
   public void agendaBrowserSelection(
     ActionEvent ae)
     {
      ConstructInspectorFrame constructInspector = ide.getConstructInspector();
      
      if (constructInspector == null) return;
      
      AgendaBrowserFrame theFrame = (AgendaBrowserFrame) ae.getSource();
        
      constructInspector.setText(agendaBrowserSelectionText(theFrame));
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
        { agendaBrowserSelection(ae); }
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
