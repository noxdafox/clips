package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;

import net.sf.clipsrules.jni.*;

public class WatchMenu extends JMenu 
                   implements ActionListener, MenuListener
  {  
   private Environment clips;
   
   private static final String watchActivationsAction = "WatchActivations";
   private static final String watchCompilationsAction = "WatchCompilations";
   private static final String watchDeffunctionsAction = "WatchDeffunctions";
   private static final String watchFactsAction = "WatchFacts";
   private static final String watchFocusAction = "WatchFocus";
   private static final String watchGenericFunctionsAction = "WatchGenericFunctions";
   private static final String watchGlobalsAction = "WatchGlobals";
   private static final String watchInstancesAction = "WatchInstances";
   private static final String watchMessageHandlersAction = "WatchMessageHandlers";
   private static final String watchMessagesAction = "WatchMessages";
   private static final String watchMethodsAction = "WatchMethods";
   private static final String watchRulesAction = "WatchRules";
   private static final String watchSlotsAction = "WatchSlots";
   private static final String watchStatisticsAction = "WatchStatistics";
   private static final String watchAllAction = "WatchAll";
   private static final String watchNoneAction = "WatchNone";

   private JCheckBoxMenuItem jmiWatchActivations = null;
   private JCheckBoxMenuItem jmiWatchCompilations = null;
   private JCheckBoxMenuItem jmiWatchDeffunctions = null;
   private JCheckBoxMenuItem jmiWatchFacts = null;
   private JCheckBoxMenuItem jmiWatchFocus = null;
   private JCheckBoxMenuItem jmiWatchGenericFunctions = null;
   private JCheckBoxMenuItem jmiWatchGlobals = null;
   private JCheckBoxMenuItem jmiWatchInstances = null;
   private JCheckBoxMenuItem jmiWatchMessageHandlers = null;
   private JCheckBoxMenuItem jmiWatchMessages = null;
   private JCheckBoxMenuItem jmiWatchMethods = null;
   private JCheckBoxMenuItem jmiWatchRules = null;
   private JCheckBoxMenuItem jmiWatchSlots = null;
   private JCheckBoxMenuItem jmiWatchStatistics = null;
   private JMenuItem jmiWatchAll = null;
   private JMenuItem jmiWatchNone = null;

   /*************/
   /* WatchMenu */
   /*************/
   WatchMenu(
     Environment theEnv)
     {  
      super("Watch");
       
      clips = theEnv; 
      addMenuListener(this);
            
      jmiWatchActivations = new JCheckBoxMenuItem("Activations"); 
      jmiWatchActivations.addActionListener(this);
      jmiWatchActivations.setActionCommand(watchActivationsAction);
      add(jmiWatchActivations);
   
      jmiWatchCompilations = new JCheckBoxMenuItem("Compilations"); 
      jmiWatchCompilations.addActionListener(this);
      jmiWatchCompilations.setActionCommand(watchCompilationsAction);
      add(jmiWatchCompilations);
   
      jmiWatchDeffunctions = new JCheckBoxMenuItem("Deffunctions"); 
      jmiWatchDeffunctions.addActionListener(this);
      jmiWatchDeffunctions.setActionCommand(watchDeffunctionsAction);
      add(jmiWatchDeffunctions);
   
      jmiWatchFacts = new JCheckBoxMenuItem("Facts"); 
      jmiWatchFacts.addActionListener(this);
      jmiWatchFacts.setActionCommand(watchFactsAction);
      add(jmiWatchFacts);
   
      jmiWatchFocus = new JCheckBoxMenuItem("Focus"); 
      jmiWatchFocus.addActionListener(this);
      jmiWatchFocus.setActionCommand(watchFocusAction);
      add(jmiWatchFocus);
   
      jmiWatchGenericFunctions = new JCheckBoxMenuItem("Generic Functions"); 
      jmiWatchGenericFunctions.addActionListener(this);
      jmiWatchGenericFunctions.setActionCommand(watchGenericFunctionsAction);
      add(jmiWatchGenericFunctions);
   
      jmiWatchGlobals = new JCheckBoxMenuItem("Globals"); 
      jmiWatchGlobals.addActionListener(this);
      jmiWatchGlobals.setActionCommand(watchGlobalsAction);
      add(jmiWatchGlobals);
   
      jmiWatchInstances = new JCheckBoxMenuItem("Instances"); 
      jmiWatchInstances.addActionListener(this);
      jmiWatchInstances.setActionCommand(watchInstancesAction);
      add(jmiWatchInstances);
   
      jmiWatchMessageHandlers = new JCheckBoxMenuItem("Message Handlers"); 
      jmiWatchMessageHandlers.addActionListener(this);
      jmiWatchMessageHandlers.setActionCommand(watchMessageHandlersAction);
      add(jmiWatchMessageHandlers);
   
      jmiWatchMessages = new JCheckBoxMenuItem("Messages"); 
      jmiWatchMessages.addActionListener(this);
      jmiWatchMessages.setActionCommand(watchMessagesAction);
      add(jmiWatchMessages);
   
      jmiWatchMethods = new JCheckBoxMenuItem("Methods"); 
      jmiWatchMethods.addActionListener(this);
      jmiWatchMethods.setActionCommand(watchMethodsAction);
      add(jmiWatchMethods);
   
      jmiWatchRules = new JCheckBoxMenuItem("Rules"); 
      jmiWatchRules.addActionListener(this);
      jmiWatchRules.setActionCommand(watchRulesAction);
      add(jmiWatchRules);
   
      jmiWatchSlots = new JCheckBoxMenuItem("Slots"); 
      jmiWatchSlots.addActionListener(this);
      jmiWatchSlots.setActionCommand(watchSlotsAction);
      add(jmiWatchSlots);
   
      jmiWatchStatistics = new JCheckBoxMenuItem("Statistics"); 
      jmiWatchStatistics.addActionListener(this);
      jmiWatchStatistics.setActionCommand(watchStatisticsAction);
      add(jmiWatchStatistics);
      
      addSeparator();
   
      jmiWatchAll = new JMenuItem("All"); 
      jmiWatchAll.addActionListener(this);
      jmiWatchAll.setActionCommand(watchAllAction);
      add(jmiWatchAll);
   
      jmiWatchNone = new JMenuItem("None"); 
      jmiWatchNone.addActionListener(this);
      jmiWatchNone.setActionCommand(watchNoneAction);
      add(jmiWatchNone);
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
      if (ae.getActionCommand().equals(watchActivationsAction))  
        { toggleWatch("activations"); }
      else if (ae.getActionCommand().equals(watchCompilationsAction))  
        { toggleWatch("compilations"); }
      else if (ae.getActionCommand().equals(watchDeffunctionsAction))  
        { toggleWatch("deffunctions"); }
      else if (ae.getActionCommand().equals(watchFactsAction))  
        { toggleWatch("facts"); }
      else if (ae.getActionCommand().equals(watchFocusAction))  
        { toggleWatch("focus"); }
      else if (ae.getActionCommand().equals(watchGenericFunctionsAction))  
        { toggleWatch("generic-functions"); }
      else if (ae.getActionCommand().equals(watchGlobalsAction))  
        { toggleWatch("globals"); }
      else if (ae.getActionCommand().equals(watchInstancesAction))  
        { toggleWatch("instances"); }
      else if (ae.getActionCommand().equals(watchMessageHandlersAction))  
        { toggleWatch("message-handlers"); }
      else if (ae.getActionCommand().equals(watchMessagesAction))  
        { toggleWatch("messages"); }
      else if (ae.getActionCommand().equals(watchMethodsAction))  
        { toggleWatch("methods"); }
      else if (ae.getActionCommand().equals(watchRulesAction))  
        { toggleWatch("rules"); }
      else if (ae.getActionCommand().equals(watchSlotsAction))  
        { toggleWatch("slots"); }
      else if (ae.getActionCommand().equals(watchStatisticsAction))  
        { toggleWatch("statistics"); }
      else if (ae.getActionCommand().equals(watchAllAction))  
        { watchAll(); }
      else if (ae.getActionCommand().equals(watchNoneAction))  
        { watchNone(); }
     }
     
   /***************/
   /* toggleWatch */
   /***************/  
   public void toggleWatch(
     String watchItem)
     {
      boolean currentValue;
      
      currentValue = clips.getWatchItem(watchItem); 
      clips.setWatchItem(watchItem,! currentValue);     
     }

   /************/
   /* watchAll */
   /************/  
   public void watchAll()
     {
      clips.watch("all");
     }

   /*************/
   /* watchNone */
   /*************/  
   public void watchNone()
     {
      clips.unwatch("all");
     }
     
   /*******************/
   /* updateWatchMenu */
   /*******************/
   public void updateWatchMenu() 
     {
      if (clips.getWatchItem("activations"))
        { jmiWatchActivations.setState(true); }
      else
        { jmiWatchActivations.setState(false); }

      if (clips.getWatchItem("compilations"))
        { jmiWatchCompilations.setState(true); }
      else
        { jmiWatchCompilations.setState(false); }

      if (clips.getWatchItem("deffunctions"))
        { jmiWatchDeffunctions.setState(true); }
      else
        { jmiWatchDeffunctions.setState(false); }

      if (clips.getWatchItem("facts"))
        { jmiWatchFacts.setState(true); }
      else
        { jmiWatchFacts.setState(false); }

      if (clips.getWatchItem("focus"))
        { jmiWatchFocus.setState(true); }
      else
        { jmiWatchFocus.setState(false); }

      if (clips.getWatchItem("generic-functions"))
        { jmiWatchGenericFunctions.setState(true); }
      else
        { jmiWatchGenericFunctions.setState(false); }

      if (clips.getWatchItem("globals"))
        { jmiWatchGlobals.setState(true); }
      else
        { jmiWatchGlobals.setState(false); }

      if (clips.getWatchItem("instances"))
        { jmiWatchInstances.setState(true); }
      else
        { jmiWatchInstances.setState(false); }

      if (clips.getWatchItem("message-handlers"))
        { jmiWatchMessageHandlers.setState(true); }
      else
        { jmiWatchMessageHandlers.setState(false); }

      if (clips.getWatchItem("messages"))
        { jmiWatchMessages.setState(true); }
      else
        { jmiWatchMessages.setState(false); }

      if (clips.getWatchItem("methods"))
        { jmiWatchMethods.setState(true); }
      else
        { jmiWatchMethods.setState(false); }

      if (clips.getWatchItem("rules"))
        { jmiWatchRules.setState(true); }
      else
        { jmiWatchRules.setState(false); }

      if (clips.getWatchItem("slots"))
        { jmiWatchSlots.setState(true); }
      else
        { jmiWatchSlots.setState(false); }

      if (clips.getWatchItem("statistics"))
        { jmiWatchStatistics.setState(true); }
      else
        { jmiWatchStatistics.setState(false); }
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
      updateWatchMenu();
     }
   
   /******************/
   /* menuDeselected */
   /******************/  
   public void menuDeselected(MenuEvent e)
     {
     }

  }  
