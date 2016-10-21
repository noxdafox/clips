package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.table.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.util.HashMap;
import java.awt.EventQueue;
import java.awt.AWTEvent;

import java.util.ArrayList;
import java.util.List;
import java.util.BitSet;

import net.sf.clipsrules.jni.*;

public class FactBrowserFrame extends JInternalFrame
                             implements ActionListener, ListSelectionListener
  {
   private JFrame browserFrame;
   private JTable modulesTable, factsTable, slotsTable;
   private ModuleTableModel modulesModel;
   private FactInstanceTableModel factsModel;
   private SlotValueTableModel slotsModel;
   private JScrollPane modulesPane, factsPane, slotsPane;
   private JSplitPane factsSlotsPane, contentPane;
   private static int factCount = 1;
   private JCheckBox displayDefaultedValuesCheckBox; 
   private List<Module> modules;
   private List<FactInstance> facts;
   private HashMap<Long,BitSet> scopes;
   
   private static final String BROWSER_SELECTION_ACTION = "BrowserSelectionAction";
   private static final String DISPLAY_DEFAULTED_VALUES_ACTION = "DisplayDefaultedValues";
   
   private ActionListener actionTarget = null;

   /***************/
   /* FactBrowser */
   /***************/
   FactBrowserFrame()
     {      
      this(new ArrayList<Module>(),
           new ArrayList<FactInstance>(),
           new HashMap<Long,BitSet>());
     }

   /***************/
   /* FactBrowser */
   /***************/
   FactBrowserFrame(
     List<Module> theModules,
     List<FactInstance> theFacts,
     HashMap<Long,BitSet> theScopes)
     {  
      super("Fact #" + factCount++,true,true,true,true);
            
      /*===================================*/
      /* Create a new JFrame container and */
      /* assign a layout manager to it.    */
      /*===================================*/

      this.getContentPane().setLayout(new BoxLayout(this.getContentPane(),BoxLayout.Y_AXIS));
      
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      this.setSize(600,200);  
      this.setMinimumSize(new Dimension(450,100));
      
      /*===========================================*/
      /* The close button closes just the browser. */
      /*===========================================*/
     
      this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);  
      
      /*=========================*/
      /* Create the upper panel. */
      /*=========================*/
      
      JPanel upperPanel = new JPanel(); 
      upperPanel.setPreferredSize(new Dimension(600,40));
      upperPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
      
      displayDefaultedValuesCheckBox = new JCheckBox("Display Defaulted Values");
      displayDefaultedValuesCheckBox.setEnabled(true);
      displayDefaultedValuesCheckBox.setActionCommand(DISPLAY_DEFAULTED_VALUES_ACTION);
      displayDefaultedValuesCheckBox.addActionListener(this);
      upperPanel.add(displayDefaultedValuesCheckBox);

      this.getContentPane().add(upperPanel); 

      /****************************/
      /* Create the content area. */
      /****************************/

      DefaultTableCellRenderer centerRenderer = new DefaultTableCellRenderer();
      centerRenderer.setHorizontalAlignment(JLabel.CENTER);

      modulesModel = new ModuleTableModel();
      modulesTable = new JTable(modulesModel);
      modulesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      modulesTable.getSelectionModel().addListSelectionListener(this);
      modulesTable.getTableHeader().setReorderingAllowed(false);
      modulesTable.getColumnModel().getColumn(0).setCellRenderer(centerRenderer);
                 
      modulesPane = new JScrollPane(modulesTable);

      /*=============*/
      /* Facts table */
      /*=============*/
      
      factsModel = new FactInstanceTableModel("Index","Template");
      factsTable = new JTable(factsModel);
      factsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      factsTable.getSelectionModel().addListSelectionListener(this);

      factsTable.getTableHeader().setReorderingAllowed(false);

      TableRowSorter<FactInstanceTableModel> factSorter 
         = new TableRowSorter<FactInstanceTableModel>(factsModel);

      RowFilter<FactInstanceTableModel,Integer> factFilter 
         = new RowFilter<FactInstanceTableModel,Integer>() 
            {
             public boolean include(Entry<? extends FactInstanceTableModel, ? extends Integer> entry) 
               {
                FactInstanceTableModel theModel = entry.getModel();
                FactInstance fi = theModel.getFactInstance(entry.getIdentifier());

                int moduleIndex = modulesTable.getSelectedRow();
                if (moduleIndex == -1) return true;
                moduleIndex = modulesTable.convertRowIndexToModel(moduleIndex);

                BitSet theBitSet = scopes.get(new Long(fi.getTypeAddress()));
                if (theBitSet.get(moduleIndex))
                  { return true; }
                else
                  { return false; }
               }
            };
            
      factSorter.setRowFilter(factFilter);
      
      factsTable.setRowSorter(factSorter);

      factsPane = new JScrollPane(factsTable);

      factsTable.getColumnModel().getColumn(0).setMinWidth(65);
      factsTable.getColumnModel().getColumn(0).setPreferredWidth(75);
      factsTable.getColumnModel().getColumn(1).setPreferredWidth(75);

      factsTable.getColumnModel().getColumn(0).setCellRenderer(centerRenderer);
      factsTable.getColumnModel().getColumn(1).setCellRenderer(centerRenderer);

      /*=============*/
      /* Slots table */
      /*=============*/

      slotsModel = new SlotValueTableModel();
      slotsTable = new JTable(slotsModel);
      slotsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      slotsTable.getSelectionModel().addListSelectionListener(this);

      slotsTable.getTableHeader().setReorderingAllowed(false);
      
      TableRowSorter<SlotValueTableModel> slotSorter 
         = new TableRowSorter<SlotValueTableModel>(slotsModel);
            
      RowFilter<SlotValueTableModel,Integer> slotFilter 
         = new RowFilter<SlotValueTableModel,Integer>() 
            {
             public boolean include(Entry<? extends SlotValueTableModel, ? extends Integer> entry) 
               {
                if (displayDefaultedValuesCheckBox.isSelected())
                  { return true; }
                else
                  {
                   SlotValueTableModel theModel = entry.getModel();
                   SlotValue sv = theModel.getSlotValue(entry.getIdentifier());

                   if (sv.isDefault()) 
                     { return false; }
                   return true;
                  }
               }
            };

      slotSorter.setRowFilter(slotFilter);
      
      slotsTable.setRowSorter(slotSorter);

      slotsPane = new JScrollPane(slotsTable);

      slotsTable.getColumnModel().getColumn(0).setMinWidth(65);
      slotsTable.getColumnModel().getColumn(0).setPreferredWidth(75);
      slotsTable.getColumnModel().getColumn(1).setPreferredWidth(75);

      slotsTable.getColumnModel().getColumn(0).setCellRenderer(centerRenderer);
      slotsTable.getColumnModel().getColumn(1).setCellRenderer(centerRenderer);

      factsSlotsPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,factsPane,slotsPane);
      factsSlotsPane.setOneTouchExpandable(true);
      factsSlotsPane.setDividerLocation(200);
      factsSlotsPane.setDividerSize(15);

      factsSlotsPane.setPreferredSize(new Dimension(400,160));
      factsSlotsPane.setAlignmentX(Component.CENTER_ALIGNMENT);

      contentPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,modulesPane,factsSlotsPane);
      contentPane.setOneTouchExpandable(true);
      contentPane.setDividerLocation(125);
      contentPane.setDividerSize(15);

      contentPane.setPreferredSize(new Dimension(600,160));
      contentPane.setAlignmentX(Component.CENTER_ALIGNMENT);

      this.getContentPane().add(contentPane); 
      
      assignData(theModules,theFacts,theScopes);

      /*====================*/
      /* Display the frame. */
      /*====================*/

      this.pack();
     }  
 
   /*******************/
   /* setActionTarget */
   /*******************/
   public void setActionTarget(
     ActionListener theActionTarget)
     {
      actionTarget = theActionTarget;
     }

   /**************/
   /* assignData */
   /**************/
   public void assignData(
     List<Module> theModules,
     List<FactInstance> theFacts,
     HashMap<Long,BitSet> theScopes)
     {
      modules = theModules;
      facts = theFacts;
      scopes = theScopes;

      modulesModel.setModules(theModules);
      if (modules.size() == 0)
        { 
         factsModel.setItems(null);
         slotsModel.setItem(null);
        }
      else
        { 
         factsModel.setItems(facts); 
         if (facts.size() == 0)
           { slotsModel.setItem(null); }
         else
           { slotsModel.setItem(facts.get(0)); }
        }

      if (modules.size() != 0)
        { 
         modulesTable.setRowSelectionInterval(0,0); 
         if (factsTable.getRowCount() != 0)
           { factsTable.setRowSelectionInterval(0,0); }
        }
     }

   /***************************/
   /* selectedFactDeftemplate */
   /***************************/
   public long selectedFactDeftemplate() 
     {
      int viewRow = factsTable.getSelectedRow();
      if (viewRow == -1) return -1;
      viewRow = factsTable.convertRowIndexToModel(viewRow);
         
      FactInstance theFact = facts.get(viewRow);
     /*
      int focusSelection, activationSelection;
      
      focusSelection = focusStackTable.getSelectedRow();
      if (focusSelection == -1) return null;
      activationSelection = activationTable.getSelectedRow();
      if (activationSelection == -1) return null;

      Focus theFocus = focusStack.get(focusSelection);
      Agenda theAgenda = agendaMap.get(theFocus);
      Activation theActivation = theAgenda.get(activationSelection);
      
      return theFocus.getModuleName() + "::" + theActivation.getRuleName();
      */
      //return fi;
      return theFact.getTypeAddress();
     }
     
   /****************/
   /* valueChanged */
   /****************/
   public void valueChanged(ListSelectionEvent event) 
     {
      if (event.getValueIsAdjusting()) return;

      if (event.getSource().equals(modulesTable.getSelectionModel()))
        {
         int moduleIndex = modulesTable.getSelectedRow();
         if (moduleIndex == -1) return;
         moduleIndex = modulesTable.convertRowIndexToModel(moduleIndex);
         factsModel.fireTableDataChanged();
         if (factsTable.getRowCount() != 0)
           { factsTable.setRowSelectionInterval(0,0); }
        }
      else if (event.getSource().equals(factsTable.getSelectionModel()))
        {
         int viewRow = factsTable.getSelectedRow();
         if (viewRow == -1) return;
         viewRow = factsTable.convertRowIndexToModel(viewRow);
         
          if (facts.size() == 0)
            { slotsModel.setItem(null); }
          else
            { 
             FactInstance theFact = facts.get(viewRow);
             slotsModel.setItem(theFact);
            }
        }

      actionPerformed(new ActionEvent(this,AWTEvent.RESERVED_ID_MAX + 1,BROWSER_SELECTION_ACTION));
     }
   
   /*########################*/
   /* ActionListener Methods */
   /*########################*/

   /*********************/
   /* onActionPerformed */
   /*********************/  
   public void onActionPerformed(
     ActionEvent ae) throws Exception 
     {     
      if (ae.getActionCommand().equals(BROWSER_SELECTION_ACTION))  
        { 
         if (actionTarget == null) return;
         actionTarget.actionPerformed(ae);
        } 
      else if (ae.getActionCommand().equals(DISPLAY_DEFAULTED_VALUES_ACTION))  
        { slotsModel.fireTableDataChanged(); }
     }
          
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