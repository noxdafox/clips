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

import java.awt.BorderLayout;
import java.awt.Color;

import net.sf.clipsrules.jni.*;

public class InstanceBrowserFrame extends JInternalFrame
                             implements ActionListener, ListSelectionListener
  {
   private JFrame browserFrame;
   private JTable modulesTable, instancesTable, slotsTable;
   private ModuleTableModel modulesModel;
   private FactInstanceTableModel instancesModel;
   private SlotValueTableModel slotsModel;
   private JScrollPane modulesPane, instancesPane, slotsPane;
   private JSplitPane instancesSlotsPane, contentPane;
   private static int instanceCount = 1;
   private JCheckBox displayDefaultedValuesCheckBox; 
   private List<Module> modules;
   private List<FactInstance> instances;
   private HashMap<Long,BitSet> scopes;
   
   public static final String BROWSER_SELECTION_ACTION = "InstanceBrowserSelectionAction";
   private static final String DISPLAY_DEFAULTED_VALUES_ACTION = "DisplayDefaultedValues";

   private ActionListener actionTarget = null;

   /*******************/
   /* InstanceBrowser */
   /*******************/
   InstanceBrowserFrame()
     {      
      this(new ArrayList<Module>(),
           new ArrayList<FactInstance>(),
           new HashMap<Long,BitSet>());
     }

   /*******************/
   /* InstanceBrowser */
   /*******************/
   InstanceBrowserFrame(
     List<Module> theModules,
     List<FactInstance> theInstances,
     HashMap<Long,BitSet> theScopes)
     {  
      super("Instance #" + instanceCount++,true,true,true,true);
            
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
/*
      JTextField searchField = new JTextField(20);
      TextPrompt searchPrompt = new TextPrompt("Search",searchField);
      searchPrompt.setForeground( Color.RED );
      searchPrompt.changeAlpha(0.5f);
      upperPanel.add(searchPrompt);
*/

      JTextField searchField = new JTextField(20);
      upperPanel.add(searchField);

/*
      SearchField searchField = new SearchField("Search");
      upperPanel.add(searchField);
*/
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

      /*=================*/
      /* Instances table */
      /*=================*/
      
      instancesModel = new FactInstanceTableModel("Name","Class");
      instancesTable = new JTable(instancesModel);
      instancesTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      instancesTable.getSelectionModel().addListSelectionListener(this);

      instancesTable.getTableHeader().setReorderingAllowed(false);

      TableRowSorter<FactInstanceTableModel> instanceSorter 
         = new TableRowSorter<FactInstanceTableModel>(instancesModel);

      RowFilter<FactInstanceTableModel,Integer> instanceFilter 
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
            
      instanceSorter.setRowFilter(instanceFilter);
      
      instancesTable.setRowSorter(instanceSorter);

      instancesPane = new JScrollPane(instancesTable);

      instancesTable.getColumnModel().getColumn(0).setMinWidth(65);
      instancesTable.getColumnModel().getColumn(0).setPreferredWidth(75);
      instancesTable.getColumnModel().getColumn(1).setPreferredWidth(75);

      instancesTable.getColumnModel().getColumn(0).setCellRenderer(centerRenderer);
      instancesTable.getColumnModel().getColumn(1).setCellRenderer(centerRenderer);

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

      instancesSlotsPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,instancesPane,slotsPane);
      instancesSlotsPane.setOneTouchExpandable(true);
      instancesSlotsPane.setDividerLocation(200);
      instancesSlotsPane.setDividerSize(15);

      instancesSlotsPane.setPreferredSize(new Dimension(400,160));
      instancesSlotsPane.setAlignmentX(Component.CENTER_ALIGNMENT);

      contentPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,modulesPane,instancesSlotsPane);
      contentPane.setOneTouchExpandable(true);
      contentPane.setDividerLocation(125);
      contentPane.setDividerSize(15);

      contentPane.setPreferredSize(new Dimension(600,160));
      contentPane.setAlignmentX(Component.CENTER_ALIGNMENT);

      this.getContentPane().add(contentPane); 
      
      assignData(theModules,theInstances,theScopes);

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
     List<FactInstance> theInstances,
     HashMap<Long,BitSet> theScopes)
     {
      modules = theModules;
      instances = theInstances;
      scopes = theScopes;

      modulesModel.setModules(theModules);
      if (modules.size() == 0)
        { 
         instancesModel.setItems(null);
         slotsModel.setItem(null);
        }
      else
        { 
         instancesModel.setItems(instances); 
         if (instances.size() == 0)
           { slotsModel.setItem(null); }
         else
           { slotsModel.setItem(instances.get(0)); }
        }

      if (modules.size() != 0)
        { 
         modulesTable.setRowSelectionInterval(0,0); 
         if (instancesTable.getRowCount() != 0)
           { instancesTable.setRowSelectionInterval(0,0); }
        }
     }

   /****************************/
   /* selectedInstanceDefclass */
   /****************************/
   public long selectedInstanceDefclass() 
     {
      int viewRow = instancesTable.getSelectedRow();
      if (viewRow == -1) return -1;
      viewRow = instancesTable.convertRowIndexToModel(viewRow);
         
      FactInstance theInstance = instances.get(viewRow);

      return theInstance.getTypeAddress();
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
         instancesModel.fireTableDataChanged();
         if (instancesTable.getRowCount() != 0)
           { instancesTable.setRowSelectionInterval(0,0); }
        }
      else if (event.getSource().equals(instancesTable.getSelectionModel()))
        {
         int viewRow = instancesTable.getSelectedRow();
         if (viewRow == -1) return;
         viewRow = instancesTable.convertRowIndexToModel(viewRow);
         
          if (instances.size() == 0)
            { slotsModel.setItem(null); }
          else
            { 
             FactInstance theInstance = instances.get(viewRow);
             slotsModel.setItem(theInstance);
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