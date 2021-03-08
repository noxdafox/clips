package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.table.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.util.HashMap;
import java.awt.EventQueue;
import java.awt.AWTEvent;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import java.util.ArrayList;
import java.util.List;
import java.util.BitSet;

import net.sf.clipsrules.jni.*;

public class EntityBrowserFrame extends JInternalFrame
                             implements ActionListener, ListSelectionListener
  {
   private JFrame browserFrame;
   private JTable modulesTable, entityTable, slotsTable;
   private ModuleTableModel modulesModel;
   private FactInstanceTableModel entityModel;
   private SlotValueTableModel slotsModel;
   private JScrollPane modulesPane, entitiesPane, slotsPane;
   private JSplitPane entitiesSlotsPane, contentPane;
   private JTextField searchField;
   private JCheckBox displayDefaultedValuesCheckBox; 
   private List<net.sf.clipsrules.jni.Module> modules;
   private List<FactInstance> entities;
   private HashMap<Long,BitSet> scopes;
   private String entityName;
   
   private String lastModule;
   private int lastModuleRow;
   private String lastEntity;
   private int lastEntityRow;
   
   public static final String BROWSER_SELECTION_ACTION = "BrowserSelectionAction";
   private static final String DISPLAY_DEFAULTED_VALUES_ACTION = "DisplayDefaultedValues";

   private ActionListener actionTarget = null;

   /**********************/
   /* EntityBrowserFrame */
   /**********************/
   EntityBrowserFrame(
     String theEntityName,
     String theIDName,
     String theConstructName,
     int browserIndex,
     Font browserFont)
     {      
      this(new ArrayList<net.sf.clipsrules.jni.Module>(),
           new ArrayList<FactInstance>(),
           new HashMap<Long,BitSet>(),
           theEntityName,
           theIDName,
           theConstructName,
           browserIndex,
           browserFont);
     }

   /**********************/
   /* EntityBrowserFrame */
   /**********************/
   EntityBrowserFrame(
     List<net.sf.clipsrules.jni.Module> theModules,
     List<FactInstance> theEntities,
     HashMap<Long,BitSet> theScopes,
     String theEntityName,
     String theIDName,
     String theConstructName,
     int browserIndex,
     Font browserFont)
     {  
      super(theEntityName + " Browser #" + browserIndex,true,true,true,true);
      entityName = theEntityName;
      
      /*===================================*/
      /* Create a new JFrame container and */
      /* assign a layout manager to it.    */
      /*===================================*/

      this.getContentPane().setLayout(new BoxLayout(this.getContentPane(),BoxLayout.Y_AXIS));
      
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      this.setSize(600,200);  
      this.setMinimumSize(new Dimension(450,150));
      
      /*===========================================*/
      /* The close button closes just the browser. */
      /*===========================================*/
     
      this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);  
      
      /*=========================*/
      /* Create the upper panel. */
      /*=========================*/
      
      JPanel upperPanel = new JPanel(); 
      upperPanel.setPreferredSize(new Dimension(600,40));

      displayDefaultedValuesCheckBox = new JCheckBox("Display Defaulted Values");
      displayDefaultedValuesCheckBox.setEnabled(true);
      displayDefaultedValuesCheckBox.setActionCommand(DISPLAY_DEFAULTED_VALUES_ACTION);
      displayDefaultedValuesCheckBox.addActionListener(this);
      
      upperPanel.add(displayDefaultedValuesCheckBox);
      
      JLabel searchLabel = new JLabel("Search:  ");
      searchField = new JTextField(20);

      searchField.addActionListener(new ActionListener() 
        {
         @Override
         public void actionPerformed(ActionEvent event) 
         {
          performSearch();
         }
        });
        
      JPanel searchPanel = new JPanel();
      searchPanel.setLayout(new BoxLayout(searchPanel,BoxLayout.X_AXIS));
      
      searchPanel.add(searchLabel);
      searchPanel.add(searchField);

      upperPanel.add(searchPanel);

      GroupLayout layout = new GroupLayout(upperPanel);
      upperPanel.setLayout(layout);
      layout.setAutoCreateGaps(true);
      layout.setAutoCreateContainerGaps(true);
      
      layout.setHorizontalGroup(layout.createSequentialGroup()
                                      .addComponent(displayDefaultedValuesCheckBox)
                                      .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                      .addComponent(searchPanel));
                                      
      layout.setVerticalGroup(layout.createParallelGroup(GroupLayout.Alignment.CENTER)
                                    .addComponent(displayDefaultedValuesCheckBox)
                                    .addComponent(searchPanel));

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

      /*==============*/
      /* Entity table */
      /*==============*/
      
      entityModel = new FactInstanceTableModel(theIDName,theConstructName);
      entityTable = new JTable(entityModel);
      entityTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      entityTable.getSelectionModel().addListSelectionListener(this);

      entityTable.getTableHeader().setReorderingAllowed(false);

      TableRowSorter<FactInstanceTableModel> entitySorter 
         = new TableRowSorter<FactInstanceTableModel>(entityModel);

      RowFilter<FactInstanceTableModel,Integer> entityFilter 
         = new RowFilter<FactInstanceTableModel,Integer>() 
            {
             public boolean include(Entry<? extends FactInstanceTableModel, ? extends Integer> entry) 
               {
                FactInstanceTableModel theModel = entry.getModel();
                FactInstance fi = theModel.getFactInstance(entry.getIdentifier());

                int moduleIndex = modulesTable.getSelectedRow();
                if (moduleIndex == -1) return true;
                moduleIndex = modulesTable.convertRowIndexToModel(moduleIndex);

                BitSet theBitSet = scopes.get(Long.valueOf(fi.getTypeAddress()));
                if (theBitSet.get(moduleIndex))
                  { 
                   if (fi.searchForString(searchField.getText()))
                     { return true; }
                   else
                     { return false; } 
                  }
                else
                  { return false; }
               }
            };
            
      entitySorter.setRowFilter(entityFilter);
      
      entityTable.setRowSorter(entitySorter);

      entitiesPane = new JScrollPane(entityTable);

      entityTable.getColumnModel().getColumn(0).setMinWidth(65);
      entityTable.getColumnModel().getColumn(0).setPreferredWidth(75);
      entityTable.getColumnModel().getColumn(1).setPreferredWidth(75);

      entityTable.getColumnModel().getColumn(0).setCellRenderer(centerRenderer);
      entityTable.getColumnModel().getColumn(1).setCellRenderer(centerRenderer);

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

      entitiesSlotsPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,entitiesPane,slotsPane);
      entitiesSlotsPane.setOneTouchExpandable(true);
      entitiesSlotsPane.setDividerLocation(200);
      entitiesSlotsPane.setDividerSize(15);

      entitiesSlotsPane.setPreferredSize(new Dimension(400,160));
      entitiesSlotsPane.setAlignmentX(Component.CENTER_ALIGNMENT);

      contentPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,modulesPane,entitiesSlotsPane);
      contentPane.setOneTouchExpandable(true);
      contentPane.setDividerLocation(125);
      contentPane.setDividerSize(15);

      contentPane.setPreferredSize(new Dimension(600,160));
      contentPane.setAlignmentX(Component.CENTER_ALIGNMENT);

      this.getContentPane().add(contentPane); 
      
      lastModule = null;
      lastModuleRow = -1;
      lastEntity = null;
      lastEntityRow = -1;

      assignData(theModules,theEntities,theScopes);
      
      /*===============*/
      /* Set the font. */
      /*===============*/

      assignFont(browserFont);

      /*====================*/
      /* Display the frame. */
      /*====================*/

      this.pack();
     }  

   /**************/
   /* assignFont */
   /**************/
   public void assignFont(
     Font browserFont)
     {
      FontMetrics metrics = modulesTable.getFontMetrics(browserFont);
      int theHeight = metrics.getHeight() + 2;

      modulesTable.setFont(browserFont);
      modulesTable.setRowHeight(theHeight);
      modulesTable.getTableHeader().setFont(browserFont);
      
      entityTable.setFont(browserFont);
      entityTable.setRowHeight(theHeight);
      entityTable.getTableHeader().setFont(browserFont);
      
      slotsTable.setFont(browserFont);
      slotsTable.setRowHeight(theHeight);
      slotsTable.getTableHeader().setFont(browserFont);
     }
     
   /*****************/
   /* getEntityName */
   /*****************/
   public String getEntityName()
     {
      return entityName;
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
     List<net.sf.clipsrules.jni.Module> theModules,
     List<FactInstance> theEntities,
     HashMap<Long,BitSet> theScopes)
     {
      saveSelection();

      modules = theModules;
      entities = theEntities;
      scopes = theScopes;

      modulesModel.setModules(theModules);
      if (modules.size() == 0)
        { 
         entityModel.setItems(null);
         slotsModel.setItem(null);
        }
      else
        { 
         entityModel.setItems(entities); 
         slotsModel.setItem(null);
        }

      restoreSelection();
     }

   /*****************/
   /* saveSelection */
   /*****************/
   private void saveSelection()
     {
      int theRow;
   
      theRow = modulesTable.getSelectedRow();
      if (theRow != -1)
        {
         lastModuleRow = theRow;
         
         theRow = modulesTable.convertRowIndexToModel(theRow);
         net.sf.clipsrules.jni.Module theModule = modules.get(theRow);
         lastModule = theModule.getModuleName();
        }
      else
        {
         lastModule = null;
         lastModuleRow = -1;
        }

      theRow = entityTable.getSelectedRow();
      if (theRow != -1)
        {
         lastEntityRow = theRow;

         theRow = entityTable.convertRowIndexToModel(theRow);
         FactInstance theEntity = entities.get(theRow);
         lastEntity = theEntity.getName();
        }
      else
        {
         lastEntity = null;
         lastEntityRow = -1;
        }
     }
     
   /*********************/
   /* restoreSelection: */
   /*********************/
   private void restoreSelection()
     {
      int i, count, theRow;
      boolean found;
      FactInstance theEntity;
    
      if (lastModuleRow == -1)
        {
         if (modulesTable.getRowCount() > 0)
           { modulesTable.setRowSelectionInterval(0,0);  }
        }
      else
        {
         count = modulesTable.getRowCount();
         found = false;
         
         if (lastModuleRow < count)
           {
            theRow = modulesTable.convertRowIndexToModel(lastModuleRow);
            net.sf.clipsrules.jni.Module theModule = modules.get(theRow);
                 
            if (theModule.getModuleName().equals(lastModule))       
              {
               modulesTable.setRowSelectionInterval(lastModuleRow,lastModuleRow);
               found = true;
              }
           }
           
         if (! found)
           {
            for (i = 0; i < count; i++)
              {
               theRow = modulesTable.convertRowIndexToModel(i);
               net.sf.clipsrules.jni.Module theModule = modules.get(theRow);

               if (theModule.getModuleName().equals(lastModule))       
                 {
                  found = true;
                  modulesTable.setRowSelectionInterval(i,i); 
                  break;
                 }
              }
           }
           
         if (! found)
           { 
            lastEntityRow = -1;
            lastEntity = null;
            if (count > 0)
              {
               if (lastModuleRow < count)
                 { modulesTable.setRowSelectionInterval(lastModuleRow,lastModuleRow); }
               else 
                 { modulesTable.setRowSelectionInterval(count-1,count-1); }
              }
           }
        }
        
      if (lastEntityRow == -1)
        { 
         if (entityTable.getRowCount() > 0)
           { 
            theEntity = entities.get(0);
            entityTable.setRowSelectionInterval(0,0); 
            slotsModel.setItem(theEntity);
           }
        }
      else
        {
         count = entityTable.getRowCount();
         found = false;
         
         if (lastEntityRow < count)
           {
            theRow = entityTable.convertRowIndexToModel(lastEntityRow);
            theEntity = entities.get(theRow);
                        
            if (theEntity.getName().equals(lastEntity))
              {
               entityTable.setRowSelectionInterval(lastEntityRow,lastEntityRow);
               slotsModel.setItem(theEntity);
               found = true;
              }
           }
           
         if (! found)
           {
            for (i = 0; i < count; i++)
              {
               theRow = entityTable.convertRowIndexToModel(i);
               theEntity = entities.get(theRow);              

               if (theEntity.getName().equals(lastEntity))
                 {
                  found = true;
                  entityTable.setRowSelectionInterval(i,i);
                  slotsModel.setItem(theEntity);
                  break;
                 }
              }
           }
           
         if (! found)
           {
            if (count > 0)
              {
               if (lastEntityRow < count)
                 { 
                  theEntity = entities.get(lastEntityRow);
                  entityTable.setRowSelectionInterval(lastEntityRow,lastEntityRow); 
                  slotsModel.setItem(theEntity);
                 }
               else 
                 { 
                  theEntity = entities.get(count-1);
                  entityTable.setRowSelectionInterval(count-1,count-1); 
                  slotsModel.setItem(theEntity);
                 }
              }
           }
        }
   
      lastModuleRow = -1;
      lastModule = null;
      lastEntity = null;
      lastEntityRow = -1;
     }
          
   /***************************/
   /* selectedEntityConstruct */
   /***************************/
   public long selectedEntityConstruct() 
     {
      int viewRow = entityTable.getSelectedRow();
      if (viewRow == -1) return -1;
      viewRow = entityTable.convertRowIndexToModel(viewRow);
         
      FactInstance theEntity = entities.get(viewRow);

      return theEntity.getTypeAddress();
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
         entityModel.fireTableDataChanged();
         if (entityTable.getRowCount() != 0)
           { entityTable.setRowSelectionInterval(0,0); }
        }
      else if (event.getSource().equals(entityTable.getSelectionModel()))
        {
         int viewRow = entityTable.getSelectedRow();
         if (viewRow == -1) return;
         viewRow = entityTable.convertRowIndexToModel(viewRow);
         
          if (entities.size() == 0)
            { slotsModel.setItem(null); }
          else
            { 
             FactInstance theEntity = entities.get(viewRow);
             slotsModel.setItem(theEntity);
            }
        }

      actionPerformed(new ActionEvent(this,AWTEvent.RESERVED_ID_MAX + 1,BROWSER_SELECTION_ACTION));
     }
     
   /*****************/
   /* performSearch */
   /*****************/
   private void performSearch() 
     {
      entityModel.fireTableDataChanged();
      if (entityTable.getRowCount() != 0)
        { entityTable.setRowSelectionInterval(0,0); }
      else
        { slotsModel.setItem(null); }
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
