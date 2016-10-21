package net.sf.clipsrules.jni.examples.ide;

import javax.swing.table.AbstractTableModel;
import java.util.List;

import net.sf.clipsrules.jni.*;

class ModuleTableModel extends AbstractTableModel 
  {
   private String[] columnNames = { "Module" };

   private List<Module> modules;

   public ModuleTableModel() 
     {
     }
   
   public ModuleTableModel(
     List<Module> theModules) 
     {
      modules = theModules;
     }

   public void setModules(
     List<Module> theModules) 
     {
      modules = theModules;
      this.fireTableDataChanged();
     }

   public int getColumnCount()
     {
      return columnNames.length;
     }
 
   public int getRowCount()
     {
      if (modules == null) return 0;
      return modules.size();
     }
 
   public String getColumnName(
     int col)
     {
      return columnNames[col];
     }
 
   public Object getValueAt(int row, int col) 
     {
      return modules.get(row);
     }
 
   public Class getColumnClass(
     int c) 
     {
      return String.class;
     }
  }