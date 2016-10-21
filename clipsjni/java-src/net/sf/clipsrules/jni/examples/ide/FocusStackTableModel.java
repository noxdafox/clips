package net.sf.clipsrules.jni.examples.ide;

import javax.swing.table.AbstractTableModel;

import net.sf.clipsrules.jni.*;

class FocusStackTableModel extends AbstractTableModel 
  {
   private String[] columnNames = { "Focus Stack" };
   
   private FocusStack stack = null;

   public FocusStackTableModel() 
     {
     }
   
   public FocusStackTableModel(
     FocusStack theStack) 
     {
      stack = theStack;
     }

   public void setStack(
     FocusStack theStack) 
     {
      stack = theStack;
      this.fireTableDataChanged();
     }

   public int getColumnCount()
     {
      return columnNames.length;
     }
 
   public int getRowCount()
     {
      if (stack == null) return 0;
      return stack.size();
     }
 
   public String getColumnName(
     int col)
     {
      return columnNames[col];
     }
 
   public Object getValueAt(int row, int col) 
     {
      return stack.get(row);
     }
 
   public Class getColumnClass(
     int c) 
     {
      return String.class;
     }
  }