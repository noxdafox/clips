package net.sf.clipsrules.jni.examples.ide;

import javax.swing.table.AbstractTableModel;

import java.util.List;

import net.sf.clipsrules.jni.*;

class FactInstanceTableModel extends AbstractTableModel 
  {
   private String[] columnNames = { "" , "" };

   private List<FactInstance> items;
                                   
   public FactInstanceTableModel(
     String theName,
     String theRelation) 
     {
      this(theName,theRelation,null);
     }

   public FactInstanceTableModel(
     String theName,
     String theRelation,
     List<FactInstance> theItems) 
     {
      columnNames[0] = theName;
      columnNames[1] = theRelation;
      items = theItems;
     }

   public void setItems(
     List<FactInstance> theItems) 
     {
      items = theItems;
      this.fireTableDataChanged();
     }
 
   public int getColumnCount()
     {
      return 2;
     }
 
   public int getRowCount()
     {
      if (items == null) return(0);
      return items.size();
     }
 
   public String getColumnName(
     int col)
     {
      return columnNames[col];
     }
     
    public FactInstance getFactInstance(int row)
     {
      return items.get(row);
     }

   public Object getValueAt(int row, int col) 
     {
      if (col == 0)
        { return items.get(row).getName(); }
      else 
        { return items.get(row).getRelationName(); }
     }
 
   public Class getColumnClass(
     int c) 
     {
      return String.class;
     }
  }