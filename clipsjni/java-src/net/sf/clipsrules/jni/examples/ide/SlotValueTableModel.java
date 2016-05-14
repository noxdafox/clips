package net.sf.clipsrules.jni.examples.ide;

import javax.swing.table.AbstractTableModel;

import java.util.List;

import net.sf.clipsrules.jni.*;

class SlotValueTableModel extends AbstractTableModel 
  {
   private String[] columnNames = { "Slot" , "Value" };

   private FactInstance item;
                                   
   public SlotValueTableModel() 
     {
      this(null);
     }

   public SlotValueTableModel(
     FactInstance theItem) 
     {
      item = theItem;
     }

   public void setItem(
     FactInstance theItem) 
     {
      item = theItem;
      this.fireTableDataChanged();
     }
 
   public int getColumnCount()
     {
      return 2;
     }
 
   public int getRowCount()
     {
      if (item == null) return(0);
      
      List<SlotValue> slotValues = item.getSlotValues();
      
      if (slotValues == null) return 0;
      else return slotValues.size();
     }
 
   public String getColumnName(
     int col)
     {
      return columnNames[col];
     }
 
   public SlotValue getSlotValue(int row)
     {
      return item.getSlotValues().get(row);
     }
     
   public Object getValueAt(int row, int col) 
     {
      if (col == 0)
        { return item.getSlotValues().get(row).getSlotName(); }
      else 
        { return item.getSlotValues().get(row).getSlotValue(); }
     }
 
   public Class getColumnClass(
     int c) 
     {
      return String.class;
     }
  }