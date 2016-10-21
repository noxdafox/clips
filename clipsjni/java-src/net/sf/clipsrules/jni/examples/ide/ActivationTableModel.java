package net.sf.clipsrules.jni.examples.ide;

import javax.swing.table.AbstractTableModel;

import net.sf.clipsrules.jni.*;

class ActivationTableModel extends AbstractTableModel 
  {
   private String[] columnNames = { "Salience" , "Rule" , "Basis" };
                                   
   private Agenda agenda = null;

   public ActivationTableModel() 
     {
     }

   public ActivationTableModel(
     Agenda theAgenda) 
     {
      agenda = theAgenda;
     }

   public void setAgenda(
     Agenda theAgenda) 
     {
      agenda = theAgenda;
      this.fireTableDataChanged();
     }
 
   public int getColumnCount()
     {
      return columnNames.length;
     }
 
   public int getRowCount()
     {
      if (agenda == null) return(0);
      return agenda.size();
     }
 
   public String getColumnName(
     int col)
     {
      return columnNames[col];
     }
 
   public Object getValueAt(int row, int col) 
     {
      if (col == 0)
        { return agenda.get(row).getSalience(); }
      else if (col == 1)
        { return agenda.get(row).getRuleName(); }
      else
        { return agenda.get(row).getBasis(); }
     }
 
   public Class getColumnClass(
     int c) 
     {
      if (c == 0) return Integer.class;
      else return String.class;
     }
  }