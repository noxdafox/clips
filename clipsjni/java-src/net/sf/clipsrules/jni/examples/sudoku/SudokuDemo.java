package net.sf.clipsrules.jni.examples.sudoku;

import java.awt.*;    
import java.awt.event.*;
import javax.swing.*;    
import javax.swing.event.*; 
import javax.swing.table.*; 

import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;

import net.sf.clipsrules.jni.*;

/* TBD Allow tabbing between different grids. */
/* TBD Allow arrow keys to move between different grids. */
/* TBD Web links for techniques. */

/*

Notes:

This example creates just a single environment. If you create multiple environments,
call the destroy method when you no longer need the environment. This will free the
C data structures associated with the environment.

   clips = new Environment();
      .
      . 
      .
   clips.destroy();

Calling the clear, reset, load, loadFacts, run, eval, build, assertString,
and makeInstance methods can trigger CLIPS garbage collection. If you need
to retain access to a PrimitiveValue returned by a prior eval, assertString,
or makeInstance call, retain it and then release it after the call is made.

   PrimitiveValue pv1 = clips.eval("(myFunction foo)");
   pv1.retain();
   PrimitiveValue pv2 = clips.eval("(myFunction bar)");
      .
      .
      .
   pv1.release();

*/


class SudokuDemo implements ActionListener, FocusListener, KeyListener
  {  
   JFrame jfrm;
   JPanel mainGrid;
   
   JButton clearButton;
   JButton resetButton;
   JButton solveButton;
   JButton techniquesButton;
   
   Object resetValues[][][] = new Object[9][3][3];
   
   boolean solved = false;
   
   ResourceBundle sudokuResources;
   
   Environment clips;
   boolean isExecuting = false;
   Thread executionThread;
   
   /**************/
   /* SudokuDemo */
   /**************/
   SudokuDemo() 
     {    
      JTable theSubGrid;
      int r, c;
      
      /*====================================*/
      /* Load the internationalized string  */
      /* resources used by the application. */
      /*====================================*/
      
      try
        {
         sudokuResources = ResourceBundle.getBundle("net.sf.clipsrules.jni.examples.sudoku.resources.SudokuResources",Locale.getDefault());
        }
      catch (MissingResourceException mre)
        {
         mre.printStackTrace();
         return;
        }
       
      /*===================================*/
      /* Create the main JFrame container. */
      /*===================================*/
      
      jfrm = new JFrame(sudokuResources.getString("SudokuDemo"));  
      jfrm.getContentPane().setLayout(new BorderLayout());

      /*=============================================================*/
      /* Terminate the program when the user closes the application. */
      /*=============================================================*/
          
      jfrm.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);    
  
      /*=======================================================*/
      /* Create the JPanel which will contain the sudoku grid. */
      /*=======================================================*/
      
      mainGrid = new JPanel(); 
      
      GridLayout theLayout = new GridLayout(3,3);
      theLayout.setHgap(-1);
      theLayout.setVgap(-1); 

      mainGrid.setLayout(theLayout);   
      mainGrid.setOpaque(true);
      
      /*=================================================*/
      /* Create a renderer based on the default renderer */
      /* that will center the text within the cell.      */
      /*=================================================*/
      
      DefaultTableCellRenderer renderer = 
         new DefaultTableCellRenderer()
           {
            public Component getTableCellRendererComponent(
              JTable table,Object value,boolean isSelected,boolean hasFocus,int row,int column)
              {
               Component comp = super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column);
               if (comp instanceof JLabel)
                 { 
                  ((JLabel) comp).setHorizontalAlignment(JLabel.CENTER); 
                  if (value instanceof String)
                    { 
                     if ("?".equals(value))
                       { ((JLabel) comp).setForeground(Color.red); }
                     else if (((String) value).length() > 1)
                       { ((JLabel) comp).setForeground(Color.green.darker()); }
                     else
                       { ((JLabel) comp).setForeground(Color.black); }
                    }
                 }
               return comp;
              }
           };

      /*========================================*/
      /* Create each of the nine 3x3 grids that */
      /* will go inside the main sudoku grid.   */
      /*========================================*/
      
      for (r = 0; r < 3; r++)
        {
         for (c = 0; c < 3; c++)
           {
            theSubGrid = 
               new JTable(3,3)
                 {
                  public boolean isCellEditable(int rowIndex,int vColIndex) 
                    { return false; }
                 };
              
            theSubGrid.setRowSelectionAllowed(false);
            theSubGrid.setShowGrid(true);
            theSubGrid.setRowHeight(25);
            theSubGrid.setGridColor(Color.black);
            theSubGrid.setBorder(BorderFactory.createLineBorder(Color.black,2));     
            theSubGrid.setDefaultRenderer(Object.class,renderer);
            
            theSubGrid.addFocusListener(this);
            theSubGrid.addKeyListener(this);
                                                                                            
            TableColumn column = null;
            for (int i = 0; i < 3; i++) 
              {
               column = theSubGrid.getColumnModel().getColumn(i);
               column.setMaxWidth(25);
              }

            mainGrid.add(theSubGrid);
           }
        }
       
      /*========================================*/
      /* Set up the panel containing the Clear, */
      /* Reset, Solve, and Techniques buttons.  */
      /*========================================*/

      JPanel buttonGrid = new JPanel();
      
      theLayout = new GridLayout(4,1);

      buttonGrid.setLayout(theLayout);   
      buttonGrid.setOpaque(true);
            
      clearButton = new JButton(sudokuResources.getString("Clear")); 
      clearButton.setActionCommand("Clear");
      buttonGrid.add(clearButton);
      clearButton.addActionListener(this);
      clearButton.setToolTipText(sudokuResources.getString("ClearTip")); 
      
      resetButton = new JButton(sudokuResources.getString("Reset")); 
      resetButton.setActionCommand("Reset");
      resetButton.setEnabled(false);
      buttonGrid.add(resetButton);
      resetButton.addActionListener(this);
      resetButton.setToolTipText(sudokuResources.getString("ResetTip")); 
      
      solveButton = new JButton(sudokuResources.getString("Solve")); 
      solveButton.setActionCommand("Solve");
      buttonGrid.add(solveButton);
      solveButton.addActionListener(this);
      solveButton.setToolTipText(sudokuResources.getString("SolveTip")); 
      
      techniquesButton = new JButton(sudokuResources.getString("Techniques"));
      techniquesButton.setActionCommand("Techniques");
      techniquesButton.setEnabled(false); 
      buttonGrid.add(techniquesButton);
      techniquesButton.addActionListener(this);
      techniquesButton.setToolTipText(sudokuResources.getString("TechniquesTip")); 
      
      /*=============================================*/
      /* Add the grid and button panels to the pane. */
      /*=============================================*/

      JPanel mainPanel = new JPanel(); 
      mainPanel.setLayout(new FlowLayout());
      mainPanel.add(mainGrid);
      mainPanel.add(buttonGrid);
      jfrm.getContentPane().add(mainPanel,BorderLayout.NORTH);

      JLabel instructions = new JLabel("<html><p style=\"font-size:95%\">" + sudokuResources.getString("Instructions") + "</p><br>");
      JPanel labelPanel = new JPanel(); 
      labelPanel.setLayout(new FlowLayout());
      labelPanel.add(instructions);
      jfrm.getContentPane().add(labelPanel,BorderLayout.SOUTH);

      /*==========================*/
      /* Load the sudoku program. */
      /*==========================*/
      
      clips = new Environment();

      try
        {
         clips.loadFromResource("/net/sf/clipsrules/jni/examples/sudoku/resources/sudoku.clp");
         clips.loadFromResource("/net/sf/clipsrules/jni/examples/sudoku/resources/solve.clp");
        }
      catch (Exception e)
        {
         e.printStackTrace();
         System.exit(1);
        }
            
      /**********************/
      /* Display the frame. */
      /**********************/
      
      jfrm.pack(); 
      jfrm.setVisible(true);    
     }    
   
   /********/
   /* main */
   /********/
   public static void main(
     String args[])
     {    
      /*===================================================*/
      /* Create the frame on the event dispatching thread. */
      /*===================================================*/
      
      SwingUtilities.invokeLater(
        new Runnable() 
          {    
           public void run() { new SudokuDemo(); }    
          });    
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
     
   /*************/
   /* runSudoku */
   /*************/  
   public void runSudoku()
     {
      Runnable runThread = 
         new Runnable()
           {
            public void run()
              {
               try
                 { clips.run(); }
               catch (CLIPSException e)
                 { e.printStackTrace(); }
               
               SwingUtilities.invokeLater(
                  new Runnable()
                    {
                     public void run()
                       {
                        try 
                          { updateGrid(); }
                        catch (Exception e)
                          { e.printStackTrace(); }
                       }
                    });
              }
           };
      
      isExecuting = true;
      
      executionThread = new Thread(runThread);
      
      jfrm.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
         
      executionThread.start();
     }

   /*********************/
   /* onActionPerformed */
   /*********************/  
   public void onActionPerformed(
     ActionEvent ae) throws Exception 
     {      
      if (isExecuting) return;

      /*==========================*/
      /* Handle the Clear button. */
      /*==========================*/

      if (ae.getActionCommand().equals("Clear"))  
        { 
         solved = false;
         
         solveButton.setEnabled(true);
         techniquesButton.setEnabled(false);
         
         for (int i = 0; i < 9; i++)
           {
            JTable theTable = (JTable) mainGrid.getComponent(i);

            for (int r = 0; r < 3; r++)
              {
               for (int c = 0; c < 3; c++)
                 { theTable.setValueAt("",r,c);  }         
              }
           }
        }
        
      /*==========================*/
      /* Handle the Reset button. */
      /*==========================*/
        
      else if (ae.getActionCommand().equals("Reset"))  
        {
         solved = false;
         solveButton.setEnabled(true);
         techniquesButton.setEnabled(false);


         for (int i = 0; i < 9; i++)
           {
            JTable theTable = (JTable) mainGrid.getComponent(i);

            for (int r = 0; r < 3; r++)
              {
               for (int c = 0; c < 3; c++)
                 { theTable.setValueAt(resetValues[i][r][c],r,c); }         
              }
           }
        }

      /*==========================*/
      /* Handle the Solve button. */
      /*==========================*/

      else if (ae.getActionCommand().equals("Solve"))  
        {
         /*==============*/
         /* Reset CLIPS. */
         /*==============*/
         
         clips.reset();
         clips.assertString("(phase expand-any)");
         clips.assertString("(size 3)");

         /*======================================*/
         /* Remember the initial starting values */
         /* of the puzzle for the reset command. */
         /*======================================*/
         
         for (int i = 0; i < 9; i++)
           {
            JTable theTable = (JTable) mainGrid.getComponent(i);
            int rowGroup = i / 3;
            int colGroup = i % 3;
            
            for (int r = 0; r < 3; r++)
              {
               for (int c = 0; c < 3; c++)
                 { 
                  resetValues[i][r][c] = theTable.getValueAt(r,c); 
                  
                  String assertStr;
                  
                  assertStr = "(possible (row " + (r + (rowGroup * 3) + 1) + ") " +
                                        "(column " + (c + (colGroup * 3) + 1) + ") " +
                                        "(group " + (i + 1) + ") " +
                                        "(id " + ((i * 9) + (r * 3) + c + 1) + ") ";
                                        
                  if ((resetValues[i][r][c] == null) ||
                      (resetValues[i][r][c].equals("")))
                    { assertStr = assertStr + "(value any))"; }
                  else
                    { assertStr = assertStr + "(value " + resetValues[i][r][c] + "))"; }
                    
                  clips.assertString(assertStr);
                 }         
              }
           }

         /*===================================*/
         /* Update the status of the buttons. */
         /*===================================*/
         
         clearButton.setEnabled(false);
         resetButton.setEnabled(false);
         solveButton.setEnabled(false);
         techniquesButton.setEnabled(false);
         
         /*===================*/
         /* Solve the puzzle. */
         /*===================*/

         runSudoku();
        }

      /*===============================*/
      /* Handle the Techniques button. */
      /*===============================*/
      
      else if (ae.getActionCommand().equals("Techniques"))  
        {
         String evalStr;
         String messageStr = "<html><p style=\"font-size:95%\">";
                  
         List<FactAddressValue> techniqueFacts = clips.findAllFacts("technique");
         
         int tNum = techniqueFacts.size();
         
         for (int i = 1; i <= tNum; i++)
           {
            FactAddressValue fv = clips.findFact("?f","technique-employed","(eq ?f:priority " + i + ")");
            
            if (fv == null) continue;
            
            messageStr = messageStr + ((NumberValue) fv.getSlotValue("priority")).intValue() + ". " +
                                      ((LexemeValue) fv.getSlotValue("reason")).getValue() + "<br>";
           }
        
         JOptionPane.showMessageDialog(jfrm,messageStr,sudokuResources.getString("SolutionTechniques"),JOptionPane.PLAIN_MESSAGE);
        }
     } 
     
   /**************/
   /* updateGrid */
   /**************/  
   private void updateGrid() throws Exception
     { 
      /*===================================*/
      /* Retrieve the solution from CLIPS. */
      /*===================================*/
 
      for (int i = 0; i < 9; i++)
        {
         JTable theTable = (JTable) mainGrid.getComponent(i);
         int rowGroup = i / 3;
         int colGroup = i % 3;
            
         for (int r = 0; r < 3; r++)
           {
            for (int c = 0; c < 3; c++)
              { 
               resetValues[i][r][c] = theTable.getValueAt(r,c); 

               if ((resetValues[i][r][c] != null) &&
                   (! resetValues[i][r][c].equals("")))
                 { continue; }
                  
               String condition = "(and (eq ?f:row " + (r + (rowGroup * 3) + 1) + ") " +
                                       "(eq ?f:column " + (c + (colGroup * 3) + 1) + "))";
                                         
               List<FactAddressValue> possibleValues = clips.findAllFacts("?f","possible",condition);
                                                          
               if (possibleValues.size() != 1) continue;
                  
               FactAddressValue fv = possibleValues.get(0);
                  
               theTable.setValueAt(" " + fv.getSlotValue("value") + " ",r,c);
              }         
           }
        }

      /*===============================================*/
      /* Any cells that have not been assigned a value */
      /* are given a '?' for their content.            */
      /*===============================================*/
         
      for (int i = 0; i < 9; i++)
        {
         JTable theTable = (JTable) mainGrid.getComponent(i);

         for (int r = 0; r < 3; r++)
           {
            for (int c = 0; c < 3; c++)
              { 
               if ((theTable.getValueAt(r,c) == null) ||
                   (theTable.getValueAt(r,c).equals("")))
                 { theTable.setValueAt("?",r,c);  }
              }         
           }
        }

      /*===================================*/
      /* Update the status of the buttons. */
      /*===================================*/

      jfrm.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
         
      solved = true;
      clearButton.setEnabled(true);
      resetButton.setEnabled(true);
      solveButton.setEnabled(false);
      techniquesButton.setEnabled(true);
           
      executionThread = null;
      
      isExecuting = false;
     }     

   /*#######################*/
   /* FocusListener Methods */
   /*#######################*/
   
   /***************/
   /* focusGained */
   /***************/  
   public void focusGained(FocusEvent e) {}

   /*************/
   /* focusLost */
   /*************/
   public void focusLost(FocusEvent e)
     {
      JTable theTable = (JTable) e.getComponent();
      int r = theTable.getEditingRow();
      int c = theTable.getEditingColumn();

      /*====================================================*/
      /* If a cell wasn't being edited, do nothing further. */
      /*====================================================*/

      if ((r == -1) || (c == -1)) return;

      /*========================*/
      /* Stop editing the cell. */
      /*========================*/
      
      TableCellEditor tableCellEditor = theTable.getCellEditor(r,c);
      tableCellEditor.stopCellEditing();
      
      /*=================================*/
      /* Clear selections for the table. */
      /*=================================*/

      theTable.clearSelection();
     }     
        
   /*#####################*/
   /* KeyListener Methods */
   /*#####################*/
   
   /**************/
   /* keyPressed */
   /**************/
     
   public void keyPressed(KeyEvent e) {}
   
   /***************/
   /* keyReleased */
   /***************/
   
   public void keyReleased(KeyEvent e) {}

   /************/
   /* keyTyped */
   /************/
      
   public void keyTyped(
     KeyEvent e)
     {
      JTable theTable = (JTable) e.getComponent();
      int row = theTable.getSelectedRow();
      int col = theTable.getSelectedColumn();
      
      /*=================================*/
      /* Cells can't be change while the */
      /* puzzle is in solution state.    */
      /*=================================*/
      
      if (solved || isExecuting) return;
      
      /*=================================================*/
      /* If a cell isn't selected, ignore the typed key. */
      /*=================================================*/
      
      if ((row == -1) || (col == -1)) return;
      
      /***************************/
      /* Retrieve the typed key. */
      /***************************/
      
      char theChar = e.getKeyChar();
      
      /*=======================================================*/
      /* A backspace removes the value from the selected cell. */
      /*=======================================================*/
      
      if (theChar == '\b')
        {
         theTable.setValueAt("",row,col); 
         return;
        }
        
      /*=========================================================*/
      /* Any character other than the digits 1 to 9 is invalid.  */
      /*=========================================================*/
      
      if ((theChar != '1') && (theChar != '2') && (theChar != '3') &&
          (theChar != '4') && (theChar != '5') && (theChar != '6') &&
          (theChar != '7') && (theChar != '8') && (theChar != '9'))
        {
         Toolkit.getDefaultToolkit().beep();
         return;
        }
      
      /*=====================================*/  
      /* Set the value of the selected cell. */
      /*=====================================*/  
        
      String theCharStr = Character.toString(theChar);
      theTable.setValueAt(theCharStr,row,col); 
      
      /*===========================================*/
      /* Remove any other occurences of this digit */
      /* from the same 3x3 grid.                   */
      /*===========================================*/
      
      for (int r = 0; r < 3; r++)
        {
         for (int c = 0; c < 3; c++)
           {
            if (((r != row) || (c != col)) &&
                (theCharStr.equals(theTable.getValueAt(r,c))))
              { theTable.setValueAt("",r,c);  }          
           }
        }
     }
  }
