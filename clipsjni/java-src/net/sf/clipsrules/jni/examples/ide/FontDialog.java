package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.border.*;

public class FontDialog extends JDialog
                        implements ActionListener
  {
   protected FontProperties propertiesPanel;
   protected FontSample samplePanel;
   private Font selectedFont = null;
   private String selectedFontDescription = "";

   /**************/
   /* FontDialog */
   /**************/
   public FontDialog(
     Frame frame,
     Component theComponent,
     String title,
     Font initialValue)
     {
      super(frame,title,true);

      FontDialogBuilder(title,initialValue);
      setLocationRelativeTo(theComponent);
     }

   /**************/
   /* FontDialog */
   /**************/
   public FontDialog(
     JDialog owner,
     Component theComponent,
     String title,
     Font initialValue)
     {
      super(owner,title,true);
      
      FontDialogBuilder(title,initialValue);
      setLocationRelativeTo(owner);
     }

   /*********************/
   /* FontDialogBuilder */
   /*********************/
   public void FontDialogBuilder(
     String title,
     Font initialValue)
     {
      Container contentPane = getContentPane();
      GridBagConstraints constraints = new GridBagConstraints();
      GridBagLayout layout = new GridBagLayout();
      contentPane.setLayout(layout);
      
      constraints.insets = new Insets(5,10,5,10);
      constraints.fill = GridBagConstraints.BOTH;
      constraints.weightx = 1;
      constraints.gridx = 0;

      /*============================================*/
      /* Add the font properties area to the frame. */
      /*============================================*/
      
      propertiesPanel = new FontProperties(this);
      constraints.weighty = 0.85;
      layout.setConstraints(propertiesPanel,constraints);
      contentPane.add(propertiesPanel);
      
      /*========================================*/
      /* Add the text sample area to the frame. */
      /*========================================*/
            
      samplePanel = new FontSample(JLabel.CENTER);
      TitledBorder tb = new TitledBorder(new EtchedBorder(),"Sample");
      samplePanel.setBorder(tb);
      constraints.weighty = 0.0;      
      layout.setConstraints(samplePanel,constraints);
      contentPane.add(samplePanel);
     
      /*====================================*/
      /* Create the Cancel and Set buttons. */
      /*====================================*/
            
      JButton cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(this);

      final JButton setButton = new JButton("Set");
      setButton.setActionCommand("Set");
      setButton.addActionListener(this);
      getRootPane().setDefaultButton(setButton);
      
      /*=========================================*/
      /* Lay out the buttons from left to right. */
      /*=========================================*/
        
      JPanel buttonPanel = new JPanel();
      buttonPanel.setLayout(new BoxLayout(buttonPanel,BoxLayout.LINE_AXIS));
      buttonPanel.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
      buttonPanel.add(Box.createHorizontalGlue());
      buttonPanel.add(cancelButton);
      buttonPanel.add(Box.createRigidArea(new Dimension(10, 0)));
      buttonPanel.add(setButton);

      constraints.weighty = 0.0;
      layout.setConstraints(buttonPanel,constraints);
      contentPane.add(buttonPanel);

      /*====================*/
      /* Initialize values. */
      /*====================*/
      
      refreshDisplayFont();
      setValue(initialValue);
      pack();
     }

   /**************/
   /* showDialog */
   /**************/
   public Font showDialog() 
     {            
      setMinimumSize(new Dimension(410,230));
      setVisible(true);
      return selectedFont;
     }
     
   /*******************/
   /* actionPerformed */
   /*******************/
   public void actionPerformed(ActionEvent e)
     {
      if ("Set".equals(e.getActionCommand()))
        { selectedFontDescription = getSelectedFontDescription(selectedFont); }
      else
        { selectedFont = null; }
             
      setVisible(false);
     }
     
   /**********************/
   /* refreshDisplayFont */
   /**********************/
   public void refreshDisplayFont()
     {
      Font theFont = getSelectedFont();
      String theDescription = getSelectedFontDescription(theFont);
      
      samplePanel.refreshSampleText(theDescription,theFont);      
     }
   
   /*******************/
   /* getSelectedFont */
   /*******************/
   public Font getSelectedFont()
     {
      String name = propertiesPanel.getSelectedFontName();
      int style = 0;
      style += (propertiesPanel.isBoldSelected() ? Font.BOLD : 0);
      style += (propertiesPanel.isItalicSelected() ? Font.ITALIC : 0);
      int size = propertiesPanel.getSelectedFontSize();
      
      selectedFont = new Font(name,style,size);
      
      return selectedFont;
     }

   /******************************/
   /* getSelectedFontDescription */
   /******************************/
   public static String getSelectedFontDescription(
     Font theFont)
     {
      String style;
                  
      if (theFont.isBold())
        {
         if (theFont.isItalic())
           { style = " Bold Italic"; }
         else
           { style = " Bold"; }
        }
      else if (theFont.isItalic())
        { style = " Italic"; }
      else
        { style = ""; }

      return theFont.getFamily() + " " + theFont.getSize() + style;      
     }
     
   /************/
   /* setValue */
   /************/
   private void setValue(
     Font newValue)
     {
      String style;
      
      selectedFont = newValue;
            
      if (selectedFont.isBold())
        {
         if (selectedFont.isItalic())
           { style = "Bold Italic"; }
         else
           { style = "Bold"; }
        }
      else if (selectedFont.isItalic())
        { style = "Italic"; }
      else
        { style = "Regular"; }

      propertiesPanel.setFont(selectedFont.getFamily(),style,selectedFont.getSize());
     }
  }

