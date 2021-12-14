package net.sf.clipsrules.jni.examples.ide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

public class FontProperties extends JPanel
  {
   protected JList<String> nameList;
   protected JList<String> styleList;
   protected JList<Integer> sizeList;
   protected FontDialog frame;
   public final static Integer [] fontSizes = 
      { Integer.valueOf(9),  Integer.valueOf(10), Integer.valueOf(11), Integer.valueOf(12), 
        Integer.valueOf(13), Integer.valueOf(14), Integer.valueOf(16), Integer.valueOf(18), 
        Integer.valueOf(20), Integer.valueOf(22), Integer.valueOf(24), Integer.valueOf(26), 
        Integer.valueOf(28), Integer.valueOf(32), Integer.valueOf(36), Integer.valueOf(48), 
        Integer.valueOf(64) };
   public final static String[] fontStyles = { "Regular" , "Italic" , "Bold" , "Bold Italic" };

   /******************/
   /* FontProperties */
   /******************/
   public FontProperties(FontDialog stf) 
     {
      super();
      frame = stf;
      createComponents();
      buildLayout();
     }
   
   /***************/
   /* buildLayout */
   /***************/
   protected void buildLayout()
     { 
      GridBagConstraints constraints = new GridBagConstraints();
      GridBagLayout layout = new GridBagLayout();
      setLayout(layout);

      constraints.gridy = 0;
      constraints.weighty = 1.0;
      constraints.insets = new Insets(5,5,5,5);

      /*=======================*/
      /* Create the font list. */
      /*=======================*/
      
      JPanel fontPane = new JPanel();
      fontPane.setLayout(new BoxLayout(fontPane,BoxLayout.PAGE_AXIS)); 
      
      JLabel fontLabel = new JLabel("Font:"); 
      fontLabel.setLabelFor(nameList);

      JPanel fontLabelPanel = new JPanel();
      fontLabelPanel.setLayout(new BoxLayout(fontLabelPanel,BoxLayout.LINE_AXIS));
      fontLabelPanel.add(fontLabel);
      fontLabelPanel.add(Box.createHorizontalGlue());

      nameList.setVisibleRowCount(5);
      JScrollPane jsp = new JScrollPane(nameList);

      jsp.setPreferredSize(new Dimension(240,160));
      jsp.setMinimumSize(new Dimension(240,65));

      constraints.fill = GridBagConstraints.BOTH;
      constraints.gridx = 0;
      constraints.weightx = 0.55; 
      layout.setConstraints(fontPane,constraints); 
      
      fontPane.add(fontLabelPanel);
      fontPane.add(jsp); 
      add(fontPane);
      
      /*========================*/
      /* Create the style list. */
      /*========================*/

      JPanel stylePane = new JPanel();
      stylePane.setLayout(new BoxLayout(stylePane,BoxLayout.PAGE_AXIS)); 
      
      JLabel styleLabel = new JLabel("Style:"); 
      fontLabel.setLabelFor(styleList);

      JPanel styleLabelPanel = new JPanel();
      styleLabelPanel.setLayout(new BoxLayout(styleLabelPanel,BoxLayout.LINE_AXIS));
      styleLabelPanel.add(styleLabel);
      styleLabelPanel.add(Box.createHorizontalGlue());

      styleList.setVisibleRowCount(4);
      jsp = new JScrollPane(styleList);
                    
      jsp.setPreferredSize(new Dimension(90,95));
      jsp.setMinimumSize(new Dimension(90,65));

      constraints.fill = GridBagConstraints.BOTH; 
      constraints.gridx = 1;
      constraints.weightx = 0.3; 
      layout.setConstraints(stylePane,constraints);
      
      stylePane.add(styleLabelPanel);
      stylePane.add(jsp); 
      add(stylePane);
      
      /*=======================*/
      /* Create the size list. */
      /*=======================*/
      
      JPanel sizePane = new JPanel();
      sizePane.setLayout(new BoxLayout(sizePane,BoxLayout.PAGE_AXIS)); 
      
      JLabel sizeLabel = new JLabel("Size:"); 
      sizeLabel.setLabelFor(sizeList);
      
      JPanel sizeLabelPanel = new JPanel();
      sizeLabelPanel.setLayout(new BoxLayout(sizeLabelPanel,BoxLayout.LINE_AXIS));
      sizeLabelPanel.add(sizeLabel);
      sizeLabelPanel.add(Box.createHorizontalGlue());

      sizeList.setVisibleRowCount(5);
      jsp = new JScrollPane(sizeList);
                     
      jsp.setPreferredSize(new Dimension(90,95));
      jsp.setMinimumSize(new Dimension(90,65));

      constraints.fill = GridBagConstraints.BOTH; 
      constraints.gridx = 2;
      constraints.weightx = 0.15;
      layout.setConstraints(sizePane,constraints);
      
      sizePane.add(sizeLabelPanel);
      sizePane.add(jsp); 
      add(sizePane);
     }
   
   /********************/
   /* createComponents */
   /********************/
   protected void createComponents()
     {
      GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();

      /**********************************/
      /* Create the list of font names. */
      /**********************************/
      
      String[] names = ge.getAvailableFontFamilyNames();
      nameList = new JList<String>(names);
      nameList.setSelectedIndex(0);
      nameList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      nameList.addListSelectionListener(
         new ListSelectionListener()
           {
            public void valueChanged(ListSelectionEvent event)
              {
               if (! event.getValueIsAdjusting())
                 { handleFontPropertyChange(); }
              }
            } );
            
      /*=================================*/
      /* Create the list of font styles. */
      /*=================================*/
      
      styleList = new JList<String>(fontStyles);
      styleList.setSelectedIndex(0);
      styleList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      styleList.addListSelectionListener(
         new ListSelectionListener()
           {
            public void valueChanged(ListSelectionEvent event)
              {
               if (! event.getValueIsAdjusting())
                 { handleFontPropertyChange(); }
              }
            } );

      /*================================*/
      /* Create the list of font sizes. */
      /*================================*/
      
      sizeList = new JList<Integer>(fontSizes);
      sizeList.setSelectedIndex(0);
      sizeList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      sizeList.addListSelectionListener(
         new ListSelectionListener()
           {
            public void valueChanged(ListSelectionEvent event)
              {
               if (! event.getValueIsAdjusting())
                 { handleFontPropertyChange(); }
              }
            } );
     }
     
   /***********/
   /* setFont */
   /***********/
   public void setFont(
     String name,
     String style,
     Integer size)
     { 
      nameList.setSelectedValue(name,true);
      styleList.setSelectedValue(style,true);
      sizeList.setSelectedValue(size,true);
     }
          
   /****************************/
   /* handleFontPropertyChange */
   /****************************/
   protected void handleFontPropertyChange()
     { 
      frame.refreshDisplayFont();
     }
   
   /***********************/
   /* getSelectedFontName */
   /***********************/
   public String getSelectedFontName()
     { 
      return (String)(nameList.getSelectedValue());
     }
     
   /***********************/
   /* getSelectedFontSize */
   /***********************/
   public int getSelectedFontSize()
     {
      Integer size = (Integer) sizeList.getSelectedValue();
      
      return size.intValue();
     }
     
   /******************/
   /* isBoldSelected */
   /******************/
   public boolean isBoldSelected()
     {
      String style = (String) styleList.getSelectedValue();
      
      return style.contains("Bold");
     }
   
   /********************/
   /* isItalicSelected */
   /********************/
   public boolean isItalicSelected()
     {
      String style = (String) styleList.getSelectedValue();

      return style.contains("Italic");
     }
  }
  