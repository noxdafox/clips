package net.sf.clipsrules.jni.examples.ide;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;

public class FontSample extends JPanel
  {
   protected JLabel displayText;

   /**************/
   /* FontSample */
   /**************/
   public FontSample(
     int labelPosition) 
     {
      super();
      createComponents(labelPosition);
      buildLayout();
     }

   /**************/
   /* FontSample */
   /**************/
   public FontSample(
     Font theFont,
     int labelPosition) 
     {
      this(labelPosition);
      refreshSampleText(theFont);
     }
   
   /***************/
   /* buildLayout */
   /***************/
   protected void buildLayout()
     {          
      setLayout(new BorderLayout());
      add(displayText,BorderLayout.PAGE_START);
     }
   
   /********************/
   /* createComponents */
   /********************/
   protected void createComponents(
     int labelPosition)
     {
      displayText = new JLabel("Sample Text",labelPosition);
     }

   /*********************/
   /* refreshSampleText */
   /*********************/
   public void refreshSampleText(
     String description,
     Font font)
     {
      displayText.setText(description);
      displayText.setFont(font);
     }

   /*********************/
   /* refreshSampleText */
   /*********************/
   public void refreshSampleText(
     Font font)
     {
      refreshSampleText(getSelectedFontDescription(font),font);
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
  }
  