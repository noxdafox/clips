package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;
import java.awt.event.*;
import java.util.HashMap;

public class FontPreferencesDialog extends JDialog                         
                                   implements ActionListener
  {
   HashMap<String,Font> fonts;
   HashMap<String,FontSample> samples;
   HashMap<String,Font> originalFonts;

   private static final String dialogFontDesc = "Set the dialog window font";
   private static final String editorFontDesc = "Set the editor window font";
   private static final String browserFontDesc = "Set the browser window font";
      
   /*************************/
   /* FontPreferencesDialog */
   /*************************/
    public FontPreferencesDialog(
      Frame frame,
      String title,
      HashMap<String,Font> theFonts)
      {
       super(frame,title,true);
       
       fonts = theFonts;
       samples = new HashMap<String,FontSample>();
       originalFonts = new HashMap<String,Font>(fonts);
       
       /*========================*/
       /* Create the tab panels. */
       /*========================*/
       
       JPanel dialogPanel = createFontDialogBox("dialog");
       JPanel editorPanel = createFontDialogBox("editor"); 
       JPanel browserPanel = createFontDialogBox("browser"); 

       /*========================*/
       /* Set the panel borders. */
       /*========================*/
       
       Border padding = BorderFactory.createEmptyBorder(20,20,5,20);
       dialogPanel.setBorder(padding);
       editorPanel.setBorder(padding);
       browserPanel.setBorder(padding);

       /*============================*/
       /* Create the tabbed pane and */
       /* add the tab panels to it.  */
       /*============================*/
       
       JTabbedPane tabbedPane = new JTabbedPane();
       tabbedPane.addTab("Dialog", null,
                          dialogPanel,
                          dialogFontDesc);
       tabbedPane.addTab("Editor", null,
                          editorPanel,
                          editorFontDesc); 
       tabbedPane.addTab("Browser", null,
                          browserPanel,
                          browserFontDesc);

       add(tabbedPane,BorderLayout.CENTER);
       
       /*=============================*/
       /* Create the Defaults button. */
       /*=============================*/
            
       JButton defaultsButton = new JButton("Defaults");
       defaultsButton.addActionListener(this);
       defaultsButton.setActionCommand("Defaults");

       JButton cancelButton = new JButton("Cancel");
       cancelButton.addActionListener(this);
       cancelButton.setActionCommand("Cancel");
 
       JButton doneButton = new JButton("Done");
       doneButton.addActionListener(this);
       doneButton.setActionCommand("Done");
       getRootPane().setDefaultButton(doneButton);
     
       /*=========================================*/
       /* Lay out the buttons from left to right. */
       /*=========================================*/
        
       JPanel buttonPanel = new JPanel();
       buttonPanel.setLayout(new BoxLayout(buttonPanel,BoxLayout.LINE_AXIS));
       buttonPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
       
       buttonPanel.add(defaultsButton);
       buttonPanel.add(Box.createHorizontalGlue());
       buttonPanel.add(cancelButton);
       buttonPanel.add(Box.createRigidArea(new Dimension(10, 0)));
       buttonPanel.add(doneButton);
      
       add(buttonPanel,BorderLayout.SOUTH);
         
       /*===============================*/
       /* Size and position the dialog. */
       /*===============================*/
       
       pack();
       setLocationRelativeTo(frame);
      }
             
    /***********************/
    /* createFontDialogBox */
    /***********************/
    private JPanel createFontDialogBox(
      String fontTab)
      {
       final JButton setFontButton = new JButton("Change...");
       Frame theFrame = (JFrame) this.getParent();

       JLabel label = new JLabel("Font:");

       FontSample samplePanel = new FontSample(fonts.get(fontTab),JLabel.LEFT);
       samples.put(fontTab,samplePanel);
                               
       JPanel box = new JPanel();
       box.setLayout(new BoxLayout(box,BoxLayout.LINE_AXIS));
       box.add(label);
       box.add(Box.createHorizontalStrut(10));
       box.add(setFontButton);       
       box.add(Box.createHorizontalGlue());

       JPanel pane = new JPanel();
       pane.setLayout(new BoxLayout(pane,BoxLayout.PAGE_AXIS));
       pane.add(box);
       pane.add(Box.createVerticalStrut(10));
       pane.add(samplePanel);
          
       setFontButton.addActionListener(new ActionListener() 
         {
          public void actionPerformed(ActionEvent e) 
            {
             FontDialog theDialog = new FontDialog(FontPreferencesDialog.this,
                                                   FontPreferencesDialog.this,
                                                   "Font Chooser",fonts.get(fontTab));

             Font selectedFont = theDialog.showDialog();
                          
             if (selectedFont != null)
               {                                                   
                samplePanel.refreshSampleText(selectedFont);
                fonts.put(fontTab,selectedFont);
               }                                                   
            }
         });

       return pane;
      }

    /**************/
    /* showDialog */
    /**************/
    public HashMap<String,Font> showDialog(
      HashMap<String,Font> theFonts)
      {       
       setMinimumSize(new Dimension(390,170));
       setVisible(true);
       return theFonts;
      }
      
   /*******************/
   /* actionPerformed */
   /*******************/
   public void actionPerformed(ActionEvent e)
     {
      if ("Defaults".equals(e.getActionCommand()))
        {  
         Font dialogFont = new Font("Monospaced",Font.PLAIN,12);
         Font editorFont = new Font("Monospaced",Font.PLAIN,12);
         Font browserFont = new Font("Dialog",Font.PLAIN,12);
         
         fonts.put("dialog",dialogFont);
         fonts.put("editor",editorFont);
         fonts.put("browser",browserFont);
         
         samples.get("dialog").refreshSampleText(dialogFont);
         samples.get("editor").refreshSampleText(editorFont);
         samples.get("browser").refreshSampleText(browserFont);
        }
      else if ("Done".equals(e.getActionCommand()))
        { setVisible(false); }
      else if ("Cancel".equals(e.getActionCommand()))
        { 
         fonts.putAll(originalFonts);
         setVisible(false); 
        }
     }
  }
