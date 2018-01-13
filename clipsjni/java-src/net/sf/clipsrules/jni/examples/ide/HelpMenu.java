package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import java.awt.event.ActionEvent;
import java.net.URI;
import java.net.URL;
import java.awt.Desktop;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import java.awt.Component;
import java.awt.Font;
import java.awt.Dimension;

import javax.swing.ImageIcon;
import javax.imageio.ImageIO;
import java.io.InputStream;

public class HelpMenu extends JMenu 
                   implements ActionListener
  {  
   private static final String clipsHomePageAction = "CLIPSHomePage";
   private static final String onlineDocumentationAction = "OnlineDocumentation";
   private static final String onlineExamplesAction = "OnlineExamples";
   private static final String clipsExpertSystemGroupAction = "CLIPSExpertSystemGroup";
   private static final String sourceForgeForumsAction = "SourceForgeForums";
   private static final String stackOverflowQAAction = "StackOverflowQ&A";
   private static final String aboutCLIPSIDEAction = "AboutCLIPSIDE";

   private JMenuItem jmiCLIPSHomePage = null;
   private JMenuItem jmiOnlineDocumentation = null;   
   private JMenuItem jmiOnlineExamples = null;
   private JMenuItem jmiCLIPSExpertSystemGroup = null;
   private JMenuItem jmiSourceForgeForums = null;
   private JMenuItem jmiStackOverflowQA = null;
   private JMenuItem jmiAboutCLIPSIDE = null;
   
   private JFrame parentFrame;

   class AboutCLIPSIDEDialog extends JDialog 
     {
      public AboutCLIPSIDEDialog(JFrame parent) 
        {
         super(parent,"About CLIPS IDE",true);

         final String imageName = "/net/sf/clipsrules/jni/examples/ide/resources/CLIPS.png";
         
         setLayout(new BoxLayout(getContentPane(),BoxLayout.Y_AXIS));

         getRootPane().setBorder(BorderFactory.createEmptyBorder(10,40,18,40));
         
         try
           {
            InputStream imageInput = getClass().getResourceAsStream(imageName);
            JLabel label = new JLabel(new ImageIcon(ImageIO.read(imageInput)));
            label.setAlignmentX(0.5f);
            add(label);
           }
         catch (Exception e)
           { e.printStackTrace(); }

         JLabel name = new JLabel("CLIPS IDE");
         name.setAlignmentX(0.5f);
         Font font = name.getFont();
         font = new Font(font.getFontName(),Font.BOLD,18);
         name.setFont(font);
         add(name);
 
         name = new JLabel("Version 6.4");
         name.setAlignmentX(0.5f);
         font = new Font(font.getFontName(),Font.PLAIN,12);
         name.setFont(font);
         add(name);
         
         add(Box.createRigidArea(new Dimension(0,18)));

         name = new JLabel("Design and Development");
         name.setAlignmentX(0.5f);
         font = new Font(font.getFontName(),Font.BOLD,14);
         name.setFont(font);
         add(name);
         
         name = new JLabel("Gary Riley");
         name.setAlignmentX(0.5f);
         font = new Font(font.getFontName(),Font.PLAIN,14);
         name.setFont(font);
         add(name);
                  
         add(Box.createRigidArea(new Dimension(0,18)));

         name = new JLabel("Public Domain Release, November 2017");
         name.setAlignmentX(0.5f);
         font = new Font(font.getFontName(),Font.PLAIN,12);
         name.setFont(font);
         add(name);

         add(Box.createRigidArea(new Dimension(0,18)));
         
         JButton ok = new JButton("OK");
         
         ok.addActionListener(
            new ActionListener()
              {
               public void actionPerformed(ActionEvent evt) 
                 { 
                  setVisible(false); 
                  dispose();
                 }
              });
              
         ok.setAlignmentX(Component.CENTER_ALIGNMENT);
         add(ok);

         setModalityType(ModalityType.APPLICATION_MODAL);
         setDefaultCloseOperation(DISPOSE_ON_CLOSE);
         
         pack();
         
         setResizable(false);
        }
     }

   /************/
   /* HelpMenu */
   /************/
   HelpMenu(JFrame theFrame)
     {  
      super("Help");
      
      parentFrame = theFrame;
      
      jmiCLIPSHomePage = new JMenuItem("CLIPS Home Page");
      jmiCLIPSHomePage.setActionCommand(clipsHomePageAction);
      jmiCLIPSHomePage.addActionListener(this);
      add(jmiCLIPSHomePage);

      jmiOnlineDocumentation = new JMenuItem("Online Documentation");
      jmiOnlineDocumentation.setActionCommand(onlineDocumentationAction);
      jmiOnlineDocumentation.addActionListener(this);
      add(jmiOnlineDocumentation);

      jmiOnlineExamples = new JMenuItem("Online Examples");
      jmiOnlineExamples.setActionCommand(onlineExamplesAction);
      jmiOnlineExamples.addActionListener(this);
      add(jmiOnlineExamples);

      jmiCLIPSExpertSystemGroup = new JMenuItem("CLIPS Expert System Group");
      jmiCLIPSExpertSystemGroup.setActionCommand(clipsExpertSystemGroupAction);
      jmiCLIPSExpertSystemGroup.addActionListener(this);
      add(jmiCLIPSExpertSystemGroup);

      jmiSourceForgeForums = new JMenuItem("SourceForge Forums");
      jmiSourceForgeForums.setActionCommand(sourceForgeForumsAction);
      jmiSourceForgeForums.addActionListener(this);
      add(jmiSourceForgeForums);

      jmiStackOverflowQA = new JMenuItem("Stack Overflow Q&A");
      jmiStackOverflowQA.setActionCommand(stackOverflowQAAction);
      jmiStackOverflowQA.addActionListener(this);
      add(jmiStackOverflowQA);
      
      addSeparator();

      jmiAboutCLIPSIDE = new JMenuItem("About CLIPS IDE");
      jmiAboutCLIPSIDE.setActionCommand(aboutCLIPSIDEAction);
      jmiAboutCLIPSIDE.addActionListener(this);
      add(jmiAboutCLIPSIDE);
     }  

   /*################*/
   /* Action Methods */
   /*################*/

   /*********************/
   /* onActionPerformed */
   /*********************/  
   public void onActionPerformed(
     ActionEvent ae) throws Exception 
     {     
      if (ae.getActionCommand().equals(clipsHomePageAction))  
        { openCLIPSHomePage(); }
      else if (ae.getActionCommand().equals(onlineDocumentationAction))  
        { openOnlineDocumentation(); }
      else if (ae.getActionCommand().equals(onlineExamplesAction))  
        { openOnlineExamples(); }
      else if (ae.getActionCommand().equals(clipsExpertSystemGroupAction))  
        { openCLIPSExpertSystemGroup(); }
      else if (ae.getActionCommand().equals(sourceForgeForumsAction))  
        { openSourceForgeForums(); }
      else if (ae.getActionCommand().equals(stackOverflowQAAction))  
        { openStackOverflowQA(); }
      else if (ae.getActionCommand().equals(aboutCLIPSIDEAction))  
        { aboutCLIPSIDE(); }
     }
     
   /*********************/
   /* openCLIPSHomePage */
   /*********************/  
   public void openCLIPSHomePage()
     {
      try
        { openWebpage(new URL("http://www.clipsrules.net")); }
      catch (Exception e)
        { e.printStackTrace(); }
     }
   
   /***************************/
   /* openOnlineDocumentation */
   /***************************/  
   public void openOnlineDocumentation()
     {
      try
        { openWebpage(new URL("http://www.clipsrules.net/?q=Documentation")); }
      catch (Exception e)
        { e.printStackTrace(); }
     }
     
   /**********************/
   /* openOnlineExamples */
   /**********************/  
   public void openOnlineExamples()
     {
      try
        { openWebpage(new URL("https://sourceforge.net/p/clipsrules/code/HEAD/tree/branches/64x/examples/")); }
      catch (Exception e)
        { e.printStackTrace(); }
     }
     
   /******************************/
   /* openCLIPSExpertSystemGroup */
   /******************************/  
   public void openCLIPSExpertSystemGroup()
     {
      try
        { openWebpage(new URL("http://groups.google.com/group/CLIPSESG/")); }
      catch (Exception e)
        { e.printStackTrace(); }
     }

   /*************************/
   /* openSourceForgeForums */
   /*************************/  
   public void openSourceForgeForums()
     {
      try
        { openWebpage(new URL("http://sourceforge.net/p/clipsrules/discussion")); }
      catch (Exception e)
        { e.printStackTrace(); }
     }

   /***********************/
   /* openStackOverflowQA */
   /***********************/  
   public void openStackOverflowQA()
     {
      try
        { openWebpage(new URL("http://stackoverflow.com/questions/tagged/clips")); }
      catch (Exception e)
        { e.printStackTrace(); }
     }

   /***************/
   /* openWebpage */
   /***************/  
    public static void openWebpage(URL url) 
      {
       Desktop desktop = Desktop.isDesktopSupported() ? Desktop.getDesktop() : null;
       if (desktop != null && desktop.isSupported(Desktop.Action.BROWSE)) 
         {
          try
            {
             URI uri = url.toURI(); 
             desktop.browse(uri); 
            } 
          catch (Exception e)
            { e.printStackTrace(); }
         }
      }

   /*****************/
   /* aboutCLIPSIDE */
   /*****************/  
   public void aboutCLIPSIDE()
     {      
      JDialog f = new AboutCLIPSIDEDialog(parentFrame);
      f.setLocationRelativeTo(parentFrame);
      f.setVisible(true);
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
  }  
