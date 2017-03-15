package net.sf.clipsrules.jni.examples.ide;

import java.awt.event.ActionListener;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import java.awt.event.ActionEvent;
import java.net.URI;
import java.net.URL;
import java.awt.Desktop;

public class HelpMenu extends JMenu 
                   implements ActionListener
  {  
   private static final String clipsHomePageAction = "CLIPSHomePage";
   private static final String onlineDocumentationAction = "OnlineDocumentation";
   private static final String onlineExamplesAction = "OnlineExamples";
   private static final String clipsExpertSystemGroupAction = "CLIPSExpertSystemGroup";
   private static final String sourceForgeForumsAction = "SourceForgeForums";
   private static final String stackOverflowQAAction = "StackOverflowQ&A";

   private JMenuItem jmiCLIPSHomePage = null;
   private JMenuItem jmiOnlineDocumentation = null;   
   private JMenuItem jmiOnlineExamples = null;
   private JMenuItem jmiCLIPSExpertSystemGroup = null;
   private JMenuItem jmiSourceForgeForums = null;
   private JMenuItem jmiStackOverflowQA = null;

   /************/
   /* HelpMenu */
   /************/
   HelpMenu()
     {  
      super("Help");
        
      //addMenuListener(this);
      
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
