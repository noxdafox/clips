package net.sf.clipsrules.jni.examples.router;

import javax.swing.*; 
import javax.swing.border.*; 
import javax.swing.table.*;
import java.awt.*; 
import java.awt.event.*; 
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.lang.Byte;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.MissingResourceException;

import net.sf.clipsrules.jni.*;

class RouterDemo implements ActionListener
  {  
   class CLIPSProgramRunner extends SwingWorker<Integer,Integer>
     {
      Environment workEnv;
      String loadFiles[];
      JButton restartButton;
      static final String resourcePath = "/net/sf/clipsrules/jni/examples/router/resources/";
      
      public CLIPSProgramRunner(
        Environment theEnv,
        String theLoadFiles[],
        JButton theRestartButton)
        {
         this.workEnv = theEnv;
         this.loadFiles = theLoadFiles;
         this.restartButton = theRestartButton;
        }
      
      @Override
      protected Integer doInBackground() throws Exception
        { 
         workEnv.clear();
         if (loadFiles != null)
           {
            try
              {
               for (String fileName : loadFiles)
                { workEnv.loadFromResource(resourcePath + fileName); }
              }
            catch (CLIPSLoadException e)
              {
               e.printStackTrace();
               return 0;
              }
           }
          
         workEnv.reset();
         workEnv.run();
         return 1;
        }

      @Override
      protected void done()
        {
         try
           { restartButton.setEnabled(true); }
         catch (Exception e)
           {
            e.printStackTrace(); 
           }
        }
     }

   JFrame routerDemoFrame;
   RouterTextArea autoTextArea;
   RouterTextArea animalTextArea;
   Environment autoEnv;
   Environment animalEnv;
   JButton restartAutoButton;
   JButton restartAnimalButton;
   ResourceBundle routerResources;

   /**************/
   /* RouterDemo */
   /**************/
   RouterDemo()
     {  
      try
        {
         routerResources = ResourceBundle.getBundle("net.sf.clipsrules.jni.examples.router.resources.RouterResources",Locale.getDefault());
        }
      catch (MissingResourceException mre)
        {
         mre.printStackTrace();
         return;
        }

      /*==========================*/
      /* Create the environments. */
      /*==========================*/
      
      autoEnv = new Environment();
      animalEnv = new Environment();

      /*===================================*/
      /* Create a new JFrame container and */
      /* assign a layout manager to it.    */
      /*===================================*/
     
      routerDemoFrame = new JFrame(routerResources.getString("RouterDemo"));          
      routerDemoFrame.getContentPane().setLayout(new BoxLayout(routerDemoFrame.getContentPane(),
                                                               BoxLayout.Y_AXIS));
    
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      routerDemoFrame.setSize(800,550);  
      routerDemoFrame.setMinimumSize(new Dimension(400,250));
        
      /*=============================================================*/
      /* Terminate the program when the user closes the application. */
      /*=============================================================*/
     
      routerDemoFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);  
 
      /*=============================*/
      /* Create the text field area. */
      /*=============================*/
 
      try
        { autoTextArea = new RouterTextArea(autoEnv); }
      catch (Exception e)
        { 
         e.printStackTrace();
         return;
        }       

      try
        { animalTextArea = new RouterTextArea(animalEnv); }
      catch (Exception e)
        { 
         e.printStackTrace();
         return;
        }       
      
      /*=======================================*/
      /* Put the text areas into scroll panes. */
      /*=======================================*/

      JScrollPane autoPane = new JScrollPane(autoTextArea);
      autoPane.setPreferredSize(new Dimension(800,250));
      JScrollPane animalPane = new JScrollPane(animalTextArea);
      animalPane.setPreferredSize(new Dimension(800,250));

      /*==================================*/
      /* Create the restart button panel. */
      /*==================================*/
      
      JPanel restartPanel = new JPanel(); 
      restartPanel.setPreferredSize(new Dimension(800,50));
      
      //restartAutoButton = new JButton("Restart Auto");
      restartAutoButton = new JButton(routerResources.getString("RestartAuto"));
      restartAutoButton.setEnabled(false);
      restartAutoButton.setActionCommand("RestartAuto");
      restartAutoButton.addActionListener(this);
      
      //restartAnimalButton = new JButton("Restart Animal");
      restartAnimalButton = new JButton(routerResources.getString("RestartAnimal"));
      restartAnimalButton.setEnabled(false);
      restartAnimalButton.setActionCommand("RestartAnimal");
      restartAnimalButton.addActionListener(this);

      restartPanel.add(restartAutoButton);
      restartPanel.add(restartAnimalButton);

      GroupLayout layout = new GroupLayout(restartPanel);
      restartPanel.setLayout(layout);
      layout.setAutoCreateGaps(true);
      layout.setAutoCreateContainerGaps(true);
      layout.setHorizontalGroup(layout.createSequentialGroup()
                                      .addComponent(restartAutoButton)
                                      .addComponent(restartAnimalButton));
      layout.setVerticalGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(restartAutoButton)
                                    .addComponent(restartAnimalButton));
      
      /*========================================*/
      /* Add the scroll pane to the main frame. */
      /*========================================*/
      
      routerDemoFrame.getContentPane().add(restartPanel); 
      routerDemoFrame.getContentPane().add(autoPane); 
      routerDemoFrame.getContentPane().add(animalPane); 
      
      /*====================*/
      /* Display the frame. */
      /*====================*/

      routerDemoFrame.pack();
      routerDemoFrame.setVisible(true);  

      /*===================*/
      /* Run the programs. */
      /*===================*/

      runAuto();
      runAnimal();
     }  

   public void runAuto()
     {
      String autoFileNames [] 
         = { "auto.clp",
             "auto_" + Locale.getDefault().getLanguage() + ".clp"};
      new CLIPSProgramRunner(autoEnv,autoFileNames,restartAutoButton).execute();
     }
 
   public void runAnimal()
     {
      String animalFileNames [] 
         = { "bcengine.clp",
             "animal.clp",
             "animal_" + Locale.getDefault().getLanguage() + ".clp"};
      new CLIPSProgramRunner(animalEnv,animalFileNames,restartAnimalButton).execute();
     }
            
   /********/
   /* main */
   /********/  
   public static void main(String args[])
     {  
      /*===================================================*/
      /* Create the frame on the event dispatching thread. */
      /*===================================================*/
      
      SwingUtilities.invokeLater(
        new Runnable() 
          {  
           public void run() { new RouterDemo(); }  
          });   
     }  
 
   /*********************/
   /* onActionPerformed */
   /*********************/  
   private void onActionPerformed(
     ActionEvent ae) throws Exception
     { 
      if (ae.getActionCommand().equals("RestartAuto"))
        { 
         autoTextArea.setText("");
         restartAutoButton.setEnabled(false);
         autoTextArea.grabFocus();
         runAuto();
        }
      else if (ae.getActionCommand().equals("RestartAnimal"))
        { 
         animalTextArea.setText("");
         restartAnimalButton.setEnabled(false);
         animalTextArea.grabFocus();
         runAnimal();
        }
     }
    
   /*########################*/
   /* ActionListener Methods */
   /*########################*/

   /*******************/
   /* actionPerformed */
   /*******************/  
   @Override
   public void actionPerformed(
     ActionEvent ae)
     { 
      try
        { onActionPerformed(ae); }
      catch (Exception e)
        { e.printStackTrace(); }
     }

  }