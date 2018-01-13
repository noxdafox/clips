package net.sf.clipsrules.jni.examples.ide;

import javax.swing.*;
import javax.swing.table.*;
import javax.swing.border.*; 
import java.awt.*;
import java.awt.event.*;
import java.io.File;

import net.sf.clipsrules.jni.*;

public class DialogFrame extends JInternalFrame
                      implements ActionListener, CommandExecutionListener
  {
   private JFrame ideFrame;

   private CommandPromptTextArea commandTextArea;
   
   private JLabel currentDirectoryLabel;
   
   private JToggleButton pauseButton = null;
      
   private Environment clips;
   
   static final String pauseAction = "Pause";

   /***************/
   /* DialogFrame */
   /***************/
   DialogFrame()
     {
      this(new Environment(),null);
     }

   /***************/
   /* DialogFrame */
   /***************/
   DialogFrame(     
     Environment theEnv)
     {
      this(theEnv,null);
     }

   /***************/
   /* DialogFrame */
   /***************/
   DialogFrame(
     File currentDirectory)
     {
      this(new Environment(),currentDirectory);
     }

   /***************/
   /* DialogFrame */
   /***************/
   DialogFrame(
     Environment theEnv,
     File currentDirectory)
     {  
      super("Dialog",true,false,true,true);
      
      /*===============================*/
      /* Create the clips environment. */
      /*===============================*/
      
      clips = theEnv;
     
      /*============================*/
      /* Set the current directory. */
      /*============================*/
      
      if (currentDirectory == null)
        {
         File workingDirectory = new File(System.getProperty("user.dir")); 
         currentDirectory = workingDirectory.getAbsoluteFile();
        }
        
      int dirChanged = clips.changeDirectory(currentDirectory.getAbsolutePath());

      /*===================================*/
      /* Create a new JFrame container and */
      /* assign a layout manager to it.    */
      /*===================================*/

      this.getContentPane().setLayout(new BoxLayout(this.getContentPane(),BoxLayout.Y_AXIS));
      
      /*=================================*/
      /* Give the frame an initial size. */
      /*=================================*/
     
      this.setSize(600,400);  
      this.setMinimumSize(new Dimension(550,250));
      
      /*===========================================*/
      /* The close button closes just the browser. */
      /*===========================================*/
     
      this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);  

      /*==========================*/
      /* Create the status panel. */
      /*==========================*/

      JPanel statusPanel = new JPanel(); 
      statusPanel.setPreferredSize(new Dimension(600,40));
      
      currentDirectoryLabel = new JLabel("Dir: ");
      if (dirChanged == 0)
        { currentDirectoryLabel.setText("Dir: " + currentDirectory.getAbsolutePath()); }
      else
        { currentDirectoryLabel.setText("Dir: "); }
 
      statusPanel.add(currentDirectoryLabel);

      pauseButton = new JToggleButton("Pause");
      pauseButton.setEnabled(false);
      pauseButton.setActionCommand(pauseAction);
      pauseButton.addActionListener(this);
      statusPanel.add(pauseButton);
      
      GroupLayout layout = new GroupLayout(statusPanel);
      statusPanel.setLayout(layout);
      layout.setAutoCreateGaps(true);
      layout.setAutoCreateContainerGaps(true);
      
      layout.setHorizontalGroup(layout.createSequentialGroup()
                                      .addComponent(currentDirectoryLabel)
                                      .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                                      .addComponent(pauseButton));
                                      
      layout.setVerticalGroup(layout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                                    .addComponent(currentDirectoryLabel)
                                    .addComponent(pauseButton));

      this.getContentPane().add(statusPanel); 
      
      /*=============================*/
      /* Create the text field area. */
      /*=============================*/

      try
        { 
         commandTextArea = new CommandPromptTextArea(clips); 
         commandTextArea.addCommandExecutionListener(this); 
        }
      catch (Exception e)
        { 
         e.printStackTrace();
         return;
        }       

      /*=======================================*/
      /* Put the text area into a scroll pane. */
      /*=======================================*/

      JScrollPane commandPane = new JScrollPane(commandTextArea);
      commandPane.setPreferredSize(new Dimension(600,360));
      commandPane.setViewportBorder(BorderFactory.createEmptyBorder(0,0,2,0));
      
      /*========================================*/
      /* Add the scroll pane to the main frame. */
      /*========================================*/
      
      this.getContentPane().add(commandPane); 

      /*===================================================*/
      /* Override copy/paste for the CommandPromptTextArea */
      /* so that we can define our own menu accelerators.  */
      /*===================================================*/

      KeyStroke cut = KeyStroke.getKeyStroke(KeyEvent.VK_X,KeyEvent.CTRL_MASK);
      KeyStroke copy = KeyStroke.getKeyStroke(KeyEvent.VK_C,KeyEvent.CTRL_MASK);
      KeyStroke paste = KeyStroke.getKeyStroke(KeyEvent.VK_V,KeyEvent.CTRL_MASK);
      InputMap map = commandTextArea.getInputMap();
      map.put(cut,"none");
      map.put(copy,"none");
      map.put(paste,"none");

      /*====================*/
      /* Display the frame. */
      /*====================*/

      this.pack();
     }  
 
   /*******************************/
   /* addCommandExecutionListener */
   /*******************************/  
    public void addCommandExecutionListener(
     CommandExecutionListener theListener)
     {
      commandTextArea.addCommandExecutionListener(theListener); 
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
      if (ae.getActionCommand().equals(pauseAction))  
        { pause(); }
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
     
   /*##################################*/
   /* CommandExecutionListener Methods */
   /*##################################*/
   
   /*********/
   /* pause */
   /*********/  
   public void pause()
     {
      if (commandTextArea.isPaused())
        { commandTextArea.setPaused(false); }
      else
        { commandTextArea.setPaused(true); }
     }

   /*********************/
   /* updatePauseButton */
   /*********************/  
   public void updatePauseButton(final boolean value)
     {
      if (EventQueue.isDispatchThread())
        { 
         pauseButton.setEnabled(value);
         return;
        }
      try
        {
         SwingUtilities.invokeAndWait(
           new Runnable() 
             {  
              public void run() 
                 { pauseButton.setEnabled(value);  }  
             });   
        }
      catch (Exception e) 
        { e.printStackTrace(); }
     }
     
   /*********************************/
   /* commandExecutionEventOccurred */
   /*********************************/  
   public void commandExecutionEventOccurred(
     CommandExecutionEvent theEvent)
     {    
      if (theEvent.getExecutionEvent().equals(CommandExecutionEvent.START_EVENT))
        { updatePauseButton(true); }
      else if (theEvent.getExecutionEvent().equals(CommandExecutionEvent.FINISH_EVENT))
        { updatePauseButton(false);  }
     }  

   /******************/
   /* getEnvironment */
   /******************/
   public Environment getEnvironment()
     {
      return clips;
     }

   /************************/
   /* hasCuttableSelection */
   /************************/
   public boolean hasCuttableSelection()
     {
      return commandTextArea.hasCuttableSelection();
     }

   /*************************/
   /* hasPasteableSelection */
   /*************************/
   public boolean hasPasteableSelection()
     {
      return commandTextArea.hasPasteableSelection();
     }

   /****************/
   /* hasSelection */
   /****************/
   public boolean hasSelection()
     {
      return commandTextArea.hasSelection();
     }

   /*******/
   /* cut */
   /*******/
   public void cut()
     {
      commandTextArea.cut(); 
     }

   /********/
   /* copy */
   /********/
   public void copy()
     {
      commandTextArea.copy(); 
     }

   /*********/
   /* paste */
   /*********/
   public void paste()
     {
      commandTextArea.paste(); 
     }

   /***************/
   /* isExecuting */
   /***************/
   public boolean isExecuting()
     {
      return commandTextArea.getExecuting();
     }
     
   /*************/
   /* haltRules */
   /*************/  
   public void haltRules()
     {
      clips.setHaltRules(true);
     }

   /*****************/
   /* haltExecution */
   /*****************/  
   public void haltExecution()
     {
      clips.setHaltExecution(true);
     }
     
   /******************/
   /* replaceCommand */
   /******************/  
   public void replaceCommand(
     String newCommand)
     {
      commandTextArea.replaceCommand(newCommand);
     }
     
   /*******************/
   /* clearScrollback */
   /*******************/  
   public void clearScrollback()
     {
      commandTextArea.clear();
      clips.printPrompt();
      clips.print(clips.getInputBuffer());
     }
     
   /****************/
   /* executeBatch */
   /****************/
   public void executeBatch()
     {
      commandTextArea.executeBatch();
     }
     
   /****************/
   /* setDirectory */
   /****************/  
   public boolean setDirectory(
     File newDirectory)
     {
      int dirChanged = clips.changeDirectory(newDirectory.getAbsolutePath());
      
      if (dirChanged == 0)
        { 
         currentDirectoryLabel.setText("Dir: " + newDirectory.getAbsolutePath()); 
         return true;
        }
        
      return false;
     }     
  }
