package net.sf.clipsrules.jni.examples.ide;

import java.io.File;
import java.util.prefs.Preferences;

public class IDEPreferences 
  {  
   private Preferences prefs = null;
   private File currentDirectory = null;
   private static final String CURRENT_DIRECTORY = "currentDirectory";
  
   /******************/
   /* IDEPreferences */
   /******************/
   IDEPreferences()
     {  
      /************************/
      /* Get the preferences. */
      /************************/
      
      prefs = Preferences.userNodeForPackage(net.sf.clipsrules.jni.examples.ide.CLIPSIDE.class);       
      String directoryPref = prefs.get(CURRENT_DIRECTORY,null);         

      /*==================================*/
      /* Determine the working directory. */
      /*==================================*/
      
      File workingDirectory;
      if (directoryPref == null)
        { workingDirectory = new File(System.getProperty("user.dir")); }
      else
        { 
         workingDirectory = new File(directoryPref); 
         if (! workingDirectory.exists())
           { workingDirectory = new File(System.getProperty("user.dir")); }
        }
      
      currentDirectory = workingDirectory.getAbsoluteFile();
     }  
          
   /***********************/
   /* getCurrentDirectory */
   /***********************/
   public File getCurrentDirectory()
     {
      return currentDirectory;
     }
     
   /***********************/
   /* setCurrentDirectory */
   /***********************/
   public void setCurrentDirectory(
     File theFile)
     {
      currentDirectory = theFile;
     }
     
   /************************/
   /* saveCurrentDirectory */
   /************************/
   public void saveCurrentDirectory(
     File theFile)
     {
      prefs.put(CURRENT_DIRECTORY,theFile.getAbsolutePath());
     }
  }
   