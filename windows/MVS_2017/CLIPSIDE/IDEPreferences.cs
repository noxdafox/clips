using System;

namespace CLIPSIDE
  {
   class IDEPreferences
     {
      private String currentDirectory;

      public IDEPreferences() 
        {
         currentDirectory = Properties.Settings.Default.currentDirectory;
        }

      public String GetCurrentDirectory()
        {
         return currentDirectory;
        }

      public void SetCurrentDirectory(
        String theDirectory)
        {
         currentDirectory = theDirectory;
        }

      public void SaveCurrentDirectory(
        String theDirectory)
        {
         currentDirectory = theDirectory;
         Properties.Settings.Default.currentDirectory = theDirectory;
         Properties.Settings.Default.Save();
        }
     }
  }
