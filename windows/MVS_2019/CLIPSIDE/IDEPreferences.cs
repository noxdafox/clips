using System;

namespace CLIPSIDE
  {
   public class IDEPreferences
     {
      private String currentDirectory;
      private String dialogFont;
      private String browserFont;
      public IDEPreferences() 
        {
         currentDirectory = Properties.Settings.Default.currentDirectory;
         dialogFont = Properties.Settings.Default.dialogFont;
         browserFont = Properties.Settings.Default.browserFont;
        }

      public String GetCurrentDirectory()
        {
         return currentDirectory;
        }

      public String GetDialogFont()
        {
         return dialogFont;
        }
      public String GetBrowserFont()
        {
         return browserFont;
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

      public void SaveDialogFont(
         String fontString)
        {
         dialogFont = fontString;
         Properties.Settings.Default.dialogFont = fontString;
         Properties.Settings.Default.Save();
        }
      public void SaveBrowserFont(
        String fontString)
        {
         browserFont = fontString;
         Properties.Settings.Default.browserFont = fontString;
         Properties.Settings.Default.Save();
        }
     }
  }
