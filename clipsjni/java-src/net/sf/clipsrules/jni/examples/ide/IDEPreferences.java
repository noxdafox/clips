package net.sf.clipsrules.jni.examples.ide;

import java.io.File;
import java.awt.Font;
import java.util.prefs.Preferences;

public class IDEPreferences 
  {  
   private Preferences prefs = null;
   private File currentDirectory = null;
   private static final String CURRENT_DIRECTORY = "currentDirectory";

   private Font dialogFont = null;
   private static final String DIALOG_FONT_NAME = "dialogFontName";
   private static final String DIALOG_FONT_SIZE = "dialogFontSize";
   private static final String DIALOG_FONT_STYLE = "dialogFontStyle";

   private Font editorFont = null;
   private static final String EDITOR_FONT_NAME = "editorFontName";
   private static final String EDITOR_FONT_SIZE = "editorFontSize";
   private static final String EDITOR_FONT_STYLE = "editorFontStyle";

   private Font browserFont = null;
   private static final String BROWSER_FONT_NAME = "browserFontName";
   private static final String BROWSER_FONT_SIZE = "browserFontSize";
   private static final String BROWSER_FONT_STYLE = "browserFontStyle";
  
   /******************/
   /* IDEPreferences */
   /******************/
   IDEPreferences()
     {  
      String directoryPref;
      String fontName;
      int fontSize, fontStyle;
      
      /************************/
      /* Get the preferences. */
      /************************/
      
      prefs = Preferences.userNodeForPackage(net.sf.clipsrules.jni.examples.ide.CLIPSIDE.class);       

      /*==================================*/
      /* Determine the working directory. */
      /*==================================*/

      directoryPref = prefs.get(CURRENT_DIRECTORY,null);         
      
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
      
      /*============================*/
      /* Determine the dialog font. */
      /*============================*/
      
      fontName = prefs.get(DIALOG_FONT_NAME,null);   
      if (fontName == null)
        { dialogFont = new Font("monospaced",Font.PLAIN,12); }
      else
        {
         fontSize = prefs.getInt(DIALOG_FONT_SIZE,12);   
         fontStyle = prefs.getInt(DIALOG_FONT_STYLE,Font.PLAIN);   
         dialogFont = new Font(fontName,fontStyle,fontSize);
        }

      /*============================*/
      /* Determine the editor font. */
      /*============================*/
      
      fontName = prefs.get(EDITOR_FONT_NAME,null);   
      if (fontName == null)
        { editorFont = new Font("monospaced",Font.PLAIN,12); }
      else
        {
         fontSize = prefs.getInt(EDITOR_FONT_SIZE,12);   
         fontStyle = prefs.getInt(EDITOR_FONT_STYLE,Font.PLAIN);   
         editorFont = new Font(fontName,fontStyle,fontSize);
        }

      /*=============================*/
      /* Determine the browser font. */
      /*=============================*/
      
      fontName = prefs.get(BROWSER_FONT_NAME,null);   
      if (fontName == null)
        { browserFont = new Font("dialog",Font.PLAIN,12); }
      else
        {
         fontSize = prefs.getInt(DIALOG_FONT_SIZE,12);   
         fontStyle = prefs.getInt(DIALOG_FONT_STYLE,Font.PLAIN);   
         browserFont = new Font(fontName,fontStyle,fontSize);
        }
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

   /*****************/
   /* getDialogFont */
   /*****************/
   public Font getDialogFont()
     {
      return dialogFont;
     }
     
   /*****************/
   /* setDialogFont */
   /*****************/
   public void setDialogFont(
     Font theFont)
     {
      dialogFont = theFont;
     }
     
   /******************/
   /* saveDialogFont */
   /******************/
   public void saveDialogFont(
     Font theFont)
     {
      prefs.put(DIALOG_FONT_NAME,theFont.getFamily());
      prefs.putInt(DIALOG_FONT_SIZE,theFont.getSize());
      prefs.putInt(DIALOG_FONT_STYLE,theFont.getStyle());
     }

   /*****************/
   /* getEditorFont */
   /*****************/
   public Font getEditorFont()
     {
      return editorFont;
     }
     
   /*****************/
   /* setEditorFont */
   /*****************/
   public void setEditorFont(
     Font theFont)
     {
      editorFont = theFont;
     }
     
   /******************/
   /* saveEditorFont */
   /******************/
   public void saveEditorFont(
     Font theFont)
     {
      prefs.put(EDITOR_FONT_NAME,theFont.getFamily());
      prefs.putInt(EDITOR_FONT_SIZE,theFont.getSize());
      prefs.putInt(EDITOR_FONT_STYLE,theFont.getStyle());
     }
     
   /******************/
   /* getBrowserFont */
   /******************/
   public Font getBrowserFont()
     {
      return browserFont;
     }
     
   /******************/
   /* setBrowserFont */
   /******************/
   public void setBrowserFont(
     Font theFont)
     {
      browserFont = theFont;
     }
     
   /*******************/
   /* saveBrowserFont */
   /*******************/
   public void saveBrowserFont(
     Font theFont)
     {
      prefs.put(BROWSER_FONT_NAME,theFont.getFamily());
      prefs.putInt(BROWSER_FONT_SIZE,theFont.getSize());
      prefs.putInt(BROWSER_FONT_STYLE,theFont.getStyle());
     }
  }
   