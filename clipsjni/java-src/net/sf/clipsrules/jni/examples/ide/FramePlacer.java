package net.sf.clipsrules.jni.examples.ide;

import javax.swing.JInternalFrame;
import javax.swing.JDesktopPane;

public class FramePlacer 
  {  
   private int windowStartX = 28;
   private int windowStartY = 28;
   private int windowCurrentX = -1;
   private int windowCurrentY = -1;
   private int windowXIncrement = 28;
   private int windowYIncrement = 28;
   private JDesktopPane ideDesktopPane = null;
   
   /***************/
   /* FramePlacer */
   /***************/
   FramePlacer(
     JDesktopPane theDesktopPane)
     {  
      ideDesktopPane = theDesktopPane;
     }
     
   /**********************/
   /* placeInternalFrame */
   /**********************/  
   public void placeInternalFrame(
     JInternalFrame frame)
     {
      if (windowCurrentX == -1)
        {
         windowCurrentX = windowStartX;
         windowCurrentY = windowStartY;
         frame.setLocation(windowCurrentX,windowCurrentY);
         return;
        }
      
      windowCurrentX += windowXIncrement;
      windowCurrentY += windowYIncrement;

      if ((frame.getHeight() + windowCurrentY) > ideDesktopPane.getHeight())
        { windowCurrentY = windowStartY; }
      
      if ((frame.getWidth() + windowCurrentX) > ideDesktopPane.getWidth())
        { windowCurrentX = windowStartX; }

      frame.setLocation(windowCurrentX,windowCurrentY);
     }
  }  
