package net.sf.clipsrules.jni.examples.ide;

import javax.swing.DefaultDesktopManager;
import javax.swing.JInternalFrame;
import javax.swing.JComponent;
import java.awt.Dimension;
import java.awt.Container;
import javax.swing.JDesktopPane;

public class BoundsDesktopManager extends DefaultDesktopManager
   {
    @Override
    public void dragFrame(
      JComponent component, 
      int x, 
      int y)
      {
       if (component instanceof JInternalFrame)
         {
          JInternalFrame frame = (JInternalFrame) component;
          JDesktopPane desktop = frame.getDesktopPane();
          Dimension d = desktop.getSize();

          if (y < 0)
            { y = 0; }
         }

       super.dragFrame(component,x,y);
      }
  }