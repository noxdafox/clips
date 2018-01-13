package net.sf.clipsrules.jni;

public class CaptureRouter extends BaseRouter
  {   
   private String captureString = "";
   private boolean forwardOutput = false;

   /*****************/
   /* CaptureRouter */
   /*****************/
   public CaptureRouter(
     Environment theEnv,
     String [] theCaptureNames) 
     {  
      super(theEnv,theCaptureNames,30);
     }

   /*****************/
   /* CaptureRouter */
   /*****************/
   public CaptureRouter(
     Environment theEnv,
     String [] theCaptureNames,
     boolean shouldForwardOutput) 
     {  
      super(theEnv,theCaptureNames,30);
      forwardOutput = shouldForwardOutput;
     }
     
   /*********/
   /* clear */
   /*********/
   public void clear()
     {
      captureString = "";
     }

   /*************/
   /* getOutput */
   /*************/
   public String getOutput()
     {
      return captureString;
     }
    
   /**********/
   /* write: */
   /**********/
   @Override
   public void write(
     String logName,
     String printString)
     {
      captureString = captureString.concat(printString);
      if (forwardOutput)
        { clips.callNextPrintRouter(this,logName,printString); }
     }
  }