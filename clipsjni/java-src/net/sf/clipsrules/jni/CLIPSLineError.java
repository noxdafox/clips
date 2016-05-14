package net.sf.clipsrules.jni;

public class CLIPSLineError
  {   
   private String fileName;
   private long lineNumber;
   private String message;
   
   public CLIPSLineError(
     String theFileName,
     long theLineNumber,
     String theMessage) 
     {  
      fileName = theFileName;
      lineNumber = theLineNumber;
      message = theMessage;
     }
     
   public String getFileName()
     { return fileName; }
 
   public long getLineNumber()
     { return lineNumber; }
    
   public String getMessage()
     { return message; }
  }