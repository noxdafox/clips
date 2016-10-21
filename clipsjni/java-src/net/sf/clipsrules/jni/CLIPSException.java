package net.sf.clipsrules.jni;

public class CLIPSException extends Exception 
  {
   public CLIPSException() 
     { super(); }
   
   public CLIPSException(String message) 
     { super(message); }
   
   public CLIPSException(String message, Throwable cause) 
     { super(message, cause); }
     
   public CLIPSException(Throwable cause) 
     { super(cause); }
  }