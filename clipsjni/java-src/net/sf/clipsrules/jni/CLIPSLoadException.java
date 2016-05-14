package net.sf.clipsrules.jni;

import java.util.List;
import java.util.ArrayList;

public class CLIPSLoadException extends CLIPSException 
  {
   private List<CLIPSLineError> errorList;
   
   public CLIPSLoadException() 
     { 
      super(); 
      errorList = new ArrayList<CLIPSLineError>();
     }
   
   public CLIPSLoadException(String message) 
     { 
      super(message); 
      errorList = new ArrayList<CLIPSLineError>();
     }
   
   public CLIPSLoadException(String message, Throwable cause) 
     { 
      super(message, cause); 
      errorList = new ArrayList<CLIPSLineError>();
     }
     
   public CLIPSLoadException(Throwable cause) 
     { 
      super(cause); 
      errorList = new ArrayList<CLIPSLineError>();
     }

   public CLIPSLoadException(
     List<CLIPSLineError> list) 
     { 
      super(); 
      errorList = new ArrayList<CLIPSLineError>(list);
     }
   
   public CLIPSLoadException(String message,
     List<CLIPSLineError> list) 
     { 
      super(message); 
      errorList = new ArrayList<CLIPSLineError>(list);
     }
   
   public CLIPSLoadException(String message, Throwable cause,
     List<CLIPSLineError> list) 
     { 
      super(message, cause); 
      errorList = new ArrayList<CLIPSLineError>(list);
     }
     
   public CLIPSLoadException(Throwable cause,
     List<CLIPSLineError> list) 
     { 
      super(cause); 
      errorList = new ArrayList<CLIPSLineError>(list);
     }
     
   public List<CLIPSLineError> getErrorList()
     {
      return errorList;
     }
  }
  