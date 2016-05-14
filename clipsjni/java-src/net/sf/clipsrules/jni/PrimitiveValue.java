package net.sf.clipsrules.jni;

import java.util.List;

public abstract class PrimitiveValue
  { 
   /*******************/
   /* PrimitiveValue: */
   /*******************/
   protected PrimitiveValue()
     {
     }

   /*************/
   /* getValue: */
   /*************/
   public Object getValue()
     {
      return null;
     }

   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {
      return "";
     }
     
   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      return 0;
     }
     
   /***********/
   /* equals: */
   /***********/
   @Override
   public boolean equals(Object obj) 
	  {
	   if (this == obj) return true;
	   if (obj == null) return false;
	   if (this.getClass() != obj.getClass()) return false;
	   return true;
      }
   
   public CLIPSType getCLIPSType()
     { return CLIPSType.VOID; }

   public int getCLIPSTypeValue()
     { return getCLIPSType().getType(); }
     
   public boolean isVoid()
     { return false; }
   
   public boolean isLexeme()
     { return false; }
   
   public boolean isSymbol()
     { return false; }
   
   public boolean isString()
     { return false; }
   
   public boolean isInstanceName()
     { return false; }
   
   public boolean isNumber()
     { return false; }
   
   public boolean isFloat()
     { return false; }
   
   public boolean isInteger()
     { return false; }
   
   public boolean isFactAddress()
     { return false; }

   public boolean isInstance()
     { return false; }
	     
   public boolean isInstanceAddress()
     { return false; }
   
   public boolean isMultifield()
     { return false; }
         
   public boolean isExternalAddress()
     { return false; }
   
   /************/
   /* retain: */
   /************/
   public void retain()
     {
     }

   /************/
   /* release: */
   /************/
   public void release()
     {
     }
  }
