package net.sf.clipsrules.jni;

import java.util.List;

public abstract class PrimitiveValue
  {
   private Object value;
   
   /*******************/
   /* PrimitiveValue: */
   /*******************/
   protected PrimitiveValue(
     Object value)
     {
      this.value = value;
     }

   /*************/
   /* getValue: */
   /*************/
   public Object getValue()
     {
      return this.value;
     }

   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {
      if (this.value != null)
        { return this.value.toString(); }
        
      return "";
     }
     
   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      final int prime = 31;
      int result = 17;
      result = prime * result + ((this.value == null) ? 
                                 0 : this.value.hashCode());
      return result;
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
	   PrimitiveValue pv = (PrimitiveValue) obj;
	   if (this.value == null) return (pv.value == null);
	   return this.value.equals(pv.value);
      }
      
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
