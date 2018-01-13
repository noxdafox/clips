package net.sf.clipsrules.jni;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MultifieldValue extends PrimitiveValue implements Iterable<PrimitiveValue>
  {
   private List<PrimitiveValue> value;
   
   /********************/
   /* MultifieldValue: */
   /********************/
   public MultifieldValue()
     {
      this.value = new ArrayList<PrimitiveValue>();
     }

   /********************/
   /* MultifieldValue: */
   /********************/
   public MultifieldValue(
     List<PrimitiveValue> value)
     {
      this.value = value;
     }

   /*************/
   /* getValue: */
   /*************/
   @Override
   public List<PrimitiveValue> getValue()
     {
      return this.value;
     }

   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.MULTIFIELD; }
     
   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      if (value == null) return 0;
      return value.hashCode();
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
	   
	   MultifieldValue mv = (MultifieldValue) obj;
	   if (this.value == null) return (mv.value == null);
	   return this.value.equals(mv.value);
      }

   /********/
   /* get: */
   /********/
   public PrimitiveValue get(
     int index)
     {
      List<PrimitiveValue> theList = getValue();
      
      return theList.get(index);
     }
     
   /*********/
   /* size: */
   /*********/
   public int size()
     {
      final List<PrimitiveValue> theList = getValue();
      
      return theList.size();
     }
     
   /***********/
   /* retain: */
   /***********/
   @Override
   public void retain()
     {
      final List<PrimitiveValue> theList = getValue();
      
      for (PrimitiveValue theValue : theList)
        { theValue.retain(); }
     }

   /************/
   /* release: */
   /************/
   @Override
   public void release()
     {
      final List<PrimitiveValue> theList = getValue();
      
      for (PrimitiveValue theValue : theList)
        { theValue.release(); }
     }
     
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {  
      final List<PrimitiveValue> theList = getValue();
      boolean first = true;
      
      String theString = "(";
      
      for (PrimitiveValue theValue : theList)
        {
         if (! first)
          { theString = theString + " " + theValue; }
         else
          { 
           theString = theString + theValue; 
           first = false;
          }
        }      
                
      theString = theString + ")";
      
      return theString;
     }

   /************/
   /* iterator */
   /************/ 
   public Iterator<PrimitiveValue> iterator() 
     {
      return getValue().iterator();
     }

   @Override
   public boolean isMultifield()
     { return true; }

  }
