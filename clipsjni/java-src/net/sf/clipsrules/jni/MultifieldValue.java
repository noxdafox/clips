package net.sf.clipsrules.jni;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class MultifieldValue extends PrimitiveValue
  {
   /********************/
   /* MultifieldValue: */
   /********************/
   public MultifieldValue()
     {
      super(new ArrayList());
     }

   /********************/
   /* MultifieldValue: */
   /********************/
   public MultifieldValue(
     List value)
     {
      super(value);
     }
     
   /********************/
   /* multifieldValue: */
   /********************/
   public List multifieldValue()
     {
      return (List) getValue();
     }

   /********/
   /* get: */
   /********/
   public PrimitiveValue get(
     int index)
     {
      List theList = (List) getValue();
      
      return (PrimitiveValue) theList.get(index);
     }
     
   /*********/
   /* size: */
   /*********/
   public int size()
     {
      final List theList = (List) getValue();
      
      return theList.size();
     }

   /***********/
   /* retain: */
   /***********/
   @Override
   public void retain()
     {
      final List theList = (List) getValue();
      
      for (Iterator itr = theList.iterator(); itr.hasNext(); ) 
        {
         PrimitiveValue pv = (PrimitiveValue) itr.next();
         pv.retain();
        }            
     }

   /************/
   /* release: */
   /************/
   @Override
   public void release()
     {
      final List theList = (List) getValue();
      
      for (Iterator itr = theList.iterator(); itr.hasNext(); ) 
        {
         PrimitiveValue pv = (PrimitiveValue) itr.next();
         pv.release();
        }            
     }
     
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {  
      final List theList = (List) getValue();
      boolean first = true;
      
      String theString = "(";
      
      for (Iterator itr = theList.iterator(); itr.hasNext(); ) 
        {
         if (! first)
          { theString = theString + " " + itr.next(); }
         else
          { 
           theString = theString + itr.next(); 
           first = false;
          }
        }      
        
      theString = theString + ")";
      
      return theString;
     }
  }
