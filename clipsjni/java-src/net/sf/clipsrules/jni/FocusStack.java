package net.sf.clipsrules.jni;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class FocusStack implements Iterable<Focus>
  {
   private List<Focus> stack;
   
   /***************/
   /* FocusStack: */
   /***************/
   public FocusStack()
     {
      this.stack = new ArrayList<Focus>();
     }

   /***************/
   /* FocusStack: */
   /***************/
   public FocusStack(
     List<Focus> stack)
     {
      this.stack = stack;
     }

   /*************/
   /* getStack: */
   /*************/
   public List<Focus> getStack()
     {
      return this.stack;
     }

   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      if (stack == null) return 0;
      return stack.hashCode();
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
	   
	   FocusStack cfs = (FocusStack) obj;
	   if (this.stack == null) return (cfs.stack == null);
	   return this.stack.equals(cfs.stack);
      }
      
   /********/
   /* get: */
   /********/
   public Focus get(
     int index)
     {
      List<Focus> theStack = getStack();
      
      return theStack.get(index);
     }
     
   /*********/
   /* size: */
   /*********/
   public int size()
     {
      final List<Focus> theStack = getStack();
      
      return theStack.size();
     }

   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {  
      final List<Focus> theStack = getStack();
      boolean first = true;
      
      String theString = "(";
      
      for (Focus theFocus : theStack)
        {
         if (! first)
           { theString = theString + " " + theFocus; }
         else
           { 
            theString = theString + theFocus; 
            first = false;
           }
        }
                
      theString = theString + ")";
      
      return theString;
     }
     
   /************/
   /* iterator */
   /************/ 
   public Iterator<Focus> iterator() 
     {
      return getStack().iterator();
     }
  }