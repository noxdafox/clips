package net.sf.clipsrules.jni;

public class IntegerValue extends NumberValue
  {
   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue()
     {
      super(new Long(0));
     }

   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue(
     long value)
     {
      super(new Long(value));
     }
     
   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue(
     Long value)
     {
      super(value);
     }
  }
