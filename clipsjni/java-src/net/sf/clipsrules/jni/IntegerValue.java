package net.sf.clipsrules.jni;

public class IntegerValue extends NumberValue
  {
   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue()
     {
      super(Long.valueOf(0));
     }

   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue(
     long value)
     {
      super(Long.valueOf(value));
     }
     
   /*****************/
   /* IntegerValue: */
   /*****************/
   public IntegerValue(
     Long value)
     {
      super(value);
     }

   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.INTEGER; }
     
   @Override
   public boolean isInteger()
     { return true; }

  }
