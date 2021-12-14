package net.sf.clipsrules.jni;

public class FloatValue extends NumberValue
  {
   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue()
     {
      super(Double.valueOf(0.0));
     }

   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue(
     double value)
     {
      super(Double.valueOf(value));
     }

   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue(
     Double value)
     {
      super(value);
     }

   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.FLOAT; }
     
   @Override
   public boolean isFloat()
     { return true; }
  }
