package net.sf.clipsrules.jni;

public class FloatValue extends NumberValue
  {
   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue()
     {
      super(new Double(0.0));
     }

   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue(
     double value)
     {
      super(new Double(value));
     }

   /***************/
   /* FloatValue: */
   /***************/
   public FloatValue(
     Double value)
     {
      super(value);
     }
  }
