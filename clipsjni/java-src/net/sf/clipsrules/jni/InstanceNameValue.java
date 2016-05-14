package net.sf.clipsrules.jni;

public class InstanceNameValue extends LexemeValue
  {
   /**********************/
   /* InstanceNameValue: */
   /**********************/
   public InstanceNameValue()
     {
      super(new String(""));
     }

   /**********************/
   /* InstanceNameValue: */
   /**********************/
   public InstanceNameValue(
     String value)
     {
      super(value);
     }
     
   /**********************/
   /* instanceNameValue: */
   /**********************/
   public String instanceNameValue()
     {
      return getValue();
     }
   
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {        
      return "[" + super.toString() + "]";
     }

   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.INSTANCE_NAME; }
     
   @Override
   public boolean isInstanceName()
     { return true; }

  }
