package net.sf.clipsrules.jni;

public class InstanceNameValue extends InstanceValue
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
      return (String) getValue();
     }
   
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {        
      return "[" + super.toString() + "]";
     }
  }
