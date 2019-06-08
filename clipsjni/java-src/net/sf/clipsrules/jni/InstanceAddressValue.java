package net.sf.clipsrules.jni;

public class InstanceAddressValue extends InstanceValue
  {
   private Environment owner;

   /*************************/
   /* InstanceAddressValue: */
   /*************************/
   public InstanceAddressValue(
     long value,
     Environment env)
     {
      super(new Long(value));
      
      owner = env;
     }

   /*******************/
   /* getEnvironment: */
   /*******************/
   public Environment getEnvironment()
     { return owner; }
     
   /***********************/
   /* getInstanceAddress: */
   /***********************/     
   public long getInstanceAddress()
     { return ((Long) getValue()).longValue(); }

   /******************/
   /* directGetSlot: */
   /******************/     
   public PrimitiveValue directGetSlot(
     String slotName)
     { return Environment.directGetSlot(this,slotName); }

   /********************/
   /* getInstanceName: */
   /********************/     
   public String getInstanceName()
     { return Environment.getInstanceName(this); }
     
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {        
      return "<Instance-" + getInstanceName() + ">";
     }

   /***********/
   /* retain: */
   /***********/
   @Override
   public void retain()
     {
      owner.incrementInstanceCount(this);
     }

   /************/
   /* release: */
   /************/
   @Override
   public void release()
     {
      owner.decrementInstanceCount(this);
     }
  }
