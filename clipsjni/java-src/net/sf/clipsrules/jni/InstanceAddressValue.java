package net.sf.clipsrules.jni;

public class InstanceAddressValue extends PrimitiveValue
  {
   private Environment owner;
   private Long value;
   
   /*************************/
   /* InstanceAddressValue: */
   /*************************/
   public InstanceAddressValue(
     long value,
     Environment env)
     {
      this.value = value;      
      owner = env;
     }

   /*************/
   /* getValue: */
   /*************/
   @Override
   public Long getValue()
     {
      return this.value;
     }

   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.INSTANCE_ADDRESS; }

   /*******************/
   /* getEnvironment: */
   /*******************/
   public Environment getEnvironment()
     { return owner; }
     
   /***********************/
   /* getInstanceAddress: */
   /***********************/     
   public long getInstanceAddress()
     { return getValue().longValue(); }

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
   
   @Override
   public boolean isInstanceAddress()
     { return true; }

  }
