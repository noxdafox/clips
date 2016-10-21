package net.sf.clipsrules.jni;

public class ExternalAddressValue extends PrimitiveValue
  {
   private Environment owner;
   private long value;

   /*************************/
   /* ExternalAddressValue: */
   /*************************/
   public ExternalAddressValue(
     Object externalAddress,
     Environment env)
     {
      this.value = 0; // value;
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
     { return CLIPSType.EXTERNAL_ADDRESS; }

   /*******************/
   /* getEnvironment: */
   /*******************/
   public Environment getEnvironment()
     { return owner; }
     
   /***********************/
   /* getExternalAddress: */
   /***********************/     
   public long getExternalAddress()
     { return getValue().longValue(); }

   /***********/
   /* retain: */
   /***********/
   @Override
   public void retain()
     {
      owner.incrementAddressCount(this);
     }

   /************/
   /* release: */
   /************/
   @Override
   public void release()
     {
      owner.decrementAddressCount(this);
     }
     
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {        
      return "<ExternalAddress-" + getValue() + ">";
     }

   @Override
   public boolean isExternalAddress()
     { return true; }
  }
