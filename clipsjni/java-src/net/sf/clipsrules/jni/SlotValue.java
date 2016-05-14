package net.sf.clipsrules.jni;

public class SlotValue
  {
   private String slotName;
   private String slotValue;
   private boolean isDefault;
   
   /**************/
   /* SlotValue: */
   /**************/
   public SlotValue()
     {
      this("","",true);
     }

   /**************/
   /* SlotValue: */
   /**************/
   public SlotValue(
     String theSlotName,
     String theSlotValue,
     boolean isDefaultValue)
     {
      slotName = theSlotName;
      slotValue = theSlotValue;
      isDefault = isDefaultValue;
     }

   /***************/
   /* getSlotName */
   /***************/
   public String getSlotName()
     {
      return slotName;
     }

   /****************/
   /* getSlotValue */
   /****************/
   public String getSlotValue()
     {
      return slotValue;
     }
     
   /*************/
   /* isDefault */
   /*************/
   public boolean isDefault()
     {
      return isDefault;
     }
  }
