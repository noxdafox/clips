package net.sf.clipsrules.jni;

import java.util.ArrayList;
import java.util.List;

public class FactInstance 
  {
   private Long typeAddress;
   private String name;
   private String relationName;
   private List<SlotValue> slotValues;
   
   /*****************/
   /* FactInstance: */
   /*****************/
   public FactInstance()
     {
      this(0,"","",null);
     }

   /*****************/
   /* FactInstance: */
   /*****************/
   public FactInstance(
     long theTypeAddress,
     String theName,
     String theRelationName,
     List<SlotValue> theSlotValues)
     {
      typeAddress = theTypeAddress;
      name = theName;
      relationName = theRelationName;
      slotValues = theSlotValues;
     }

   /******************/
   /* getTypeAddress */
   /******************/
   public long getTypeAddress()
     {
      return typeAddress;
     }

   /***********/
   /* getName */
   /***********/
   public String getName()
     {
      return name;
     }

   /*******************/
   /* getRelationName */
   /*******************/
   public String getRelationName()
     {
      return relationName;
     }
     
   /*****************/
   /* getSlotValues */
   /*****************/
   public List<SlotValue> getSlotValues()
     {
      return slotValues;
     }
  }
