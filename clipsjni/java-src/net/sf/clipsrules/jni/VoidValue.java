package net.sf.clipsrules.jni;

public class VoidValue extends PrimitiveValue
  {
   /***************/
   /* VoidValue: */
   /***************/
   public VoidValue()
     {
     }
  
   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.VOID; }

   @Override
   public boolean isVoid()
     { return true; }
  }
