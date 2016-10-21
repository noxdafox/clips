package net.sf.clipsrules.jni;

public enum CLIPSType
  {
   FLOAT(0),
   INTEGER(1),
   SYMBOL(2),
   STRING(3),
   MULTIFIELD(4),
   EXTERNAL_ADDRESS(5),
   FACT_ADDRESS(6),
   INSTANCE_ADDRESS(7),
   INSTANCE_NAME(8),
   VOID(175);
   
   private int type;
   
   private CLIPSType(int type)
     { this.type = type; }
     
   public int getType()
     { return type; }
  }

