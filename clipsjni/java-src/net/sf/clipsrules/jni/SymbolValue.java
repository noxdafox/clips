package net.sf.clipsrules.jni;

public class SymbolValue extends LexemeValue
  {
   /****************/
   /* SymbolValue: */
   /****************/
   public SymbolValue()
     {
      super(new String(""));
     }

   /****************/
   /* SymbolValue: */
   /****************/
   public SymbolValue(
     String value)
     {
      super(value);
     }

   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.SYMBOL; }
  
   @Override
   public boolean isSymbol()
     { return true; }

  }
