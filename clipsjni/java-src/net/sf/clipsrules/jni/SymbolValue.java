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

   /****************/
   /* symbolValue: */
   /****************/
   public String symbolValue() throws Exception
     {
      return (String) getValue();
     }
  }
