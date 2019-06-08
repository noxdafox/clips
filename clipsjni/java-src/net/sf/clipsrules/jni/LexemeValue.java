package net.sf.clipsrules.jni;

public abstract class LexemeValue extends PrimitiveValue 
  {
   protected LexemeValue(String value) 
     {
      super(value);
	 }
	
   public String lexemeValue()
     {
      return (String) getValue();
     }
  }
