package net.sf.clipsrules.jni;

public class StringValue extends LexemeValue
  {
   /****************/
   /* StringValue: */
   /****************/
   public StringValue()
     {
      super(new String(""));
     }

   /****************/
   /* StringValue: */
   /****************/
   public StringValue(
     String value)
     {
      super(value);
     }

   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {        
      return "\"" + super.toString() + "\"";
     }

   @Override
   public CLIPSType getCLIPSType()
     { return CLIPSType.STRING; }
    
   @Override
   public boolean isString()
     { return true; }

  }
