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
     
   /****************/
   /* stringValue: */
   /****************/
   public String stringValue()
     {
      return (String) getValue();
     }

   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {        
      return "\"" + super.toString() + "\"";
     }

  }
