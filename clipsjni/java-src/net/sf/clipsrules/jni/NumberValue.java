package net.sf.clipsrules.jni;

public abstract class NumberValue extends PrimitiveValue 
  {
   /****************/
   /* NumberValue: */
   /****************/

  protected NumberValue(Number value) 
     {
      super(value);
	 }

   /****************/
   /* NumberValue: */
   /****************/
	
   public Number numberValue()
     {
      return (Number) getValue();
     } 
     
   /*************/
   /* intValue: */
   /*************/
   public int intValue()
     {
      return numberValue().intValue();
     }

   /**************/
   /* longValue: */
   /**************/
   public long longValue()
     {
      return numberValue().longValue();
     }
     
   /***************/
   /* floatValue: */
   /***************/
   public float floatValue()
     {
      return numberValue().floatValue();
     }

   /****************/
   /* doubleValue: */
   /****************/
   public double doubleValue()
     {
      return numberValue().doubleValue();
     }
  }
