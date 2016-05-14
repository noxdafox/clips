package net.sf.clipsrules.jni;

public abstract class NumberValue extends PrimitiveValue 
  {
   private Number value;
   
   /****************/
   /* NumberValue: */
   /****************/
   protected NumberValue(Number value) 
     {
      this.value = value;
	 }

   /*************/
   /* getValue: */
   /*************/
   @Override
   public Number getValue()
     {
      return this.value;
     }

   /****************/
   /* NumberValue: */
   /****************/
   public Number numberValue()
     {
      return getValue();
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
     
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {
      if (value == null) return null;
      return value.toString();
     }
     
   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      if (value == null) return 0;
      return value.hashCode();
     }
     
   /***********/
   /* equals: */
   /***********/
	@Override
	public boolean equals(Object obj) 
	  {
	   if (this == obj) return true;
	   if (obj == null) return false;
	   if (this.getClass() != obj.getClass()) return false;
	   
	   NumberValue nv = (NumberValue) obj;
	   if (this.value == null) return (nv.value == null);
	   return this.value.equals(nv.value);
      }
      
   @Override
   public boolean isNumber()
     { return true; }

  }
