package net.sf.clipsrules.jni;

public abstract class LexemeValue extends PrimitiveValue 
  {
   private String value;
   
   /****************/
   /* LexemeValue: */
   /****************/
   protected LexemeValue(String value) 
     {
      this.value = value;
     }

   /*************/
   /* getValue: */
   /*************/
   @Override
   public String getValue()
     {
      return this.value;
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
	   
	   LexemeValue lv = (LexemeValue) obj;
	   if (this.value == null) return (lv.value == null);
	   return this.value.equals(lv.value);
      }
   
   @Override
   public boolean isLexeme()
     { return true; }
  }
