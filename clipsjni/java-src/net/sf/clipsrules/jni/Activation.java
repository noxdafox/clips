package net.sf.clipsrules.jni;

public class Activation 
  {
   private int salience;
   private String ruleName;
   private String basis;
   
   /***************/
   /* Activation: */
   /***************/
   public Activation(
     String ruleName,
     int salience,
     String basis) 
     {
      this.ruleName = ruleName;
      this.salience = salience;
      this.basis = basis;
     }

   /****************/
   /* getSalience: */
   /****************/
   public int getSalience()
     {
      return this.salience;
     }

   /****************/
   /* getRuleName: */
   /****************/
   public String getRuleName()
     {
      return this.ruleName;
     }

   /*************/
   /* getBasis: */
   /*************/
   public String getBasis()
     {
      return this.basis;
     }

     
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {
      return salience + " " + ruleName + ": " + basis;
     }
     
   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      int value = this.salience;
      
      if (ruleName != null) value += ruleName.hashCode();
      if (basis != null) value += basis.hashCode();
      
      return value;
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
	   
	   Activation ca = (Activation) obj;

	   if (this.salience != ca.salience) return false;

	   if (this.ruleName == null) 
	     { if (ca.ruleName != null) return false; }
	   else
	     { if (! this.ruleName.equals(ca.ruleName)) return false; }

	   if (this.basis == null) 
	     { if (ca.basis != null) return false; }
	   else
	     { if (! this.basis.equals(ca.basis)) return false; }
	   
	   return true;
      }
  }