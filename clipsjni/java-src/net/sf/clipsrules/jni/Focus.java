package net.sf.clipsrules.jni;

public class Focus 
  {
   private String moduleName;
   
   /**********/
   /* Focus: */
   /**********/
   public Focus(String moduleName) 
     {
      this.moduleName = moduleName;
     }

   /******************/
   /* getModuleName: */
   /******************/
   public String getModuleName()
     {
      return this.moduleName;
     }
     
   /*************/
   /* toString: */
   /*************/
   @Override
   public String toString()
     {
      return moduleName;
     }
     
   /*************/
   /* hashCode: */
   /*************/
   @Override
   public int hashCode()
     {
      if (moduleName == null) return 0;
      return moduleName.hashCode();
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
	   
	   Focus cf = (Focus) obj;
	   if (this.moduleName == null) return (cf.moduleName == null);
	   return this.moduleName.equals(cf.moduleName);
      }
  }