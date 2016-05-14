package net.sf.clipsrules.jni;

public class Module 
  {
   private String moduleName;
   
   /***********/
   /* Module: */
   /***********/
   public Module(String moduleName) 
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
	   
	   Module cm = (Module) obj;
	   if (this.moduleName == null) return (cm.moduleName == null);
	   return this.moduleName.equals(cm.moduleName);
      }
  }