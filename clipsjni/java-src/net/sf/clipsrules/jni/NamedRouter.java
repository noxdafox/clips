package net.sf.clipsrules.jni;

public class NamedRouter implements Router
  {   
   private static int NamedRouterNameIndex = 0;
   private String routerName;
   protected int priority;
   private String [] queryNames;
   protected Environment clips;

   /***************/
   /* NamedRouter */
   /***************/
   public NamedRouter(
     Environment theEnv,
     String [] theQueryNames,
     int thePriority) 
     {  
      clips = theEnv;
      queryNames = theQueryNames;
      priority = thePriority;
      routerName = "NamedRouter" + NamedRouterNameIndex++;
      clips.addRouter(this);
     }
     
   /*################*/
   /* Router Methods */
   /*#################*/

   /****************/
   /* getPriority: */
   /****************/
   @Override
   public int getPriority()
     {
      return priority;
     }
       
   /************/
   /* getName: */
   /************/
   @Override
   public String getName()
     {
      return routerName;
     }
     
   /**********/
   /* query: */
   /**********/
   @Override
   public boolean query(
     String logName)
     {      
      for (String name : queryNames)
        {
         if (name.equals(logName))
           { return true; }
        }

      return false;
     }

   /**********/
   /* print: */
   /**********/
   @Override
   public void print(
     String logName,
     String printString)
     {
     }
     
   /************/
   /* getchar: */
   /************/
   @Override
   public int getchar(
     String routerName)
     {
      return -1;
     }

   /**************/
   /* ungetchar: */
   /**************/
   @Override
   public int ungetchar(
     String routerName,
     int theChar)
     {
      return -1;
     }

   /*********/
   /* exit: */
   /*********/
   @Override
   public boolean exit(
     int exitCode)
     {      
      return true;
     }  
  }