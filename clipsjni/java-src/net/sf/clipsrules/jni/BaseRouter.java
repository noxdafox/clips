package net.sf.clipsrules.jni;

public class BaseRouter implements Router
  {   
   private static int BaseRouterNameIndex = 0;
   private String routerName;
   protected int priority;
   private String [] queryNames;
   protected Environment clips;

   /**************/
   /* BaseRouter */
   /**************/
   public BaseRouter(
     Environment theEnv,
     String [] theQueryNames)
     {
      this(theEnv,theQueryNames,0);
     } 

   public BaseRouter(
     Environment theEnv,
     String [] theQueryNames,
     int thePriority) 
     {  
      this(theEnv,theQueryNames,thePriority,
           "BaseRouter" + BaseRouterNameIndex++);
     }

   public BaseRouter(
     Environment theEnv,
     String [] theQueryNames,
     int thePriority,
     String theRouterName) 
     {  
      clips = theEnv;
      queryNames = theQueryNames;
      priority = thePriority;
      routerName = theRouterName;
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
   /* write: */
   /**********/
   @Override
   public void write(
     String logName,
     String printString)
     {
     }
     
   /*********/
   /* read: */
   /*********/
   @Override
   public int read(
     String routerName)
     {
      return -1;
     }

   /***********/
   /* unread: */
   /***********/
   @Override
   public int unread(
     String routerName,
     int theChar)
     {
      return -1;
     }

   /*********/
   /* exit: */
   /*********/
   @Override
   public void exit(
     boolean failure)
     {      
     }  
  }