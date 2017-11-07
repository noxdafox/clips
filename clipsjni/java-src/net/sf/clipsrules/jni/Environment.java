package net.sf.clipsrules.jni;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.concurrent.Callable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.lang.NullPointerException;
import java.io.FileNotFoundException;

public class Environment
  {
   private static final String CLIPSJNI_VERSION = "0.5";

   public static final String FACTS = "facts";
   public static final String RULES = "rules";
   public static final String DEFFUNCTIONS = "deffunctions";
   public static final String COMPILATIONS = "compilations";
   public static final String INSTANCES = "instances";
   public static final String SLOTS = "slots";
   public static final String ACTIVATIONS = "activations";
   public static final String STATISTICS = "statistics";
   public static final String FOCUS = "focus";
   public static final String GENERIC_FUNCTIONS = "generic-functions";
   public static final String METHODS = "methods";
   public static final String GLOBALS = "globals";
   public static final String MESSAGES = "messages";
   public static final String MESSAGE_HANDLERS = "message-handlers";
   public static final String NONE = "none";
   public static final String ALL = "all";
         
   private HashMap<String,Router> routerMap = new HashMap<String,Router>();
   private List<CLIPSLineError> errorList = new ArrayList<CLIPSLineError>();

   static { System.loadLibrary("CLIPSJNI"); }

   private long theEnvironment;

   /*********/
   /* main: */
   /*********/
   public static void main(String args[])
     {  
      Environment clips;
      
      clips = new Environment();

      clips.commandLoop();
     }  

   /****************/
   /* Environment: */
   /****************/
   public Environment()
     {
      super();
      theEnvironment = createEnvironment();
     }
     
   /***************************/
   /* createCLIPSEnvironment: */
   /***************************/
   private native long createEnvironment();

   /****************/
   /* captureStart */
   /****************/
   public CaptureRouter captureStart()
     {
      setErrorCallback(true);
      
      errorList.clear();

      return new CaptureRouter(this,new String [] { Router.STDERR } );
     }

   /**************/
   /* captureEnd */
   /**************/
   public void captureEnd(
     CaptureRouter commandCapture) throws CLIPSException
     {
      String error = commandCapture.getOutput();
      
      setErrorCallback(false);
      
      this.deleteRouter(commandCapture);
            
      if (! error.isEmpty())
        { throw new CLIPSException(error); }
     }

   /**************************/
   /* captureEndWithoutCheck */
   /**************************/
   public void captureEndWithoutCheck(
     CaptureRouter commandCapture)
     {
      String error = commandCapture.getOutput();
      
      setErrorCallback(false);
      
      this.deleteRouter(commandCapture);
     }
     
   /****************************************/
   /* clear: Clears the CLIPS environment. */
   /****************************************/
   private native void clear(long env);

   /**********/
   /* clear: */
   /**********/
   public void clear() throws CLIPSException
     {
      voidCommandWrapper(
         new Callable<Void>() 
               {
                public Void call() throws Exception 
                  { 
                   clear(theEnvironment);  
                   return null;
                  }
               });
     }

   /*********/
   /* load: */
   /*********/
   private native void load(long env,String filename);

   /*********/
   /* load: */
   /*********/
   public void load(String fileName) throws CLIPSLoadException
     {
      if (fileName == null)
        { throw new NullPointerException("fileName"); }
        
      CaptureRouter loadCapture = captureStart();
      load(theEnvironment,fileName);
      captureEndWithoutCheck(loadCapture);
      
      checkForErrors("Load");
     }

   /*******************/
   /* loadFromString: */
   /*******************/
   private native void loadFromString(long env,String loadString);

   /*******************/
   /* loadFromString: */
   /*******************/
   public void loadFromString(String loadString) throws CLIPSLoadException
     {
      if (loadString == null)
        { throw new NullPointerException("loadString"); }
        
      String oldName = getParsingFileName();
      setParsingFileName("<String>");
      CaptureRouter loadCapture = captureStart();
      loadFromString(theEnvironment,loadString);
      captureEndWithoutCheck(loadCapture);
      setParsingFileName(oldName);
      checkForErrors("LoadFromString");
     }
      
   /*****************************/
   /* loadFromStringWithOutput: */
   /*****************************/
   private native void loadFromStringWithOutput(long env,String loadString);

   /*****************************/
   /* loadFromStringWithOutput: */
   /*****************************/
   public void loadFromStringWithOutput(String loadString)
     {
      String oldName = getParsingFileName();
      setParsingFileName("<String>");
      loadFromStringWithOutput(theEnvironment,loadString);
      setParsingFileName(oldName);
     }

   /**************************/
   /* convertStreamToString: */
   /**************************/  
    private static String convertStreamToString(java.io.InputStream is) 
      {
       java.util.Scanner s = new java.util.Scanner(is,"UTF-8").useDelimiter("\\A");
       return s.hasNext() ? s.next() : "";
      }

   /*********************/
   /* loadFromResource: */
   /*********************/
   public void loadFromResource(String resourceFile) throws CLIPSLoadException, FileNotFoundException
     {
      if (resourceFile == null)
        { throw new NullPointerException("resourceFile"); }
        
      InputStream input = getClass().getResourceAsStream(resourceFile);
      if (input == null) 
        { throw new FileNotFoundException(resourceFile + " (no such file)"); }
        
      String oldName = getParsingFileName();
      setParsingFileName(resourceFile);
      CaptureRouter loadCapture = captureStart();
      loadFromString(theEnvironment,convertStreamToString(input));
      captureEndWithoutCheck(loadCapture);
      setParsingFileName(oldName);
      checkForErrors("LoadFromResource");
     }

   /**********/
   /* build: */
   /**********/
   private native boolean build(long env,String buildStr);

   /**********/
   /* build: */
   /**********/
   public void build(String buildString) throws CLIPSException
     {
      if (buildString == null)
        { throw new NullPointerException("buildString"); }
        
      CaptureRouter buildCapture = captureStart();

      build(theEnvironment,buildString);
      
      captureEnd(buildCapture);
     }

   /****************************************/
   /* reset: Resets the CLIPS environment. */
   /****************************************/
   private native void reset(long env);

   /**********/
   /* reset: */
   /**********/
   public void reset() throws CLIPSException
     {
      voidCommandWrapper(
         new Callable<Void>() 
               {
                public Void call() throws Exception 
                  { 
                   reset(theEnvironment);  
                   return null;
                  }
               });
     }

   /********/
   /* run: */
   /********/
   private native long run(long env,long runLimit);

   /********/
   /* run: */
   /********/
   public long run(
     final long runLimit) throws CLIPSException
     {
      return longCommandWrapper(
         new Callable<Long>() 
               {
                public Long call() throws Exception 
                  { 
                   return run(theEnvironment,runLimit);
                  }
               });
     }

   /********/
   /* run: */
   /********/
   public long run() throws CLIPSException
     {
      return run(-1);
     }

   /*****************/
   /* assertString: */
   /*****************/
   private native FactAddressValue assertString(long env,String factStr);

   /*****************/
   /* assertString: */
   /*****************/
   public FactAddressValue assertString(String factString) throws CLIPSException
     {
      if (factString == null)
        { throw new NullPointerException("factString"); }
        
      CaptureRouter assertCapture = captureStart();

      FactAddressValue fav = assertString(theEnvironment,factString);
      
      captureEnd(assertCapture);

      return fav;
     }

   /*****************/
   /* makeInstance: */
   /*****************/
   private native InstanceAddressValue makeInstance(long env,String instanceStr);

   /*****************/
   /* makeInstance: */
   /*****************/
   public InstanceAddressValue makeInstance(String instanceString) throws CLIPSException
     {
      if (instanceString == null)
        { throw new NullPointerException("instanceString"); }
        
      CaptureRouter miCapture = captureStart();

      InstanceAddressValue iav = makeInstance(theEnvironment,instanceString);
      
      captureEnd(miCapture);

      return iav;
     }

   /************/
   /* findFact */
   /************/ 
   public FactAddressValue findFact(
     String deftemplate) throws CLIPSException
     {
      return findFact("?f",deftemplate,"TRUE");
     }
     
   /************/
   /* findFact */
   /************/ 
   public FactAddressValue findFact(
     String variable,
     String deftemplate,
     String condition) throws CLIPSException
     {
      if (variable == null)
        { throw new NullPointerException("variable"); }
        
      if (deftemplate == null)
        { throw new NullPointerException("deftemplate"); }
        
      if (condition == null)
        { throw new NullPointerException("condition"); }
        
      String query = "(find-fact " + 
                        "((" + variable + " " + deftemplate + ")) " + condition + ")";
                        
      PrimitiveValue pv = eval(query);
      
      if (! pv.isMultifield())
        { return null; }
      
      MultifieldValue mv = (MultifieldValue) pv;
     
      if (mv.size() == 0)
        { return null; }
        
      return (FactAddressValue) mv.get(0);
     }
     
   /****************/
   /* findAllFacts */
   /****************/ 
   public List<FactAddressValue> findAllFacts(
     String deftemplate) throws CLIPSException
     {
      return findAllFacts("?f",deftemplate,"TRUE");
     }
     
   /****************/
   /* findAllFacts */
   /****************/ 
   public List<FactAddressValue> findAllFacts(
     String variable,
     String deftemplate,
     String condition) throws CLIPSException
     {
      if (variable == null)
        { throw new NullPointerException("variable"); }
        
      if (deftemplate == null)
        { throw new NullPointerException("deftemplate"); }
        
      if (condition == null)
        { throw new NullPointerException("condition"); }
        
      String query = "(find-all-facts " + 
                        "((" + variable + " " + deftemplate + ")) " + condition + ")";
                        
      PrimitiveValue pv = eval(query);
      
      if (! pv.isMultifield())
        { return null; }
      
      MultifieldValue mv = (MultifieldValue) pv;
      
      List<FactAddressValue> rv = new ArrayList<FactAddressValue>(mv.size());
      
      for (PrimitiveValue theValue : mv)
        { rv.add((FactAddressValue) theValue); }

      return rv;
     }
     
   /****************/
   /* findInstance */
   /****************/ 
   public InstanceAddressValue findInstance(
     String defclass) throws CLIPSException
     {
      return findInstance("?i",defclass,"TRUE");
     }
     
   /****************/
   /* findInstance */
   /****************/ 
   public InstanceAddressValue findInstance(
     String variable,
     String defclass,
     String condition) throws CLIPSException
     {
      if (variable == null)
        { throw new NullPointerException("variable"); }
        
      if (defclass == null)
        { throw new NullPointerException("defclass"); }
        
      if (condition == null)
        { throw new NullPointerException("condition"); }
        
      String query = "(find-instance " + 
                        "((" + variable + " " + defclass + ")) " + condition + ")";
                        
      PrimitiveValue pv = eval(query);
      
      if (! pv.isMultifield())
        { return null; }
      
      MultifieldValue mv = (MultifieldValue) pv;
     
      if (mv.size() == 0)
        { return null; }
        
      return ((InstanceNameValue) mv.get(0)).getInstance(this);
     }
     
   /********************/
   /* findAllInstances */
   /********************/ 
   public List<InstanceAddressValue> findAllInstances(
     String defclass) throws CLIPSException
     {
      return findAllInstances("?i",defclass,"TRUE");
     }
     
   /********************/
   /* findAllInstances */
   /********************/ 
   public List<InstanceAddressValue> findAllInstances(
     String variable,
     String defclass,
     String condition) throws CLIPSException
     {
      if (variable == null)
        { throw new NullPointerException("variable"); }
        
      if (defclass == null)
        { throw new NullPointerException("defclass"); }
        
      if (condition == null)
        { throw new NullPointerException("condition"); }
        
      String query = "(find-all-instances " + 
                        "((" + variable + " " + defclass + ")) " + condition + ")";
                        
      PrimitiveValue pv = eval(query);
      
      if (! pv.isMultifield())
        { return null; }
      
      MultifieldValue mv = (MultifieldValue) pv;

      List<InstanceAddressValue> rv = new ArrayList<InstanceAddressValue>(mv.size());
      
      for (PrimitiveValue theValue : mv)
        { rv.add(((InstanceNameValue) theValue).getInstance(this)); }

      return rv;
     }

   /*********/
   /* eval: */
   /*********/
   private native PrimitiveValue eval(long env,String evalStr);

   /*********/
   /* eval: */
   /*********/
   public PrimitiveValue eval(String evalString) throws CLIPSException
     {
      if (evalString == null)
        { throw new NullPointerException("evalString"); }
        
      CaptureRouter evalCapture = captureStart();

      PrimitiveValue pv = eval(theEnvironment,evalString);
      
      captureEnd(evalCapture);
      
      return pv;
     }

   /**********/
   /* watch: */
   /**********/
   private native boolean watch(long env,String watchItem);

   /**********/
   /* watch: */
   /**********/
   public void watch(String watchItem)
     {
      if (watchItem == null)
        { throw new NullPointerException("watchItem"); }
        
      if (! validWatchItem(theEnvironment,watchItem))
        { throw new IllegalArgumentException("watchItem '"+ watchItem + "' is invalid."); }
        
      watch(theEnvironment,watchItem);
     }

   /************/
   /* unwatch: */
   /************/
   private native boolean unwatch(long env,String watchItem);

   /************/
   /* unwatch: */
   /************/
   public void unwatch(String watchItem)
     {
      if (watchItem == null)
        { throw new NullPointerException("watchItem"); }

      if (! validWatchItem(theEnvironment,watchItem))
        { throw new IllegalArgumentException("watchItem '"+ watchItem + "' is invalid."); }
        
      unwatch(theEnvironment,watchItem);
     }

   /*******************/
   /* validWatchItem: */
   /*******************/
   private native boolean validWatchItem(long env,String watchItem);
     
   /*****************/
   /* getWatchItem: */
   /*****************/
   private native boolean getWatchItem(long env,String watchItem);

   /*****************/
   /* getWatchItem: */
   /*****************/
   public boolean getWatchItem(String watchItem)
     {
      if (watchItem == null)
        { throw new NullPointerException("watchItem"); }
        
      if (! validWatchItem(theEnvironment,watchItem))
        { throw new IllegalArgumentException("watchItem '"+ watchItem + "' is invalid."); }
        
      return getWatchItem(theEnvironment,watchItem);
     }

   /*****************/
   /* setWatchItem: */
   /*****************/
   public void setWatchItem(String watchItem,boolean newValue)
     {
      if (watchItem == null)
        { throw new NullPointerException("watchItem"); }
      
      if (! validWatchItem(theEnvironment,watchItem))
        { throw new IllegalArgumentException("watchItem '"+ watchItem + "' is invalid."); }

      if (newValue)
        { watch(watchItem); }
      else
        { unwatch(watchItem); }
     }
     
   /******************************************/
   /* addUserFunction: Adds a user function. */
   /******************************************/
   private native int addUserFunction(long env,
                                      String functionName,
                                      String returnTypes,
                                      int minArgs,
                                      int maxArgs,
                                      String restrictions,
                                      UserFunction callback);
     
   /*******************/
   /* addUserFunction */
   /*******************/
   public void addUserFunction(
     String functionName,
     UserFunction callback)
     {
      addUserFunction(functionName,"*",0,UserFunction.UNBOUNDED,null,callback);
     } 

   /*******************/
   /* addUserFunction */
   /*******************/
   public void addUserFunction(
     String functionName,
     String returnTypes,
     int minArgs,
     int maxArgs,
     String restrictions,
     UserFunction callback)
     {
      int rv;
      
      if (functionName == null)
        { throw new NullPointerException("functionName"); }
        
      if (callback == null)
        { throw new NullPointerException("callback"); }
        
      rv = addUserFunction(theEnvironment,functionName,returnTypes,
                           minArgs,maxArgs,restrictions,callback);
                                                      
      switch (rv)
        {
         case 0:
           break;
           
         case 1:
           throw new IllegalArgumentException("Function '" + functionName + "' minArgs exceeds maxArgs."); 
           
         case 2:
           throw new IllegalArgumentException("Function '" + functionName + "' already exists."); 
           
         case 3:
           throw new IllegalArgumentException("Function '" + functionName + "' invalid argument type."); 
           
         case 4:
           throw new IllegalArgumentException("Function '" + functionName + "' invalid return type."); 
        }
      } 

   /************************************************/
   /* removeUserFunction: Removes a user function. */
   /************************************************/
   private native boolean removeUserFunction(long env,
                                             String functionName);
                                             
   /**********************/
   /* removeUserFunction */
   /**********************/
   public void removeUserFunction(
     String functionName)
     {
      boolean rv;
      
      if (functionName == null)
        { throw new NullPointerException("functionName"); }

      rv = removeUserFunction(theEnvironment,functionName);
      
      if (! rv)
        { throw new IllegalArgumentException("Function '" + functionName + "' does not exist."); }
     } 

   /**************/
   /* addRouter: */
   /**************/
   private native boolean addRouter(long env,String routerName,int priority,Router theRouter);
    
   /**************/
   /* addRouter: */
   /**************/
   public void addRouter(
     Router theRouter)
     {
      if (theRouter == null)
        { throw new NullPointerException("theRouter"); }
        
      if (routerMap.containsKey(theRouter.getName()))
        { throw new IllegalArgumentException("Router named '" + theRouter.getName() +"' already exists."); }
         
      boolean rv = addRouter(theEnvironment,theRouter.getName(),theRouter.getPriority(),theRouter);
      
      if (rv)
        { routerMap.put(theRouter.getName(),theRouter); }
      else
        { throw new IllegalArgumentException("Router named '" + theRouter.getName() +"' already exists."); }
     }

   /*****************/
   /* deleteRouter: */
   /*****************/
   private native boolean deleteRouter(long env,String routerName);
    
   /*****************/
   /* deleteRouter: */
   /*****************/
   public void deleteRouter(
     Router theRouter)
     {
      if (theRouter == null)
        { throw new NullPointerException("theRouter"); }
                
      if (deleteRouter(theEnvironment,theRouter.getName()))
        { routerMap.remove(theRouter); }
      else
        { throw new IllegalArgumentException("Router named '" + theRouter.getName() +"' does not exist."); }
     }

   /*******************/
   /* activateRouter: */
   /*******************/
   private native boolean activateRouter(long env,String routerName);

   /*******************/
   /* activateRouter: */
   /*******************/
   public void activateRouter(
     Router theRouter)
     {
      if (theRouter == null)
        { throw new NullPointerException("theRouter"); }
                
      if (! activateRouter(theEnvironment,theRouter.getName()))
        { throw new IllegalArgumentException("Router named '" + theRouter.getName() + "' does not exist."); }
     }

   /*********************/
   /* deactivateRouter: */
   /*********************/
   private native boolean deactivateRouter(long env,String routerName);

   /*********************/
   /* deactivateRouter: */
   /*********************/
   public void deactivateRouter(
     Router theRouter)
     {
      if (theRouter == null)
        { throw new NullPointerException("theRouter"); }
        
      if (! deactivateRouter(theEnvironment,theRouter.getName()))
        { throw new IllegalArgumentException("Router named '" + theRouter.getName() + "' does not exist."); }
     }

   /****************/
   /* printString: */
   /****************/
   private native void printString(long env,String logName,String printString);

   /**********************/
   /* printRouterExists: */
   /**********************/
   private native boolean printRouterExists(long env,String logName);

   /**********/
   /* print: */
   /**********/
   public void print(
     String logicalName,
     String printString)
     {
      if (logicalName == null)
        { throw new NullPointerException("logicalName"); }
        
      if (printString == null)
        { throw new NullPointerException("printString"); }
        
      if (! printRouterExists(theEnvironment,logicalName))
        { throw new IllegalArgumentException("No print router for logicalName '" + logicalName +"' exists."); }

      printString(theEnvironment,logicalName,printString);
     }
     
   /**********/
   /* print: */
   /**********/
   public void print(
     String printString)
     {
      if (printString == null)
        { throw new NullPointerException("printString"); }
        
      printString(theEnvironment,Router.STDOUT,printString);
     }

   /************/
   /* println: */
   /************/
   public void println(
     String logicalName,
     String printString)
     {
      if (logicalName == null)
        { throw new NullPointerException("logicalName"); }
        
      if (printString == null)
        { throw new NullPointerException("printString"); }
        
      if (! printRouterExists(theEnvironment,logicalName))
        { throw new IllegalArgumentException("No print router for logicalName '" + logicalName +"' exists."); }
        
      printString(theEnvironment,logicalName,printString + "\n");
     }

   /************/
   /* println: */
   /************/
   public void println(
     String printString)
     {
      if (printString == null)
        { throw new NullPointerException("printString"); }
        
      printString(theEnvironment,Router.STDOUT,printString + "\n");
     }

   /****************/
   /* commandLoop: */
   /****************/
   private native void commandLoop(long env);
    
   /****************/
   /* commandLoop: */
   /****************/
   public void commandLoop()
     {
      commandLoop(theEnvironment);
     }

   /********************/
   /* changeDirectory: */
   /********************/
   private native int changeDirectory(long env,String directory);
   
   /********************/
   /* changeDirectory: */
   /********************/
   public int changeDirectory(String directory)
     {
      return changeDirectory(theEnvironment,directory);
     }
     
   /*******************/
   /* checkForErrors: */
   /*******************/
   public void checkForErrors(String function) throws CLIPSLoadException
     {
      if (errorList.isEmpty())
        { return; }
        
      String exceptionString;
         
      if (errorList.size() == 1)
        { exceptionString = "\n" + function + " encountered 1 error:\n"; }
      else
        { exceptionString = "\n" + function + " encountered " + errorList.size() + " errors:\n"; }
           
      for (CLIPSLineError theError : errorList)
        {
         exceptionString = exceptionString.concat("\n" + 
                                                  theError.getFileName() + " (Line " +
                                                  theError.getLineNumber() + ") : " +
                                                  theError.getMessage());
        }
           
      CLIPSLoadException e = new CLIPSLoadException(exceptionString,errorList);
      errorList.clear();
      throw e;
     }
    
   /***********************/
   /* getParsingFileName: */
   /***********************/
   private native String getParsingFileName(long env);

   /***********************/
   /* getParsingFileName: */
   /***********************/
   public String getParsingFileName()
     {
      return getParsingFileName(theEnvironment);
     }

   /***********************/
   /* setParsingFileName: */
   /***********************/
   private native void setParsingFileName(long env,String theString);

   /***********************/
   /* setParsingFileName: */
   /***********************/
   public void setParsingFileName(
     String theString)
     {
      setParsingFileName(theEnvironment,theString);
     }

   /*************/
   /* addError: */
   /*************/
   public void addError(
     String fileName,
     long lineNumber,
     String message)
     {
      errorList.add(new CLIPSLineError(fileName,lineNumber,message));
     }
     
   /**************/
   /* loadFacts: */
   /**************/
   private native boolean loadFacts(long env,String filename);

   /**************/
   /* loadFacts: */
   /**************/
   public boolean loadFacts(String filename)
     {
      return loadFacts(theEnvironment,filename);
     }

   /*********************/
   /* setHaltExecution: */
   /*********************/
   private native void setHaltExecution(long env,boolean value);

   /*********************/
   /* setHaltExecution: */
   /*********************/
   public void setHaltExecution(
     boolean value)
     {
      setHaltExecution(theEnvironment,value);
     }

   /*****************/
   /* setHaltRules: */
   /*****************/
   private native void setHaltRules(long env,boolean value);

   /*****************/
   /* setHaltRules: */
   /*****************/
   public void setHaltRules(
     boolean value)
     {
      setHaltRules(theEnvironment,value);
     }
        
   /******************/
   /* getModuleList: */
   /******************/
   private native List<Module> getModuleList(long env);

   /******************/
   /* getModuleList: */
   /******************/
   public List<Module> getModuleList()
     {
      return getModuleList(theEnvironment);
     }

   /******************/
   /* getFocusStack: */
   /******************/
   private native FocusStack getFocusStack(long env);

   /******************/
   /* getFocusStack: */
   /******************/
   public FocusStack getFocusStack()
     {
      return getFocusStack(theEnvironment);
     }

   /******************/
   /* getFactScopes: */
   /******************/
   private native HashMap<Long,BitSet> getFactScopes(long env);

   /*****************/
   /* getFactScopes */
   /*****************/
   public HashMap<Long,BitSet> getFactScopes()
     {
      return getFactScopes(theEnvironment);
     }
     
   /**********************/
   /* getInstanceScopes: */
   /**********************/
   private native HashMap<Long,BitSet> getInstanceScopes(long env);

   /*********************/
   /* getInstanceScopes */
   /*********************/
   public HashMap<Long,BitSet> getInstanceScopes()
     {
      return getInstanceScopes(theEnvironment);
     }
     
   /****************/
   /* getFactList: */
   /****************/
   private native List<FactInstance> getFactList(long env);

   /****************/
   /* getFactList: */
   /****************/
   public List<FactInstance> getFactList()
     {
      return getFactList(theEnvironment);
     }

   /********************/
   /* getInstanceList: */
   /********************/
   private native List<FactInstance> getInstanceList(long env);

   /********************/
   /* getInstanceList: */
   /********************/
   public List<FactInstance> getInstanceList()
     {
      return getInstanceList(theEnvironment);
     }

   /**************/
   /* getAgenda: */
   /**************/
   private native Agenda getAgenda(long env,String moduleName);

   /**************/
   /* getAgenda: */
   /**************/
   public Agenda getAgenda(String moduleName)
     {
      return getAgenda(theEnvironment,moduleName);
     }

   /**************/
   /* getAgenda: */
   /**************/
   public Agenda getAgenda(
     Focus theFocus)
     {
      return getAgenda(theEnvironment,theFocus.getModuleName());
     }

   /***********************/
   /* getDeftemplateText: */
   /***********************/
   private native String getDeftemplateText(long env,long templatePtr);

   /***********************/
   /* getDeftemplateText: */
   /***********************/
   public String getDeftemplateText(long templatePtr)
     {
      return getDeftemplateText(theEnvironment,templatePtr);
     }

   /********************/
   /* getDefclassText: */
   /********************/
   private native String getDefclassText(long env,long classPtr);

   /********************/
   /* getDefclassText: */
   /********************/
   public String getDefclassText(long classPtr)
     {
      return getDefclassText(theEnvironment,classPtr);
     }

   /*******************/
   /* getDefruleText: */
   /*******************/
   private native String getDefruleText(long env,String ruleName);

   /*******************/
   /* getDefruleText: */
   /*******************/
   public String getDefruleText(String ruleName)
     {
      return getDefruleText(theEnvironment,ruleName);
     }

   /**************/
   /* factIndex: */
   /**************/
   private static native long factIndex(Environment javaEnv,long env,long fact);

   /**************/
   /* factIndex: */
   /**************/
   public static long factIndex(
     FactAddressValue theFact)
     {
      return factIndex(theFact.getEnvironment(),
                       theFact.getEnvironment().getEnvironmentAddress(),
                       theFact.getFactAddress());
     }

   /****************/
   /* getFactSlot: */
   /****************/
   private static native PrimitiveValue getFactSlot(Environment javaEnv,long env,long fact,String slotName);

   /****************/
   /* getFactSlot: */
   /****************/
   public static PrimitiveValue getFactSlot(
     FactAddressValue theFact,
     String slotName)
     {
      PrimitiveValue theValue;
      
      if (slotName == null)
        { throw new NullPointerException("slotName"); }
        
      theValue = getFactSlot(theFact.getEnvironment(),
                             theFact.getEnvironment().getEnvironmentAddress(),
                             theFact.getFactAddress(),slotName);
                             
      if (theValue == null)
        { throw new IllegalArgumentException("Slot name '" + slotName + "' is invalid."); }

      return theValue;
     }

   /********************/
   /* getInstanceName: */
   /********************/
   private static native String getInstanceName(Environment javaEnv,long env,long instance);

   /********************/
   /* getInstanceName: */
   /********************/
   public static String getInstanceName(
     InstanceAddressValue theInstance)
     {
      return getInstanceName(theInstance.getEnvironment(),
                             theInstance.getEnvironment().getEnvironmentAddress(),
                             theInstance.getInstanceAddress());
     }

   /******************/
   /* directGetSlot: */
   /******************/
   private static native PrimitiveValue directGetSlot(Environment javaEnv,long env,long instance,String slotName);

   /******************/
   /* directGetSlot: */
   /******************/
   public static PrimitiveValue directGetSlot(
     InstanceAddressValue theInstance,
     String slotName)
     {
      PrimitiveValue theValue;
      
      if (slotName == null)
        { throw new NullPointerException("slotName"); }
        
      theValue = directGetSlot(theInstance.getEnvironment(),
                               theInstance.getEnvironment().getEnvironmentAddress(),
                               theInstance.getInstanceAddress(),slotName);
                           
      if (theValue == null)
        { throw new IllegalArgumentException("Slot name '" + slotName + "' is invalid."); }
        
      return theValue;
     }

   /***********************/
   /* destroyEnvironment: */
   /***********************/
   private native void destroyEnvironment(long env);

   /*********************/
   /* flushInputBuffer: */
   /*********************/
   private native void flushInputBuffer(long env);

   /*********************/
   /* flushInputBuffer: */
   /*********************/
   public void flushInputBuffer()
     {
      flushInputBuffer(theEnvironment);
     }

   /*******************/
   /* getInputBuffer: */
   /*******************/
   private native String getInputBuffer(long env);

   /*******************/
   /* getInputBuffer: */
   /*******************/
   public String getInputBuffer()
     {
      return getInputBuffer(theEnvironment);
     }

   /*******************/
   /* setInputBuffer: */
   /*******************/
   private native void setInputBuffer(long env,String theString);

   /*******************/
   /* setInputBuffer: */
   /*******************/
   public void setInputBuffer(
     String theString)
     {
      setInputBuffer(theEnvironment,theString);
     }

   /************************/
   /* getInputBufferCount: */
   /************************/
   private native long getInputBufferCount(long env);

   /************************/
   /* getInputBufferCount: */
   /************************/
   public long getInputBufferCount()
     {
      return getInputBufferCount(theEnvironment);
     }

   /************************/
   /* setInputBufferCount: */
   /************************/
   private native long setInputBufferCount(long env,long theValue);

   /************************/
   /* setInputBufferCount: */
   /************************/
   public long setInputBufferCount(
     long theValue)
     {
      return setInputBufferCount(theEnvironment,theValue);
     }

   /**********************/
   /* expandInputBuffer: */
   /**********************/
   private native void expandInputBuffer(long env,int theChar);

   /**********************/
   /* expandInputBuffer: */
   /**********************/
   public void expandInputBuffer(
     int theChar)
     {
      expandInputBuffer(theEnvironment,theChar);
     }

   /**********************/
   /* appendInputBuffer: */
   /**********************/
   private native void appendInputBuffer(long env,String theString);

   /**********************/
   /* appendInputBuffer: */
   /**********************/
   public void appendInputBuffer(
     String theString)
     {
      appendInputBuffer(theEnvironment,theString);
     }

   /******************/
   /* appendDribble: */
   /******************/
   private native void appendDribble(long env,String theString);

   /******************/
   /* appendDribble: */
   /******************/
   public void appendDribble(
     String theString)
     {
      appendDribble(theEnvironment,theString);
     }

   /*******************************/
   /* inputBufferContainsCommand: */
   /*******************************/
   private native boolean inputBufferContainsCommand(long env);

   /*******************************/
   /* inputBufferContainsCommand: */
   /*******************************/
   public boolean inputBufferContainsCommand()
     {
      return inputBufferContainsCommand(theEnvironment);
     }

   /*****************************/
   /* commandLoopOnceThenBatch: */
   /*****************************/
   private native void commandLoopOnceThenBatch(long env);
    
   /*****************************/
   /* commandLoopOnceThenBatch: */
   /*****************************/
   public void commandLoopOnceThenBatch()
     {
      commandLoopOnceThenBatch(theEnvironment);
     }

   /***************************/
   /* commandLoopBatchDriver: */
   /***************************/
   private native void commandLoopBatchDriver(long env);
    
   /***************************/
   /* commandLoopBatchDriver: */
   /***************************/
   public void commandLoopBatchDriver()
     {
      commandLoopBatchDriver(theEnvironment);
     }

   /********************/
   /* openStringBatch: */
   /********************/
   private native boolean openStringBatch(long env,String stringName,String data,boolean placeAtEnd);
    
   /********************/
   /* openStringBatch: */
   /********************/
   public boolean openStringBatch(
     String stringName,
     String data,
     boolean placeAtEnd)
     {
      return openStringBatch(theEnvironment,stringName,data,placeAtEnd);
     }

   /*******************************/
   /* setPeriodicCallbackEnabled: */
   /*******************************/
   private native void setPeriodicCallbackEnabled(long env,boolean value);    

   /*******************************/
   /* setPeriodicCallbackEnabled: */
   /*******************************/
   public void setPeriodicCallbackEnabled(
     boolean value)
     {
      setPeriodicCallbackEnabled(theEnvironment,value);
     }

   /************************/
   /* addPeriodicCallback: */
   /************************/
   private native void addPeriodicCallback(long env,String name,int priority,PeriodicCallback thePC);    

   /************************/
   /* addPeriodicCallback: */
   /************************/
   public void addPeriodicCallback(
     String name,
     int priority,
     PeriodicCallback thePC)
     {
      addPeriodicCallback(theEnvironment,name,priority,thePC);
     }

   /***************************/
   /* removePeriodicCallback: */
   /***************************/
   private native boolean removePeriodicCallback(long env,String name);    

   /***************************/
   /* removePeriodicCallback: */
   /***************************/
   public boolean removePeriodicCallback(
     String name)
     {
      return removePeriodicCallback(theEnvironment,name);
     }
     
   /****************/
   /* printBanner: */
   /****************/
   private native void printBanner(long env);
    
   /****************/
   /* printBanner: */
   /****************/
   public void printBanner()
     {
      printBanner(theEnvironment);
     }

   /****************/
   /* printPrompt: */
   /****************/
   private native void printPrompt(long env);
    
   /****************/
   /* printPrompt: */
   /****************/
   public void printPrompt()
     {
      printPrompt(theEnvironment);
     }

   /************************/
   /* callNextPrintRouter: */
   /************************/
   public void callNextPrintRouter(
     Router theRouter,
     String logName,
     String printString)
     {
      deactivateRouter(theRouter);
      print(logName,printString);
      activateRouter(theRouter);
     }

   /*********************/
   /* getAgendaChanged: */
   /*********************/
   private native boolean getAgendaChanged(long env);

   /*********************/
   /* getAgendaChanged: */
   /*********************/
   public boolean getAgendaChanged()
     {
      return getAgendaChanged(theEnvironment);
     }

   /*********************/
   /* setAgendaChanged: */
   /*********************/
   private native void setAgendaChanged(long env,boolean value);

   /*********************/
   /* setAgendaChanged: */
   /*********************/
   public void setAgendaChanged(
     boolean value)
     {
      setAgendaChanged(theEnvironment,value);
     }

   /********************/
   /* getFocusChanged: */
   /********************/
   private native boolean getFocusChanged(long env);

   /********************/
   /* getFocusChanged: */
   /********************/
   public boolean getFocusChanged()
     {
      return getFocusChanged(theEnvironment);
     }

   /********************/
   /* setFocusChanged: */
   /********************/
   private native void setFocusChanged(long env,boolean value);

   /********************/
   /* setFocusChanged: */
   /********************/
   public void setFocusChanged(
     boolean value)
     {
      setFocusChanged(theEnvironment,value);
     }

   /***********************/
   /* getFactListChanged: */
   /***********************/
   private native boolean getFactListChanged(long env);

   /***********************/
   /* getFactListChanged: */
   /***********************/
   public boolean getFactListChanged()
     {
      return getFactListChanged(theEnvironment);
     }

   /***********************/
   /* setFactListChanged: */
   /***********************/
   private native void setFactListChanged(long env,boolean value);

   /***********************/
   /* setFactListChanged: */
   /***********************/
   public void setFactListChanged(
     boolean value)
     {
      setFactListChanged(theEnvironment,value);
     }

   /************************/
   /* getInstancesChanged: */
   /************************/
   private native boolean getInstancesChanged(long env);

   /************************/
   /* getInstancesChanged: */
   /************************/
   public boolean getInstancesChanged()
     {
      return getInstancesChanged(theEnvironment);
     }

   /************************/
   /* setInstancesChanged: */
   /************************/
   private native void setInstancesChanged(long env,boolean value);

   /************************/
   /* setInstancesChanged: */
   /************************/
   public void setInstancesChanged(
     boolean value)
     {
      setInstancesChanged(theEnvironment,value);
     }

   /***************/
   /* retainFact: */
   /***************/
   private native void retainFact(Environment javaEnv,long env,long fact);

   /****************/
   /* releaseFact: */
   /****************/
   private native void releaseFact(Environment javaEnv,long env,long fact);

   /***************/
   /* retainFact: */
   /***************/
   public void retainFact(
     FactAddressValue theFact)
     {
      retainFact(theFact.getEnvironment(),
                 theFact.getEnvironment().getEnvironmentAddress(),
                 theFact.getFactAddress());
     }

   /****************/
   /* releaseFact: */
   /****************/
   public void releaseFact(
     FactAddressValue theFact)
     {
      releaseFact(theFact.getEnvironment(),
                  theFact.getEnvironment().getEnvironmentAddress(),
                  theFact.getFactAddress());
     }

   /*******************/
   /* retainInstance: */
   /*******************/
   private native void retainInstance(Environment javaEnv,long env,long instance);

   /********************/
   /* releaseInstance: */
   /********************/
   private native void releaseInstance(Environment javaEnv,long env,long instance);

   /*******************/
   /* retainInstance: */
   /*******************/
   public void retainInstance(
     InstanceAddressValue theInstance)
     {
      retainInstance(theInstance.getEnvironment(),
                     theInstance.getEnvironment().getEnvironmentAddress(),
                     theInstance.getInstanceAddress());
     }

   /********************/
   /* releaseInstance: */
   /********************/
   public void releaseInstance(
     InstanceAddressValue theInstance)
     {
      releaseInstance(theInstance.getEnvironment(),
                      theInstance.getEnvironment().getEnvironmentAddress(),
                      theInstance.getInstanceAddress());
     }

   /***********************/
   /* findInstanceByName: */
   /***********************/   
   private native InstanceAddressValue findInstanceByName(long env,String instanceName);

   public InstanceAddressValue findInstanceByName(
     String instanceName)
     {
      return findInstanceByName(theEnvironment,instanceName);
     }
  
   /**************************/
   /* incrementAddressCount: */
   /**************************/
   private native void incrementAddressCount(Environment javaEnv,long env,long externalAddress);

   /**************************/
   /* decrementAddressCount: */
   /**************************/
   private native void decrementAddressCount(Environment javaEnv,long env,long externalAddress);

   /**************************/
   /* incrementAddressCount: */
   /**************************/
   public void incrementAddressCount(
     ExternalAddressValue theAddress)
     {
      incrementAddressCount(theAddress.getEnvironment(),
                            theAddress.getEnvironment().getEnvironmentAddress(),
                            theAddress.getExternalAddress());
     }

   /**************************/
   /* decrementAddressCount: */
   /**************************/
   public void decrementAddressCount(
     ExternalAddressValue theAddress)
     {
      decrementAddressCount(theAddress.getEnvironment(),
                            theAddress.getEnvironment().getEnvironmentAddress(),
                            theAddress.getExternalAddress());
     }

   /*******************************/
   /* createExternalAddressValue: */
   /*******************************/
   public ExternalAddressValue createExternalAddressValue(Object externalAddress)
     {
      return new ExternalAddressValue(externalAddress,this);
     }

   /*********************/
   /* setErrorCallback: */
   /*********************/
   private native void setErrorCallback(long env,boolean value);

   /*********************/
   /* setErrorCallback: */
   /*********************/
   public void setErrorCallback(
     boolean value)
     {
      setErrorCallback(theEnvironment,value);
     }
     
   /************/
   /* destroy: */
   /************/
   public void destroy()
     {
      for (String key : routerMap.keySet())
        { deleteRouter(routerMap.get(key)); }

      destroyEnvironment(theEnvironment);
     }
     
   /*********************************************************/
   /* getCLIPSJNIVersion: Gets the CLIPSJNI version number. */
   /*********************************************************/
   public static String getCLIPSJNIVersion() 
     {
      return CLIPSJNI_VERSION;
     }

   /***************************************************/
   /* getCLIPSVersion: Gets the CLIPS version number. */
   /***************************************************/
   public static native String getCLIPSVersion();

   /**************************************************************/
   /* getVersion: Gets the JClips and the CLIPS version numbers. */
   /**************************************************************/
   public static String getVersion() 
     {
      return "CLIPSJNI version " + getCLIPSJNIVersion() + 
              " (CLIPS version " + getCLIPSVersion() + ")";
     }

   /**************************/
   /* getEnvironmentAddress: */
   /**************************/
   public long getEnvironmentAddress()
     { return theEnvironment; }
     
   /***********************/
   /* voidCommandWrapper: */
   /***********************/
   private void voidCommandWrapper(Callable<Void> callable) throws CLIPSException
     {
      CaptureRouter commandCapture = captureStart();
      
      try
        { callable.call(); }
      catch (Exception e)
        {
         captureEndWithoutCheck(commandCapture);
         throw new CLIPSException(e.getMessage(),e.getCause()); 
        }
      
      captureEnd(commandCapture);
     }
     
   /***********************/
   /* longCommandWrapper: */
   /***********************/
   private long longCommandWrapper(Callable<Long> callable) throws CLIPSException
     {
      CaptureRouter commandCapture = captureStart();
      Long rv;
      
      try
        { rv = callable.call(); }
      catch (Exception e)
        {
         captureEndWithoutCheck(commandCapture);
         throw new CLIPSException(e.getMessage(),e.getCause()); 
        }
      
      captureEnd(commandCapture);
        
      return rv;
     }
  }
