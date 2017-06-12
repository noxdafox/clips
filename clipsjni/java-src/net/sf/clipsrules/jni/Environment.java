package net.sf.clipsrules.jni;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.concurrent.Callable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
   
   public static final int UNBOUNDED = -1;
      
   private List<Router> routerList = new ArrayList<Router>(); 
   private List<CLIPSLineError> errorList = new ArrayList<CLIPSLineError>();

   static { System.loadLibrary("CLIPSJNI"); }

   private long theEnvironment;

   /****************/
   /* Environment: */
   /****************/
   public Environment()
     {
      super();
      theEnvironment = createEnvironment();
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
     
   /***************************/
   /* createCLIPSEnvironment: */
   /***************************/
   private native long createEnvironment();

   /***********************/
   /* voidCommandWrapper: */
   /***********************/
   private void voidCommandWrapper(Callable<Void> callable) throws CLIPSException
     {
      CaptureRouter commandCapture = new CaptureRouter(this,new String [] { Router.ERROR } );
      
      try
        { callable.call(); }
      catch(Exception e)
        {
         this.deleteRouter(commandCapture);
         throw new CLIPSException(e.getMessage(),e.getCause()); 
        }
      
      String error = commandCapture.getOutput();
      this.deleteRouter(commandCapture);
      
      if (! error.isEmpty())
        { throw new CLIPSException(error); }
     }
     
   /***********************/
   /* longCommandWrapper: */
   /***********************/
   private long longCommandWrapper(Callable<Long> callable) throws CLIPSException
     {
      CaptureRouter commandCapture = new CaptureRouter(this,new String [] { Router.ERROR } );
      Long rv;
      
      try
        { rv = callable.call(); }
      catch(Exception e)
        {
         this.deleteRouter(commandCapture);
         throw new CLIPSException(e.getMessage(),e.getCause()); 
        }
      
      String error = commandCapture.getOutput();
      this.deleteRouter(commandCapture);
      
      if (! error.isEmpty())
        { throw new CLIPSException(error); }
        
      return rv;
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
        
   /************************************************/
   /* removeUserFunction: Removes a user function. */
   /************************************************/
   private native boolean removeUserFunction(long env,
                                             String functionName);
                                             
   /**********************/
   /* removeUserFunction */
   /**********************/
   public boolean removeUserFunction(
     String functionName)
     {
      return removeUserFunction(theEnvironment,functionName);
     } 

   /******************************************/
   /* addUserFunction: Adds a user function. */
   /******************************************/
   private native boolean addUserFunction(long env,
                                          String functionName,
                                          String returnTypes,
                                          int minArgs,
                                          int maxArgs,
                                          String restrictions,
                                          UserFunction callback);
     
   /*******************/
   /* addUserFunction */
   /*******************/
   public boolean addUserFunction(
     String functionName,
     UserFunction callback)
     {
      return addUserFunction(theEnvironment,functionName,"*",0,UNBOUNDED,null,callback);
     } 

   /*******************/
   /* addUserFunction */
   /*******************/
   public boolean addUserFunction(
     String functionName,
     String returnTypes,
     int minArgs,
     int maxArgs,
     String restrictions,
     UserFunction callback)
     {
      return addUserFunction(theEnvironment,functionName,returnTypes,
                             minArgs,maxArgs,restrictions,callback);
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
   public void loadFromResource(String resourceFile) throws CLIPSLoadException
     {
      InputStream input = getClass().getResourceAsStream(resourceFile);
      if (input == null) return;
      String oldName = getParsingFileName();
      setParsingFileName(resourceFile);
      CaptureRouter loadCapture = new CaptureRouter(this,new String [] { Router.ERROR } );
      loadFromString(theEnvironment,convertStreamToString(input));
      this.deleteRouter(loadCapture);
      setParsingFileName(oldName);
      checkForErrors("LoadFromResource");
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
   /* loadFromString: */
   /*******************/
   private native void loadFromString(long env,String loadString);

   /*******************/
   /* loadFromString: */
   /*******************/
   public void loadFromString(String loadString) throws CLIPSLoadException
     {
      String oldName = getParsingFileName();
      setParsingFileName("<String>");
      CaptureRouter loadCapture = new CaptureRouter(this,new String [] { Router.ERROR } );
      loadFromString(theEnvironment,loadString);
      this.deleteRouter(loadCapture);
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

   /*********/
   /* load: */
   /*********/
   private native void load(long env,String filename);

   /*********/
   /* load: */
   /*********/
   public void load(String filename) throws CLIPSLoadException
     {
      CaptureRouter loadCapture = new CaptureRouter(this,new String [] { Router.ERROR } );
      load(theEnvironment,filename);
      this.deleteRouter(loadCapture);
      
      checkForErrors("Load");
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

   /*****************/
   /* getWatchItem: */
   /*****************/
   private native boolean getWatchItem(long env,String watchItem);

   /*****************/
   /* getWatchItem: */
   /*****************/
   public boolean getWatchItem(String watchItem)
     {
      return getWatchItem(theEnvironment,watchItem);
     }

   /*****************/
   /* setWatchItem: */
   /*****************/
   public void setWatchItem(String watchItem,boolean newValue)
     {
      if (newValue)
        { watch(watchItem); }
      else
        { unwatch(watchItem); }
     }

   /**********/
   /* watch: */
   /**********/
   private native boolean watch(long env,String watchItem);

   /**********/
   /* watch: */
   /**********/
   public boolean watch(String watchItem)
     {
      return watch(theEnvironment,watchItem);
     }

   /************/
   /* unwatch: */
   /************/
   private native boolean unwatch(long env,String watchItem);

   /************/
   /* unwatch: */
   /************/
   public boolean unwatch(String watchItem)
     {
      return unwatch(theEnvironment,watchItem);
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
          
   /********/
   /* run: */
   /********/
   private native long run(long env,long runLimit);

   /********/
   /* run: */
   /********/
   public long run(
     long runLimit) throws CLIPSException
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

   /*********/
   /* eval: */
   /*********/
   private native PrimitiveValue eval(long env,String evalStr);

   /*********/
   /* eval: */
   /*********/
   public PrimitiveValue eval(String evalStr) throws CLIPSException
     {
      PrimitiveValue pv;
      CaptureRouter evalCapture = new CaptureRouter(this,new String [] { Router.ERROR } );

      pv = eval(theEnvironment,evalStr);
      String error = evalCapture.getOutput();
      this.deleteRouter(evalCapture);
      
      if (! error.isEmpty())
        { 
         throw new CLIPSException(error);
        }
      
      return pv;
     }

   /**********/
   /* build: */
   /**********/
   private native boolean build(long env,String buildStr);

   /**********/
   /* build: */
   /**********/
   public void build(String buildStr) throws CLIPSException
     {
      CaptureRouter buildCapture = new CaptureRouter(this,new String [] { Router.ERROR } );

      boolean rv = build(theEnvironment,buildStr);
      
      String error = buildCapture.getOutput();
      
      this.deleteRouter(buildCapture);
      
      if ((! rv) || (! error.isEmpty()))
        { throw new CLIPSException(error); }
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

   /*****************/
   /* assertString: */
   /*****************/
   private native FactAddressValue assertString(long env,String factStr);

   /*****************/
   /* assertString: */
   /*****************/
   public FactAddressValue assertString(String factStr) throws CLIPSException
     {
      FactAddressValue fav;
      CaptureRouter assertCapture = new CaptureRouter(this,new String [] { Router.ERROR } );

      fav = assertString(theEnvironment,factStr);
      
      String error = assertCapture.getOutput();
      this.deleteRouter(assertCapture);
      
      if ((fav == null) || (! error.isEmpty()))
        { throw new CLIPSException(error); }

      return fav;
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
     String slotName) throws CLIPSException
     {
      PrimitiveValue theValue;
      
      theValue = getFactSlot(theFact.getEnvironment(),
                             theFact.getEnvironment().getEnvironmentAddress(),
                             theFact.getFactAddress(),slotName);
                             
      if (theValue == null)
        { throw new CLIPSException("Slot " + slotName + " is invalid"); }

      return theValue;
     }

   /*****************/
   /* makeInstance: */
   /*****************/
   private native InstanceAddressValue makeInstance(long env,String instanceStr);

   /*****************/
   /* makeInstance: */
   /*****************/
   public InstanceAddressValue makeInstance(String instanceStr) throws CLIPSException
     {
      InstanceAddressValue iav;
      CaptureRouter miCapture = new CaptureRouter(this,new String [] { Router.ERROR } );

      iav = makeInstance(theEnvironment,instanceStr);
      
      String error = miCapture.getOutput();
      this.deleteRouter(miCapture);
      
      if ((iav == null) || (! error.isEmpty()))
        { throw new CLIPSException(error); }

      return iav;
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
     String slotName) throws CLIPSException
     {
      PrimitiveValue theValue;
      
      theValue = directGetSlot(theInstance.getEnvironment(),
                           theInstance.getEnvironment().getEnvironmentAddress(),
                           theInstance.getInstanceAddress(),slotName);
                           
      if (theValue == null)
        { throw new CLIPSException("Slot " + slotName + " is invalid"); }
        
      return theValue;
     }

   /***********************/
   /* destroyEnvironment: */
   /***********************/
   private native void destroyEnvironment(long env);

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

   /**************/
   /* addRouter: */
   /**************/
   private native boolean addRouter(long env,String routerName,int priority,Router theRouter);
    
   /**************/
   /* addRouter: */
   /**************/
   public boolean addRouter(
     Router theRouter)
     {
      boolean rv = addRouter(theEnvironment,theRouter.getName(),theRouter.getPriority(),theRouter);
      
      if (rv)
        { routerList.add(theRouter); }
      return rv;
     }

   /*****************/
   /* deleteRouter: */
   /*****************/
   private native boolean deleteRouter(long env,String routerName);
    
   /*****************/
   /* deleteRouter: */
   /*****************/
   public boolean deleteRouter(
     Router theRouter)
     {
      routerList.remove(theRouter);
      return deleteRouter(theEnvironment,theRouter.getName());
     }

   /****************/
   /* printRouter: */
   /****************/
   private native void printRouter(long env,String logName,String printString);

   /****************/
   /* printRouter: */
   /****************/
   public void printRouter(
     String logName,
     String printString)
     {
      printRouter(theEnvironment,logName,printString);
     }
     
   /**********/
   /* print: */
   /**********/
   public void print(
     String printString)
     {
      printRouter(theEnvironment,Router.STANDARD_OUTPUT,printString);
     }

   /************/
   /* println: */
   /************/
   public void println(
     String printString)
     {
      printRouter(theEnvironment,Router.STANDARD_OUTPUT,printString + "\n");
     }

   /*******************/
   /* activateRouter: */
   /*******************/
   private native boolean activateRouter(long env,String routerName);

   /*******************/
   /* activateRouter: */
   /*******************/
   public boolean activateRouter(
     Router theRouter)
     {
      return activateRouter(theEnvironment,theRouter.getName());
     }

   /*********************/
   /* deactivateRouter: */
   /*********************/
   private native boolean deactivateRouter(long env,String routerName);

   /*********************/
   /* deactivateRouter: */
   /*********************/
   public boolean deactivateRouter(
     Router theRouter)
     {
      return deactivateRouter(theEnvironment,theRouter.getName());
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
      printRouter(logName,printString);
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

   /***********************/
   /* incrementFactCount: */
   /***********************/
   private native void incrementFactCount(Environment javaEnv,long env,long fact);

   /***********************/
   /* decrementFactCount: */
   /***********************/
   private native void decrementFactCount(Environment javaEnv,long env,long fact);

   /***********************/
   /* incrementFactCount: */
   /***********************/
   public void incrementFactCount(
     FactAddressValue theFact)
     {
      incrementFactCount(theFact.getEnvironment(),
                         theFact.getEnvironment().getEnvironmentAddress(),
                         theFact.getFactAddress());
     }

   /***********************/
   /* decrementFactCount: */
   /***********************/
   public void decrementFactCount(
     FactAddressValue theFact)
     {
      decrementFactCount(theFact.getEnvironment(),
                         theFact.getEnvironment().getEnvironmentAddress(),
                         theFact.getFactAddress());
     }

   /***************************/
   /* incrementInstanceCount: */
   /***************************/
   private native void incrementInstanceCount(Environment javaEnv,long env,long instance);

   /***************************/
   /* decrementInstanceCount: */
   /***************************/
   private native void decrementInstanceCount(Environment javaEnv,long env,long instance);

   /***************************/
   /* incrementInstanceCount: */
   /***************************/
   public void incrementInstanceCount(
     InstanceAddressValue theInstance)
     {
      incrementInstanceCount(theInstance.getEnvironment(),
                             theInstance.getEnvironment().getEnvironmentAddress(),
                             theInstance.getInstanceAddress());
     }

   /***************************/
   /* decrementInstanceCount: */
   /***************************/
   public void decrementInstanceCount(
     InstanceAddressValue theInstance)
     {
      decrementInstanceCount(theInstance.getEnvironment(),
                             theInstance.getEnvironment().getEnvironmentAddress(),
                             theInstance.getInstanceAddress());
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

   /************/
   /* destroy: */
   /************/
   public void destroy()
     {
      for (Router router : routerList)
        { deleteRouter(router); }

      destroyEnvironment(theEnvironment);
     }
     
   /*********/
   /* main: */
   /*********/
   public static void main(String args[])
     {  
      Environment clips;

      clips = new Environment();
      
      clips.commandLoop();
     }  
  }
