#include "net_sf_clipsrules_jni_Environment.h"

#include "clipsjni_data.h"
#include "clipsjni_utilities.h"
#include "clipsjni_glue.h"

/******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getCLIPSVersion: Native */
/*   function for the CLIPSJNI getCLIPSVersion method.            */
/* Class:     CLIPSJNI_Environment                                */
/* Method:    getCLIPSVersion                                     */
/* Signature: ()Ljava/lang/String;                                */
/******************************************************************/
JNIEXPORT jstring JNICALL Java_net_sf_clipsrules_jni_Environment_getCLIPSVersion(
  JNIEnv *env, 
  jclass cls) 
  {
   return (*env)->NewStringUTF(env,VERSION_STRING);
  }

/*************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_createEnvironment: */
/*                                                           */
/*    Class:     net_sf_clipsrules_jni_Environment           */
/*    Method:    createEnvironment                           */
/*    Signature: ()J                                         */
/*************************************************************/
JNIEXPORT jlong JNICALL Java_net_sf_clipsrules_jni_Environment_createEnvironment(
  JNIEnv *env, 
  jobject obj)
  {
   return CreateCLIPSJNIEnvironment(env,obj);
  }

/********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_clear: Native */ 
/*   function for the CLIPSJNI clear method.            */
/*                                                      */
/* Class:     net_sf_clipsrules_jni_Environment         */
/* Method:    clear                                     */
/* Signature: (J)V                                      */
/********************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_clear(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   
   Clear(JLongToPointer(clipsEnv));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_reset: Native */ 
/*   function for the CLIPSJNI reset method.            */
/*                                                      */
/* Class:     net_sf_clipsrules_jni_Environment         */
/* Method:    reset                                     */
/* Signature: (J)V                                      */
/********************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_reset(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   
   Reset(JLongToPointer(clipsEnv));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/*******************************************************/
/* Java_net_sf_clipsrules_jni_Environment_load: Native */ 
/*   function for the CLIPSJNI load method.            */
/*                                                     */
/* Class:     net_sf_clipsrules_jni_Environment        */
/* Method:    load                                     */
/* Signature: (JLjava/lang/String;)V                   */
/*******************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_load(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring fileName)
  {
   const char *cFileName = (*env)->GetStringUTFChars(env,fileName,NULL);
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   void *theEnv;
   
   theEnv = JLongToPointer(clipsEnv);
   
   Load(theEnv,(char *) cFileName);
   
   (*env)->ReleaseStringUTFChars(env,fileName,cFileName);
   
   SetEnvironmentContext(theEnv,oldContext);
  }

/******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_changeDirectory: Native */ 
/*   function for the CLIPSJNI changeDirectory method.            */
/*                                                                */
/* Class:     net_sf_clipsrules_jni_Environment                   */
/* Method:    changeDirectory                                     */
/* Signature: (JLjava/lang/String;)I                              */
/******************************************************************/
JNIEXPORT jint JNICALL Java_net_sf_clipsrules_jni_Environment_changeDirectory(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring directory)
  {
   int rv;
   const char *cDirectory = (*env)->GetStringUTFChars(env,directory,NULL);
   
   rv = genchdir(cDirectory);
   
   (*env)->ReleaseStringUTFChars(env,directory,cDirectory);
   
   return rv;
  }

/*****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_loadFromString: Native */ 
/*   function for the CLIPSJNI loadFromString method.            */
/*                                                               */
/* Class:     net_sf_clipsrules_jni_Environment                  */
/* Method:    load                                               */
/* Signature: (JLjava/lang/String;)V                             */
/*****************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_loadFromString(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring loadString)
  {
   const char *cLoadString = (*env)->GetStringUTFChars(env,loadString,NULL);
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   void *theEnv;
   
   theEnv = JLongToPointer(clipsEnv);

   OpenStringSource(theEnv,"clipsjniloadfromstring",cLoadString,0); 
   LoadConstructsFromLogicalName(theEnv,"clipsjniloadfromstring");
   CloseStringSource(theEnv,"clipsjniloadfromstring");
   
   (*env)->ReleaseStringUTFChars(env,loadString,cLoadString);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/***************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_loadFromStringWithOutput: Native */ 
/*   function for the CLIPSJNI loadFromStringWithOutput method.            */
/*                                                                         */
/* Class:     net_sf_clipsrules_jni_Environment                            */
/* Method:    load                                                         */
/* Signature: (JLjava/lang/String;)V                                       */
/***************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_loadFromStringWithOutput(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring loadString)
  {
   const char *cLoadString = (*env)->GetStringUTFChars(env,loadString,NULL);
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   void *theEnv;
   
   theEnv = JLongToPointer(clipsEnv);

   OpenStringSource(theEnv,"clipsjniloadfromstringwo",cLoadString,0); 
   SetPrintWhileLoading(theEnv,true);
   LoadConstructsFromLogicalName(theEnv,"clipsjniloadfromstringwo");
   SetPrintWhileLoading(theEnv,false);
   CloseStringSource(theEnv,"clipsjniloadfromstringwo");
   
   (*env)->ReleaseStringUTFChars(env,loadString,cLoadString);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/*********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getParsingFileName: Native */
/*   function for the CLIPSJNI getParsingFileName method.            */
/*                                                                   */
/* Class:     net_sf_clipsrules_jni_Environment                      */
/* Method:    getParsingFileName                                     */
/* Signature: (J)Ljava/lang/String;                                  */
/*********************************************************************/
JNIEXPORT jstring JNICALL Java_net_sf_clipsrules_jni_Environment_getParsingFileName(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jstring rv;
   
   char *fileName;
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   fileName = GetParsingFileName(JLongToPointer(clipsEnv));
   
   if (fileName == NULL)
     { 
      rv = (*env)->NewStringUTF(env,""); 
      SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
      return rv;
     }
     
   rv = (*env)->NewStringUTF(env,fileName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }

/*********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setParsingFileName: Native */
/*   function for the CLIPSJNI setParsingFileName method.            */
/*                                                                   */
/* Class:     net_sf_clipsrules_jni_Environment                      */
/* Method:    setParsingFileName                                     */
/* Signature: (JLjava/lang/String;)V                                 */
/*********************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setParsingFileName(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring fileName)
  {
   const char *cFileName = (*env)->GetStringUTFChars(env,fileName,NULL);

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   SetParsingFileName(JLongToPointer(clipsEnv),(char *) cFileName);
   
   (*env)->ReleaseStringUTFChars(env,fileName,cFileName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/**********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_validWatchItem: */
/*   Native function for the CLIPSJNI validWatchItem      */
/*   method.                                              */
/*                                                        */
/* Class:     net_sf_clipsrules_jni_Environment           */
/* Method:    validWatchItem                              */
/* Signature: (JLjava/lang/String;)Z                      */
/**********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_validWatchItem(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring watchItem)
  {
   int rv;
   const char *cWatchItem = (*env)->GetStringUTFChars(env,watchItem,NULL);
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = GetWatchItem(JLongToPointer(clipsEnv),(char *) cWatchItem);
   
   (*env)->ReleaseStringUTFChars(env,watchItem,cWatchItem);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   if (rv == -1) return false;
   else return true;
  }

/***********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getWatchItem:    */
/*   Native function for the CLIPSJNI getWatchItem method. */
/*                                                         */
/* Class:     net_sf_clipsrules_jni_Environment            */
/* Method:    getWatchItem                                 */
/* Signature: (JLjava/lang/String;)Z                       */
/***********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_getWatchItem(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring watchItem)
  {
   int rv;
   const char *cWatchItem = (*env)->GetStringUTFChars(env,watchItem,NULL);
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = GetWatchItem(JLongToPointer(clipsEnv),(char *) cWatchItem);
   
   (*env)->ReleaseStringUTFChars(env,watchItem,cWatchItem);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   if (rv == 1) return true;
   else return false;
  }

/********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_watch: Native */ 
/*   function for the CLIPSJNI watch method.            */
/*                                                      */
/* Class:     net_sf_clipsrules_jni_Environment         */
/* Method:    watch                                     */
/* Signature: (JLjava/lang/String;)Z                    */
/********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_watch(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring watchItem)
  {
   jboolean rv;
   const char *cWatchItem = (*env)->GetStringUTFChars(env,watchItem,NULL);
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = WatchString(JLongToPointer(clipsEnv),(char *) cWatchItem);
   
   (*env)->ReleaseStringUTFChars(env,watchItem,cWatchItem);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }

/**********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_unwatch: Native */ 
/*   function for the CLIPSJNI unwatch method.            */
/*                                                        */
/* Class:     net_sf_clipsrules_jni_Environment           */
/* Method:    unwatch                                     */
/* Signature: (JLjava/lang/String;)Z                      */
/**********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_unwatch(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring watchItem)
  {
   jboolean rv;
   const char *cWatchItem = (*env)->GetStringUTFChars(env,watchItem,NULL);

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = UnwatchString(JLongToPointer(clipsEnv),(char *) cWatchItem);
   
   (*env)->ReleaseStringUTFChars(env,watchItem,cWatchItem);

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }

/*******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setHaltExecution: Native */ 
/*   function for the CLIPSJNI setHaltExecution method.            */
/*                                                                 */
/* Class:     net_sf_clipsrules_jni_Environment                    */
/* Method:    setHaltExecution                                     */
/* Signature: (JZ)V                                                */
/*******************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setHaltExecution(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   SetHaltExecution(JLongToPointer(clipsEnv),value);
  }

/**************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_eval: Native function for the   */
/*   CLIPSJNI eval method.                                                */
/*                                                                        */
/* Class:     net_sf_clipsrules_jni_Environment                           */
/* Method:    eval                                                        */
/* Signature: (JLjava/lang/String;)Lnet/sf/clipsrules/jni/PrimitiveValue; */
/*                                                                        */
/**************************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_eval(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring evalStr)
  {
   CLIPSValue theDO;
   jobject result = NULL;
   const char *cEvalStr;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   cEvalStr = (*env)->GetStringUTFChars(env,evalStr,NULL);
   
   Eval(theCLIPSEnv,(char *) cEvalStr,&theDO);

   (*env)->ReleaseStringUTFChars(env,evalStr,cEvalStr);
   
   result = ConvertDataObject(env,obj,theCLIPSEnv,&theDO);

   SetEnvironmentContext(theCLIPSEnv,oldContext);

   return result;  
  }

/********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_build: Native */
/*   function for the CLIPSJNI build method.            */
/*                                                      */
/* Class:     net_sf_clipsrules_jni_Environment         */
/* Method:    build                                     */
/* Signature: (JLjava/lang/String;)Z                    */
/********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_build(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jstring buildStr)
  {
   jboolean rv;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cBuildStr = (*env)->GetStringUTFChars(env,buildStr,NULL);

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   rv = (jboolean) Build(theCLIPSEnv,(char *) cBuildStr);

   (*env)->ReleaseStringUTFChars(env,buildStr,cBuildStr);

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }
  
/****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getModuleList: Native */
/*   function for the CLIPSJNI getModuleList method.            */
/*                                                              */
/* Class:     net_sf_clipsrules_jni_Environment                 */
/* Method:    getFocusStack                                     */
/* Signature: (J)Ljava/util/List;                               */
/*                                                              */
/****************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getModuleList(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   int moduleCount = 0;
   Defmodule *theModule;
   jobject arrayList, returnModule, moduleName;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);

   for (theModule = GetNextDefmodule(theCLIPSEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theCLIPSEnv,theModule))
     { moduleCount++; }
     
   arrayList = (*env)->NewObject(env,
                                 CLIPSJNIData(clipsEnv)->arrayListClass,
                                 CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                 (jint) moduleCount);
                                   
   if (arrayList == NULL)
     { return NULL; }
  
   for (theModule = GetNextDefmodule(theCLIPSEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theCLIPSEnv,theModule))
     {  
      moduleName = (*env)->NewStringUTF(env,theModule->header.name->contents);
      
      returnModule = (*env)->NewObject(env,
                                       CLIPSJNIData(clipsEnv)->moduleClass,
                                       CLIPSJNIData(clipsEnv)->moduleInitMethod,
                                       moduleName);
                                      
      (*env)->DeleteLocalRef(env,moduleName);

      if (returnModule != NULL)
        { 
         (*env)->CallBooleanMethod(env,arrayList,CLIPSJNIData(clipsEnv)->arrayListAddMethod,returnModule); 
         (*env)->DeleteLocalRef(env,returnModule);
        }
     }
     
   return arrayList;
  }
  
/***********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_incrementAddressCount: Native */
/*   function for the CLIPSJNI incrementAddressCount method.            */
/*                                                                      */
/* Class:     net_sf_clipsrules_jni_Environment                         */
/* Method:    incrementAddressCount                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V                  */
/************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_incrementAddressCount(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsExternalAddress)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   RetainExternalAddress(JLongToPointer(clipsEnv),JLongToPointer(clipsExternalAddress));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }
  
/************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_decrementAddressCount: Native */
/*   function for the CLIPSJNI decrementAddressCount method.            */
/*                                                                      */
/* Class:     net_sf_clipsrules_jni_Environment                         */
/* Method:    decrementFactCount                                        */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V                  */
/************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_decrementAddressCount(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsExternalAddress)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   ReleaseExternalAddress(JLongToPointer(clipsEnv),JLongToPointer(clipsExternalAddress));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }
   
/*********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_destroyEnvironment: Native */
/*   function for the CLIPSJNI destroyEnvironment method.            */
/*                                                                   */
/* Class:     net_sf_clipsrules_jni_Environment                      */
/* Method:    destroyEnvironment                                     */
/* Signature: (J)V                                                   */
/*********************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_destroyEnvironment(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   DestroyEnvironment(JLongToPointer(clipsEnv));
  }

/*****************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_addPeriodicCallbackEnabled: Native */
/*   function for the CLIPSJNI setPeriodicCallbackEnabled method.            */
/*                                                                           */
/* Class:     net_sf_clipsrules_jni_Environment                              */
/* Method:    setPeriodicCallbackEnabled                                     */
/* Signature: (JZ)V                                                          */
/*****************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setPeriodicCallbackEnabled(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   EnablePeriodicFunctions(JLongToPointer(clipsEnv),value);
  }

/******************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_addPeriodicCallback: Native         */
/*   function for the CLIPSJNI addPeriodicCallback method.                    */
/*                                                                            */
/* Class:     net_sf_clipsrules_jni_Environment                               */
/* Method:    addPeriodicCallback                                             */
/* Signature: (JLjava/lang/String;ILnet/sf/clipsrules/jni/PeriodicCallback;)V */
/******************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_addPeriodicCallback(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring listenerName, 
  jint priority, 
  jobject context)
  {
   jobject nobj;   
   const char *cListenerName = (*env)->GetStringUTFChars(env,listenerName,NULL);

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env); 
      
   nobj = (*env)->NewGlobalRef(env,context);
   
   AddPeriodicFunction(JLongToPointer(clipsEnv),(char *) cListenerName,
                       JNIPeriodicCallback,
                       (int) priority,(void *) nobj);
   
   (*env)->ReleaseStringUTFChars(env,listenerName,cListenerName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext); 
  }
  
/*************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_removePeriodicCallback: Native */
/*   function for the CLIPSJNI removePeriodicCallback method.            */
/*                                                                       */
/* Class:     net_sf_clipsrules_jni_Environment                          */
/* Method:    removePeriodicCallback                                     */
/* Signature: (JLjava/lang/String;)Z                                     */
/*************************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_removePeriodicCallback(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring listenerName) 
  {
   int rv;
   void *periodicContext;
   const char *cListenerName = (*env)->GetStringUTFChars(env,listenerName,NULL);

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env); 

   periodicContext = GetPeriodicFunctionContext(JLongToPointer(clipsEnv),cListenerName);
   
   rv = RemovePeriodicFunction(JLongToPointer(clipsEnv),cListenerName);
   
   if (periodicContext != NULL)
     { (*env)->DeleteGlobalRef(env,periodicContext); }
       
   (*env)->ReleaseStringUTFChars(env,listenerName,cListenerName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext); 

   return rv;
  }

/******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_addUserFunction: Native */
/*   function for the CLIPSJNI addUserFunction method.            */
/*                                                                */
/* Class:     net_sf_clipsrules_jni_Environment                   */
/* Method:    addUserFunction                                     */
/* Signature: (J                                                  */
/*             Ljava/lang/String;Ljava/lang/String;               */
/*             Lnet/sf/clipsrules/jni/UserFunction;)I             */
/******************************************************************/
JNIEXPORT jint JNICALL Java_net_sf_clipsrules_jni_Environment_addUserFunction(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring functionName, 
  jstring returnTypes, 
  jint minArgs,
  jint maxArgs,
  jstring restrictions, 
  jobject context)
  {
   int rv;
   jobject nobj;   

   const char *cFunctionName = (*env)->GetStringUTFChars(env,functionName,NULL);
   const char *cRestrictions;
   const char *cReturnTypes;

   if (restrictions != NULL)
     { cRestrictions = (*env)->GetStringUTFChars(env,restrictions,NULL); }
   else 
     { cRestrictions = NULL; }

   if (returnTypes != NULL)
     { cReturnTypes = (*env)->GetStringUTFChars(env,returnTypes,NULL); }
   else 
     { cReturnTypes = NULL; }

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env); 

   nobj = (*env)->NewGlobalRef(env,context);
 
   if (minArgs < 0)
     { minArgs = 0; }
     
   if (maxArgs < 0)
     { maxArgs = UNBOUNDED; }
     
   rv = AddUDF(JLongToPointer(clipsEnv),cFunctionName,"*",(unsigned short) minArgs,(unsigned short) maxArgs,
                  cRestrictions,JNIUserFunction,"JNIUserFunction",nobj);

   (*env)->ReleaseStringUTFChars(env,functionName,cFunctionName);

   if (restrictions != NULL)
     { (*env)->ReleaseStringUTFChars(env,restrictions,cRestrictions); }
     
   if (returnTypes != NULL)
     { (*env)->ReleaseStringUTFChars(env,returnTypes,cReturnTypes); }
                                     
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext); 

   return rv;
  }

/*********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_removeUserFunction: Native */
/*   function for the CLIPSJNI removeUserFunction method.            */
/*                                                                   */
/* Class:     net_sf_clipsrules_jni_Environment                      */
/* Method:    removeUserFunction                                     */
/* Signature: (JLjava/lang/String;)Z                                 */
/*********************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_removeUserFunction(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring functionName)
  {
   int rv;
   jobject context;
   void *theEnv;
   
   theEnv = JLongToPointer(clipsEnv);

   const char *cFunctionName = (*env)->GetStringUTFChars(env,functionName,NULL);

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env); 

   context = GetUDFContext(theEnv,cFunctionName);
   if (context != NULL)
     {  (*env)->DeleteGlobalRef(env,context); }

   rv = RemoveUDF(JLongToPointer(clipsEnv),cFunctionName);
   
   (*env)->ReleaseStringUTFChars(env,functionName,cFunctionName);
                                     
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext); 

   return rv;
  }
  
/***********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setErrorCallback */
/* Class:     net_sf_clipsrules_jni_Environment            */
/* Method:    setErrorCallback                             */
/* Signature: (JZ)Z                                        */
/***********************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setErrorCallback(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   Environment *theCLIPSEnv = (Environment *) JLongToPointer(clipsEnv);
   
   if (value)
     { SetParserErrorCallback(theCLIPSEnv,JNIParserErrorCallback,NULL); }
    else
     { SetParserErrorCallback(theCLIPSEnv,NULL,NULL); }
  }
   

