
#include "net_sf_clipsrules_jni_Environment.h"

#include "clipsjni_data.h"
#include "clipsjni_utilities.h"
#include "clipsjni_glue.h"

/********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_addRouter: Native         */
/*   function for the CLIPSJNI addRouter method.                    */
/*                                                                  */
/* Class:     net_sf_clipsrules_jni_Environment                     */
/* Method:    addRouter                                             */
/* Signature: (JLjava/lang/String;ILnet/sf/clipsrules/jni/Router;)Z */
/********************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_addRouter(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring routerName, 
  jint priority, 
  jobject context)
  {
   int rv;
   jobject nobj;   
   const char *cRouterName = (*env)->GetStringUTFChars(env,routerName,NULL);

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env); 
      
   nobj = (*env)->NewGlobalRef(env,context);
   
   rv = AddRouter(JLongToPointer(clipsEnv),(char *) cRouterName,(int) priority,
                  QueryJNICallback,WriteJNICallback,ReadJNICallback,
                  UnreadJNICallback,ExitJNICallback,(void *) nobj);
   
   (*env)->ReleaseStringUTFChars(env,routerName,cRouterName);

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext); 

   return rv;
  }

/***************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_deleteRouter: Native */
/*   function for the CLIPSJNI deleteRouter method.            */
/*                                                             */
/* Class:     net_sf_clipsrules_jni_Environment                */
/* Method:    deleteRouter                                     */
/* Signature: (JLjava/lang/String;)Z                           */
/***************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_deleteRouter(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring routerName)
  {
   int rv;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cRouterName = (*env)->GetStringUTFChars(env,routerName,NULL);
   Router *theRouter;
   
   theRouter = FindRouter(theCLIPSEnv,cRouterName);
   if (theRouter == NULL) 
     { return 0; }

   (*env)->DeleteGlobalRef(env,theRouter->context);
     
   rv = DeleteRouter(theCLIPSEnv,cRouterName);
  
   (*env)->ReleaseStringUTFChars(env,routerName,cRouterName);

   return rv;
  }

/************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_printRouterExists */
/* Class:     net_sf_clipsrules_jni_Environment             */
/* Method:    printRouterExists                             */
/* Signature: (JLjava/lang/String;)Z                        */
/************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_printRouterExists(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring logName)
  {
   const char *cLogName;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   bool rv;
  
   cLogName = (*env)->GetStringUTFChars(env,logName,NULL);
   
   rv = PrintRouterExists(theCLIPSEnv,cLogName);
   
   (*env)->ReleaseStringUTFChars(env,logName,cLogName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }
  
/*******************************************************/
/* Java_net_sf_clipsrules_jni_Environment_printString  */
/* Class:     net_sf_clipsrules_jni_Environment        */
/* Method:    printString                              */
/* Signature: (JLjava/lang/String;Ljava/lang/String;)V */
/*******************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_printString(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring logName, 
  jstring printString)
  {
   const char *cLogName;
   const char *cPrintString;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   cLogName = (*env)->GetStringUTFChars(env,logName,NULL);
   cPrintString = (*env)->GetStringUTFChars(env,printString,NULL);

   WriteString(theCLIPSEnv,cLogName,cPrintString);

   (*env)->ReleaseStringUTFChars(env,logName,cLogName);
   (*env)->ReleaseStringUTFChars(env,printString,cPrintString);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/*********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_activateRouter */
/* Class:     net_sf_clipsrules_jni_Environment          */
/* Method:    activateRouter                             */
/* Signature: (JLjava/lang/String;)Z                     */
/*********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_activateRouter(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring routerName)
  {
   jboolean rv;
   const char *cRouterName;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   cRouterName = (*env)->GetStringUTFChars(env,routerName,NULL);

   rv = ActivateRouter(theCLIPSEnv,cRouterName);

   (*env)->ReleaseStringUTFChars(env,routerName,cRouterName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }

/***********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_deactivateRouter */
/* Class:     net_sf_clipsrules_jni_Environment            */
/* Method:    deactivateRouter                             */
/* Signature: (JLjava/lang/String;)Z                       */
/***********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_deactivateRouter(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring routerName)
  {
   jboolean rv;
   const char *cRouterName;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   cRouterName = (*env)->GetStringUTFChars(env,routerName,NULL);

   rv = DeactivateRouter(theCLIPSEnv,cRouterName);

   (*env)->ReleaseStringUTFChars(env,routerName,cRouterName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }

/**********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_openStringBatch */
/* Class:     net_sf_clipsrules_jni_Environment           */
/* Method:    openStringBatch                             */
/* Signature: (JLjava/lang/String;Ljava/lang/String;Z)Z   */
/**********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_openStringBatch(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring stringName,
  jstring data,
  jboolean placeAtEnd)
  {
   jboolean rv;
   const char *cStringName, *cData, *dataCopy;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   cStringName = (*env)->GetStringUTFChars(env,stringName,NULL);
   cData = (*env)->GetStringUTFChars(env,data,NULL);

   dataCopy = CopyString(theCLIPSEnv,cData); // TBD Move copy to OpenStringBatch
   rv = OpenStringBatch(theCLIPSEnv,cStringName,dataCopy,placeAtEnd);
   
   (*env)->ReleaseStringUTFChars(env,stringName,cStringName);
   (*env)->ReleaseStringUTFChars(env,data,cData);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }


