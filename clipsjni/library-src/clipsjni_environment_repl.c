
#include "net_sf_clipsrules_jni_Environment.h"

#include "clipsjni_data.h"
#include "clipsjni_utilities.h"
#include "clipsjni_glue.h"

/**************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_commandLoop: Native */
/*   function for the CLIPSJNI commandLoop method.            */
/*                                                            */
/* Class:     net_sf_clipsrules_jni_Environment               */
/* Method:    commandLoop                                     */
/* Signature: (J)V                                            */
/**************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_commandLoop(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   SetEnvironmentContext(theCLIPSEnv,(void *) env);

   CommandLoop(theCLIPSEnv);
  }

/***************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_commandLoopOnceThenBatch: Native */
/*   function for the CLIPSJNI commandLoopOnceThenBatch method.            */
/*                                                                         */
/* Class:     net_sf_clipsrules_jni_Environment                            */
/* Method:    commandLoopOnceThenBatch                                     */
/* Signature: (J)V                                                         */
/***************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_commandLoopOnceThenBatch(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   SetEnvironmentContext(theCLIPSEnv,(void *) env);

   CommandLoopOnceThenBatch(theCLIPSEnv);
  }

/*************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_commandLoopBatchDriver: Native */
/*   function for the CLIPSJNI commandLoopBatchDriver method.            */
/*                                                                       */
/* Class:     net_sf_clipsrules_jni_Environment                          */
/* Method:    commandLoopOnceThenBatch                                   */
/* Signature: (J)V                                                       */
/*************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_commandLoopBatchDriver(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   SetEnvironmentContext(theCLIPSEnv,(void *) env);

   CommandLoopBatchDriver(theCLIPSEnv);
  }

/**************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_printBanner: Native */
/*   function for the CLIPSJNI printBanner method.            */
/*                                                            */
/* Class:     net_sf_clipsrules_jni_Environment               */
/* Method:    printBanner                                     */
/* Signature: (J)V                                            */
/**************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_printBanner(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   PrintBanner(theCLIPSEnv);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
  }

/**************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_printPrompt: Native */
/*   function for the CLIPSJNI printPrompt method.            */
/*                                                            */
/* Class:     net_sf_clipsrules_jni_Environment               */
/* Method:    printPrompt                                     */
/* Signature: (J)V                                            */
/**************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_printPrompt(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   PrintPrompt(theCLIPSEnv);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
  }

/*****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getInputBuffer: Native */
/*   function for the CLIPSJNI getInputBuffer method.            */
/*                                                               */
/* Class:     net_sf_clipsrules_jni_Environment                  */
/* Method:    getInputBuffer                                     */
/* Signature: (J)Ljava/lang/String;                              */
/*****************************************************************/
JNIEXPORT jstring JNICALL Java_net_sf_clipsrules_jni_Environment_getInputBuffer(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   jstring rv;
   
   char *command;
   
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   command = GetCommandString(theCLIPSEnv);
   
   if (command == NULL)
     { 
      rv = (*env)->NewStringUTF(env,""); 
      SetEnvironmentContext(theCLIPSEnv,oldContext);
      return rv;
     }
     
   rv = (*env)->NewStringUTF(env,GetCommandString(theCLIPSEnv));
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
   
   return rv;
  }
  
/*******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_flushInputBuffer: Native */
/*   function for the CLIPSJNI flushInputBuffer method.            */
/*                                                                 */
/* Class:     net_sf_clipsrules_jni_Environment                    */
/* Method:    flushInputBuffer                                     */
/* Signature: (J)V                                                 */
/*******************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_flushInputBuffer(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   FlushCommandString(theCLIPSEnv);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
  }

/*****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setInputBuffer: Native */
/*   function for the CLIPSJNI setInputBuffer method.            */
/*                                                               */
/* Class:     net_sf_clipsrules_jni_Environment                  */
/* Method:    setInputBuffer                                     */
/* Signature: (JLjava/lang/String;)V                             */
/*****************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setInputBuffer(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring command)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cCommand = (*env)->GetStringUTFChars(env,command,NULL);

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   SetCommandString(theCLIPSEnv,(char *) cCommand);
   
   (*env)->ReleaseStringUTFChars(env,command,cCommand);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
  }

/**********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getInputBufferCount: Native */
/*   function for the CLIPSJNI getInputBufferCount method.            */
/*                                                                    */
/* Class:     net_sf_clipsrules_jni_Environment                       */
/* Method:    getInputBufferCount                                     */
/* Signature: (J)J                                                    */
/**********************************************************************/
JNIEXPORT jlong JNICALL Java_net_sf_clipsrules_jni_Environment_getInputBufferCount(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv)
  {
   jlong rv;   
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   rv = RouterData(theCLIPSEnv)->CommandBufferInputCount;

   SetEnvironmentContext(theCLIPSEnv,oldContext);

   return rv;
  }

/**********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setInputBufferCount: Native */
/*   function for the CLIPSJNI setInputBufferCount method.            */
/*                                                                    */
/* Class:     net_sf_clipsrules_jni_Environment                       */
/* Method:    setInputBufferCount                                     */
/* Signature: (JJ)J                                                   */
/**********************************************************************/
JNIEXPORT jlong JNICALL Java_net_sf_clipsrules_jni_Environment_setInputBufferCount(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jlong theValue)
  {
   jlong rv;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);   
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   rv = RouterData(theCLIPSEnv)->CommandBufferInputCount;
   
   RouterData(theCLIPSEnv)->CommandBufferInputCount = (size_t) theValue;

   SetEnvironmentContext(theCLIPSEnv,oldContext);

   return rv;
  }

/********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_appendInputBuffer: Native */
/*   function for the CLIPSJNI appendInputBuffer method.            */
/*                                                                  */
/* Class:     net_sf_clipsrules_jni_Environment                     */
/* Method:    appendInputBuffer                                     */
/* Signature: (JLjava/lang/String;)V                                */
/********************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_appendInputBuffer(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring commandString)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cCommandString = (*env)->GetStringUTFChars(env,commandString,NULL);
   
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   AppendCommandString(theCLIPSEnv,(char *) cCommandString);

   (*env)->ReleaseStringUTFChars(env,commandString,cCommandString);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
  }

/****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_appendDribble: Native */
/*   function for the CLIPSJNI appendDribble method.            */
/*                                                              */
/* Class:     net_sf_clipsrules_jni_Environment                 */
/* Method:    appendDribble                                     */
/* Signature: (JLjava/lang/String;)V                            */
/****************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_appendDribble(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring commandString)
  {  
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cCommandString = (*env)->GetStringUTFChars(env,commandString,NULL);
   
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   AppendDribble(theCLIPSEnv,(char *) cCommandString);

   (*env)->ReleaseStringUTFChars(env,commandString,cCommandString);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
  }

/********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_expandInputBuffer: Native */
/*   function for the CLIPSJNI expandInputBuffer method.            */
/*                                                                  */
/* Class:     net_sf_clipsrules_jni_Environment                     */
/* Method:    expandInputBuffer                                     */
/* Signature: (JC)V                                                 */
/********************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_expandInputBuffer(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jint theChar)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   ExpandCommandString(theCLIPSEnv,(int) theChar);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
  }

/*****************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_inputBufferContainsCommand: Native */
/*   function for the CLIPSJNI inputBufferContainsCommand method.            */
/*                                                                           */
/* Class:     net_sf_clipsrules_jni_Environment                              */
/* Method:    inputBufferContainsCommand                                     */
/* Signature: (J)Z                                                           */
/*****************************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_inputBufferContainsCommand(
  JNIEnv *env,
  jobject obj, 
  jlong clipsEnv)
  {
   jboolean rv;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
  
   rv = CommandCompleteAndNotEmpty(theCLIPSEnv);
   
   SetEnvironmentContext(theCLIPSEnv,oldContext);
   
   return rv;
  }

