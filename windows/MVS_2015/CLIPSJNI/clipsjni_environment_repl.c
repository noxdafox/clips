
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
   SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   CommandLoop(JLongToPointer(clipsEnv));
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
   SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   CommandLoopOnceThenBatch(JLongToPointer(clipsEnv));
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
   SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   CommandLoopBatchDriver(JLongToPointer(clipsEnv));
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
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   PrintBanner(JLongToPointer(clipsEnv));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
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
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   PrintPrompt(JLongToPointer(clipsEnv));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
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
   jstring rv;
   
   char *command;
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   command = GetCommandString(JLongToPointer(clipsEnv));
   
   if (command == NULL)
     { 
      rv = (*env)->NewStringUTF(env,""); 
      SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
      return rv;
     }
     
   rv = (*env)->NewStringUTF(env,GetCommandString(JLongToPointer(clipsEnv)));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
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
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   FlushCommandString(JLongToPointer(clipsEnv));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
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
   const char *cCommand = (*env)->GetStringUTFChars(env,command,NULL);

   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   SetCommandString(JLongToPointer(clipsEnv),(char *) cCommand);
   
   (*env)->ReleaseStringUTFChars(env,command,cCommand);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
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
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   
   rv = RouterData(JLongToPointer(clipsEnv))->CommandBufferInputCount;

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

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
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = RouterData(JLongToPointer(clipsEnv))->CommandBufferInputCount;
   
   RouterData(JLongToPointer(clipsEnv))->CommandBufferInputCount = (size_t) theValue;

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

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
   const char *cCommandString = (*env)->GetStringUTFChars(env,commandString,NULL);
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   
   AppendCommandString(JLongToPointer(clipsEnv),(char *) cCommandString);

   (*env)->ReleaseStringUTFChars(env,commandString,cCommandString);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
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
   const char *cCommandString = (*env)->GetStringUTFChars(env,commandString,NULL);
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   
   AppendDribble(JLongToPointer(clipsEnv),(char *) cCommandString);

   (*env)->ReleaseStringUTFChars(env,commandString,cCommandString);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
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
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
   
   ExpandCommandString(JLongToPointer(clipsEnv),(int) theChar);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
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
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);
  
   rv = CommandCompleteAndNotEmpty(JLongToPointer(clipsEnv));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }

