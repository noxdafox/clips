
#include "net_sf_clipsrules_jni_Environment.h"

#include "clipsjni_data.h"
#include "clipsjni_utilities.h"
#include "clipsjni_glue.h"

/********************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_makeInstance: Native function for the */
/*   CLIPSJNI makeInstance method.                                              */
/*                                                                              */
/* Class:     net_sf_clipsrules_jni_Environment                                 */
/* Method:    makeInstance                                                      */
/* Signature: (JLjava/lang/String;)Lnet/sf/clipsrules/jni/InstanceAddressValue; */
/********************************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_makeInstance(
  JNIEnv *env,
  jobject obj, 
  jlong clipsEnv, 
  jstring instanceStr)
  {
   jobject rv;
   void *theInstance;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cInstanceStr = (*env)->GetStringUTFChars(env,instanceStr,NULL);
   
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   theInstance = EnvMakeInstance(theCLIPSEnv,(char *) cInstanceStr);

   (*env)->ReleaseStringUTFChars(env,instanceStr,cInstanceStr);
   
   if (theInstance == NULL)
     {
      SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
      return(NULL); 
     }
     
   rv = ConvertSingleFieldValue(env,obj,theCLIPSEnv,INSTANCE_ADDRESS,theInstance);
      
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }

/************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getInstanceName: Native       */
/*   function for the CLIPSJNI getInstanceName method.                  */
/*                                                                      */
/* Class:     net_sf_clipsrules_jni_Environment                         */
/* Method:    getInstanceName                                           */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)Ljava/lang/String; */
/************************************************************************/
JNIEXPORT jstring JNICALL Java_net_sf_clipsrules_jni_Environment_getInstanceName(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsInstance)
  {
   jstring rv;
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = (*env)->NewStringUTF(env,EnvGetInstanceName(JLongToPointer(clipsEnv),JLongToPointer(clipsInstance)));

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }

/*************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_incrementInstanceCount: Native */
/*   function for the CLIPSJNI incrementInstanceCount method.            */
/*                                                                       */
/*                                                                       */
/* Class:     net_sf_clipsrules_jni_Environment                          */
/* Method:    incrementInstanceCount                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V                   */
/*************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_incrementInstanceCount(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsInstance)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   EnvIncrementInstanceCount(JLongToPointer(clipsEnv),JLongToPointer(clipsInstance));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }
  
/*************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_decrementInstanceCount: Native */
/*   function for the CLIPSJNI decrementInstanceCount method.            */
/*                                                                       */
/*                                                                       */
/* Class:     net_sf_clipsrules_jni_Environment                          */
/* Method:    decrementInstanceCount                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V                   */
/*************************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_decrementInstanceCount(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsInstance)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   EnvDecrementInstanceCount(JLongToPointer(clipsEnv),JLongToPointer(clipsInstance));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/*************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_directGetSlot: Native function */
/*   for the CLIPSJNI directGetSlot method.                              */
/*                                                                       */
/* Class:     net_sf_clipsrules_jni_Environment                          */
/* Method:    directGetSlot                                              */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJLjava/lang/String;)  */
/*            Lnet/sf/clipsrules/jni/PrimitiveValue;                     */
/*************************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_directGetSlot(
  JNIEnv *env,
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv,
  jlong clipsInstance,
  jstring slotName)
  {
   jobject rv;
   DATA_OBJECT theDO;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cSlotName = (*env)->GetStringUTFChars(env,slotName,NULL);

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   EnvDirectGetSlot(JLongToPointer(clipsEnv),JLongToPointer(clipsInstance),(char *) cSlotName,&theDO);

   (*env)->ReleaseStringUTFChars(env,slotName,cSlotName);
   
   rv = ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }
