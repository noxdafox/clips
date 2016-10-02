
#include "net_sf_clipsrules_jni_Environment.h"

#include "clipsjni_data.h"
#include "clipsjni_utilities.h"
#include "clipsjni_glue.h"

/********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getInstanceScopes: Native */
/*   function for the CLIPSJNI getInstanceScopes method.            */
/*                                                                  */
/* Class:     net_sf_clipsrules_jni_Environment                     */
/* Method:    getInstanceScopes                                     */
/* Signature: (J)Ljava/util/HashMap;                                */
/*                                                                  */
/********************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getInstanceScopes(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jobject scopeMap, moduleSet, theDefclassIndex;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   struct defmodule *theModule;
   struct defclassModule *theModuleItem;
   struct defclass *theDefclass;
   jint moduleCount = 0, whichBit;
   CLIPSBitMap *theScopeMap;

   scopeMap = (*env)->NewObject(env,
                                CLIPSJNIData(theCLIPSEnv)->hashMapClass,
                                CLIPSJNIData(theCLIPSEnv)->hashMapInitMethod);

   if (scopeMap == NULL) return NULL;

   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theCLIPSEnv,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theCLIPSEnv,theModule))
     { moduleCount++; }
        
   for (theModule = (struct defmodule *) EnvGetNextDefmodule(theCLIPSEnv,NULL);
        theModule != NULL;
        theModule = (struct defmodule *) EnvGetNextDefmodule(theCLIPSEnv,theModule))
     { 
      theModuleItem = (struct defclassModule *) 
                      GetModuleItem(theCLIPSEnv,theModule,DefclassData(theCLIPSEnv)->DefclassModuleIndex);

      for (theDefclass = (struct defclass *) theModuleItem->header.firstItem;
           theDefclass != NULL;
           theDefclass = (struct defclass *) EnvGetNextDefclass(theCLIPSEnv,theDefclass))
        { 
         if (theDefclass->instanceList != NULL)
           { 
            theDefclassIndex = (*env)->NewObject(env,
                                CLIPSJNIData(theCLIPSEnv)->longClass,
                                CLIPSJNIData(theCLIPSEnv)->longInitMethod,
                                (jlong) theDefclass);

            moduleSet = (*env)->NewObject(env,
                                CLIPSJNIData(theCLIPSEnv)->bitSetClass,
                                CLIPSJNIData(theCLIPSEnv)->bitSetInitMethod,
                                moduleCount);
                                
            theScopeMap = (CLIPSBitMap *) CreateClassScopeMap(theCLIPSEnv,theDefclass);
         
            for (whichBit = 0; whichBit < moduleCount; whichBit++)
              {
               if (TestBitMap(theScopeMap->contents,whichBit))
                 { (*env)->CallVoidMethod(env,moduleSet,CLIPSJNIData(clipsEnv)->bitSetSetMethod,whichBit); }
              }
              
            (*env)->CallObjectMethod(env,scopeMap,CLIPSJNIData(clipsEnv)->hashMapPutMethod,theDefclassIndex,moduleSet); 

            (*env)->DeleteLocalRef(env,theDefclassIndex);
            (*env)->DeleteLocalRef(env,moduleSet);
           }
        }
     }

   return scopeMap;
  }

/******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getInstanceList: Native */
/*   function for the CLIPSJNI getInstanceList method.            */
/*                                                                */
/* Class:     net_sf_clipsrules_jni_Environment                   */
/* Method:    getInstanceList                                     */
/* Signature: (J)Ljava/util/List;                                 */
/*                                                                */
/******************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getInstanceList(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   struct instance *instancePtr;
   int instanceCount = 0;
   int i;
   jobject arrayList, slotValueList, javaSlotValueObject, theJavaInstance, instanceName, instanceClass;
   jobject theJavaSlotName, theJavaSlotValue;
   CLIPSValue slotNames;
   CLIPSValue slotValue;
   CLIPSValue defaultValue;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   void *theClass;
  
   for (instancePtr = EnvGetNextInstance(theCLIPSEnv,NULL);
        instancePtr != NULL;
        instancePtr = EnvGetNextInstance(theCLIPSEnv,instancePtr))
     { instanceCount++; }
     
   arrayList = (*env)->NewObject(env,
                                 CLIPSJNIData(clipsEnv)->arrayListClass,
                                 CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                 (jint) instanceCount);
                                   
   if (arrayList == NULL)
     { return NULL; }
     
   for (instancePtr = EnvGetNextInstance(theCLIPSEnv,NULL);
        instancePtr != NULL;
        instancePtr = EnvGetNextInstance(theCLIPSEnv,instancePtr))
     {
      theClass = EnvGetInstanceClass(theCLIPSEnv,instancePtr);
      
      EnvClassSlots(theCLIPSEnv,theClass,&slotNames,true);
   
      slotValueList = (*env)->NewObject(env,
                                        CLIPSJNIData(clipsEnv)->arrayListClass,
                                        CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                        (jint) GetDOLength(slotNames));

      if (slotValueList == NULL)
        { break; }

      for (i = 1; i <= GetDOLength(slotNames); i++)
        {
         const char *theCSlotName, *theCSlotValue;
         jboolean defaulted = false;
         
         theCSlotName = slotNames.multifieldValue->theFields[i].lexemeValue->contents;
         
         EnvDirectGetSlot(theCLIPSEnv,instancePtr,slotNames.multifieldValue->theFields[i].lexemeValue->contents,&slotValue);

         if (EnvSlotDefaultP(theCLIPSEnv,theClass,theCSlotName) == STATIC_DEFAULT)
           {
            EnvSlotDefaultValue(theCLIPSEnv,theClass,theCSlotName,&defaultValue);
                                           
            if (DOsEqual(&slotValue,&defaultValue))
              { defaulted = true; }
           }

         theCSlotValue = DataObjectToString(theCLIPSEnv,&slotValue);

         theJavaSlotName = (*env)->NewStringUTF(env,theCSlotName);
         theJavaSlotValue = (*env)->NewStringUTF(env,theCSlotValue);

         javaSlotValueObject = (*env)->NewObject(env,
                                        CLIPSJNIData(clipsEnv)->slotValueClass,
                                        CLIPSJNIData(clipsEnv)->slotValueInitMethod,
                                        theJavaSlotName,theJavaSlotValue,defaulted);
                                        
         (*env)->DeleteLocalRef(env,theJavaSlotName);
         (*env)->DeleteLocalRef(env,theJavaSlotValue);
         
         if (javaSlotValueObject != NULL)
           { 
            (*env)->CallBooleanMethod(env,slotValueList,CLIPSJNIData(clipsEnv)->arrayListAddMethod,javaSlotValueObject); 
            (*env)->DeleteLocalRef(env,javaSlotValueObject);
           }
        }

      instanceName = (*env)->NewStringUTF(env,EnvGetInstanceName(theCLIPSEnv,instancePtr));
      instanceClass = (*env)->NewStringUTF(env,EnvGetDefclassName(theCLIPSEnv,theClass));

      theJavaInstance = (*env)->NewObject(env,
                                      CLIPSJNIData(clipsEnv)->factInstanceClass,
                                      CLIPSJNIData(clipsEnv)->factInstanceInitMethod,
                                      ((jlong) theClass), 
                                      instanceName,instanceClass,slotValueList);
                                      
      (*env)->DeleteLocalRef(env,slotValueList);
      (*env)->DeleteLocalRef(env,instanceName);
      (*env)->DeleteLocalRef(env,instanceClass);

      if (theJavaInstance != NULL)
        { 
         (*env)->CallBooleanMethod(env,arrayList,CLIPSJNIData(clipsEnv)->arrayListAddMethod,theJavaInstance); 
         (*env)->DeleteLocalRef(env,theJavaInstance);
        }
     }

   return arrayList;
  }

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

/**************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getInstancesChanged */
/* Class:     net_sf_clipsrules_jni_Environment               */
/* Method:    getInstancesChanged                             */
/* Signature: (J)Z                                            */
/**************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_getInstancesChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jboolean rv;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   rv = EnvGetInstancesChanged(theCLIPSEnv);
   return rv;
  } 

/**************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setInstancesChanged */
/* Class:     net_sf_clipsrules_jni_Environment               */
/* Method:    setInstancesChanged                             */
/* Signature: (JZ)Z                                           */
/**************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setInstancesChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   EnvSetInstancesChanged(theCLIPSEnv,value);
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
   CLIPSValue theDO;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cSlotName = (*env)->GetStringUTFChars(env,slotName,NULL);

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   EnvDirectGetSlot(JLongToPointer(clipsEnv),JLongToPointer(clipsInstance),(char *) cSlotName,&theDO);

   (*env)->ReleaseStringUTFChars(env,slotName,cSlotName);
   
   rv = ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }
  
/******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getDefclassText: Native */ 
/*   function for the CLIPSJNI getDefclassText method.            */
/*                                                                */
/* Class:     net_sf_clipsrules_jni_Environment                   */
/* Method:    getDefclassText                                     */
/* Signature: (JJ)Ljava/lang/String;                              */
/******************************************************************/
JNIEXPORT jstring JNICALL Java_net_sf_clipsrules_jni_Environment_getDefclassText(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jlong defclassLong)
  {
   void *defclassPtr = JLongToPointer(defclassLong);
   void *theEnv = JLongToPointer(clipsEnv);
   const char *defclassText = NULL;
      
   if (defclassPtr != NULL)
     { defclassText = EnvGetDefclassPPForm(theEnv,defclassPtr); }   

   if (defclassText == NULL)
     { return (*env)->NewStringUTF(env,""); }
   else     
     { return (*env)->NewStringUTF(env,defclassText); }   
  }

