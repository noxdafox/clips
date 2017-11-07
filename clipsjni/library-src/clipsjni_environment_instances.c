
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
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   Defmodule *theModule;
   struct defclassModule *theModuleItem;
   Defclass *theDefclass;
   jint moduleCount = 0, whichBit;
   CLIPSBitMap *theScopeMap;

   scopeMap = (*env)->NewObject(env,
                                CLIPSJNIData(theCLIPSEnv)->hashMapClass,
                                CLIPSJNIData(theCLIPSEnv)->hashMapInitMethod);

   if (scopeMap == NULL) return NULL;

   for (theModule = GetNextDefmodule(theCLIPSEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theCLIPSEnv,theModule))
     { moduleCount++; }
        
   for (theModule = GetNextDefmodule(theCLIPSEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theCLIPSEnv,theModule))
     { 
      theModuleItem = (struct defclassModule *) 
                      GetModuleItem(theCLIPSEnv,theModule,DefclassData(theCLIPSEnv)->DefclassModuleIndex);

      for (theDefclass = (Defclass *) theModuleItem->header.firstItem;
           theDefclass != NULL;
           theDefclass = GetNextDefclass(theCLIPSEnv,theDefclass))
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
   Instance *instancePtr;
   int instanceCount = 0;
   size_t i;
   jobject arrayList, slotValueList, javaSlotValueObject, theJavaInstance, instanceName, instanceClass;
   jobject theJavaSlotName, theJavaSlotValue;
   CLIPSValue slotNames;
   CLIPSValue temp;
   UDFValue slotValue;
   UDFValue defaultValue;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   Defclass *theClass;
  
   for (instancePtr = GetNextInstance(theCLIPSEnv,NULL);
        instancePtr != NULL;
        instancePtr = GetNextInstance(theCLIPSEnv,instancePtr))
     { instanceCount++; }
     
   arrayList = (*env)->NewObject(env,
                                 CLIPSJNIData(clipsEnv)->arrayListClass,
                                 CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                 (jint) instanceCount);
                                   
   if (arrayList == NULL)
     { return NULL; }
     
   for (instancePtr = GetNextInstance(theCLIPSEnv,NULL);
        instancePtr != NULL;
        instancePtr = GetNextInstance(theCLIPSEnv,instancePtr))
     {
      theClass = InstanceClass(instancePtr);
      
      ClassSlots(theClass,&slotNames,true);
   
      slotValueList = (*env)->NewObject(env,
                                        CLIPSJNIData(clipsEnv)->arrayListClass,
                                        CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                        (jint) slotNames.multifieldValue->length);

      if (slotValueList == NULL)
        { break; }

      for (i = 0; i < slotNames.multifieldValue->length; i++)
        {
         const char *theCSlotName, *theCSlotValue;
         jboolean defaulted = false;
         
         theCSlotName = slotNames.multifieldValue->contents[i].lexemeValue->contents;
         
         DirectGetSlot(instancePtr,slotNames.multifieldValue->contents[i].lexemeValue->contents,&temp);
         CLIPSToUDFValue(&temp,&slotValue);

         if (SlotDefaultP(theCLIPSEnv,theClass,theCSlotName) == STATIC_DEFAULT)
           {
            SlotDefaultValue(theClass,theCSlotName,&temp);
            CLIPSToUDFValue(&temp,&defaultValue);
                                           
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

      instanceName = (*env)->NewStringUTF(env,InstanceName(instancePtr));
      instanceClass = (*env)->NewStringUTF(env,DefclassName(theClass));

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
   Instance *theInstance;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cInstanceStr = (*env)->GetStringUTFChars(env,instanceStr,NULL);
   
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   theInstance = MakeInstance(theCLIPSEnv,(char *) cInstanceStr);

   (*env)->ReleaseStringUTFChars(env,instanceStr,cInstanceStr);
   
   if (theInstance == NULL)
     {
      SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
      return NULL; 
     }
     
   rv = ConvertSingleFieldValue(env,obj,theCLIPSEnv,INSTANCE_ADDRESS_TYPE,theInstance);
      
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

   rv = (*env)->NewStringUTF(env,InstanceName(JLongToPointer(clipsInstance)));

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }

/*****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_retainInstance: Native */
/*   function for the CLIPSJNI retainInstance method.            */
/*                                                               */
/*                                                               */
/* Class:     net_sf_clipsrules_jni_Environment                  */
/* Method:    retainInstance                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V           */
/*****************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_retainInstance(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsInstance)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   RetainInstance(JLongToPointer(clipsInstance));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }
  
/******************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_releaseInstance: Native */
/*   function for the CLIPSJNI releaseInstance method.            */
/*                                                                */
/*                                                                */
/* Class:     net_sf_clipsrules_jni_Environment                   */
/* Method:    releaseInstance                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V            */
/******************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_releaseInstance(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsInstance)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   ReleaseInstance(JLongToPointer(clipsInstance));
   
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
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   rv = GetInstancesChanged(theCLIPSEnv);
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
   
   SetInstancesChanged(theCLIPSEnv,value);
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
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cSlotName = (*env)->GetStringUTFChars(env,slotName,NULL);
   GetSlotError error;

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   error = DirectGetSlot(JLongToPointer(clipsInstance),(char *) cSlotName,&theDO);

   (*env)->ReleaseStringUTFChars(env,slotName,cSlotName);
   
   if (error == GSE_NO_ERROR)
     { rv = ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO); }
   else
     { rv = NULL; }
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }
    
/********************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_findInstanceByName: Native function   */
/*   for the CLIPSJNI findInstanceBySymbol method.                              */
/*                                                                              */
/*                                                                              */
/* Class:     net_sf_clipsrules_jni_Environment                                 */
/* Method:    findInstanceByName                                                */
/* Signature: (JLjava/lang/String;)Lnet/sf/clipsrules/jni/InstanceAddressValue; */
/********************************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_findInstanceByName(
  JNIEnv *env, 
  jobject javaEnv, 
  jlong clipsEnv, 
  jstring instanceName)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   CLIPSValue theDO;
   Instance *theInstance;
   jobject rv;
   
   const char *cInstanceName = (*env)->GetStringUTFChars(env,instanceName,NULL);
   
   theInstance = FindInstance(theCLIPSEnv,NULL,cInstanceName,true);
   
   (*env)->ReleaseStringUTFChars(env,instanceName,cInstanceName);
   
   if (theInstance == NULL) 
     { rv = NULL; }
   else
     {
      theDO.instanceValue = theInstance;
      rv = ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO); 
     }

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
   Defclass *defclassPtr = JLongToPointer(defclassLong);
   const char *defclassText = NULL;
      
   if (defclassPtr != NULL)
     { defclassText = DefclassPPForm(defclassPtr); }   

   if (defclassText == NULL)
     { return (*env)->NewStringUTF(env,""); }
   else     
     { return (*env)->NewStringUTF(env,defclassText); }   
  }

