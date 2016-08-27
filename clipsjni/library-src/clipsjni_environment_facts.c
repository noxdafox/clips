#include "net_sf_clipsrules_jni_Environment.h"

#include "clipsjni_data.h"
#include "clipsjni_utilities.h"
#include "clipsjni_glue.h"

/****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getFactScopes: Native */
/*   function for the CLIPSJNI getFactScopes method.            */
/*                                                              */
/* Class:     net_sf_clipsrules_jni_Environment                 */
/* Method:    getFactScopes                                     */
/* Signature: (J)Ljava/util/HashMap;                            */
/*                                                              */
/****************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getFactScopes(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jobject scopeMap, moduleSet, theDeftemplateIndex;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   struct defmodule *theModule;
   struct deftemplateModule *theModuleItem;
   struct deftemplate *theDeftemplate;
   jint moduleCount = 0, whichBit;
   BITMAP_HN *theScopeMap;

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
      theModuleItem = (struct deftemplateModule *) 
                   GetModuleItem(theCLIPSEnv,theModule,DeftemplateData(theCLIPSEnv)->DeftemplateModuleIndex);

      for (theDeftemplate = (struct deftemplate *) theModuleItem->header.firstItem;
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) EnvGetNextDeftemplate(theCLIPSEnv,theDeftemplate))
        { 
         if (theDeftemplate->factList != NULL)
           { 
            theDeftemplateIndex = (*env)->NewObject(env,
                                CLIPSJNIData(theCLIPSEnv)->longClass,
                                CLIPSJNIData(theCLIPSEnv)->longInitMethod,
                                (jlong) theDeftemplate);

            moduleSet = (*env)->NewObject(env,
                                CLIPSJNIData(theCLIPSEnv)->bitSetClass,
                                CLIPSJNIData(theCLIPSEnv)->bitSetInitMethod,
                                moduleCount);
                                
            theScopeMap = (BITMAP_HN *) CreateDeftemplateScopeMap(theCLIPSEnv,theDeftemplate);
            
            for (whichBit = 0; whichBit < moduleCount; whichBit++)
              {
               if (TestBitMap(theScopeMap->contents,whichBit))
                 { (*env)->CallVoidMethod(env,moduleSet,CLIPSJNIData(clipsEnv)->bitSetSetMethod,whichBit); }
              }
              
            (*env)->CallObjectMethod(env,scopeMap,CLIPSJNIData(clipsEnv)->hashMapPutMethod,theDeftemplateIndex,moduleSet); 

            (*env)->DeleteLocalRef(env,theDeftemplateIndex);
            (*env)->DeleteLocalRef(env,moduleSet);
           }
        }
     }

   return scopeMap;
  }

/**************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getFactList: Native */
/*   function for the CLIPSJNI getFactList method.            */
/*                                                            */
/* Class:     net_sf_clipsrules_jni_Environment               */
/* Method:    getFactList                                     */
/* Signature: (J)Ljava/util/List;                             */
/*                                                            */
/**************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getFactList(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   struct fact *factPtr;
   int factCount = 0;
   int i;
   jobject arrayList, slotValueList, javaSlotValueObject, theJavaFact, factName, factRelation;
   jobject theJavaSlotName, theJavaSlotValue;
   CLIPSValue slotNames;
   CLIPSValue slotValue;
   CLIPSValue defaultValue;
   char factNameBuffer[32]; 
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   for (factPtr = EnvGetNextFact(theCLIPSEnv,NULL);
        factPtr != NULL;
        factPtr = EnvGetNextFact(theCLIPSEnv,factPtr))
     { factCount++; }
     
   arrayList = (*env)->NewObject(env,
                                 CLIPSJNIData(clipsEnv)->arrayListClass,
                                 CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                 (jint) factCount);
                                   
   if (arrayList == NULL)
     { return NULL; }
     
   for (factPtr = EnvGetNextFact(theCLIPSEnv,NULL);
        factPtr != NULL;
        factPtr = EnvGetNextFact(theCLIPSEnv,factPtr))
     {
      EnvFactSlotNames(theCLIPSEnv,factPtr,&slotNames);
   
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
         
         theCSlotName = ValueToString(GetMFValue(GetValue(slotNames),i));
         
         FactSlotValue(theCLIPSEnv,factPtr,ValueToString(GetMFValue(GetValue(slotNames),i)),&slotValue);

         if (EnvDeftemplateSlotDefaultP(theCLIPSEnv,EnvFactDeftemplate(theCLIPSEnv,factPtr),theCSlotName) == STATIC_DEFAULT)
           {
            EnvDeftemplateSlotDefaultValue(theCLIPSEnv,
                                           EnvFactDeftemplate(theCLIPSEnv,factPtr),
                                           theCSlotName,&defaultValue);
                                           
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

      sprintf(factNameBuffer,"f-%lld", EnvFactIndex(theCLIPSEnv,factPtr));
      
      factName = (*env)->NewStringUTF(env,factNameBuffer);
      factRelation = (*env)->NewStringUTF(env,ValueToString(factPtr->whichDeftemplate->header.name));

      theJavaFact = (*env)->NewObject(env,
                                      CLIPSJNIData(clipsEnv)->factInstanceClass,
                                      CLIPSJNIData(clipsEnv)->factInstanceInitMethod,
                                      ((jlong) factPtr->whichDeftemplate), 
                                      factName,factRelation,slotValueList);
                                      
      (*env)->DeleteLocalRef(env,slotValueList);
      (*env)->DeleteLocalRef(env,factName);
      (*env)->DeleteLocalRef(env,factRelation);

      if (theJavaFact != NULL)
        { 
         (*env)->CallBooleanMethod(env,arrayList,CLIPSJNIData(clipsEnv)->arrayListAddMethod,theJavaFact); 
         (*env)->DeleteLocalRef(env,theJavaFact);
        }
     }

   return arrayList;
  }

/****************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_assertString: Native function for */
/*   the CLIPSJNI assertString method.                                      */
/*                                                                          */
/* Class:     net_sf_clipsrules_jni_Environment                             */
/* Method:    assertString                                                  */
/* Signature: (JLjava/lang/String;)Lnet/sf/clipsrules/jni/FactAddressValue; */
/****************************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_assertString(
  JNIEnv *env,
  jobject obj, 
  jlong clipsEnv, 
  jstring factStr)
  {
   jobject rv;
   void *theFact;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cFactStr = (*env)->GetStringUTFChars(env,factStr,NULL);
   
   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);

   theFact = EnvAssertString(theCLIPSEnv,(char *) cFactStr);

   (*env)->ReleaseStringUTFChars(env,factStr,cFactStr);
   
   if (theFact == NULL)
     { return(NULL); }
     
   rv = ConvertSingleFieldValue(env,obj,theCLIPSEnv,FACT_ADDRESS,theFact);

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }

/************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_factIndex: Native */
/*   function for the CLIPSJNI factIndex method.            */
/*                                                          */
/* Class:     net_sf_clipsrules_jni_Environment             */
/* Method:    factIndex                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)J      */
/************************************************************/
JNIEXPORT jlong JNICALL Java_net_sf_clipsrules_jni_Environment_factIndex(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsFact)
  {
   jlong rv;
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = EnvFactIndex(JLongToPointer(clipsEnv),JLongToPointer(clipsFact));

   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }

/************************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getFactSlot: Native function  */
/*   for the CLIPSJNI getFactSlot method.                               */
/*                                                                      */
/* Class:     net_sf_clipsrules_jni_Environment                         */
/* Method:    getFactSlot                                               */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJLjava/lang/String;) */
/*            Lnet/sf/clipsrules/jni/PrimitiveValue;                    */
/************************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getFactSlot(
  JNIEnv *env,
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv,
  jlong clipsFact,
  jstring slotName)
  {
   jobject rv;
   CLIPSValue theDO;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cSlotName = (*env)->GetStringUTFChars(env,slotName,NULL);

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   EnvGetFactSlot(JLongToPointer(clipsEnv),JLongToPointer(clipsFact),(char *) cSlotName,&theDO);

   (*env)->ReleaseStringUTFChars(env,slotName,cSlotName);
   
   rv = ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }

/*********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_incrementFactCount: Native */
/*   function for the CLIPSJNI incrementFactCount method.            */
/*                                                                   */
/* Class:     net_sf_clipsrules_jni_Environment                      */
/* Method:    incrementFactCount                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V               */
/*********************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_incrementFactCount(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsFact)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   EnvIncrementFactCount(JLongToPointer(clipsEnv),JLongToPointer(clipsFact));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }
  
/*********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_decrementFactCount: Native */
/*   function for the CLIPSJNI decrementFactCount method.            */
/*                                                                   */
/* Class:     net_sf_clipsrules_jni_Environment                      */
/* Method:    decrementFactCount                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V               */
/*********************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_decrementFactCount(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsFact)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   EnvDecrementFactCount(JLongToPointer(clipsEnv),JLongToPointer(clipsFact));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }

/*************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getFactListChanged */
/* Class:     net_sf_clipsrules_jni_Environment              */
/* Method:    getFactListChanged                             */
/* Signature: (J)Z                                           */
/*************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_getFactListChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jboolean rv;
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   rv = EnvGetFactListChanged(theCLIPSEnv);
   return rv;
  } 

/*************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setFactListChanged */
/* Class:     net_sf_clipsrules_jni_Environment              */
/* Method:    setFactListChanged                             */
/* Signature: (JZ)Z                                          */
/*************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setFactListChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   void *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   EnvSetFactListChanged(theCLIPSEnv,value);
  }
  
/************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_loadFacts: Native */ 
/*   function for the CLIPSJNI loadFacts method.            */
/*                                                          */
/* Class:     net_sf_clipsrules_jni_Environment             */
/* Method:    loadFact                                      */
/* Signature: (JLjava/lang/String;)Z                        */
/************************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_loadFacts(
  JNIEnv *env, 
  jobject obj,
  jlong clipsEnv,
  jstring fileName)
  {
   jboolean rv;
   const char *cFileName = (*env)->GetStringUTFChars(env,fileName,NULL);
   
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = EnvLoadFacts(JLongToPointer(clipsEnv),(char *) cFileName);
   
   (*env)->ReleaseStringUTFChars(env,fileName,cFileName);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }
  
/*********************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getDeftemplateText: Native */ 
/*   function for the CLIPSJNI getDeftemplateText method.            */
/*                                                                   */
/* Class:     net_sf_clipsrules_jni_Environment                      */
/* Method:    getDeftemplateText                                     */
/* Signature: (JJ)Ljava/lang/String;                                 */
/*********************************************************************/
JNIEXPORT jstring JNICALL Java_net_sf_clipsrules_jni_Environment_getDeftemplateText(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jlong deftemplateLong)
  {
   void *deftemplatePtr = JLongToPointer(deftemplateLong);
   void *theEnv = JLongToPointer(clipsEnv);
   const char *deftemplateText = NULL;
      
   if (deftemplatePtr != NULL)
     { deftemplateText = EnvGetDeftemplatePPForm(theEnv,deftemplatePtr); }   

   if (deftemplateText == NULL)
     { return (*env)->NewStringUTF(env,""); }
   else     
     { return (*env)->NewStringUTF(env,deftemplateText); }   
  }

