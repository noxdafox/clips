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
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   Defmodule *theModule;
   struct deftemplateModule *theModuleItem;
   Deftemplate *theDeftemplate;
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
      theModuleItem = (struct deftemplateModule *) 
                   GetModuleItem(theCLIPSEnv,theModule,DeftemplateData(theCLIPSEnv)->DeftemplateModuleIndex);

      for (theDeftemplate = (struct deftemplate *) theModuleItem->header.firstItem;
           theDeftemplate != NULL;
           theDeftemplate = (struct deftemplate *) GetNextDeftemplate(theCLIPSEnv,theDeftemplate))
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
                                
            theScopeMap = (CLIPSBitMap *) CreateDeftemplateScopeMap(theCLIPSEnv,theDeftemplate);
            
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
   size_t i;
   jobject arrayList, slotValueList, javaSlotValueObject, theJavaFact, factName, factRelation;
   jobject theJavaSlotName, theJavaSlotValue;
   CLIPSValue slotNames;
   CLIPSValue temp;
   UDFValue slotValue;
   UDFValue defaultValue;
   char factNameBuffer[32]; 
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   for (factPtr = GetNextFact(theCLIPSEnv,NULL);
        factPtr != NULL;
        factPtr = GetNextFact(theCLIPSEnv,factPtr))
     { factCount++; }
     
   arrayList = (*env)->NewObject(env,
                                 CLIPSJNIData(clipsEnv)->arrayListClass,
                                 CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                 (jint) factCount);
                                   
   if (arrayList == NULL)
     { return NULL; }
     
   for (factPtr = GetNextFact(theCLIPSEnv,NULL);
        factPtr != NULL;
        factPtr = GetNextFact(theCLIPSEnv,factPtr))
     {
      FactSlotNames(factPtr,&slotNames);
   
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
         
         FactSlotValue(theCLIPSEnv,factPtr,slotNames.multifieldValue->contents[i].lexemeValue->contents,&temp);
         CLIPSToUDFValue(&temp,&slotValue);
         if (DeftemplateSlotDefaultP(FactDeftemplate(factPtr),theCSlotName) == STATIC_DEFAULT)
           {
            DeftemplateSlotDefaultValue(FactDeftemplate(factPtr),
                                        theCSlotName,&temp);
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

      sprintf(factNameBuffer,"f-%lld", FactIndex(factPtr));
      
      factName = (*env)->NewStringUTF(env,factNameBuffer);
      factRelation = (*env)->NewStringUTF(env,factPtr->whichDeftemplate->header.name->contents);

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

   theFact = AssertString(theCLIPSEnv,(char *) cFactStr);

   (*env)->ReleaseStringUTFChars(env,factStr,cFactStr);
   
   if (theFact == NULL)
     { return NULL; }
     
   rv = ConvertSingleFieldValue(env,obj,theCLIPSEnv,FACT_ADDRESS_TYPE,theFact);

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

   rv = FactIndex(JLongToPointer(clipsFact));

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
   GetSlotError error;
   
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cSlotName = NULL;
   
   if (slotName != NULL)
     { cSlotName = (*env)->GetStringUTFChars(env,slotName,NULL); }

   void *oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   error = GetFactSlot(JLongToPointer(clipsFact),(char *) cSlotName,&theDO);

   if (slotName != NULL)
     { (*env)->ReleaseStringUTFChars(env,slotName,cSlotName); }
   
   if (error == GSE_NO_ERROR)
     { rv = ConvertDataObject(env,javaEnv,theCLIPSEnv,&theDO); }
   else
     { rv = NULL; }
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
   
   return rv;
  }

/*************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_retainFact: Native */
/*   function for the CLIPSJNI retainFact method.            */
/*                                                           */
/* Class:     net_sf_clipsrules_jni_Environment              */
/* Method:    retainFact                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V       */
/*************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_retainFact(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsFact)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   RetainFact(JLongToPointer(clipsFact));
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);
  }
  
/**************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_releaseFact: Native */
/*   function for the CLIPSJNI releaseFact method.            */
/*                                                            */
/* Class:     net_sf_clipsrules_jni_Environment               */
/* Method:    releaseFact                                     */
/* Signature: (Lnet/sf/clipsrules/jni/Environment;JJ)V        */
/**************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_releaseFact(
  JNIEnv *env, 
  jclass javaClass, 
  jobject javaEnv,
  jlong clipsEnv, 
  jlong clipsFact)
  {
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   ReleaseFact(JLongToPointer(clipsFact));
   
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
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   rv = GetFactListChanged(theCLIPSEnv);
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
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   SetFactListChanged(theCLIPSEnv,value);
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

   rv = LoadFacts(JLongToPointer(clipsEnv),(char *) cFileName);
   
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
   Deftemplate *deftemplatePtr = JLongToPointer(deftemplateLong);
   const char *deftemplateText = NULL;
      
   if (deftemplatePtr != NULL)
     { deftemplateText = DeftemplatePPForm(deftemplatePtr); }   

   if (deftemplateText == NULL)
     { return (*env)->NewStringUTF(env,""); }
   else     
     { return (*env)->NewStringUTF(env,deftemplateText); }   
  }

