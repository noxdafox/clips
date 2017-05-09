
#include "net_sf_clipsrules_jni_Environment.h"

#include "clipsjni_data.h"
#include "clipsjni_utilities.h"
#include "clipsjni_glue.h"

/***************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setHaltRules: Native */ 
/*   function for the CLIPSJNI setHaltRules method.            */
/*                                                             */
/* Class:     net_sf_clipsrules_jni_Environment                */
/* Method:    setHaltRules                                     */
/* Signature: (JZ)V                                            */
/***************************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setHaltRules(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   SetHaltRules(JLongToPointer(clipsEnv),value);
  }

/******************************************************/
/* Java_net_sf_clipsrules_jni_Environment_run: Native */ 
/*   function for the CLIPSJNI run method.            */
/*                                                    */
/* Class:     net_sf_clipsrules_jni_Environment       */
/* Method:    run                                     */
/* Signature: (JJ)J                                   */
/******************************************************/
JNIEXPORT jlong JNICALL Java_net_sf_clipsrules_jni_Environment_run(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jlong runLimit)
  {
   jlong rv;
   void *oldContext = SetEnvironmentContext(JLongToPointer(clipsEnv),(void *) env);

   rv = Run(JLongToPointer(clipsEnv),runLimit);
   
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   return rv;
  }

/************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getAgenda: Native */
/*   function for the CLIPSJNI getAgenda method.            */
/*                                                          */
/* Class:     net_sf_clipsrules_jni_Environment             */
/* Method:    getAgenda                                     */
/* Signature: (J)Lnet/sf/clipsrules/jni/Agenda;             */
/*                                                          */
/************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getAgenda(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv, 
  jstring moduleName)
  {
   Defmodule *theModule;
   struct defruleModule *theModuleItem;
   Activation *theActivation;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   const char *cModuleName = (*env)->GetStringUTFChars(env,moduleName,NULL);
   int activationCount = 0;
   jobject arrayList, theJActivation, ruleName, basis, result;
   char bindingsBuffer[1024]; // TBD Replace
   void *oldContext;

   theModule = FindDefmodule(theCLIPSEnv,cModuleName);
   (*env)->ReleaseStringUTFChars(env,moduleName,cModuleName);
   if (theModule == NULL) return NULL;

   SaveCurrentModule(theCLIPSEnv);

   SetCurrentModule(theCLIPSEnv,theModule);

   theModuleItem = (struct defruleModule *) 
                   GetModuleItem(theCLIPSEnv,NULL,DefruleData(theCLIPSEnv)->DefruleModuleIndex);
                   
   RestoreCurrentModule(theCLIPSEnv);

   if (theModuleItem == NULL) return(NULL);
   
   /*==================================*/
   /* Count the number of activations. */
   /*==================================*/
   
   for (theActivation = theModuleItem->agenda;
        theActivation != NULL;
        theActivation = GetNextActivation(theCLIPSEnv,theActivation))
     { activationCount++; }
     
   arrayList = (*env)->NewObject(env,
                                 CLIPSJNIData(clipsEnv)->arrayListClass,
                                 CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                 (jint) activationCount);
                                   
   if (arrayList == NULL)
     { return NULL; }

   oldContext = SetEnvironmentContext(theCLIPSEnv,(void *) env);
   
   for (theActivation = theModuleItem->agenda;
        theActivation != NULL;
        theActivation = GetNextActivation(theCLIPSEnv,theActivation))
     { 
      ruleName = (*env)->NewStringUTF(env,theActivation->theRule->header.name->contents);

      GetActivationBasisPPForm(theCLIPSEnv,bindingsBuffer,1024,theActivation);
      basis = (*env)->NewStringUTF(env,bindingsBuffer);

      theJActivation = (*env)->NewObject(env,
                                      CLIPSJNIData(clipsEnv)->activationClass,
                                      CLIPSJNIData(clipsEnv)->activationInitMethod,
                                      ruleName,(jint) theActivation->salience,basis);
                                      
      (*env)->DeleteLocalRef(env,ruleName);
      (*env)->DeleteLocalRef(env,basis);
      
      if (theJActivation != NULL)
        { 
         (*env)->CallBooleanMethod(env,arrayList,CLIPSJNIData(clipsEnv)->arrayListAddMethod,theJActivation); 
         (*env)->DeleteLocalRef(env,theJActivation);
        }
     }
     
   SetEnvironmentContext(JLongToPointer(clipsEnv),oldContext);

   result = (*env)->NewObject(env,
                              CLIPSJNIData(clipsEnv)->agendaClass,
                              CLIPSJNIData(clipsEnv)->agendaInitMethod,
                              arrayList);
     
   return result;
  }

/****************************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getFocusStack: Native */
/*   function for the CLIPSJNI getFocusStack method.            */
/*                                                              */
/* Class:     net_sf_clipsrules_jni_Environment                 */
/* Method:    getFocusStack                                     */
/* Signature: (J)Lnet/sf/clipsrules/jni/FocusStack;             */
/*                                                              */
/****************************************************************/
JNIEXPORT jobject JNICALL Java_net_sf_clipsrules_jni_Environment_getFocusStack(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jobject arrayList, focusModule, moduleName, result;
   int moduleCount = 0;
   FocalModule *theFocus;
   
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
 
   for (theFocus = EngineData(theCLIPSEnv)->CurrentFocus; 
        theFocus != NULL; 
        theFocus = theFocus->next)
     { moduleCount++; }
  
   arrayList = (*env)->NewObject(env,
                                 CLIPSJNIData(clipsEnv)->arrayListClass,
                                 CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                 (jint) moduleCount);
                                   
   if (arrayList == NULL)
     { return NULL; }
     
   for (theFocus = EngineData(theCLIPSEnv)->CurrentFocus; 
        theFocus != NULL; 
        theFocus = theFocus->next)
     { 
      moduleName = (*env)->NewStringUTF(env,theFocus->theModule->header.name->contents);
        
      focusModule = (*env)->NewObject(env,
                                      CLIPSJNIData(clipsEnv)->focusClass,
                                      CLIPSJNIData(clipsEnv)->focusInitMethod,
                                      moduleName);
                                      
      (*env)->DeleteLocalRef(env,moduleName);

      if (focusModule != NULL)
        { 
         (*env)->CallBooleanMethod(env,arrayList,CLIPSJNIData(clipsEnv)->arrayListAddMethod,focusModule); 
         (*env)->DeleteLocalRef(env,focusModule);
        }
     }
     
   result = (*env)->NewObject(env,
                              CLIPSJNIData(clipsEnv)->focusStackClass,
                              CLIPSJNIData(clipsEnv)->focusStackInitMethod,
                              arrayList);
     
   return result;
  }

/***********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getAgendaChanged */
/* Class:     net_sf_clipsrules_jni_Environment            */
/* Method:    getAgendaChanged                             */
/* Signature: (J)Z                                         */
/***********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_getAgendaChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jboolean rv;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   rv = GetAgendaChanged(theCLIPSEnv);
   return rv;
  } 

/***********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setAgendaChanged */
/* Class:     net_sf_clipsrules_jni_Environment            */
/* Method:    setAgendaChanged                             */
/* Signature: (JZ)Z                                        */
/***********************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setAgendaChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   SetAgendaChanged(theCLIPSEnv,value);
  }


/**********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getFocusChanged */
/* Class:     net_sf_clipsrules_jni_Environment           */
/* Method:    getFocusChanged                             */
/* Signature: (J)Z                                        */
/**********************************************************/
JNIEXPORT jboolean JNICALL Java_net_sf_clipsrules_jni_Environment_getFocusChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv)
  {
   jboolean rv;
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   rv = GetFocusChanged(theCLIPSEnv);
   return rv;
  } 

/**********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_setFocusChanged */
/* Class:     net_sf_clipsrules_jni_Environment           */
/* Method:    setFocusChanged                             */
/* Signature: (JZ)Z                                       */
/**********************************************************/
JNIEXPORT void JNICALL Java_net_sf_clipsrules_jni_Environment_setFocusChanged(
  JNIEnv *env, 
  jobject obj, 
  jlong clipsEnv,
  jboolean value)
  {
   Environment *theCLIPSEnv = JLongToPointer(clipsEnv);
   
   SetFocusChanged(theCLIPSEnv,value);
  }

/*********************************************************/
/* Java_net_sf_clipsrules_jni_Environment_getDefruleText */
/* Class:     net_sf_clipsrules_jni_Environment          */
/* Method:    getDefruleText                             */
/* Signature: (JLjava/lang/String;)Ljava/lang/String;    */
/*********************************************************/
JNIEXPORT jstring JNICALL Java_net_sf_clipsrules_jni_Environment_getDefruleText(
  JNIEnv *env,
  jobject obj,
  jlong clipsEnv,
  jstring defruleName)
  {
   const char *cDefruleName = (*env)->GetStringUTFChars(env,defruleName,NULL);
   Defrule *defrulePtr;
   Environment *theEnv;
   const char *ruleText = NULL;
   
   theEnv = JLongToPointer(clipsEnv);
   
   defrulePtr = FindDefrule(theEnv,cDefruleName);

   (*env)->ReleaseStringUTFChars(env,defruleName,cDefruleName);

   if (defrulePtr != NULL)
     { ruleText = DefrulePPForm(defrulePtr); }   

   if (ruleText == NULL)
     { return (*env)->NewStringUTF(env,""); }
   else     
     { return (*env)->NewStringUTF(env,ruleText); }   
  }
