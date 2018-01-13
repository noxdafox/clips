#include "clipsjni_glue.h"
#include "clipsjni_data.h"
#include "clipsjni_utilities.h"

static void       DeallocateJNIData(Environment *);

/********************/
/* JNIUserFunction: */
/********************/
void JNIUserFunction(
  Environment *theEnv,
  UDFContext *theUDFContext,
  UDFValue *result)
  {
   JNIEnv *env;
   jobject context;
   jobject arguments, targ, rv;
   int i, argCount;
   UDFValue theArg;
   CLIPSValue temp;
   
   result->voidValue = theEnv->VoidConstant;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);
   
   context = theUDFContext->context;

   argCount = UDFArgumentCount(theUDFContext);
   arguments = (*env)->NewObject(env,
                                 CLIPSJNIData(theEnv)->arrayListClass,
                                 CLIPSJNIData(theEnv)->arrayListInitMethod,
                                 (jint) argCount);
                                 
   for (i = 1; i <= argCount; i++)
     {
      UDFNthArgument(theUDFContext,i,ANY_TYPE_BITS,&theArg);
      NormalizeMultifield(theEnv,&theArg);
      temp.value = theArg.value;
      targ = ConvertDataObject(env,CLIPSJNIData(theEnv)->environmentObject,theEnv,&temp); 
      
      if (targ != NULL)
        { 
         (*env)->CallBooleanMethod(env,arguments,CLIPSJNIData(theEnv)->arrayListAddMethod,targ); 
         (*env)->DeleteLocalRef(env,targ);
        }
     }

   rv = (*env)->CallObjectMethod(env,context,CLIPSJNIData(theEnv)->userFunctionEvaluateMethod,arguments); 
   
   if ((*env)->ExceptionCheck(env)) 
     { 
      WriteString(theEnv,STDERR,"Exception occurred during evaluation of JNI User Function.\n");
      (*env)->ExceptionDescribe(env); 
      (*env)->ExceptionClear(env); 
      SetEvaluationError(theEnv,true);
     }
   else
     { ConvertPrimitiveValueToDataObject(theEnv,rv,result); }

   (*env)->DeleteLocalRef(env,arguments);
  }

/************************/
/* JNIPeriodicCallback: */
/************************/
void JNIPeriodicCallback(
  Environment *theEnv,
  void *vcontext)
  {
   jobject context;
   jclass cls;
   JNIEnv *env;
   jmethodID mid;
  
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = vcontext;
  
   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"periodicCallback","()V");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return; }

   (*env)->CallVoidMethod(env,context,mid);      
  }

/***************************/
/* JNIParserErrorCallback: */
/***************************/
void JNIParserErrorCallback(
  Environment *theEnv,
  const char *fileName,
  const char *warningString,
  const char *errorString,
  long lineNumber,
  void *context)
  {
   JNIEnv *env;
   jmethodID mid;
   jstring str1, str2;
     
   if (errorString == NULL) 
     { return; }
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);
   
   mid = (*env)->GetMethodID(env,CLIPSJNIData(theEnv)->environmentClass,"addError","(Ljava/lang/String;JLjava/lang/String;)V");

   if (mid == NULL)
     { return; }

   str1 = (*env)->NewStringUTF(env,fileName);
   str2 = (*env)->NewStringUTF(env,errorString);

   (*env)->CallVoidMethod(env,CLIPSJNIData(theEnv)->environmentObject,mid,str1,lineNumber,str2);

   (*env)->DeleteLocalRef(env,str1);
   (*env)->DeleteLocalRef(env,str2);
  }

/*****************************************************/
/* QueryJNICallback: Query callback for JNI routers. */
/*****************************************************/
bool QueryJNICallback(
  Environment *theEnv,
  const char *logicalName,
  void *vcontext)
  {
   jobject context;
   jclass cls;
   JNIEnv *env;
   jmethodID mid;
   jboolean rv;
   jstring str;
  
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = vcontext;
  
   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"query","(Ljava/lang/String;)Z");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return false; }

   str = (*env)->NewStringUTF(env,logicalName);

   rv = (*env)->CallBooleanMethod(env,context,mid,str);
      
   (*env)->DeleteLocalRef(env,str);
      
   return(rv);
  }

/***************************************************/
/* ExitJNICallback: Exit callback for JNI routers. */
/***************************************************/
void ExitJNICallback(
  Environment *theEnv,
  int num,
  void *vcontext)
  {
#if MAC_XCD
#pragma unused(num)
#endif
   
   jobject context;
   JNIEnv *env;
   jmethodID mid;
   jclass cls;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = vcontext;

   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"exit","(Z)V");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return; }

   (*env)->CallVoidMethod(env,context,mid,(num == EXIT_FAILURE));

   /* TBD deallocate global context reference */
  }

/*****************************************************/
/* WriteJNICallback: Write callback for JNI routers. */
/*****************************************************/
void WriteJNICallback(
  Environment *theEnv,
  const char *logicalName,
  const char *str,
  void *vcontext)
  {
   jobject context;
   JNIEnv *env;
   jmethodID mid;
   jclass cls;
   jstring str1, str2;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = vcontext;

   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"write","(Ljava/lang/String;Ljava/lang/String;)V");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return; }

   str1 = (*env)->NewStringUTF(env,logicalName);
   str2 = (*env)->NewStringUTF(env,str);

   (*env)->CallVoidMethod(env,context,mid,str1,str2);

   (*env)->DeleteLocalRef(env,str1);
   (*env)->DeleteLocalRef(env,str2);
  }

/***************************************************/
/* ReadJNICallback: Read callback for JNI routers. */
/***************************************************/
int ReadJNICallback(
  Environment *theEnv,
  const char *logicalName,
  void *vcontext)
  {
   jint theChar;
   jobject context;
   JNIEnv *env;
   jmethodID mid;
   jclass cls;
   jstring str;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = vcontext;

   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"read","(Ljava/lang/String;)I");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return -1; }

   str = (*env)->NewStringUTF(env,logicalName);

   theChar = (*env)->CallIntMethod(env,context,mid,str);

   (*env)->DeleteLocalRef(env,str);

   return((int) theChar);
  }

/*******************************************************/
/* UnreadJNICallback: Unread callback for JNI routers. */
/*******************************************************/
int UnreadJNICallback(
  Environment *theEnv,
  const char *logicalName,
  int ch,
  void *vcontext)
  {
   jint theChar;
   jobject context;
   JNIEnv *env;
   jmethodID mid;
   jclass cls;
   jstring str;

   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   context = vcontext;

   cls = (*env)->GetObjectClass(env,context);

   mid = (*env)->GetMethodID(env,cls,"unread","(Ljava/lang/String;I)I");

   (*env)->DeleteLocalRef(env,cls);

   if (mid == NULL)
     { return -1; }

   str = (*env)->NewStringUTF(env,logicalName);

   theChar = (*env)->CallIntMethod(env,context,mid,str,(jint) ch);

   (*env)->DeleteLocalRef(env,str);

   return((int) theChar);
  }
  
/*********************/
/* PrintJavaAddress: */
/*********************/
void PrintJavaAddress(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
   jobject theObject;
   jclass cls;
   jstring str;
   char *cStr;
   JNIEnv *env;
   char buffer[20];

   WriteString(theEnv,logicalName,"<Pointer-");
        
   theObject = (jobject) ((CLIPSExternalAddress *) theValue)->contents;
   
   if (theObject != NULL)
     {
      env = (JNIEnv *) GetEnvironmentContext(theEnv);
      
      cls = (*env)->GetObjectClass(env,theObject);
      
      str = (jstring) (*env)->CallObjectMethod(env,cls,CLIPSJNIData(theEnv)->classGetCanonicalNameMethod);

      cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
      
      WriteString(theEnv,logicalName,cStr);
      WriteString(theEnv,logicalName,"-");
   
      (*env)->ReleaseStringUTFChars(env,str,cStr);
     }
   else
     { WriteString(theEnv,logicalName,"java-"); }
   
   gensprintf(buffer,"%p",(void *) theObject);
   WriteString(theEnv,logicalName,buffer);
   WriteString(theEnv,logicalName,">");
  }

/********************/
/* NewJavaAddress:  */
/********************/
void NewJavaAddress(
  UDFContext *context,
  UDFValue *rv)
  {
   jclass theClass, tempClass;
   int numberOfArguments;
   JNIEnv *env;
   const char *className;
   char *classDescriptor;
   UDFValue theValue;
   size_t i, length;
   jmethodID mid;
   jobjectArray constructorList, parameterList;
   jsize theSize, c; 
   jsize paramCount, p; 
   jobject theConstructor, theObject, oldObject; 
   bool found = false, matches;
   UDFValue *newArgs;
   jvalue *javaArgs;
   Environment *theEnv = context->environment;
   
   /*=============================================*/
   /* Retrieve the JNI environment pointer stored */
   /* in the CLIPS environment structure.         */
   /*=============================================*/
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   /*======================================================================*/
   /* If the Java external address type is used, additional arguments must */
   /* at least include the Java class name of the object to be created.    */
   /*======================================================================*/
   
   numberOfArguments = UDFArgumentCount(context);
   if (numberOfArguments < 2) 
     { return; }
   
   /*=======================================*/
   /* The Java class name must be a symbol. */
   /*=======================================*/
   
   if (! UDFNthArgument(context,1,ANY_TYPE_BITS,&theValue))
     { return; }
   
   className = theValue.lexemeValue->contents;
   
   /*=============================================*/
   /* Construct the class descriptor by replacing */
   /* any periods (.) in the class name with a    */
   /* forward slash (/).                          */
   /*=============================================*/
   
   length = strlen(className);
   classDescriptor = genalloc(theEnv,length + 1);
   for (i = 0; i < length; i++)
     {
      if (className[i] != '.')
        { classDescriptor[i] = className[i]; }
      else 
        { classDescriptor[i] = '/'; }
     }
   classDescriptor[i] = 0;
   
   /*=======================*/
   /* Search for the class. */
   /*=======================*/
   
   theClass = (*env)->FindClass(env,classDescriptor); 
   
   /*========================================*/
   /* Free the constructed class descriptor. */
   /*========================================*/
   
   genfree(theEnv,classDescriptor,length + 1);

   /*============================================*/
   /* Signal an error if the class wasn't found. */
   /*============================================*/
   
   if (theClass == NULL)
     {
      if ((*env)->ExceptionOccurred(env))
        { (*env)->ExceptionClear(env); }
      SetEvaluationError(theEnv,true);
      ExpectedTypeError1(theEnv,"new (with type Java)",2,"Java class name");
      return;
     }
      
   /*===========================================================================*/
   /* Evaluate the CLIPS arguments that will be passed to the java constructor. */
   /*===========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { newArgs = NULL; }
   else
     {
      newArgs = (UDFValue *) genalloc(theEnv,sizeof(UDFValue) * (numberOfArguments - 2));
      for (i = 0; i < (size_t) numberOfArguments - 2; i++)
        {
         UDFNthArgument(context,(unsigned int) (i+3),ANY_TYPE_BITS,&newArgs[i]);
         if (GetEvaluationError(theEnv))
           {   
            (*env)->DeleteLocalRef(env,theClass);
            return;
           }
        }
     }

   /*========================================================================*/
   /* Construct an array in which to store the corresponding java arguments. */
   /*========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { javaArgs = NULL; }
   else
     { javaArgs = (jvalue *) genalloc(theEnv,sizeof(jvalue) * (numberOfArguments - 2)); }
   
   /*=============================================*/
   /* Get the method index of the getConstructors */
   /* method from the java.lang.Class class.      */
   /*=============================================*/

   tempClass = (*env)->FindClass(env,"java/lang/Class");
   mid = (*env)->GetMethodID(env,tempClass,"getConstructors","()[Ljava/lang/reflect/Constructor;"); 
   (*env)->DeleteLocalRef(env,tempClass);
   
   /*=======================================================*/
   /* Get the list of constructors for the specified class. */
   /*=======================================================*/
     
   constructorList = (jobjectArray) (*env)->CallObjectMethod(env,theClass,mid);

   /*======================================================*/
   /* Get the method index of the getParameterTypes method */
   /* from the java.lang.reflect.Constructor class.        */
   /*======================================================*/

   tempClass = (*env)->FindClass(env,"java/lang/reflect/Constructor"); 
   mid = (*env)->GetMethodID(env,tempClass,"getParameterTypes","()[Ljava/lang/Class;"); 
   (*env)->DeleteLocalRef(env,tempClass);

   /*===============================================*/
   /* Search the constructor list for a constructor */
   /* with matching arguments.                      */
   /*===============================================*/
      
   theSize = (*env)->GetArrayLength(env,constructorList); 
   for (c = 0; c < theSize; c++) 
     { 
      theConstructor = (*env)->GetObjectArrayElement(env,constructorList,c); 
      
      parameterList = (jobjectArray) (*env)->CallObjectMethod(env,theConstructor,mid);
      
      paramCount = (*env)->GetArrayLength(env,parameterList); 
      
      if (paramCount != (numberOfArguments - 2))
        { continue; }
        
      matches = true;
      
      for (p = 0; (p < paramCount) && matches; p++)
        {
         jstring str;
         char *cStr;
   
         tempClass = (jclass) (*env)->GetObjectArrayElement(env,parameterList,p);
         
         str = (jstring) (*env)->CallObjectMethod(env,tempClass,CLIPSJNIData(theEnv)->classGetCanonicalNameMethod);

         cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
                  
         if (newArgs[p].header->type == INTEGER_TYPE)
           {
            if (strcmp(cStr,"long") == 0)
              { 
               printf("p[%d] = %s\n",(int) p,cStr);
               javaArgs[p].j = newArgs[p].integerValue->contents;
              }
            else if (strcmp(cStr,"int") == 0)  
              { 
               printf("p[%d] = %s\n",(int) p,cStr);
               javaArgs[p].i = (jint) newArgs[p].integerValue->contents;
              }
            else
              { matches = false; }
           }
         else
           { matches = false; }
         
         (*env)->ReleaseStringUTFChars(env,str,cStr);
      
         (*env)->DeleteLocalRef(env,tempClass);
        }
      
      if (matches)
        { 
         found = true;
         break; 
        }
     } 

   /*==========================================*/
   /* If an appropriate constructor was found, */
   /* invoke it to create a new java object.   */
   /*==========================================*/
   
   theObject = NULL;
   if (found)
     {
      if (paramCount == 0)
        {
         mid = (*env)->FromReflectedMethod(env,theConstructor);
         theObject = (*env)->NewObject(env,theClass,mid);
        }
      else
        {
         mid = (*env)->FromReflectedMethod(env,theConstructor);
         theObject = (*env)->NewObjectA(env,theClass,mid,javaArgs);
        }
     }
 
   /*========================================================*/
   /* Delete the local reference to the class of the object. */
   /*========================================================*/
   
   (*env)->DeleteLocalRef(env,theClass);

   /*=========================================================*/
   /* If the object was created, add a global reference to it */
   /* and delete the local reference. This will prevent the   */
   /* object from being garbage collected until CLIPS deletes */
   /* the global reference.                                   */
   /*=========================================================*/
   
   if (theObject != NULL)
     {
      oldObject = theObject;
      theObject = (*env)->NewGlobalRef(env,theObject);
      (*env)->DeleteLocalRef(env,oldObject);
     }

   /*==========================================*/
   /* Return the array containing the UDFValue */
   /* arguments to the new function.           */
   /*==========================================*/
   
   if (newArgs != NULL)
     { genfree(theEnv,newArgs,sizeof(UDFValue) * (numberOfArguments - 2)); }
     
   if (javaArgs != NULL)
     { genfree(theEnv,javaArgs,sizeof(jvalue) * (numberOfArguments - 2)); }
   
   /*=============================================*/
   /* If a java exception occurred, set the CLIPS */
   /* error flag and clear the java exception.    */
   /*=============================================*/
   
   if ((*env)->ExceptionOccurred(env))
     { 
      SetEvaluationError(theEnv,true);
      (*env)->ExceptionClear(env); 
     }

   /*==========================================*/
   /* Return the newly created java object if  */
   /* it was successfully created, otherwise   */
   /* leave the default return value of FALSE. */
   /*==========================================*/
   
   if (theObject != NULL)
     {
      rv->value = CreateExternalAddress(theEnv,theObject,CLIPSJNIData(theEnv)->javaExternalAddressID);
     }
  }

/*******************/
/* CallJavaMethod: */
/*******************/
bool CallJavaMethod(
  UDFContext *context,
  UDFValue *target,
  UDFValue *rv)
  {
   int numberOfArguments;
   jobject theObject, theMethod;
   jclass objectClass, tempClass;
   jmethodID mid, getNameID;
   JNIEnv *env;
   jobjectArray methodList, parameterList;
   jsize theSize, c; 
   jsize paramCount, p; 
   UDFValue theValue;
   const char *methodName;
   jstring str;
   char *cStr;
   bool matches;
   UDFValue *newArgs;
   jvalue *javaArgs;
   int i;
   Environment *theEnv = context->environment;
   
   /*=============================================*/
   /* Retrieve the JNI environment pointer stored */
   /* in the CLIPS environment structure.         */
   /*=============================================*/
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   /*=================================================================*/
   /* If the Java external address type is used, additional arguments */
   /* must at least include the name of the method being called.      */
   /*=================================================================*/
   
   numberOfArguments = UDFArgumentCount(context);
   if (numberOfArguments < 2) 
     { return false; }

   /*========================================*/
   /* The Java method name must be a symbol. */
   /*========================================*/
   
   if (! UDFNthArgument(context,1,SYMBOL_TYPE,&theValue))
     { return false; }
   
   methodName = theValue.lexemeValue->contents;

   /*===========================================================================*/
   /* Evaluate the CLIPS arguments that will be passed to the java constructor. */
   /*===========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { newArgs = NULL; }
   else
     {
      newArgs = (UDFValue *) genalloc(theEnv,sizeof(UDFValue) * (numberOfArguments - 2));
      for (i = 0; i < numberOfArguments - 2; i++)
        {
         UDFNthArgument(context,i+3,ANY_TYPE_BITS,&newArgs[i]);
         if (GetEvaluationError(theEnv))
           { return false; }
        }
     }

   /*========================================================================*/
   /* Construct an array in which to store the corresponding java arguments. */
   /*========================================================================*/
   
   if (numberOfArguments - 2 == 0)
     { javaArgs = NULL; }
   else
     { javaArgs = (jvalue *) genalloc(theEnv,sizeof(jvalue) * (numberOfArguments - 2)); }

   /*===============================================*/
   /* If the target is an external address, then we */
   /* should be invoking a method of an instance.   */
   /*===============================================*/

   if (target->header->type == EXTERNAL_ADDRESS_TYPE)
     {
      theObject = ((CLIPSExternalAddress *) target->value)->contents;

      /*=========================================*/
      /* Determine the class of the java object. */
      /*=========================================*/
      
      objectClass = (*env)->GetObjectClass(env,theObject);

      /*=============================================*/
      /* Get the method index of the getConstructors */
      /* method from the java.lang.Class class.      */
      /*=============================================*/

      tempClass = (*env)->FindClass(env,"java/lang/Class"); /* TBD Cache this Value */
      mid = (*env)->GetMethodID(env,tempClass,"getMethods","()[Ljava/lang/reflect/Method;"); 
      (*env)->DeleteLocalRef(env,tempClass);

      /*==================================================*/
      /* Get the list of methods for the specified class. */
      /*==================================================*/
     
      methodList = (jobjectArray) (*env)->CallObjectMethod(env,objectClass,mid);
      (*env)->DeleteLocalRef(env,objectClass);

      /*======================================================*/
      /* Get the method index of the getParameterTypes method */
      /* from the java.lang.reflect.Method class.             */
      /*======================================================*/

      tempClass = (*env)->FindClass(env,"java/lang/reflect/Method"); /* TBD Cache this Value */
      mid = (*env)->GetMethodID(env,tempClass,"getParameterTypes","()[Ljava/lang/Class;"); 
      getNameID = (*env)->GetMethodID(env,tempClass,"getName","()Ljava/lang/String;"); 
      (*env)->DeleteLocalRef(env,tempClass);

      /*=====================================*/
      /* Search the method list for a method */
      /* with matching arguments.            */
      /*=====================================*/

      theSize = (*env)->GetArrayLength(env,methodList); 
      for (c = 0; c < theSize; c++) 
        { 
         theMethod = (*env)->GetObjectArrayElement(env,methodList,c); 
         str = (jstring) (*env)->CallObjectMethod(env,theMethod,getNameID);
         cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
         
         /*===================================*/
         /* If the method name doesn't match, */
         /* move on to the next method.       */
         /*===================================*/
         
         if (strcmp(methodName,cStr) != 0)
           {
            (*env)->ReleaseStringUTFChars(env,str,cStr);
            continue;
           }
         (*env)->ReleaseStringUTFChars(env,str,cStr);
         
         /*==========================================*/
         /* Get the parameter list of the method and */
         /* determine the number of parameters.      */
         /*==========================================*/
         
         parameterList = (jobjectArray) (*env)->CallObjectMethod(env,theMethod,mid);
      
         paramCount = (*env)->GetArrayLength(env,parameterList); 
      
         if (paramCount != (numberOfArguments - 2))
           { 
            (*env)->ReleaseStringUTFChars(env,str,cStr);
            continue; 
           }

         matches = true;
      
         for (p = 0; (p < paramCount) && matches; p++)
           {
            tempClass = (jclass) (*env)->GetObjectArrayElement(env,parameterList,p);
         
            str = (jstring) (*env)->CallObjectMethod(env,tempClass,CLIPSJNIData(theEnv)->classGetCanonicalNameMethod);

            cStr = (char *) (*env)->GetStringUTFChars(env,str,NULL);
             
            printf("p[%d] = %s\n",(int) p,cStr);
            
            if (newArgs[p].header->type == INTEGER_TYPE)
              {
               if (strcmp(cStr,"long") == 0)
                 { 
                  /* printf("p[%d] = %s\n",(int) p,cStr); */
                  javaArgs[p].j = newArgs[p].integerValue->contents;
                 }
               else if (strcmp(cStr,"int") == 0)  
                 { 
                  /* printf("p[%d] = %s\n",(int) p,cStr); */
                  javaArgs[p].i = (jint) newArgs[p].integerValue->contents;
                 }
               else
                 { matches = false; }
              }
            else
              { matches = false; }

            (*env)->ReleaseStringUTFChars(env,str,cStr);
         
            (*env)->DeleteLocalRef(env,tempClass);
           }
      
         if (matches) break;
        }
     }

   if (newArgs != NULL)
     { genfree(theEnv,newArgs,sizeof(UDFValue) * (numberOfArguments - 2)); }

   if (javaArgs != NULL)
     { genfree(theEnv,javaArgs,sizeof(jvalue) * (numberOfArguments - 2)); }
     
   return true;
  }
  
/***********************/
/* DiscardJavaAddress: */
/***********************/
bool DiscardJavaAddress(
  Environment *theEnv,
  void *theValue)
  {
   JNIEnv *env;

   printf("Discarding Java Address %p\n",theValue);
   
   if (theValue != NULL)
     {
      env = (JNIEnv *) GetEnvironmentContext(theEnv);
      (*env)->DeleteGlobalRef(env,theValue);
     }
   
   return true;
  }
  
/**********************************************/
/* DeallocateJNIData: Deallocates environment */
/*    data for the JNI functionality.         */
/**********************************************/
static void DeallocateJNIData(
  Environment *theEnv)
  {
   JNIEnv *env;
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);

   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->environmentClass);
   (*env)->DeleteWeakGlobalRef(env,CLIPSJNIData(theEnv)->environmentObject);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->userFunctionClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->classClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->longClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->doubleClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->arrayListClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->hashMapClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->bitSetClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->primitiveValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->voidValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->integerValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->floatValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->symbolValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->stringValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->instanceNameValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->lexemeValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->multifieldValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->factAddressValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->instanceAddressValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->factInstanceClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->slotValueClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->focusStackClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->focusClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->moduleClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->agendaClass);
   (*env)->DeleteGlobalRef(env,CLIPSJNIData(theEnv)->activationClass); 
  }

/*****************************/
/* CreateCLIPSJNIEnvironment */
/*****************************/
jlong CreateCLIPSJNIEnvironment(
  JNIEnv *env, 
  jobject obj)
  {
   void *theEnv;
   jclass theEnvironmentClass; 
   jclass theUserFunctionClass; 
   jmethodID theUserFunctionEvaluateMethod;
   jclass theClassClass; 
   jmethodID theClassGetCanonicalNameMethod;
   jclass theLongClass; 
   jmethodID theLongInitMethod;
   jclass theDoubleClass; 
   jmethodID theDoubleInitMethod;
   jclass theArrayListClass; 
   jmethodID theArrayListInitMethod, theArrayListAddMethod;
   jclass theHashMapClass; 
   jmethodID theHashMapInitMethod, theHashMapPutMethod;
   jclass theBitSetClass; 
   jmethodID theBitSetInitMethod, theBitSetSetMethod;
   jclass theVoidValueClass;
   jmethodID theVoidValueInitMethod;
   jclass thePrimitiveValueClass;
   jmethodID theGetCLIPSTypeValueMethod;
   jclass theIntegerValueClass, theFloatValueClass;
   jmethodID theIntegerValueInitMethod, theIntegerValueLongValueMethod;
   jmethodID theFloatValueInitMethod, theFloatValueDoubleValueMethod;
   jclass theSymbolValueClass, theStringValueClass, theInstanceNameValueClass;
   jmethodID theSymbolValueInitMethod, theStringValueInitMethod, theInstanceNameValueInitMethod;
   jclass theLexemeValueClass;
   jmethodID theLexemeValueGetValueMethod;
   jclass theMultifieldValueClass;
   jmethodID theMultifieldValueInitMethod;
   jmethodID theMultifieldValueGetMethod, theMultifieldValueSizeMethod;
   jclass theFactAddressValueClass;
   jmethodID theFactAddressValueInitMethod;
   jmethodID theFactAddressValueGetFactAddressMethod;
   jclass theInstanceAddressValueClass;
   jmethodID theInstanceAddressValueInitMethod;
   jmethodID theInstanceAddressValueGetInstanceAddressMethod;
   jclass theFactInstanceClass;
   jmethodID theFactInstanceInitMethod;
   jclass theSlotValueClass;
   jmethodID theSlotValueInitMethod;
   jclass theFocusClass;
   jmethodID theFocusInitMethod;
   jclass theModuleClass;
   jmethodID theModuleInitMethod;
   jclass theFocusStackClass;
   jmethodID theFocusStackInitMethod;
   jclass theAgendaClass;
   jmethodID theAgendaInitMethod;
   jclass theActivationClass;
   jmethodID theActivationInitMethod;
   struct externalAddressType javaPointer = { "java", PrintJavaAddress, PrintJavaAddress, DiscardJavaAddress, NewJavaAddress, CallJavaMethod };

   /*===========================*/
   /* Look up the Java classes. */
   /*===========================*/

   theEnvironmentClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/Environment"); 
   theUserFunctionClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/UserFunction"); 
   theClassClass = (*env)->FindClass(env,"java/lang/Class"); 
   theLongClass = (*env)->FindClass(env,"java/lang/Long"); 
   theDoubleClass = (*env)->FindClass(env,"java/lang/Double"); 
   theArrayListClass = (*env)->FindClass(env,"java/util/ArrayList"); 
   theHashMapClass = (*env)->FindClass(env,"java/util/HashMap"); 
   theBitSetClass = (*env)->FindClass(env,"java/util/BitSet"); 
   thePrimitiveValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/PrimitiveValue");
   theVoidValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/VoidValue");
   theIntegerValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/IntegerValue");
   theFloatValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/FloatValue");
   theSymbolValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/SymbolValue");
   theStringValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/StringValue");
   theInstanceNameValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/InstanceNameValue");
   theLexemeValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/LexemeValue");
   theMultifieldValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/MultifieldValue");
   theFactAddressValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/FactAddressValue");
   theInstanceAddressValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/InstanceAddressValue");
   theFactInstanceClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/FactInstance");
   theSlotValueClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/SlotValue");
   theFocusClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/Focus");
   theFocusStackClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/FocusStack");
   theModuleClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/Module");
   theAgendaClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/Agenda");
   theActivationClass = (*env)->FindClass(env,"net/sf/clipsrules/jni/Activation");
                
   /*=========================================*/
   /* If the Java classes could not be found, */
   /* abort creation of the environment.      */
   /*=========================================*/

   if ((theEnvironmentClass == NULL) ||
       (theUserFunctionClass == NULL) ||
       (theClassClass == NULL) ||
       (theLongClass == NULL) || (theDoubleClass == NULL) ||
       (theArrayListClass == NULL) ||
       (theHashMapClass == NULL) ||
       (theBitSetClass == NULL) ||
       (thePrimitiveValueClass == NULL) ||
       (theVoidValueClass == NULL) ||
       (theIntegerValueClass == NULL) || (theFloatValueClass == NULL) ||
       (theSymbolValueClass == NULL) || (theStringValueClass == NULL) || 
       (theInstanceNameValueClass == NULL) ||
       (theLexemeValueClass == NULL) ||
       (theMultifieldValueClass == NULL) ||
       (theFactAddressValueClass == NULL) ||
       (theInstanceAddressValueClass == NULL) ||
       (theFactInstanceClass == NULL) ||
       (theSlotValueClass == NULL) ||
       (theFocusClass == NULL) ||
       (theFocusStackClass == NULL) ||
       (theModuleClass == NULL) ||
       (theAgendaClass == NULL) ||
       (theActivationClass == NULL))
     { return((jlong) NULL); }
     
   /*================================*/
   /* Look up the Java init methods. */
   /*================================*/

   theUserFunctionEvaluateMethod = 
      (*env)->GetMethodID(env,theUserFunctionClass,"evaluate",
                          "(Ljava/util/List;)Lnet/sf/clipsrules/jni/PrimitiveValue;");
   
   theClassGetCanonicalNameMethod = (*env)->GetMethodID(env,theClassClass,"getCanonicalName","()Ljava/lang/String;");
   theLongInitMethod = (*env)->GetMethodID(env,theLongClass,"<init>","(J)V");
   theDoubleInitMethod = (*env)->GetMethodID(env,theDoubleClass,"<init>","(D)V");
   theArrayListInitMethod = (*env)->GetMethodID(env,theArrayListClass,"<init>","(I)V");
   theArrayListAddMethod = (*env)->GetMethodID(env,theArrayListClass,"add","(Ljava/lang/Object;)Z");
   theHashMapInitMethod = (*env)->GetMethodID(env,theHashMapClass,"<init>","()V");
   theHashMapPutMethod = (*env)->GetMethodID(env,theHashMapClass,"put","(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
   theBitSetInitMethod = (*env)->GetMethodID(env,theBitSetClass,"<init>","(I)V");
   theBitSetSetMethod = (*env)->GetMethodID(env,theBitSetClass,"set","(I)V");
   theGetCLIPSTypeValueMethod = (*env)->GetMethodID(env,thePrimitiveValueClass,"getCLIPSTypeValue","()I");
   theVoidValueInitMethod = (*env)->GetMethodID(env,theVoidValueClass,"<init>","()V");
   theIntegerValueInitMethod = (*env)->GetMethodID(env,theIntegerValueClass,"<init>","(Ljava/lang/Long;)V");
   theIntegerValueLongValueMethod = (*env)->GetMethodID(env,theIntegerValueClass,"longValue","()J");
   theFloatValueInitMethod = (*env)->GetMethodID(env,theFloatValueClass,"<init>","(Ljava/lang/Double;)V");
   theFloatValueDoubleValueMethod = (*env)->GetMethodID(env,theFloatValueClass,"doubleValue","()D");
   theSymbolValueInitMethod = (*env)->GetMethodID(env,theSymbolValueClass,"<init>","(Ljava/lang/String;)V");
   theStringValueInitMethod = (*env)->GetMethodID(env,theStringValueClass,"<init>","(Ljava/lang/String;)V");
   theLexemeValueGetValueMethod = (*env)->GetMethodID(env,theLexemeValueClass,"getValue","()Ljava/lang/String;");
   theInstanceNameValueInitMethod = (*env)->GetMethodID(env,theInstanceNameValueClass,"<init>","(Ljava/lang/String;)V");
   theMultifieldValueInitMethod = (*env)->GetMethodID(env,theMultifieldValueClass,"<init>","(Ljava/util/List;)V");
   theMultifieldValueGetMethod = (*env)->GetMethodID(env,theMultifieldValueClass,"get","(I)Lnet/sf/clipsrules/jni/PrimitiveValue;");
   theMultifieldValueSizeMethod = (*env)->GetMethodID(env,theMultifieldValueClass,"size","()I");
   theFactAddressValueInitMethod = (*env)->GetMethodID(env,theFactAddressValueClass,"<init>","(JLnet/sf/clipsrules/jni/Environment;)V");
   theFactAddressValueGetFactAddressMethod = (*env)->GetMethodID(env,theFactAddressValueClass,"getFactAddress","()J");
   theInstanceAddressValueInitMethod = (*env)->GetMethodID(env,theInstanceAddressValueClass,"<init>","(JLnet/sf/clipsrules/jni/Environment;)V");
   theInstanceAddressValueGetInstanceAddressMethod = (*env)->GetMethodID(env,theInstanceAddressValueClass,"getInstanceAddress","()J");
   theFactInstanceInitMethod = (*env)->GetMethodID(env,theFactInstanceClass,"<init>","(JLjava/lang/String;Ljava/lang/String;Ljava/util/List;)V");
   theSlotValueInitMethod = (*env)->GetMethodID(env,theSlotValueClass,"<init>","(Ljava/lang/String;Ljava/lang/String;Z)V");
   theFocusInitMethod = (*env)->GetMethodID(env,theFocusClass,"<init>","(Ljava/lang/String;)V");
   theFocusStackInitMethod = (*env)->GetMethodID(env,theFocusStackClass,"<init>","(Ljava/util/List;)V");
   theModuleInitMethod = (*env)->GetMethodID(env,theModuleClass,"<init>","(Ljava/lang/String;)V");
   theActivationInitMethod = (*env)->GetMethodID(env,theActivationClass,"<init>","(Ljava/lang/String;ILjava/lang/String;)V");
   theAgendaInitMethod = (*env)->GetMethodID(env,theAgendaClass,"<init>","(Ljava/util/List;)V");
     
   /*==============================================*/
   /* If the Java init methods could not be found, */
   /* abort creation of the enviroment.            */
   /*==============================================*/
     
   if ((theUserFunctionEvaluateMethod == NULL) ||
       (theClassGetCanonicalNameMethod == NULL) ||
       (theLongInitMethod == NULL) || (theDoubleInitMethod == NULL) || 
       (theArrayListInitMethod == NULL) || (theArrayListAddMethod == NULL) ||
       (theHashMapInitMethod == NULL) || (theHashMapPutMethod == NULL) ||
       (theBitSetInitMethod == NULL) || (theBitSetSetMethod == NULL) ||
       (theGetCLIPSTypeValueMethod == NULL) ||
       (theVoidValueInitMethod == NULL) ||
       (theIntegerValueInitMethod == NULL) || (theIntegerValueLongValueMethod == NULL) || 
       (theFloatValueInitMethod == NULL) || (theFloatValueDoubleValueMethod == NULL) ||
       (theSymbolValueInitMethod == NULL) || (theStringValueInitMethod == NULL) ||
       (theInstanceNameValueInitMethod == NULL) ||
       (theLexemeValueGetValueMethod == NULL) ||
       (theMultifieldValueInitMethod == NULL) ||
       (theMultifieldValueGetMethod == NULL) ||
       (theMultifieldValueSizeMethod == NULL) ||
       (theFactAddressValueInitMethod == NULL) ||
       (theFactAddressValueGetFactAddressMethod == NULL) ||
       (theInstanceAddressValueInitMethod == NULL) ||
       (theInstanceAddressValueGetInstanceAddressMethod == NULL) ||
       (theFactInstanceInitMethod == NULL) ||
       (theSlotValueInitMethod == NULL) ||
       (theFocusInitMethod == NULL) ||
       (theFocusStackInitMethod == NULL) ||
       (theModuleInitMethod == NULL) ||
       (theAgendaInitMethod == NULL) ||
       (theActivationInitMethod == NULL))
     { return((jlong) NULL);  }
    
   /*=========================*/
   /* Create the environment. */
   /*=========================*/

   theEnv = CreateEnvironment();
   if (theEnv == NULL) return((jlong) NULL);
   
   /*====================================*/
   /* Allocate the JNI environment data. */
   /*====================================*/

   AllocateEnvironmentData(theEnv,CLIPSJNI_DATA,sizeof(struct clipsJNIData),DeallocateJNIData);

   /*===================================================*/
   /* Cache the class and method references (converting */
   /* the local class references to global references   */
   /* so they won't be garbage collected.               */
   /*===================================================*/
   
   CLIPSJNIData(theEnv)->environmentClass = (*env)->NewGlobalRef(env,theEnvironmentClass);
   CLIPSJNIData(theEnv)->environmentObject = (*env)->NewWeakGlobalRef(env,obj);
   
   CLIPSJNIData(theEnv)->userFunctionClass = (*env)->NewGlobalRef(env,theUserFunctionClass);
   CLIPSJNIData(theEnv)->userFunctionEvaluateMethod = theUserFunctionEvaluateMethod;
   
   CLIPSJNIData(theEnv)->classClass = (*env)->NewGlobalRef(env,theClassClass);
   CLIPSJNIData(theEnv)->classGetCanonicalNameMethod = theClassGetCanonicalNameMethod;

   CLIPSJNIData(theEnv)->longClass = (*env)->NewGlobalRef(env,theLongClass);
   CLIPSJNIData(theEnv)->longInitMethod = theLongInitMethod;
   CLIPSJNIData(theEnv)->doubleClass = (*env)->NewGlobalRef(env,theDoubleClass);
   CLIPSJNIData(theEnv)->doubleInitMethod = theDoubleInitMethod;
   CLIPSJNIData(theEnv)->arrayListClass = (*env)->NewGlobalRef(env,theArrayListClass);
   CLIPSJNIData(theEnv)->arrayListInitMethod = theArrayListInitMethod;
   CLIPSJNIData(theEnv)->arrayListAddMethod = theArrayListAddMethod;

   CLIPSJNIData(theEnv)->hashMapClass = (*env)->NewGlobalRef(env,theHashMapClass);
   CLIPSJNIData(theEnv)->hashMapInitMethod = theHashMapInitMethod;
   CLIPSJNIData(theEnv)->hashMapPutMethod = theHashMapPutMethod;

   CLIPSJNIData(theEnv)->bitSetClass = (*env)->NewGlobalRef(env,theBitSetClass);
   CLIPSJNIData(theEnv)->bitSetInitMethod = theBitSetInitMethod;
   CLIPSJNIData(theEnv)->bitSetSetMethod = theBitSetSetMethod;

   CLIPSJNIData(theEnv)->primitiveValueClass = (*env)->NewGlobalRef(env,thePrimitiveValueClass);
   CLIPSJNIData(theEnv)->getCLIPSTypeValueMethod = theGetCLIPSTypeValueMethod;

   CLIPSJNIData(theEnv)->voidValueClass = (*env)->NewGlobalRef(env,theVoidValueClass);
   CLIPSJNIData(theEnv)->voidValueInitMethod = theVoidValueInitMethod;
   
   CLIPSJNIData(theEnv)->integerValueClass = (*env)->NewGlobalRef(env,theIntegerValueClass);
   CLIPSJNIData(theEnv)->integerValueInitMethod = theIntegerValueInitMethod;
   CLIPSJNIData(theEnv)->integerValueLongValueMethod = theIntegerValueLongValueMethod;
   CLIPSJNIData(theEnv)->floatValueClass = (*env)->NewGlobalRef(env,theFloatValueClass);
   CLIPSJNIData(theEnv)->floatValueInitMethod = theFloatValueInitMethod;
   CLIPSJNIData(theEnv)->floatValueDoubleValueMethod = theFloatValueDoubleValueMethod;
      
   CLIPSJNIData(theEnv)->symbolValueClass = (*env)->NewGlobalRef(env,theSymbolValueClass);
   CLIPSJNIData(theEnv)->symbolValueInitMethod = theSymbolValueInitMethod;
   CLIPSJNIData(theEnv)->stringValueClass = (*env)->NewGlobalRef(env,theStringValueClass);
   CLIPSJNIData(theEnv)->stringValueInitMethod = theStringValueInitMethod;
   CLIPSJNIData(theEnv)->instanceNameValueClass = (*env)->NewGlobalRef(env,theInstanceNameValueClass);
   CLIPSJNIData(theEnv)->instanceNameValueInitMethod = theInstanceNameValueInitMethod;
   
   CLIPSJNIData(theEnv)->lexemeValueClass = (*env)->NewGlobalRef(env,theLexemeValueClass);
   CLIPSJNIData(theEnv)->lexemeValueGetValueMethod = theLexemeValueGetValueMethod;

   CLIPSJNIData(theEnv)->multifieldValueClass = (*env)->NewGlobalRef(env,theMultifieldValueClass);
   CLIPSJNIData(theEnv)->multifieldValueInitMethod = theMultifieldValueInitMethod;
   CLIPSJNIData(theEnv)->multifieldValueGetMethod = theMultifieldValueGetMethod;
   CLIPSJNIData(theEnv)->multifieldValueSizeMethod = theMultifieldValueSizeMethod;

   CLIPSJNIData(theEnv)->factAddressValueClass = (*env)->NewGlobalRef(env,theFactAddressValueClass);
   CLIPSJNIData(theEnv)->factAddressValueInitMethod = theFactAddressValueInitMethod;
   CLIPSJNIData(theEnv)->factAddressValueGetFactAddressMethod = theFactAddressValueGetFactAddressMethod;

   CLIPSJNIData(theEnv)->instanceAddressValueClass = (*env)->NewGlobalRef(env,theInstanceAddressValueClass);
   CLIPSJNIData(theEnv)->instanceAddressValueInitMethod = theInstanceAddressValueInitMethod;
   CLIPSJNIData(theEnv)->instanceAddressValueGetInstanceAddressMethod = theInstanceAddressValueGetInstanceAddressMethod;

   CLIPSJNIData(theEnv)->factInstanceClass = (*env)->NewGlobalRef(env,theFactInstanceClass);
   CLIPSJNIData(theEnv)->factInstanceInitMethod = theFactInstanceInitMethod;

   CLIPSJNIData(theEnv)->slotValueClass = (*env)->NewGlobalRef(env,theSlotValueClass);
   CLIPSJNIData(theEnv)->slotValueInitMethod = theSlotValueInitMethod;
   
   CLIPSJNIData(theEnv)->focusClass = (*env)->NewGlobalRef(env,theFocusClass);
   CLIPSJNIData(theEnv)->focusInitMethod = theFocusInitMethod;

   CLIPSJNIData(theEnv)->focusStackClass = (*env)->NewGlobalRef(env,theFocusStackClass);
   CLIPSJNIData(theEnv)->focusStackInitMethod = theFocusStackInitMethod;
   
   CLIPSJNIData(theEnv)->moduleClass = (*env)->NewGlobalRef(env,theModuleClass);
   CLIPSJNIData(theEnv)->moduleInitMethod = theModuleInitMethod;

   CLIPSJNIData(theEnv)->activationClass = (*env)->NewGlobalRef(env,theActivationClass);
   CLIPSJNIData(theEnv)->activationInitMethod = theActivationInitMethod;

   CLIPSJNIData(theEnv)->agendaClass = (*env)->NewGlobalRef(env,theAgendaClass);
   CLIPSJNIData(theEnv)->agendaInitMethod = theAgendaInitMethod;
   
   /*======================================*/
   /* Store the java environment for later */
   /* access by the CLIPS environment.     */
   /*======================================*/
 
   SetEnvironmentContext(theEnv,(void *) env);
     
   /*=======================================*/
   /* Deallocate the local Java references. */
   /*=======================================*/
   
   (*env)->DeleteLocalRef(env,theEnvironmentClass);
   (*env)->DeleteLocalRef(env,theUserFunctionClass);
   (*env)->DeleteLocalRef(env,theClassClass);
   (*env)->DeleteLocalRef(env,theLongClass);
   (*env)->DeleteLocalRef(env,theDoubleClass);
   (*env)->DeleteLocalRef(env,theArrayListClass);
   (*env)->DeleteLocalRef(env,theHashMapClass);
   (*env)->DeleteLocalRef(env,theBitSetClass);
   (*env)->DeleteLocalRef(env,thePrimitiveValueClass);
   (*env)->DeleteLocalRef(env,theVoidValueClass);
   (*env)->DeleteLocalRef(env,theIntegerValueClass);
   (*env)->DeleteLocalRef(env,theFloatValueClass);
   (*env)->DeleteLocalRef(env,theSymbolValueClass);
   (*env)->DeleteLocalRef(env,theStringValueClass);
   (*env)->DeleteLocalRef(env,theInstanceNameValueClass);
   (*env)->DeleteLocalRef(env,theLexemeValueClass);
   (*env)->DeleteLocalRef(env,theMultifieldValueClass);
   (*env)->DeleteLocalRef(env,theFactAddressValueClass);
   (*env)->DeleteLocalRef(env,theInstanceAddressValueClass);
   (*env)->DeleteLocalRef(env,theFactInstanceClass);
   (*env)->DeleteLocalRef(env,theSlotValueClass);
   (*env)->DeleteLocalRef(env,theFocusStackClass);
   (*env)->DeleteLocalRef(env,theFocusClass);
   (*env)->DeleteLocalRef(env,theModuleClass);
   (*env)->DeleteLocalRef(env,theAgendaClass);
   (*env)->DeleteLocalRef(env,theActivationClass);

   /*=================================*/
   /* Set up Java External Addresses. */
   /*=================================*/
   
   CLIPSJNIData(theEnv)->javaExternalAddressID = InstallExternalAddressType(theEnv,&javaPointer);
   
   /*=========================*/
   /* Return the environment. */
   /*=========================*/
   
   return (PointerToJLong(theEnv));
  }
