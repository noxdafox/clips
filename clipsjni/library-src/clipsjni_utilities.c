#include "clipsjni_utilities.h"
#include "clipsjni_data.h"

static void      *ConvertSingleFieldPrimitiveValue(Environment *,int,jobject);

/******************/
/* JLongToPointer */
/******************/
void *JLongToPointer(
  jlong value)
  {
   return (void *) value;
  }

/******************/
/* PointerToJLong */
/******************/
jlong PointerToJLong(
  void *value)
  {
   return (jlong) value;
  }

/**********************/
/* ConvertDataObject: */
/**********************/
jobject ConvertDataObject(
  JNIEnv *env,
  jobject javaEnv,
  void *clipsEnv,
  CLIPSValue *theDO)
  {
   jobject result = NULL, tresult;
   jint mfLength;
   struct multifield *theList;
   long i;
   
   switch(GetpType(theDO))
     {
      case MULTIFIELD:
        mfLength = GetpDOLength(theDO);

        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->arrayListClass,
                                   CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                   (jint) mfLength);
                                   
        if (result == NULL)
          { return result; }
          
        theList = (struct multifield *) DOPToPointer(theDO);
        
        for (i = GetpDOBegin(theDO); i <= GetpDOEnd(theDO); i++)
         {
          tresult = ConvertSingleFieldValue(env,javaEnv,clipsEnv,GetMFType(theList,i),GetMFValue(theList,i));
          
          if (tresult != NULL)
             { (*env)->CallBooleanMethod(env,result,CLIPSJNIData(clipsEnv)->arrayListAddMethod,tresult); }

          (*env)->DeleteLocalRef(env,tresult);
         }
       
        tresult = result;
         
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->multifieldValueClass,
                                   CLIPSJNIData(clipsEnv)->multifieldValueInitMethod,
                                   tresult);
        break;
        
      case RVOID:
      case SYMBOL:
      case STRING:
      case INSTANCE_NAME:
      case INTEGER:
      case FLOAT:
      case FACT_ADDRESS:
      case INSTANCE_ADDRESS:
        result = ConvertSingleFieldValue(env,javaEnv,clipsEnv,GetpType(theDO),GetpValue(theDO));
        break;

      default: 
        break;
     }

   return result;  
  }
  
/****************************/
/* ConvertSingleFieldValue: */
/****************************/
jobject ConvertSingleFieldValue(
  JNIEnv *env,
  jobject javaEnv,
  void *clipsEnv,
  int type,
  void  *value)
  {
   jobject result = NULL, tresult;
   jstring sresult = NULL;
   
   switch(type)
     {
      case RVOID:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->voidValueClass,
                                   CLIPSJNIData(clipsEnv)->voidValueInitMethod); 
        break;

      case SYMBOL:
        sresult = (*env)->NewStringUTF(env,ValueToString(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->symbolValueClass,
                                   CLIPSJNIData(clipsEnv)->symbolValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
        
      case STRING:
        sresult = (*env)->NewStringUTF(env,ValueToString(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->stringValueClass,
                                   CLIPSJNIData(clipsEnv)->stringValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
      case INSTANCE_NAME:
        sresult = (*env)->NewStringUTF(env,ValueToString(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->instanceNameValueClass,
                                   CLIPSJNIData(clipsEnv)->instanceNameValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
      case INTEGER:
        tresult = (*env)->NewObject(env,
                                    CLIPSJNIData(clipsEnv)->longClass,
                                    CLIPSJNIData(clipsEnv)->longInitMethod,
                                    (jlong) ValueToLong(value));
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->integerValueClass,
                                   CLIPSJNIData(clipsEnv)->integerValueInitMethod,
                                   tresult);
        (*env)->DeleteLocalRef(env,tresult);
        break;

      case FLOAT:
        tresult = (*env)->NewObject(env,
                                    CLIPSJNIData(clipsEnv)->doubleClass,
                                    CLIPSJNIData(clipsEnv)->doubleInitMethod,
                                    (jdouble) ValueToDouble(value));

        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->floatValueClass,
                                   CLIPSJNIData(clipsEnv)->floatValueInitMethod,
                                   tresult);
        (*env)->DeleteLocalRef(env,tresult);
        break;

      case FACT_ADDRESS:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->factAddressValueClass,
                                   CLIPSJNIData(clipsEnv)->factAddressValueInitMethod,
                                   PointerToJLong(value),javaEnv);
        break;

      case INSTANCE_ADDRESS:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->instanceAddressValueClass,
                                   CLIPSJNIData(clipsEnv)->instanceAddressValueInitMethod,
                                   PointerToJLong(value),javaEnv);
        break;

      default: 
        break;
     }

   return result;  
  }
  
/*************************************/
/* ConvertSingleFieldPrimitiveValue: */
/*************************************/
static void *ConvertSingleFieldPrimitiveValue(
  Environment *theEnv,
  int theType,
  jobject theValue)
  {
   void *rv = NULL;
   JNIEnv *env;
   
   env = (JNIEnv *) GetEnvironmentContext(theEnv);
   
   switch (theType)
     {
      case SYMBOL:
      case STRING:
      case INSTANCE_NAME:
        {
         jstring theString = (*env)->CallObjectMethod(env,theValue,CLIPSJNIData(theEnv)->lexemeValueGetValueMethod);
         const char *cString = (*env)->GetStringUTFChars(env,theString,NULL);
         rv = EnvAddSymbol(theEnv,cString);
         (*env)->ReleaseStringUTFChars(env,theString,cString);
         break;
        }
        
      case FLOAT:
        {
         jdouble theDouble = (*env)->CallDoubleMethod(env,theValue,CLIPSJNIData(theEnv)->floatValueDoubleValueMethod);
         rv = EnvAddDouble(theEnv,theDouble);
         break;
        }

      case INTEGER:
        {
         jlong theLong = (*env)->CallLongMethod(env,theValue,CLIPSJNIData(theEnv)->integerValueLongValueMethod);
         rv = EnvAddLong(theEnv,theLong);
         break;
        }

      case FACT_ADDRESS:
        {
         jlong theLong = (*env)->CallLongMethod(env,theValue,CLIPSJNIData(theEnv)->factAddressValueGetFactAddressMethod);
         rv = JLongToPointer(theLong);
         break;
        }

      case INSTANCE_ADDRESS:
        {
         jlong theLong = (*env)->CallLongMethod(env,theValue,CLIPSJNIData(theEnv)->instanceAddressValueGetInstanceAddressMethod);
         rv = JLongToPointer(theLong);
         break;
        }
     }

   return rv;
  }

/**************************************/
/* ConvertPrimitiveValueToDataObject: */
/**************************************/
void ConvertPrimitiveValueToDataObject(
  Environment *theEnv,
  jobject theValue,
  CLIPSValue *theDO)
  {
   void *result = NULL;
   JNIEnv *env = (JNIEnv *) GetEnvironmentContext(theEnv);
  
   if (theValue == NULL)
     {
      theDO->type = RVOID;
      return;
     }
   
   theDO->type = (*env)->CallIntMethod(env,theValue,CLIPSJNIData(theEnv)->getCLIPSTypeValueMethod);

   switch(theDO->type)
     {
      case MULTIFIELD:
        {
         jint i;
         jint theSize = (*env)->CallIntMethod(env,theValue,CLIPSJNIData(theEnv)->multifieldValueSizeMethod);
         result = EnvCreateMultifield(theEnv,theSize);
         for (i = 0; i < theSize; i++)
           {         
            jobject mfo = (*env)->CallObjectMethod(env,theValue,CLIPSJNIData(theEnv)->multifieldValueGetMethod,i);
            int mft = (*env)->CallIntMethod(env,mfo,CLIPSJNIData(theEnv)->getCLIPSTypeValueMethod);
            void *mfv = ConvertSingleFieldPrimitiveValue(theEnv,mft,mfo); 
            SetMFType(result,i+1,mft);
            SetMFValue(result,i+1,mfv);
           }
           
         SetpDOBegin(theDO,1);
         SetpDOEnd(theDO,GetMFLength(result));
         break;
        }
        
      case RVOID:
      case SYMBOL:
      case STRING:
      case INSTANCE_NAME:
      case INTEGER:
      case FLOAT:
      case FACT_ADDRESS:
      case INSTANCE_ADDRESS:
        result = ConvertSingleFieldPrimitiveValue(theEnv,theDO->type,theValue);
        break;

      default: 
        theDO->type = RVOID;
        break;
     }

   theDO->value = result;
  }

