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
  Environment *clipsEnv,
  CLIPSValue *theDO)
  {
   jobject result = NULL, tresult;
   jint mfLength;
   Multifield *theList;
   size_t i;
   
   switch(theDO->header->type)
     {
      case MULTIFIELD_TYPE:
        mfLength = (jint) theDO->multifieldValue->length;

        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->arrayListClass,
                                   CLIPSJNIData(clipsEnv)->arrayListInitMethod,
                                   mfLength);
                                   
        if (result == NULL)
          { return result; }
          
        theList = theDO->multifieldValue;
        
        for (i = 0; i < theDO->multifieldValue->length; i++)
         {
          tresult = ConvertSingleFieldValue(env,javaEnv,clipsEnv,
                                            theList->contents[i].header->type,
                                            theList->contents[i].value);
          
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
        
      case VOID_TYPE:
      case SYMBOL_TYPE:
      case STRING_TYPE:
      case INSTANCE_NAME_TYPE:
      case INTEGER_TYPE:
      case FLOAT_TYPE:
      case FACT_ADDRESS_TYPE:
      case INSTANCE_ADDRESS_TYPE:
        result = ConvertSingleFieldValue(env,javaEnv,clipsEnv,theDO->header->type,theDO->value);
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
  Environment *clipsEnv,
  int type,
  void  *value)
  {
   jobject result = NULL, tresult;
   jstring sresult = NULL;
   
   switch(type)
     {
      case VOID_TYPE:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->voidValueClass,
                                   CLIPSJNIData(clipsEnv)->voidValueInitMethod); 
        break;

      case SYMBOL_TYPE:
        sresult = (*env)->NewStringUTF(env,((CLIPSLexeme *) value)->contents);
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->symbolValueClass,
                                   CLIPSJNIData(clipsEnv)->symbolValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
        
      case STRING_TYPE:
        sresult = (*env)->NewStringUTF(env,((CLIPSLexeme *) value)->contents);
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->stringValueClass,
                                   CLIPSJNIData(clipsEnv)->stringValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
      case INSTANCE_NAME_TYPE:
        sresult = (*env)->NewStringUTF(env,((CLIPSLexeme *) value)->contents);
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->instanceNameValueClass,
                                   CLIPSJNIData(clipsEnv)->instanceNameValueInitMethod,
                                   sresult);
        (*env)->DeleteLocalRef(env,sresult);
        break;
        
      case INTEGER_TYPE:
        tresult = (*env)->NewObject(env,
                                    CLIPSJNIData(clipsEnv)->longClass,
                                    CLIPSJNIData(clipsEnv)->longInitMethod,
                                    (jlong) ((CLIPSInteger *) value)->contents);
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->integerValueClass,
                                   CLIPSJNIData(clipsEnv)->integerValueInitMethod,
                                   tresult);
        (*env)->DeleteLocalRef(env,tresult);
        break;

      case FLOAT_TYPE:
        tresult = (*env)->NewObject(env,
                                    CLIPSJNIData(clipsEnv)->doubleClass,
                                    CLIPSJNIData(clipsEnv)->doubleInitMethod,
                                    (jdouble) ((CLIPSFloat *) value)->contents);

        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->floatValueClass,
                                   CLIPSJNIData(clipsEnv)->floatValueInitMethod,
                                   tresult);
        (*env)->DeleteLocalRef(env,tresult);
        break;

      case FACT_ADDRESS_TYPE:
        result = (*env)->NewObject(env,
                                   CLIPSJNIData(clipsEnv)->factAddressValueClass,
                                   CLIPSJNIData(clipsEnv)->factAddressValueInitMethod,
                                   PointerToJLong(value),javaEnv);
        break;

      case INSTANCE_ADDRESS_TYPE:
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
  Environment *theCLIPSEnv,
  int theType,
  jobject theValue)
  {
   void *rv = NULL;
   JNIEnv *env;
   
   env = (JNIEnv *) GetEnvironmentContext(theCLIPSEnv);
   
   switch (theType)
     {
      case SYMBOL_TYPE:
        {
         jstring theString = (*env)->CallObjectMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->lexemeValueGetValueMethod);
         const char *cString = (*env)->GetStringUTFChars(env,theString,NULL);
         rv = CreateSymbol(theCLIPSEnv,cString);
         (*env)->ReleaseStringUTFChars(env,theString,cString);
         break;
        }

      case STRING_TYPE:
        {
         jstring theString = (*env)->CallObjectMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->lexemeValueGetValueMethod);
         const char *cString = (*env)->GetStringUTFChars(env,theString,NULL);
         rv = CreateString(theCLIPSEnv,cString);
         (*env)->ReleaseStringUTFChars(env,theString,cString);
         break;
        }
      
      case INSTANCE_NAME_TYPE:
        {
         jstring theString = (*env)->CallObjectMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->lexemeValueGetValueMethod);
         const char *cString = (*env)->GetStringUTFChars(env,theString,NULL);
         rv = CreateInstanceName(theCLIPSEnv,cString);
         (*env)->ReleaseStringUTFChars(env,theString,cString);
         break;
        }
        
      case FLOAT_TYPE:
        {
         jdouble theDouble = (*env)->CallDoubleMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->floatValueDoubleValueMethod);
         rv = CreateFloat(theCLIPSEnv,theDouble);
         break;
        }

      case INTEGER_TYPE:
        {
         jlong theLong = (*env)->CallLongMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->integerValueLongValueMethod);
         rv = CreateInteger(theCLIPSEnv,theLong);
         break;
        }

      case FACT_ADDRESS_TYPE:
        {
         jlong theLong = (*env)->CallLongMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->factAddressValueGetFactAddressMethod);
         rv = JLongToPointer(theLong);
         break;
        }

      case INSTANCE_ADDRESS_TYPE:
        {
         jlong theLong = (*env)->CallLongMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->instanceAddressValueGetInstanceAddressMethod);
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
  Environment *theCLIPSEnv,
  jobject theValue,
  UDFValue *theDO)
  { 
   unsigned short theType;
   Multifield *result = NULL;
   JNIEnv *env = (JNIEnv *) GetEnvironmentContext(theCLIPSEnv);
  
   if (theValue == NULL)
     {
      theDO->voidValue = theCLIPSEnv->VoidConstant;
      return;
     }
   
   theType = (int) (*env)->CallIntMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->getCLIPSTypeValueMethod);

   switch(theType)
     {
      case MULTIFIELD_TYPE:
        {
         jint i;
         jint theSize = (*env)->CallIntMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->multifieldValueSizeMethod);
         result = CreateMultifield(theCLIPSEnv,theSize);
         for (i = 0; i < theSize; i++)
           {         
            jobject mfo = (*env)->CallObjectMethod(env,theValue,CLIPSJNIData(theCLIPSEnv)->multifieldValueGetMethod,i);
            int mft = (*env)->CallIntMethod(env,mfo,CLIPSJNIData(theCLIPSEnv)->getCLIPSTypeValueMethod);
            void *mfv = ConvertSingleFieldPrimitiveValue(theCLIPSEnv,mft,mfo);  
            result->contents[i].value = mfv;
           }
           
         theDO->begin = 0;
         theDO->range = result->length;
         theDO->value = result;
         break;
        }
        
      case VOID_TYPE:
      case SYMBOL_TYPE:
      case STRING_TYPE:
      case INSTANCE_NAME_TYPE:
      case INTEGER_TYPE:
      case FLOAT_TYPE:
      case FACT_ADDRESS_TYPE:
      case INSTANCE_ADDRESS_TYPE:
        theDO->value = ConvertSingleFieldPrimitiveValue(theCLIPSEnv,theType,theValue);
        break;

      default: 
        theDO->voidValue = theCLIPSEnv->VoidConstant;
        break;
     }
  }

