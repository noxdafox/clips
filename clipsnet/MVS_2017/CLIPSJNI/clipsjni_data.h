#include "clips.h"

#define CLIPSJNI_DATA 67

struct clipsJNIData
  { 
   int javaExternalAddressID;
      
   jclass environmentClass;
   jobject environmentObject;

   jclass userFunctionClass;
   jmethodID userFunctionEvaluateMethod;
  
   jclass classClass;
   jmethodID classGetCanonicalNameMethod;
   
   jclass longClass;
   jmethodID longInitMethod;

   jclass doubleClass;
   jmethodID doubleInitMethod;
   
   jclass arrayListClass;
   jmethodID arrayListInitMethod;
   jmethodID arrayListAddMethod;
   
   jclass hashMapClass;
   jmethodID hashMapInitMethod;
   jmethodID hashMapPutMethod;

   jclass bitSetClass;
   jmethodID bitSetInitMethod;
   jmethodID bitSetSetMethod;

   jclass primitiveValueClass;
   jmethodID getCLIPSTypeValueMethod;

   jclass voidValueClass;
   jmethodID voidValueInitMethod;

   jclass integerValueClass;
   jmethodID integerValueInitMethod;
   jmethodID integerValueLongValueMethod;
   jclass floatValueClass;
   jmethodID floatValueInitMethod;
   jmethodID floatValueDoubleValueMethod;

   jclass symbolValueClass;
   jmethodID symbolValueInitMethod;
   jclass stringValueClass;
   jmethodID stringValueInitMethod;
   jclass instanceNameValueClass;
   jmethodID instanceNameValueInitMethod;
   
   jclass lexemeValueClass;
   jmethodID lexemeValueGetValueMethod;

   jclass multifieldValueClass;
   jmethodID multifieldValueInitMethod;
   jmethodID multifieldValueGetMethod;
   jmethodID multifieldValueSizeMethod;
   
   jclass factAddressValueClass;
   jmethodID factAddressValueInitMethod;
   jmethodID factAddressValueGetFactAddressMethod;

   jclass instanceAddressValueClass;
   jmethodID instanceAddressValueInitMethod;
   jmethodID instanceAddressValueGetInstanceAddressMethod;

   jclass factInstanceClass;
   jmethodID factInstanceInitMethod;

   jclass slotValueClass;
   jmethodID slotValueInitMethod;

   jclass focusStackClass;
   jmethodID focusStackInitMethod;

   jclass focusClass;
   jmethodID focusInitMethod;

   jclass moduleClass;
   jmethodID moduleInitMethod;

   jclass agendaClass;
   jmethodID agendaInitMethod;

   jclass activationClass;
   jmethodID activationInitMethod;
  };

#define CLIPSJNIData(theEnv) ((struct clipsJNIData *) GetEnvironmentData(theEnv,CLIPSJNI_DATA))
