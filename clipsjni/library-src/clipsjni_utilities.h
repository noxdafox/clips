#include "net_sf_clipsrules_jni_Environment.h"
#include "clips.h"

#ifndef _Included_clipsjni_utilities
#define _Included_clipsjni_utilities

void       ConvertPrimitiveValueToDataObject(void *,jobject,DATA_OBJECT_PTR);
jobject    ConvertDataObject(JNIEnv *,jobject,void *,DATA_OBJECT *);
jobject    ConvertSingleFieldValue(JNIEnv *,jobject,void *,int,void *);
void      *JLongToPointer(jlong);
jlong      PointerToJLong(void *);

#endif