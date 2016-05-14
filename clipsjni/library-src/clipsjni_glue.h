#include "net_sf_clipsrules_jni_Environment.h"
#include "clips.h"

#ifndef _Included_clipsjni_glue
#define _Included_clipsjni_glue

void       JNIUserFunction(void *,DATA_OBJECT_PTR);
int        QueryJNIRouter(void *,const char *);
int        ExitJNIRouter(void *,int);
int        PrintJNIRouter(void *,const char *,const char *);
int        GetcJNIRouter(void *,const char *);
int        UngetcJNIRouter(void *,int,const char *);
void       JNIPeriodicCallback(void *);
void       JNIParserErrorCallback(void *,const char *,const char *,const char *,long);
void       PrintJavaAddress(void *,const char *,void *);
void       NewJavaAddress(void *,DATA_OBJECT *);
int        CallJavaMethod(void *,DATA_OBJECT *,DATA_OBJECT *);
int        DiscardJavaAddress(void *,void *);
jlong      CreateCLIPSJNIEnvironment(JNIEnv *,jobject);

#endif
