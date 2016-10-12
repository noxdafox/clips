#include "net_sf_clipsrules_jni_Environment.h"
#include "clips.h"

#ifndef _Included_clipsjni_glue
#define _Included_clipsjni_glue

void       JNIUserFunction(Environment *,UDFContext *,UDFValue *);
bool       QueryJNIRouter(Environment *,const char *);
void       ExitJNIRouter(Environment *,int);
void       PrintJNIRouter(Environment *,const char *,const char *);
int        GetcJNIRouter(Environment *,const char *);
int        UngetcJNIRouter(Environment *,int,const char *);
void       JNIPeriodicCallback(Environment *);
void       JNIParserErrorCallback(Environment *,const char *,const char *,const char *,long);
void       PrintJavaAddress(Environment *,const char *,void *);
void       NewJavaAddress(UDFContext *,UDFValue *);
bool       CallJavaMethod(UDFContext *,UDFValue *,UDFValue *);
bool       DiscardJavaAddress(Environment *,void *);
jlong      CreateCLIPSJNIEnvironment(JNIEnv *,jobject);

#endif
