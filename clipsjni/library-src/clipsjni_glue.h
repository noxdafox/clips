#include "net_sf_clipsrules_jni_Environment.h"
#include "clips.h"

#ifndef _Included_clipsjni_glue
#define _Included_clipsjni_glue

void       JNIUserFunction(Environment *,UDFContext *,UDFValue *);
bool       QueryJNICallback(Environment *,const char *,void *);
void       ExitJNICallback(Environment *,int,void *);
void       WriteJNICallback(Environment *,const char *,const char *,void *);
int        ReadJNICallback(Environment *,const char *,void *);
int        UnreadJNICallback(Environment *,const char *,int, void *);
void       JNIPeriodicCallback(Environment *,void *);
void       JNIParserErrorCallback(Environment *,const char *,const char *,const char *,long,void *);
void       PrintJavaAddress(Environment *,const char *,void *);
void       NewJavaAddress(UDFContext *,UDFValue *);
bool       CallJavaMethod(UDFContext *,UDFValue *,UDFValue *);
bool       DiscardJavaAddress(Environment *,void *);
jlong      CreateCLIPSJNIEnvironment(JNIEnv *,jobject);

#endif
