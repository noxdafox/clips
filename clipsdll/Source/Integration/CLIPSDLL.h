#ifndef CLIPSWin32_H
#define CLIPSWin32_H

#include "setup.h"
#include "entities.h"
#include "router.h"

#ifdef CLIPSDLL_SOURCE
#define DECLSPEC __declspec(dllexport)
#else
#define DECLSPEC __declspec(dllimport)
#endif

int DECLSPEC __EnvLoad(Environment *,const char *);
int DECLSPEC __genchdir(const char *);

bool DECLSPEC __DestroyEnvironment(Environment *);
bool DECLSPEC __EnvDeleteRouter(Environment *,const char *);
bool DECLSPEC __CommandCompleteAndNotEmpty(Environment *);
bool DECLSPEC __OpenStringSource(Environment *,const char *,const char *,size_t);
bool DECLSPEC __CloseStringSource(Environment *,const char *);
bool DECLSPEC __EnvBuild(Environment *,const char *);
bool DECLSPEC __EnvEval(Environment *,const char *,CLIPSValue *);
bool DECLSPEC __EnvGetFactSlot(Environment *,Fact *,const char *,CLIPSValue *);
bool DECLSPEC __EnvWatch(Environment *,const char *);
bool DECLSPEC __EnvUnwatch(Environment *,const char *);
bool DECLSPEC __EnvGetHaltExecution(Environment *);
bool DECLSPEC __EnvGetHaltRules(Environment *);
bool DECLSPEC __EnvGetEvaluationError(Environment *);
bool DECLSPEC __EnvAddRouterWithContext(Environment *,const char *,int,
                                                     RouterQueryFunction *,RouterPrintFunction *,
                                                     RouterGetcFunction *,RouterUngetcFunction *,
													 RouterExitFunction *,void *);

void DECLSPEC __SetCommandString(Environment *,const char *);
void DECLSPEC __CommandLoop(Environment *);
void DECLSPEC __EnvClear(Environment *);
void DECLSPEC __EnvReset(Environment *);
void DECLSPEC __LoadConstructsFromLogicalName(Environment *,const char *);
void DECLSPEC __EnvIncrementFactCount(Environment *,Fact *);
void DECLSPEC __EnvDecrementFactCount(Environment *,Fact *);
void DECLSPEC __EnvIncrementInstanceCount(Environment *,Instance *);
void DECLSPEC __EnvDecrementInstanceCount(Environment *theEnv,Instance *);
void DECLSPEC __EnvDirectGetSlot(Environment *,Instance *,const char *,CLIPSValue *);  
void DECLSPEC __EnvSetHaltExecution(Environment *,bool);
void DECLSPEC __EnvSetHaltRules(Environment *,bool);
void DECLSPEC __EnvSetEvaluationError(Environment *,bool);
void DECLSPEC __PrintPrompt(Environment *);
void DECLSPEC __PrintBanner(Environment *);
void DECLSPEC __CommandLoopOnceThenBatch(Environment *);

char DECLSPEC * __GetCommandString(Environment *theEnv);
void DECLSPEC * __GetEnvironmentContext(Environment *theEnv);
void DECLSPEC * __GetEnvironmentRouterContext(Environment *);

Fact DECLSPEC * __EnvAssertString(Environment *,const char *);

size_t DECLSPEC __EnvInputBufferCount(Environment *);

long long DECLSPEC __EnvFactIndex(Environment *,Fact *);
long long DECLSPEC __EnvRun(Environment *,long long);

const char DECLSPEC * __InstanceName(Environment *,Instance *);
  
Environment DECLSPEC * __CreateEnvironment(void);

#endif