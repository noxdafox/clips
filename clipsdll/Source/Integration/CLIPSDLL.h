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

int DECLSPEC __Load(Environment *,const char *);
int DECLSPEC __genchdir(const char *);

bool DECLSPEC __DestroyEnvironment(Environment *);
bool DECLSPEC __DeleteRouter(Environment *,const char *);
bool DECLSPEC __CommandCompleteAndNotEmpty(Environment *);
void DECLSPEC __AppendDribble(Environment *,const char *);
bool DECLSPEC __OpenStringSource(Environment *,const char *,const char *,size_t);
bool DECLSPEC __CloseStringSource(Environment *,const char *);
bool DECLSPEC __Build(Environment *,const char *);
bool DECLSPEC __Eval(Environment *,const char *,CLIPSValue *);
bool DECLSPEC __GetFactSlot(Fact *,const char *,CLIPSValue *);
bool DECLSPEC __WatchString(Environment *,const char *);
bool DECLSPEC __UnwatchString(Environment *,const char *);
bool DECLSPEC __GetHaltExecution(Environment *);
bool DECLSPEC __GetHaltRules(Environment *);
bool DECLSPEC __GetEvaluationError(Environment *);
bool DECLSPEC __AddRouter(Environment *,const char *,int,
                          RouterQueryFunction *,RouterPrintFunction *,
                          RouterGetcFunction *,RouterUngetcFunction *,
								  RouterExitFunction *,void *);

void DECLSPEC __SetCommandString(Environment *,const char *);
void DECLSPEC __CommandLoop(Environment *);
void DECLSPEC __Clear(Environment *);
void DECLSPEC __Reset(Environment *);
void DECLSPEC __LoadConstructsFromLogicalName(Environment *,const char *);
void DECLSPEC __IncrementFactReferenceCount(Fact *);
void DECLSPEC __DecrementFactReferenceCount(Fact *);
void DECLSPEC __IncrementInstanceReferenceCount(Instance *);
void DECLSPEC __DecrementInstanceReferenceCount(Instance *);
void DECLSPEC __DirectGetSlot(Instance *,const char *,CLIPSValue *);  
void DECLSPEC __SetHaltExecution(Environment *,bool);
void DECLSPEC __SetHaltRules(Environment *,bool);
void DECLSPEC __SetEvaluationError(Environment *,bool);
void DECLSPEC __PrintPrompt(Environment *);
void DECLSPEC __PrintBanner(Environment *);
void DECLSPEC __CommandLoopOnceThenBatch(Environment *);

char DECLSPEC * __GetCommandString(Environment *theEnv);
void DECLSPEC * __GetEnvironmentContext(Environment *theEnv);
void DECLSPEC * __GetEnvironmentRouterContext(Environment *);

Fact DECLSPEC * __AssertString(Environment *,const char *);

size_t DECLSPEC __InputBufferCount(Environment *);

long long DECLSPEC __FactIndex(Fact *);
long long DECLSPEC __Run(Environment *,long long);

const char DECLSPEC * __InstanceName(Environment *,Instance *);
  
Environment DECLSPEC * __CreateEnvironment(void);

#endif