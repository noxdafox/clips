#ifndef CLIPSWin32_H
#define CLIPSWin32_H

#include "setup.h"
#include "agenda.h"
#include "entities.h"
#include "moduldef.h"
#include "object.h"
#include "router.h"
#include "strngfun.h"
#include "tmpltdef.h"
#include "tmpltfun.h"

#ifdef CLIPSDLL_SOURCE
#define DECLSPEC __declspec(dllexport)
#else
#define DECLSPEC __declspec(dllimport)
#endif


void DECLSPEC __Clear(Environment *);
int DECLSPEC __Load(Environment *,const char *);
bool DECLSPEC __OpenStringSource(Environment *,const char *,const char *,size_t);
bool DECLSPEC __CloseStringSource(Environment *,const char *);
void DECLSPEC __LoadConstructsFromLogicalName(Environment *,const char *);
BuildError DECLSPEC __Build(Environment *,const char *);

void DECLSPEC __Reset(Environment *);
long long DECLSPEC __Run(Environment *,long long);

Fact DECLSPEC * __AssertString(Environment *,const char *);
Instance DECLSPEC * __MakeInstance(Environment *,const char *);

int DECLSPEC __genchdir(Environment *,const char *);

bool DECLSPEC __DestroyEnvironment(Environment *);
bool DECLSPEC __DeleteRouter(Environment *,const char *);
bool DECLSPEC __ActivateRouter(Environment *, const char *);
bool DECLSPEC __DeactivateRouter(Environment *, const char *);
bool DECLSPEC __CommandCompleteAndNotEmpty(Environment *);
void DECLSPEC __AppendDribble(Environment *,const char *);
EvalError DECLSPEC __Eval(Environment *,const char *,CLIPSValue *);
GetSlotError DECLSPEC __GetFactSlot(Fact *,const char *,CLIPSValue *);
bool DECLSPEC __WatchString(Environment *,const char *);
bool DECLSPEC __UnwatchString(Environment *,const char *);
bool DECLSPEC __GetWatchItem(Environment *,const char *);
bool DECLSPEC __GetHaltExecution(Environment *);
bool DECLSPEC __GetHaltRules(Environment *);
bool DECLSPEC __GetAgendaChanged(Environment *);
bool DECLSPEC __GetFocusChanged(Environment *);
bool DECLSPEC __GetFactListChanged(Environment *);
bool DECLSPEC __GetInstancesChanged(Environment *);
bool DECLSPEC __GetEvaluationError(Environment *);
bool DECLSPEC __AddRouter(Environment *,const char *,int,
                          RouterQueryFunction *,RouterWriteFunction *,
                          RouterReadFunction *,RouterUnreadFunction *,
						  RouterExitFunction *,void *);

void DECLSPEC __SetCommandString(Environment *,const char *);
void DECLSPEC __CommandLoop(Environment *);
void DECLSPEC __RetainFact(Fact *);
void DECLSPEC __ReleaseFact(Fact *);
void DECLSPEC __RetainInstance(Instance *);
void DECLSPEC __ReleaseInstance(Instance *);
GetSlotError DECLSPEC __DirectGetSlot(Instance *,const char *,CLIPSValue *);  
void DECLSPEC __SetHaltExecution(Environment *,bool);
void DECLSPEC __SetHaltCommandLoopBatch(Environment *,bool);
void DECLSPEC __SetHaltRules(Environment *,bool);
void DECLSPEC __SetAgendaChanged(Environment *,bool);
void DECLSPEC __SetFocusChanged(Environment *,bool);
void DECLSPEC __SetFactListChanged(Environment *,bool);
void DECLSPEC __SetInstancesChanged(Environment *,bool);
void DECLSPEC __SetEvaluationError(Environment *,bool);
bool DECLSPEC __EnablePeriodicFunctions(Environment *,bool);

void DECLSPEC __WriteString(Environment *,const char *,const char *);
void DECLSPEC __Print(Environment *,const char *);
void DECLSPEC __PrintLn(Environment *,const char *);

bool __declspec(dllexport) __AddPeriodicFunction(Environment *,const char *,VoidCallFunction *,int,void *);
bool __declspec(dllexport) __RemovePeriodicFunction(Environment *,const char *);

void DECLSPEC __PrintPrompt(Environment *);
void DECLSPEC __PrintBanner(Environment *);
void DECLSPEC __CommandLoopOnceThenBatch(Environment *);

char DECLSPEC * __GetCommandString(Environment *theEnv);
void DECLSPEC * __GetEnvironmentContext(Environment *theEnv);
void DECLSPEC * __GetEnvironmentRouterContext(Environment *);


size_t DECLSPEC __InputBufferCount(Environment *);

long long DECLSPEC __FactIndex(Fact *);

const char DECLSPEC * __InstanceName(Instance *);
  
Environment DECLSPEC * __CreateEnvironment(void);

Defmodule DECLSPEC * __FindDefmodule(Environment *,const char *);
void DECLSPEC __SaveCurrentModule(Environment *);
Defmodule DECLSPEC * __SetCurrentModule(Environment *,Defmodule *);
void DECLSPEC * __GetModuleItem(Environment *,Defmodule *,unsigned);
void DECLSPEC __RestoreCurrentModule(Environment *);
Activation DECLSPEC * __GetNextActivation(Environment *,Activation *);
void DECLSPEC __GetActivationBasisPPForm(Environment *,char *,size_t bufferLength,Activation *);
Defmodule DECLSPEC * __GetNextDefmodule(Environment *,Defmodule *);
const char DECLSPEC * __DefmoduleName(Defmodule *);

Fact DECLSPEC * __GetNextFact(Environment *,Fact *);
Deftemplate DECLSPEC * __GetNextDeftemplate(Environment *,Deftemplate *);

Deftemplate DECLSPEC * __FactDeftemplate(Fact *);
void DECLSPEC __FactSlotNames(Fact *,CLIPSValue *);
void DECLSPEC __FactSlotValue(Environment *,Fact *,const char *,CLIPSValue *);
DefaultType DECLSPEC __DeftemplateSlotDefaultP(Deftemplate *,const char *);
bool DECLSPEC __DeftemplateSlotDefaultValue(Deftemplate *,const char *,CLIPSValue *);
void DECLSPEC * __CreateDeftemplateScopeMap(Environment *,Deftemplate *);
bool DECLSPEC __DOsEqual(UDFValue *,UDFValue *);
void DECLSPEC __CLIPSToUDFValue(CLIPSValue *,UDFValue *);
const char DECLSPEC * __DataObjectToString(Environment *,UDFValue *);

Defclass DECLSPEC * __InstanceClass(Instance *);
Instance DECLSPEC * __GetNextInstance(Environment *,Instance *);
Defclass DECLSPEC * __GetNextDefclass(Environment *,Defclass *);
void DECLSPEC * __CreateClassScopeMap(Environment *,Defclass *);
const char DECLSPEC * __DefclassName(Defclass *);
void DECLSPEC __ClassSlots(Defclass *,CLIPSValue *,bool);
int DECLSPEC __SlotDefaultP(Environment *,Defclass *,const char *);
bool DECLSPEC __SlotDefaultValue(Defclass *,const char *,CLIPSValue *);

AddUDFError DECLSPEC __AddUDF(Environment *,const char *,const char *,
                       unsigned short,unsigned short,const char *,
                       UserDefinedFunction *,const char *,void *);
bool DECLSPEC __RemoveUDF(Environment *,const char *);

Multifield DECLSPEC * __CreateMultifield(Environment *,size_t);
CLIPSLexeme DECLSPEC * __CreateSymbol(Environment *,const char *);
CLIPSLexeme DECLSPEC * __CreateString(Environment *,const char *);
CLIPSLexeme DECLSPEC * __CreateInstanceName(Environment *,const char *);

CLIPSFloat DECLSPEC * __CreateFloat(Environment *,double);
CLIPSInteger DECLSPEC * __CreateInteger(Environment *,long long);

unsigned int DECLSPEC __UDFArgumentCount(UDFContext *);
bool DECLSPEC __UDFNthArgument(UDFContext *,unsigned int,unsigned,UDFValue *);
void DECLSPEC __NormalizeMultifield(Environment *,UDFValue *);

void DECLSPEC * __GetUDFContext(Environment *,const char *);

Instance DECLSPEC * __FindInstance(Environment *,Defmodule *,const char *,bool);

ParserErrorFunction DECLSPEC * __SetParserErrorCallback(Environment *,ParserErrorFunction *,void *);

void DECLSPEC __SetParsingFileName(Environment *,const char *);
char DECLSPEC * __GetParsingFileName(Environment *);
bool DECLSPEC __PrintRouterExists(Environment *,const char *);

#endif