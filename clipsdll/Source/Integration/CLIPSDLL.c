#define CLIPSDLL_SOURCE

#include "CLIPSDLL.h"

#include <windows.h>

#include "constant.h"
#include "commline.h"
#include "cstrcpsr.h"
#include "engine.h"
#include "envrnbld.h"
#include "factmngr.h"
#include "inscom.h"
#include "router.h"
#include "strngfun.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "watch.h"

BOOL WINAPI DllMain(
  HINSTANCE hinstDLL,
  DWORD fwdreason,
  LPVOID lpvReserved)
  {
   return 1;
  }

Environment __declspec(dllexport) * __CreateEnvironment()
  {
   return CreateEnvironment();
  }

bool __declspec(dllexport) __DestroyEnvironment(
  Environment *theEnv)
  {
   return DestroyEnvironment(theEnv);
  }

void __declspec(dllexport) __CommandLoop(
  Environment *theEnv)
  {
   CommandLoop(theEnv);
  }

void __declspec(dllexport) __Clear(
  Environment *theEnv)
  {
   Clear(theEnv);
  }

void __declspec(dllexport) __Reset(
  Environment *theEnv)
  {
   Reset(theEnv);
  }

int __declspec(dllexport) __Load(
  Environment *theEnv,
  const char *theFile)
  {
   return Load(theEnv,theFile);
  }

void __declspec(dllexport) __LoadConstructsFromLogicalName(
  Environment *theEnv,
  const char *logicalName)
  {
   LoadConstructsFromLogicalName(theEnv,logicalName);
  }

void __declspec(dllexport) __SetCommandString(
  Environment *theEnv,
  const char *theString)
  {
   SetCommandString(theEnv,theString);
  }

bool __declspec(dllexport) __CommandCompleteAndNotEmpty(
  Environment *theEnv)
  {
   return CommandCompleteAndNotEmpty(theEnv);
  }

bool __declspec(dllexport) __DeleteRouter(
  Environment *theEnv,
  const char *name)
  {
   return DeleteRouter(theEnv,name);
  }

char __declspec(dllexport) * __GetCommandString(
  Environment *theEnv)
  {
   return GetCommandString(theEnv);
  }

bool __declspec(dllexport) __OpenStringSource(
  Environment *theEnv,
  const char *logicalName,
  const char *stringData,
  size_t size)
  {
   return OpenStringSource(theEnv,logicalName,stringData,size);
  }

bool __declspec(dllexport) __CloseStringSource(
  Environment *theEnv,
  const char *logicalName)
  {
   return CloseStringSource(theEnv,logicalName);
  }

void __declspec(dllexport) __PrintPrompt(
  Environment *theEnv)
  {
   PrintPrompt(theEnv);
  }

void __declspec(dllexport) __PrintBanner(
  Environment *theEnv)
  {
   PrintBanner(theEnv);
  }

int __declspec(dllexport) __genchdir(
  const char *directory)
  {
   return genchdir(directory);
  }

void __declspec(dllexport) __CommandLoopOnceThenBatch(
  Environment *theEnv)
  {
   CommandLoopOnceThenBatch(theEnv);
  }

long long __declspec(dllexport) __Run(
  Environment *theEnv,
  long long runLimit)
  {
   return Run(theEnv,runLimit);
  }
  
bool __declspec(dllexport) __Build(
  Environment *theEnv,
  const char *buildString)
  {
   return Build(theEnv,buildString);
  }
  
bool __declspec(dllexport) __Eval(
  Environment *theEnv,
  const char *evalString,
  CLIPSValue *rv)
  {
   return Eval(theEnv,evalString,rv);
  }  

void __declspec(dllexport) __IncrementFactReferenceCount(
  Fact *theFact)
  {
   IncrementFactReferenceCount(theFact);
  }

void __declspec(dllexport) __DecrementFactReferenceCount(
  Fact *theFact)
  {
   DecrementFactReferenceCount(theFact);
  }

void __declspec(dllexport) __IncrementInstanceReferenceCount(
  Instance *theInstance)
  {
   IncrementInstanceReferenceCount(theInstance);
  }

void __declspec(dllexport) __DecrementInstanceReferenceCount(
  Instance *theInstance)
  {
   DecrementInstanceReferenceCount(theInstance);
  }

long long __declspec(dllexport) __FactIndex(
  Fact *theFact)
  {
   return FactIndex(theFact);
  }

bool __declspec(dllexport) __GetFactSlot(
  Fact *theFact,
  const char *slotName,
  CLIPSValue *returnValue)
  {
   return GetFactSlot(theFact,slotName,returnValue);  
  }  

Fact __declspec(dllexport) * __AssertString(
  Environment *theEnv,
  const char *factString)
  {
   return AssertString(theEnv,factString);
  }

const char __declspec(dllexport) * __InstanceName(
  Environment *theEnv,
  Instance *theInstance)
  {
   return InstanceName(theInstance);  
  } 
  
void __declspec(dllexport) __DirectGetSlot(
  Instance *theInstance,
  const char *slotName,
  CLIPSValue *returnValue)
  {
   DirectGetSlot(theInstance,slotName,returnValue);  
  }  
  
bool __declspec(dllexport) __WatchString(
  Environment *theEnv,
  const char *item)
  {
   return WatchString(theEnv,item);
  }

bool __declspec(dllexport) __UnwatchString(
  Environment *theEnv,
  const char *item)
  {
   return UnwatchString(theEnv,item);
  }

void __declspec(dllexport) * __GetEnvironmentContext(
  Environment *theEnv)
  {
   return GetEnvironmentContext(theEnv);
  }

bool __declspec(dllexport) __AddRouter(
  Environment *theEnv,
  const char *routerName,
  int priority,
  RouterQueryFunction *queryFunction,
  RouterPrintFunction *printFunction,
  RouterGetcFunction *getcFunction,
  RouterUngetcFunction *ungetcFunction,
  RouterExitFunction *exitFunction,
  void *context)
  {
   return AddRouter(theEnv,routerName,priority,queryFunction,printFunction,getcFunction,ungetcFunction,exitFunction,context);
  }
 
 size_t __declspec(dllexport) __InputBufferCount(
  Environment *theEnv)
  {
   return InputBufferCount(theEnv);
  }
 
bool __declspec(dllexport) __GetHaltExecution(
  Environment *theEnv)
  {
   return GetHaltExecution(theEnv);
  }

void __declspec(dllexport) __SetHaltExecution(
  Environment *theEnv,
  bool value)
  {
   SetHaltExecution(theEnv,value);
  }

bool __declspec(dllexport) __GetHaltRules(
  Environment *theEnv)
  {
   return GetHaltRules(theEnv);
  }

void __declspec(dllexport) __SetHaltRules(
  Environment *theEnv,
  bool value)
  {
   SetHaltRules(theEnv,value);
  }

bool __declspec(dllexport) __GetEvaluationError(
  Environment *theEnv)
  {
   return GetEvaluationError(theEnv);
  }

void __declspec(dllexport) __SetEvaluationError(
  Environment *theEnv,
  bool value)
  {
   SetEvaluationError(theEnv,value);
  }