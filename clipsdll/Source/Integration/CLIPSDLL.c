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

void __declspec(dllexport) __EnvClear(
  Environment *theEnv)
  {
   EnvClear(theEnv);
  }

void __declspec(dllexport) __EnvReset(
  Environment *theEnv)
  {
   EnvReset(theEnv);
  }

int __declspec(dllexport) __EnvLoad(
  Environment *theEnv,
  const char *theFile)
  {
   return EnvLoad(theEnv,theFile);
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

bool __declspec(dllexport) __EnvDeleteRouter(
  Environment *theEnv,
  const char *name)
  {
   return EnvDeleteRouter(theEnv,name);
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

long long __declspec(dllexport) __EnvRun(
  Environment *theEnv,
  long long runLimit)
  {
   return EnvRun(theEnv,runLimit);
  }
  
bool __declspec(dllexport) __EnvBuild(
  Environment *theEnv,
  const char *buildString)
  {
   return EnvBuild(theEnv,buildString);
  }
  
bool __declspec(dllexport) __EnvEval(
  Environment *theEnv,
  const char *evalString,
  CLIPSValue *rv)
  {
   return EnvEval(theEnv,evalString,rv);
  }  

void __declspec(dllexport) __EnvIncrementFactCount(
  Environment *theEnv,
  Fact *theFact)
  {
   EnvIncrementFactCount(theEnv,theFact);
  }

void __declspec(dllexport) __EnvDecrementFactCount(
  Environment *theEnv,
  Fact *theFact)
  {
   EnvDecrementFactCount(theEnv,theFact);
  }

void __declspec(dllexport) __EnvIncrementInstanceCount(
  Environment *theEnv,
  Instance *theInstance)
  {
   EnvIncrementInstanceCount(theEnv,theInstance);
  }

void __declspec(dllexport) __EnvDecrementInstanceCount(
  Environment *theEnv,
  Instance *theInstance)
  {
   EnvDecrementInstanceCount(theEnv,theInstance);
  }

long long __declspec(dllexport) __EnvFactIndex(
  Environment *theEnv,
  Fact *theFact)
  {
   return EnvFactIndex(theEnv,theFact);
  }

bool __declspec(dllexport) __EnvGetFactSlot(
  Environment *theEnv,
  Fact *theFact,
  const char *slotName,
  CLIPSValue *returnValue)
  {
   return EnvGetFactSlot(theEnv,theFact,slotName,returnValue);  
  }  

Fact __declspec(dllexport) * __EnvAssertString(
  Environment *theEnv,
  const char *factString)
  {
   return EnvAssertString(theEnv,factString);
  }

const char __declspec(dllexport) * __InstanceName(
  Environment *theEnv,
  Instance *theInstance)
  {
   return InstanceName(theInstance);  
  } 
  
void __declspec(dllexport) __EnvDirectGetSlot(
  Environment *theEnv,
  Instance *theInstance,
  const char *slotName,
  CLIPSValue *returnValue)
  {
   EnvDirectGetSlot(theEnv,theInstance,slotName,returnValue);  
  }  
  
bool __declspec(dllexport) __EnvWatch(
  Environment *theEnv,
  const char *item)
  {
   return EnvWatch(theEnv,item);
  }

bool __declspec(dllexport) __EnvUnwatch(
  Environment *theEnv,
  const char *item)
  {
   return EnvUnwatch(theEnv,item);
  }

void __declspec(dllexport) * __GetEnvironmentContext(
  Environment *theEnv)
  {
   return GetEnvironmentContext(theEnv);
  }

void __declspec(dllexport) * __GetEnvironmentRouterContext(
  Environment *theEnv)
  {
   return GetEnvironmentRouterContext(theEnv);
  }

bool __declspec(dllexport) __EnvAddRouterWithContext(
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
   return EnvAddRouterWithContext(theEnv,routerName,priority,queryFunction,printFunction,getcFunction,ungetcFunction,exitFunction,context);
  }
 
 size_t __declspec(dllexport) __EnvInputBufferCount(
  Environment *theEnv)
  {
   return EnvInputBufferCount(theEnv);
  }
 
bool __declspec(dllexport) __EnvGetHaltExecution(
  Environment *theEnv)
  {
   return EnvGetHaltExecution(theEnv);
  }

void __declspec(dllexport) __EnvSetHaltExecution(
  Environment *theEnv,
  bool value)
  {
   EnvSetHaltExecution(theEnv,value);
  }

bool __declspec(dllexport) __EnvGetHaltRules(
  Environment *theEnv)
  {
   return EnvGetHaltRules(theEnv);
  }

void __declspec(dllexport) __EnvSetHaltRules(
  Environment *theEnv,
  bool value)
  {
   EnvSetHaltRules(theEnv,value);
  }

bool __declspec(dllexport) __EnvGetEvaluationError(
  Environment *theEnv)
  {
   return EnvGetEvaluationError(theEnv);
  }

void __declspec(dllexport) __EnvSetEvaluationError(
  Environment *theEnv,
  bool value)
  {
   EnvSetEvaluationError(theEnv,value);
  }