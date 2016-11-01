#include <windows.h>

#include "clips.h"

BOOL WINAPI DllMain(
  HINSTANCE hinstDLL,
  DWORD fwdreason,
  LPVOID lpvReserved)
  {
   return 1;
  }

void __declspec(dllexport) * __CreateEnvironment()
  {
   return CreateEnvironment();
  }

void __declspec(dllexport) __DestroyEnvironment(
  void *theEnv)
  {
   DestroyEnvironment(theEnv);
  }

void __declspec(dllexport) __EnvClear(
  void *theEnv)
  {
   EnvClear(theEnv);
  }

void __declspec(dllexport) __EnvReset(
  void *theEnv)
  {
   EnvReset(theEnv);
  }

int __declspec(dllexport) __EnvLoad(
  void *theEnv,
  char *theFile)
  {
   return EnvLoad(theEnv,theFile);
  }

long long __declspec(dllexport) __EnvRun(
  void *theEnv,
  long long runLimit)
  {
   return EnvRun(theEnv,runLimit);
  }
  
int __declspec(dllexport) __EnvBuild(
  void *theEnv,
  char *buildString)
  {
   return EnvBuild(theEnv,buildString);
  }
  
int __declspec(dllexport) __EnvEval(
  void *theEnv,
  char *evalString,
  DATA_OBJECT *rv)
  {
   return EnvEval(theEnv,evalString,rv);
  }  

void __declspec(dllexport) __EnvIncrementFactCount(
  void *theEnv,
  void *theFact)
  {
   EnvIncrementFactCount(theEnv,theFact);
  }

void __declspec(dllexport) __EnvDecrementFactCount(
  void *theEnv,
  void *theFact)
  {
   EnvDecrementFactCount(theEnv,theFact);
  }

void __declspec(dllexport) __EnvIncrementInstanceCount(
  void *theEnv,
  void *theInstance)
  {
   EnvIncrementInstanceCount(theEnv,theInstance);
  }

void __declspec(dllexport) __EnvDecrementInstanceCount(
  void *theEnv,
  void *theInstance)
  {
   EnvDecrementFactCount(theEnv,theInstance);
  }

long long __declspec(dllexport) __EnvFactIndex(
  void *theEnv,
  void *theFact)
  {
   return EnvFactIndex(theEnv,theFact);
  }

int __declspec(dllexport) __EnvGetFactSlot(
  void *theEnv,
  void *theFact,
  char *slotName,
  void *returnValue)
  {
   return EnvGetFactSlot(theEnv,theFact,slotName,returnValue);  
  }  

const char __declspec(dllexport) * __EnvGetInstanceName(
  void *theEnv,
  void *theInstance)
  {
   return EnvGetInstanceName(theEnv,theInstance);  
  } 