#define CLIPSDLL_SOURCE

#include "CLIPSDLL.h"

#include <windows.h>

#include "constant.h"
#include "classcom.h"
#include "classexm.h"
#include "classinf.h"
#include "classpsr.h"
#include "commline.h"
#include "cstrcpsr.h"
#include "engine.h"
#include "envrnbld.h"
#include "extnfunc.h"
#include "factfun.h"
#include "factmngr.h"
#include "fileutil.h"
#include "inscom.h"
#include "prntutil.h"
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

/***********/
/* __Clear */
/***********/
void __declspec(dllexport) __Clear(
  Environment *theEnv)
  {
   Clear(theEnv);
  }

int __declspec(dllexport) __Load(
  Environment *theEnv,
  const char *theFile)
  {
   return Load(theEnv,theFile);
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

void __declspec(dllexport) __LoadConstructsFromLogicalName(
  Environment *theEnv,
  const char *logicalName)
  {
   LoadConstructsFromLogicalName(theEnv,logicalName);
  }

/***********/
/* __Build */
/***********/
bool __declspec(dllexport) __Build(
  Environment *theEnv,
  const char *buildString)
  {
   return Build(theEnv,buildString);
  }

/***********/
/* __Reset */
/***********/
void __declspec(dllexport) __Reset(
  Environment *theEnv)
  {
   Reset(theEnv);
  }
  
/*********/
/* __Run */
/*********/
long long __declspec(dllexport) __Run(
  Environment *theEnv,
  long long runLimit)
  {
   return Run(theEnv,runLimit);
  }
  
/******************/ 
/* __AssertString */
/******************/  
Fact __declspec(dllexport) * __AssertString(
  Environment *theEnv,
  const char *factString)
  {
   return AssertString(theEnv,factString);
  }

/******************/
/* __MakeInstance */
/******************/
Instance __declspec(dllexport) * __MakeInstance(
  Environment *theEnv,
  const char *instanceString)
  {
   return MakeInstance(theEnv,instanceString);
  }

void __declspec(dllexport) __CommandLoop(
  Environment *theEnv)
  {
   CommandLoop(theEnv);
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

void __declspec(dllexport) __AppendDribble(
  Environment *theEnv,
  const char *theString)
  {
   AppendDribble(theEnv,theString);
  }

bool __declspec(dllexport) __DeleteRouter(
  Environment *theEnv,
  const char *name)
  {
   return DeleteRouter(theEnv,name);
  }

bool __declspec(dllexport) __ActivateRouter(
  Environment *theEnv,
  const char *name)
  {
   return ActivateRouter(theEnv, name);
  }

bool __declspec(dllexport) __DeactivateRouter(
  Environment *theEnv,
  const char *name)
  {
   return DeactivateRouter(theEnv, name);
  }

char __declspec(dllexport) * __GetCommandString(
  Environment *theEnv)
  {
   return GetCommandString(theEnv);
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

void __declspec(dllexport) __PrintString(
  Environment *theEnv,
  const char *logicalName,
  const char *printString)
  {
   PrintString(theEnv,logicalName,printString);
  }

void __declspec(dllexport) __Print(
  Environment *theEnv,
  const char *printString)
  {
   PrintString(theEnv,STDOUT,printString);
  }

void __declspec(dllexport) __PrintLn(
  Environment *theEnv,
  const char *printString)
  {
   PrintString(theEnv,STDOUT,printString);
   PrintString(theEnv,STDOUT,"\n");
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

const char __declspec(dllexport) * __InstanceName(
  Instance *theInstance)
  {
   return InstanceName(theInstance);  
  } 
  
bool __declspec(dllexport) __DirectGetSlot(
  Instance *theInstance,
  const char *slotName,
  CLIPSValue *returnValue)
  {
   return DirectGetSlot(theInstance,slotName,returnValue);  
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

bool __declspec(dllexport) __GetWatchItem(
  Environment *theEnv,
  const char *item)
  {
   int rv;

   rv = GetWatchItem(theEnv,item);

   if (rv == 1) return true;
   else return false;
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

void __declspec(dllexport) __SetHaltCommandLoopBatch(
  Environment *theEnv,
  bool value)
  {
   SetHaltCommandLoopBatch(theEnv,value);
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

 bool __declspec(dllexport) __GetAgendaChanged(
  Environment *theEnv)
  {
   return GetAgendaChanged(theEnv);
  }

void __declspec(dllexport) __SetAgendaChanged(
  Environment *theEnv,
  bool value)
  {
   SetAgendaChanged(theEnv,value);
  }

bool __declspec(dllexport) __GetFocusChanged(
  Environment *theEnv)
  {
   return GetFocusChanged(theEnv);
  }

void __declspec(dllexport) __SetFocusChanged(
  Environment *theEnv,
  bool value)
  {
   SetFocusChanged(theEnv,value);
  }

bool __declspec(dllexport) __GetFactListChanged(
  Environment *theEnv)
  {
   return GetFactListChanged(theEnv);
  }

void __declspec(dllexport) __SetFactListChanged(
  Environment *theEnv,
  bool value)
  {
   SetFactListChanged(theEnv,value);
  }
  
bool __declspec(dllexport) __GetInstancesChanged(
  Environment *theEnv)
  {
   return GetInstancesChanged(theEnv);
  }

void __declspec(dllexport) __SetInstancesChanged(
  Environment *theEnv,
  bool value)
  {
   SetInstancesChanged(theEnv,value);
  }

bool __declspec(dllexport) __EnablePeriodicFunctions(
  Environment *theEnv,
  bool value)
  {
   return EnablePeriodicFunctions(theEnv,value);
  }

bool __declspec(dllexport) __AddPeriodicFunction(
  Environment *theEnv,
  const char *name,
  VoidCallFunction *theFunction,
  int priority,
  void *context)
  {
   return AddPeriodicFunction(theEnv,name,theFunction,priority,context);
  }

bool __declspec(dllexport) __RemovePeriodicFunction(
  Environment *theEnv,
  const char *name)
  {
   return RemovePeriodicFunction(theEnv,name);
  }

Defmodule __declspec(dllexport) *__FindDefmodule(
  Environment *theEnv,
  const char *defmoduleName)
  {
   return FindDefmodule(theEnv,defmoduleName);
  }

void __declspec(dllexport) __SaveCurrentModule(
  Environment *theEnv)
  {
   SaveCurrentModule(theEnv);
  }

Defmodule __declspec(dllexport) *__SetCurrentModule(
  Environment *theEnv,
  Defmodule *newModule)
  {
   return SetCurrentModule(theEnv,newModule);
  }

void __declspec(dllexport) * __GetModuleItem(
  Environment *theEnv,
  Defmodule *theModule,
  unsigned moduleItemIndex)
  {
   return GetModuleItem(theEnv,theModule,moduleItemIndex);
  }
 
void __declspec(dllexport) __RestoreCurrentModule(
  Environment *theEnv)
  {
   RestoreCurrentModule(theEnv);
  }

Activation __declspec(dllexport) * __GetNextActivation(
  Environment *theEnv,
  Activation *actPtr)
  {
   return GetNextActivation(theEnv,actPtr);
  }

void __declspec(dllexport) __GetActivationBasisPPForm(
  Environment *theEnv,
  char *buffer,
  size_t bufferLength,
  Activation *theActivation)
  {
   GetActivationBasisPPForm(theEnv,buffer,bufferLength,theActivation);
  }

Defmodule __declspec(dllexport) * __GetNextDefmodule(
  Environment *theEnv,
  Defmodule *defmodulePtr)
  {
   return GetNextDefmodule(theEnv,defmodulePtr);
  }

const char __declspec(dllexport) * __DefmoduleName(
  Defmodule *theDefmodule)
  {
   return DefmoduleName(theDefmodule);  
  } 

Fact __declspec(dllexport) * __GetNextFact(
  Environment *theEnv,
  Fact *factPtr)
  {
   return GetNextFact(theEnv,factPtr);
  }

Deftemplate __declspec(dllexport) * __GetNextDeftemplate(
  Environment *theEnv,
  Deftemplate *deftemplatePtr)
  {
   return GetNextDeftemplate(theEnv,deftemplatePtr);
  }

Deftemplate __declspec(dllexport) * __FactDeftemplate(
  Fact *theFact)
  {
   return FactDeftemplate(theFact);
  }

void __declspec(dllexport) __FactSlotNames(
  Fact *theFact,
  CLIPSValue *returnValue)
  {
   FactSlotNames(theFact,returnValue);
  }

void __FactSlotValue(
  Environment *theEnv,
  Fact *theFact,
  const char *theSlotName,
  CLIPSValue *returnValue)
  {
   FactSlotValue(theEnv,theFact,theSlotName,returnValue);
  }

DefaultType __DeftemplateSlotDefaultP(
  Deftemplate *theDeftemplate,
  const char *slotName)
  {
   return DeftemplateSlotDefaultP(theDeftemplate,slotName);
  }

bool __DeftemplateSlotDefaultValue(
  Deftemplate *theDeftemplate,
  const char *slotName,
  CLIPSValue *theValue)
  {
   return DeftemplateSlotDefaultValue(theDeftemplate,slotName,theValue);
  }

void * __CreateDeftemplateScopeMap(
  Environment *theEnv,
  Deftemplate *theDeftemplate)
  {
   return CreateDeftemplateScopeMap(theEnv,theDeftemplate);
  }

Instance __declspec(dllexport) * __GetNextInstance(
  Environment *theEnv,
  Instance *instancePtr)
  {
   return GetNextInstance(theEnv,instancePtr);
  }

bool __DOsEqual(
  UDFValue *dobj1,
  UDFValue *dobj2)
  {
   return DOsEqual(dobj1,dobj2);
  }

void __CLIPSToUDFValue(
  CLIPSValue *cv,
  UDFValue *uv)
  {
   CLIPSToUDFValue(cv,uv);
  }

const char * __DataObjectToString(
  Environment *theEnv,
  UDFValue *theDO)
  {
   return DataObjectToString(theEnv,theDO);
  }
  
Defclass __declspec(dllexport) * __GetNextDefclass(
  Environment *theEnv,
  Defclass *defclassPtr)
  {
   return GetNextDefclass(theEnv,defclassPtr);
  }
  
void * __CreateClassScopeMap(
  Environment *theEnv,
  Defclass *theDefclass)
  {
   return CreateClassScopeMap(theEnv,theDefclass);
  }

Defclass __declspec(dllexport) * __InstanceClass(
  Instance *theInstance)
  {
   return InstanceClass(theInstance);  
  } 

const char __declspec(dllexport) * __DefclassName(
  Defclass *theDefclass)
  {
   return DefclassName(theDefclass);  
  } 

void __declspec(dllexport) __ClassSlots(
  Defclass *theClass,
  CLIPSValue *returnValue,
  bool inherp)
  {
   ClassSlots(theClass,returnValue,inherp);
  }

int __declspec(dllexport) __SlotDefaultP(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *slotName)
  {
   return SlotDefaultP(theEnv,theDefclass,slotName);
  }

bool __declspec(dllexport) __SlotDefaultValue(
  Defclass *theDefclass,
  const char *slotName,
  CLIPSValue *theValue)
  {
   return SlotDefaultValue(theDefclass,slotName,theValue);
  }

bool __declspec(dllexport) __AddUDF(
  Environment *theEnv,
  const char *clipsFunctionName,
  const char *returnTypes,
  unsigned short minArgs,
  unsigned short maxArgs,
  const char *argumentTypes,
  UserDefinedFunction *cFunctionPointer,
  const char *cFunctionName,
  void *context)
  {
   return AddUDF(theEnv,clipsFunctionName,returnTypes,minArgs,maxArgs,argumentTypes,cFunctionPointer,cFunctionName,context);
  }

bool __declspec(dllexport) __RemoveUDF(
  Environment *theEnv,
  const char *functionName)
  {
   return RemoveUDF(theEnv,functionName);
  }

Multifield __declspec(dllexport) * __CreateMultifield(
  Environment *theEnv,
  size_t size)
  {
   return CreateMultifield(theEnv,size);
  }

CLIPSLexeme __declspec(dllexport) * __CreateSymbol(
  Environment *theEnv,
  const char *str)
  {
   return CreateSymbol(theEnv,str);
  }

CLIPSLexeme __declspec(dllexport) * __CreateString(
  Environment *theEnv,
  const char *str)
  {
   return CreateString(theEnv,str);
  }
  
CLIPSLexeme __declspec(dllexport) * __CreateInstanceName(
  Environment *theEnv,
  const char *str)
  {
   return CreateInstanceName(theEnv,str);
  }

CLIPSFloat __declspec(dllexport) * __CreateFloat(
  Environment *theEnv,
  double number)
  {
   return CreateFloat(theEnv,number);
  }
  
CLIPSInteger __declspec(dllexport) * __CreateInteger(
  Environment *theEnv,
  long long number)
  {
   return CreateInteger(theEnv,number);
  }

unsigned int __declspec(dllexport) __UDFArgumentCount(
  UDFContext *context)
  {
   return UDFArgumentCount(context);
  }

bool __declspec(dllexport) __UDFNthArgument(
  UDFContext *context,
  unsigned int argumentPosition,
  unsigned expectedType,
  UDFValue *returnValue)
  {
   return UDFNthArgument(context,argumentPosition,expectedType,returnValue);
  }

void __declspec(dllexport) __NormalizeMultifield(
  Environment *theEnv,
  UDFValue *theMF)
  {
   NormalizeMultifield(theEnv,theMF);
  }

void __declspec(dllexport) * __GetUDFContext(
  Environment *theEnv,
  const char *functionName)
  {
   return GetUDFContext(theEnv,functionName);
  }
  
Instance __declspec(dllexport) * __FindInstance(
  Environment *theEnv,
  Defmodule *theDefmodule,
  const char *instanceName,
  bool searchImports)
  {
   return FindInstance(theEnv,NULL,instanceName,true);
  }

ParserErrorFunction __declspec(dllexport) * __SetParserErrorCallback(
   Environment *theEnv,
   ParserErrorFunction *functionPtr,
   void *context)
   {
    return SetParserErrorCallback(theEnv,functionPtr,context);
   }

char __declspec(dllexport) * __GetParsingFileName(
   Environment *theEnv)
   {
    return GetParsingFileName(theEnv);
   }

void __declspec(dllexport) __SetParsingFileName(
   Environment *theEnv,
   const char *fileName)
   {
    SetParsingFileName(theEnv,fileName);
   }