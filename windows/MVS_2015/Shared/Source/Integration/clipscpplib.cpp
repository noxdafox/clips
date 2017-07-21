#include <iostream>
#include <stdexcept>
#include <unordered_map>

#include "clipscpp.h"

#ifdef CLIPS_DLL_WRAPPER
#include "CLIPSDLL.h"
#endif

using namespace CLIPS;
using std::vector;
using std::string;

#include "setup.h"
#include "constant.h"
#include "entities.h"
#include "router.h"
#include "classexm.h "
#include "classfun.h"
#include "classinf.h"
#include "classpsr.h"
#include "constant.h"
#include "commline.h"
#include "cstrcpsr.h"
#include "engine.h"
#include "envrnbld.h"
#include "factfun.h"
#include "factmngr.h"
#include "fileutil.h"
#include "inscom.h"
#include "prntutil.h"
#include "router.h"
#include "strngfun.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "tmpltdef.h"
#include "tmpltfun.h"
#include "watch.h"

/*##################*/
/* Static Functions */
/*##################*/

static bool CLIPSCPPQuery(Environment *,const char *,void *);
static void CLIPSCPPWrite(Environment *,const char *,const char *,void *);
static int CLIPSCPPRead(Environment *,const char *,void *);
static int CLIPSCPPUnread(Environment *,const char *,int,void *);
static void CLIPSCPPExit(Environment *,int,void *);
static Value *ConvertSingleFieldValue(int,void *);
static DataObject ConvertDataObject(CLIPSValue *);
static void ConvertToCLIPSValue(Environment *,DataObject,CLIPSValue *);
static void ConvertSingleFieldToCLIPSValue(Environment *,Value *,CLIPSValue *);
static void CLIPSCPPPeriodicCallback(Environment *,void *);
static void CLIPSCPPUserFunctionCallback(Environment *,UDFContext *,UDFValue *);
static void CLIPSCPPParserErrorCallback(Environment *,const char *,const char *,const char *,long,void *);

/*#####################*/
/* CLIPSCPPEnv Methods */
/*#####################*/

/***************/
/* CLIPSCPPEnv */
/***************/
CLIPSCPPEnv::CLIPSCPPEnv() : theEnv(NULL)
  {
#ifndef CLIPS_DLL_WRAPPER
   theEnv = CreateEnvironment();

   SetEnvironmentContext(theEnv,this);
#else
   theEnv = __CreateEnvironment();
   /* TBD */
#endif
  }

/****************/
/* ~CLIPSCPPEnv */
/****************/
CLIPSCPPEnv::~CLIPSCPPEnv()
  {
#ifndef CLIPS_DLL_WRAPPER
   DestroyEnvironment(theEnv);
#else
   __DestroyEnvironment(theEnv);
#endif
  }

/*********/
/* Clear */
/*********/
void CLIPSCPPEnv::Clear()
  {
#ifndef CLIPS_DLL_WRAPPER
   ::Clear(theEnv);
#else
   __Clear(theEnv);
#endif
  }

/********/
/* Load */
/********/
int CLIPSCPPEnv::Load(
  char *theFile)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::Load(theEnv,theFile);
#else
   return __Load(theEnv,theFile);
#endif
  }

/******************/
/* LoadFromString */
/******************/
void CLIPSCPPEnv::LoadFromString(
  char *loadString)
  {
#ifndef CLIPS_DLL_WRAPPER
   OpenStringSource(theEnv,"clipsnetloadfromstring",loadString,0); 
   LoadConstructsFromLogicalName(theEnv,"clipsnetloadfromstring");
   CloseStringSource(theEnv,"clipsnetloadfromstring");
#else
   __OpenStringSource(theEnv,"clipsnetloadfromstring",loadString,0); 
   __LoadConstructsFromLogicalName(theEnv,"clipsnetloadfromstring");
   __CloseStringSource(theEnv,"clipsnetloadfromstring");
#endif
  }
  
/*********/
/* Build */
/*********/
bool CLIPSCPPEnv::Build(
  char *buildString)
  {   
#ifndef CLIPS_DLL_WRAPPER
   if (::Build(theEnv,buildString))
     { return true; }
   else
     { return false; }
#else
   if (__Build(theEnv,buildString))
     { return true; }
   else
     { return false; }
#endif
  }

/*********/
/* Reset */
/*********/
void CLIPSCPPEnv::Reset()
  {
#ifndef CLIPS_DLL_WRAPPER
   ::Reset(theEnv);
#else
   __Reset(theEnv);
#endif
  }

/*******/
/* Run */
/*******/
long long CLIPSCPPEnv::Run(
  long long runLimit)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::Run(theEnv,runLimit);
#else
   return __Run(theEnv,runLimit);
#endif
  }
  
/****************/
/* AssertString */
/****************/
FactAddressValue *CLIPSCPPEnv::AssertString(
  char *factString)
  {
   Fact *rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = ::AssertString(theEnv,factString);
#else
   rv = __AssertString(theEnv,factString);
#endif
     
   if (rv == NULL) return NULL;
   return new FactAddressValue(rv);
  }
  
/****************/
/* MakeInstance */
/****************/
InstanceAddressValue *CLIPSCPPEnv::MakeInstance(
  char *instanceString)
  {
   Instance *rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = ::MakeInstance(theEnv,instanceString);
#else
   rv = __MakeInstance(theEnv,instanceString);
#endif
     
   if (rv == NULL) return NULL;
   return new InstanceAddressValue(rv);
  }
  
/**********************/
/* FindInstanceByName */
/**********************/
InstanceAddressValue *CLIPSCPPEnv::FindInstanceByName(
  char *instanceName)
  {
   Instance *rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = ::FindInstance(theEnv,NULL,instanceName,true);
#else
   rv = __FindInstance(theEnv,NULL,instanceName,true);
#endif
     
   if (rv == NULL) return NULL;
   return new InstanceAddressValue(rv);
  }
  
/********/
/* Eval */
/********/
DataObject CLIPSCPPEnv::Eval(
  char *evalString)
  {
   int rc;
   CLIPSValue rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rc = ::Eval(theEnv,evalString,&rv);
#else
   rc = __Eval(theEnv,evalString,&rv);
#endif

   if (rc == 0)
     {
      std::string excStr = "Eval: Invalid expression ";
      excStr.append(evalString);
      throw std::logic_error(excStr); 
     }
     
   return ConvertDataObject(&rv);
  }
  
/*********/
/* Watch */
/*********/
bool CLIPSCPPEnv::Watch(
  char *item)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::WatchString(theEnv,item);
#else
   return __WatchString(theEnv,item);
#endif
  }

/***********/
/* Unwatch */
/***********/
bool CLIPSCPPEnv::Unwatch(
  char *item)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::UnwatchString(theEnv,item);
#else
   return __UnwatchString(theEnv,item);
#endif
  }

/***************/
/* CommandLoop */
/***************/
void CLIPSCPPEnv::CommandLoop()
  {
#ifndef CLIPS_DLL_WRAPPER
   ::CommandLoop(theEnv);
#else
   __CommandLoop(theEnv);
#endif
  }

/****************************/
/* CommandLoopOnceThenBatch */
/****************************/
void CLIPSCPPEnv::CommandLoopOnceThenBatch()
  {
#ifndef CLIPS_DLL_WRAPPER
   ::CommandLoopOnceThenBatch(theEnv);
#else
   __CommandLoopOnceThenBatch(theEnv);
#endif
  }

/***************/
/* PrintBanner */
/***************/
void CLIPSCPPEnv::PrintBanner()
  {
#ifndef CLIPS_DLL_WRAPPER
   ::PrintBanner(theEnv);
#else
   __PrintBanner(theEnv);
#endif
  }

/***************/
/* PrintPrompt */
/***************/
void CLIPSCPPEnv::PrintPrompt()
  {
#ifndef CLIPS_DLL_WRAPPER
   ::PrintPrompt(theEnv);
#else
   __PrintPrompt(theEnv);
#endif
  }
  
/************/
/* Printout */
/************/
void CLIPSCPPEnv::Printout(
  const char *logicalName,
  const char *printString)
  {
#ifndef CLIPS_DLL_WRAPPER
   ::WriteString(theEnv,logicalName,printString);
#else
   __WriteString(theEnv,logicalName,printString);
#endif
  }

/*********/
/* Print */
/*********/
void CLIPSCPPEnv::Print(
  const char *printString)
  {
   Printout(CLIPSCPPRouter::STDOUT,printString);
  }

/***********/
/* PrintLn */
/***********/
void CLIPSCPPEnv::PrintLn(
  const char *printString)
  {
   Printout(CLIPSCPPRouter::STDOUT,printString);
   Printout(CLIPSCPPRouter::STDOUT,"\n");
  }

/********************/
/* GetHaltExecution */
/********************/
int CLIPSCPPEnv::GetHaltExecution()
{
#ifndef CLIPS_DLL_WRAPPER
    return ::GetHaltExecution(theEnv);
#else
    return __GetHaltExecution(theEnv);
#endif
}

/********************/
/* SetHaltExecution */
/********************/
void CLIPSCPPEnv::SetHaltExecution(
   bool value)
{
#ifndef CLIPS_DLL_WRAPPER
    ::SetHaltExecution(theEnv,value);
#else
    __SetHaltExecution(theEnv,value);
#endif
}

/***************************/
/* SetHaltCommandLoopBatch */
/***************************/
void CLIPSCPPEnv::SetHaltCommandLoopBatch(
   bool value)
{
#ifndef CLIPS_DLL_WRAPPER
    ::SetHaltCommandLoopBatch(theEnv,value);
#else
    __SetHaltCommandLoopBatch(theEnv,value);
#endif
}

/**********************/
/* GetEvaluationError */
/**********************/
int CLIPSCPPEnv::GetEvaluationError()
{
#ifndef CLIPS_DLL_WRAPPER
    return ::GetEvaluationError(theEnv);
#else
    return __GetEvaluationError(theEnv);
#endif
}

/**********************/
/* SetEvaluationError */
/**********************/
void CLIPSCPPEnv::SetEvaluationError(
  bool value)
{
#ifndef CLIPS_DLL_WRAPPER
    ::SetEvaluationError(theEnv,value);
#else
    __SetEvaluationError(theEnv,value);
#endif
}

/****************/
/* GetHaltRules */
/****************/
int CLIPSCPPEnv::GetHaltRules()
{
#ifndef CLIPS_DLL_WRAPPER
    return ::GetHaltRules(theEnv);
#else
    return __GetHaltRules(theEnv);
#endif
}

/****************/
/* SetHaltRules */
/****************/
void CLIPSCPPEnv::SetHaltRules(
    bool value)
{
#ifndef CLIPS_DLL_WRAPPER
    ::SetHaltRules(theEnv,value);
#else
    __SetHaltRules(theEnv,value);
#endif
}

/*******************/
/* ChangeDirectory */
/*******************/
int CLIPSCPPEnv::ChangeDirectory(
  char *directory)
  {
   int rc;
   
#ifndef CLIPS_DLL_WRAPPER
   rc = genchdir(directory);
#else
   rc = __genchdir(directory);
#endif

   return rc;
  }
  
/***********************/
/* ConvertToCLIPSValue */
/***********************/
static void ConvertToCLIPSValue(
   Environment *theEnv,
   DataObject theDO,
   CLIPSValue *theCV)
   {
    switch (theDO.GetCLIPSType())
      {
       case CPP_VOID_TYPE:
       case CPP_SYMBOL_TYPE:
       case CPP_STRING_TYPE:
       case CPP_INSTANCE_NAME_TYPE:
       case CPP_INTEGER_TYPE:
       case CPP_FLOAT_TYPE:
       case CPP_FACT_ADDRESS_TYPE:
       case CPP_INSTANCE_ADDRESS_TYPE:
       case CPP_EXTERNAL_ADDRESS_TYPE:
         ConvertSingleFieldToCLIPSValue(theEnv,theDO.GetCLIPSValue(),theCV);
         break;

       case CPP_MULTIFIELD_TYPE:
         MultifieldValue *theCPPMultifield = (MultifieldValue *) theDO.GetCLIPSValue();
         size_t mfLength = theCPPMultifield->GetMultifieldValue()->size();
         size_t i;
#ifndef CLIPS_DLL_WRAPPER
         Multifield *theMultifield = CreateMultifield(theEnv,mfLength);
#else       
         Multifield *theMultifield = __CreateMultifield(theEnv,mfLength);
#endif         
         std::vector<Value *> *theValues = theCPPMultifield->GetMultifieldValue();

         for (i = 0; i < mfLength; i++)
           { ConvertSingleFieldToCLIPSValue(theEnv,theValues->at(i),&theMultifield->contents[i]); }
         
         theCV->multifieldValue = theMultifield;
         break;
      }
   }

/**********************************/
/* ConvertSingleFieldToCLIPSValue */
/**********************************/
static void ConvertSingleFieldToCLIPSValue(
   Environment *theEnv,
   Value *theValue,
   CLIPSValue *theCV)
   {
#ifndef CLIPS_DLL_WRAPPER
    switch (theValue->GetCLIPSType())
      {
       case CPP_VOID_TYPE:
         theCV->voidValue = VoidConstant(theEnv);
         break;

       case CPP_SYMBOL_TYPE:
         {
          SymbolValue *theSymbol = (SymbolValue *) theValue;
          theCV->lexemeValue = CreateSymbol(theEnv,theSymbol->GetSymbolValue()->c_str());
          break;
         }

       case CPP_STRING_TYPE:
         {
          StringValue *theString = (StringValue *) theValue;
          theCV->lexemeValue = CreateString(theEnv,theString->GetStringValue()->c_str());
          break;
         }

       case CPP_INSTANCE_NAME_TYPE:
         {
          InstanceNameValue *theInstanceName = (InstanceNameValue *) theValue;
          theCV->lexemeValue = CreateInstanceName(theEnv,theInstanceName->GetInstanceNameValue()->c_str());
          break;
         }

       case CPP_INTEGER_TYPE:
         {
          IntegerValue *theInteger = (IntegerValue *) theValue;
          theCV->integerValue = CreateInteger(theEnv,theInteger->GetIntegerValue());
          break;
         }

       case CPP_FLOAT_TYPE:
         {
          FloatValue *theFloat = (FloatValue *) theValue;
          theCV->floatValue = CreateFloat(theEnv,theFloat->GetFloatValue());
          break;
         }

       case CPP_FACT_ADDRESS_TYPE:
         {
          FactAddressValue *theFactAddress = (FactAddressValue *) theValue;
          theCV->factValue = theFactAddress->GetFactAddressValue();
          break;
         }

       case CPP_INSTANCE_ADDRESS_TYPE:
         {
          InstanceAddressValue *theInstanceAddress = (InstanceAddressValue *) theValue;
          theCV->instanceValue = theInstanceAddress->GetInstanceAddressValue();
          break;
         }

       case CPP_EXTERNAL_ADDRESS_TYPE:
         // TBD
         break;
      }
#else
    switch (theValue->GetCLIPSType())
      {
       case CPP_VOID_TYPE:
         theCV->voidValue = VoidConstant(theEnv);
         break;

       case CPP_SYMBOL_TYPE:
         {
          SymbolValue *theSymbol = (SymbolValue *) theValue;
          theCV->lexemeValue = __CreateSymbol(theEnv,theSymbol->GetSymbolValue()->c_str());
          break;
         }

       case CPP_STRING_TYPE:
         {
          StringValue *theString = (StringValue *) theValue;
          theCV->lexemeValue = __CreateString(theEnv,theString->GetStringValue()->c_str());
          break;
         }

       case CPP_INSTANCE_NAME_TYPE:
         {
          InstanceNameValue *theInstanceName = (InstanceNameValue *) theValue;
          theCV->lexemeValue = __CreateInstanceName(theEnv,theInstanceName->GetInstanceNameValue()->c_str());
          break;
         }

       case CPP_INTEGER_TYPE:
         {
          IntegerValue *theInteger = (IntegerValue *) theValue;
          theCV->integerValue = __CreateInteger(theEnv,theInteger->GetIntegerValue());
          break;
         }

       case CPP_FLOAT_TYPE:
         {
          FloatValue *theFloat = (FloatValue *) theValue;
          theCV->floatValue = __CreateFloat(theEnv,theFloat->GetFloatValue());
          break;
         }

       case CPP_FACT_ADDRESS_TYPE:
         {
          FactAddressValue *theFactAddress = (FactAddressValue *) theValue;
          theCV->factValue = theFactAddress->GetFactAddressValue();
          break;
         }

       case CPP_INSTANCE_ADDRESS_TYPE:
         {
          InstanceAddressValue *theInstanceAddress = (InstanceAddressValue *) theValue;
          theCV->instanceValue = theInstanceAddress->GetInstanceAddressValue();
          break;
         }

       case CPP_EXTERNAL_ADDRESS_TYPE:
         // TBD
         break;
      }
#endif
   }

/*********************/
/* ConvertDataObject */
/*********************/
static DataObject ConvertDataObject(
  CLIPSValue *theCV)
  {
   DataObject tv;

   switch (theCV->header->type)
     {
      case VOID_TYPE:
      case STRING_TYPE:        
      case SYMBOL_TYPE:
      case INSTANCE_NAME_TYPE:
      case INTEGER_TYPE:
      case FLOAT_TYPE:
      case FACT_ADDRESS_TYPE:
      case INSTANCE_ADDRESS_TYPE:
      case EXTERNAL_ADDRESS_TYPE:
        return DataObject(ConvertSingleFieldValue(theCV->header->type,theCV->value));
     
      case MULTIFIELD_TYPE:
        Multifield *theList = theCV->multifieldValue;
        size_t mfLength = theCV->multifieldValue->length, i; 
        
        MultifieldValue *theMultifield = new MultifieldValue(mfLength);
        
        for (i = 0; i < mfLength; i++)
         { theMultifield->add(ConvertSingleFieldValue(theList->contents[i].header->type,theList->contents[i].value)); }

        return DataObject(theMultifield);
     }
     
   return DataObject();
  }
  
/****************************/
/* ConvertSingleFieldValue: */
/****************************/
static Value *ConvertSingleFieldValue(
  int type,
  void *value)
  {
   switch(type)
     {
      case VOID_TYPE:
        return new VoidValue();

      case SYMBOL_TYPE:
        return new SymbolValue(((CLIPSLexeme *) value)->contents);
        
      case STRING_TYPE:
        return new StringValue(((CLIPSLexeme *) value)->contents);
        
      case INSTANCE_NAME_TYPE:
        return new InstanceNameValue(((CLIPSLexeme *) value)->contents);

      case INTEGER_TYPE:
        return new IntegerValue(((CLIPSInteger *) value)->contents);

      case FLOAT_TYPE:
        return new FloatValue(((CLIPSFloat *) value)->contents);

      case FACT_ADDRESS_TYPE:
        return new FactAddressValue((Fact *) value);

      case INSTANCE_ADDRESS_TYPE:
        return new InstanceAddressValue((Instance *) value);

      case EXTERNAL_ADDRESS_TYPE:
        // TBD EXTERNAL_ADDRESS_TYPE
        break;
     }

   return new VoidValue();
  }
  
/******************/
/* ValidWatchItem */
/******************/
bool CLIPSCPPEnv::ValidWatchItem(
  char *item)
  {
   int rv;

#ifndef CLIPS_DLL_WRAPPER
   rv = ::GetWatchItem(theEnv,item);
#else
   rv = __GetWatchItem(theEnv,item);
#endif

   if (rv == -1) return false;
   else return true;
  }

/****************/
/* GetWatchItem */
/****************/
bool CLIPSCPPEnv::GetWatchItem(
  char *item)
  {
   int rv;

#ifndef CLIPS_DLL_WRAPPER
   rv = ::GetWatchItem(theEnv,item);
#else
   rv = __GetWatchItem(theEnv,item);
#endif

   if (rv == 1) return true;
   else return false;
  }
  
/****************/
/* SetWatchItem */
/****************/
void CLIPSCPPEnv::SetWatchItem(
  char *item,
  bool newValue)
  {
   if (newValue)
     { Watch(item); }
   else
     { Unwatch(item); }
  }

/********************/
/* GetAgendaChanged */
/********************/
bool CLIPSCPPEnv::GetAgendaChanged()
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::GetAgendaChanged(theEnv);
#else
    return __GetAgendaChanged(theEnv);
#endif
  }

/********************/
/* SetAgendaChanged */
/********************/
void CLIPSCPPEnv::SetAgendaChanged(
  bool newValue)
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::SetAgendaChanged(theEnv,newValue);
#else
    return __SetAgendaChanged(theEnv,newValue);
#endif
  }

/*******************/
/* GetFocusChanged */
/*******************/
bool CLIPSCPPEnv::GetFocusChanged()
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::GetFocusChanged(theEnv);
#else
    return __GetFocusChanged(theEnv);
#endif
  }

/*******************/
/* SetFocusChanged */
/*******************/
void CLIPSCPPEnv::SetFocusChanged(
  bool newValue)
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::SetFocusChanged(theEnv,newValue);
#else
    return __SetFocusChanged(theEnv,newValue);
#endif
  }

/**********************/
/* GetFactListChanged */
/**********************/
bool CLIPSCPPEnv::GetFactListChanged()
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::GetFactListChanged(theEnv);
#else
    return __GetFactListChanged(theEnv);
#endif
  }

/**********************/
/* SetFactListChanged */
/**********************/
void CLIPSCPPEnv::SetFactListChanged(
  bool newValue)
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::SetFactListChanged(theEnv,newValue);
#else
    return __SetFactListChanged(theEnv,newValue);
#endif
  }

/***********************/
/* GetInstancesChanged */
/***********************/
bool CLIPSCPPEnv::GetInstancesChanged()
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::GetInstancesChanged(theEnv);
#else
    return __GetInstancesChanged(theEnv);
#endif
  }

/***********************/
/* SetInstancesChanged */
/***********************/
void CLIPSCPPEnv::SetInstancesChanged(
  bool newValue)
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::SetInstancesChanged(theEnv,newValue);
#else
    return __SetInstancesChanged(theEnv,newValue);
#endif
  }

/*************/
/* AddRouter */
/*************/
bool CLIPSCPPEnv::AddRouter(
  char *routerName,
  int priority,
  CLIPSCPPRouter *router)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::AddRouter(theEnv,routerName,priority,CLIPSCPPQuery,
                                  CLIPSCPPWrite,CLIPSCPPRead,CLIPSCPPUnread,
                                  CLIPSCPPExit,router);
#else
   return __AddRouter(theEnv,routerName,priority,CLIPSCPPQuery,
                                    CLIPSCPPWrite,CLIPSCPPRead,CLIPSCPPUnread,
                                    CLIPSCPPExit,router);
#endif
  }

/****************/
/* DeleteRouter */
/****************/
bool CLIPSCPPEnv::DeleteRouter(
  char *routerName)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::DeleteRouter(theEnv,routerName);
#else
   return __DeleteRouter(theEnv,routerName);
#endif
  }

/******************/
/* ActivateRouter */
/******************/
bool CLIPSCPPEnv::ActivateRouter(
    char *routerName)
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::ActivateRouter(theEnv, routerName);
#else
    return __ActivateRouter(theEnv, routerName);
#endif
  }

/********************/
/* DeactivateRouter */
/********************/
bool CLIPSCPPEnv::DeactivateRouter(
  char *routerName)
  {
#ifndef CLIPS_DLL_WRAPPER
    return ::DeactivateRouter(theEnv, routerName);
#else
    return __DeactivateRouter(theEnv, routerName);
#endif
  }

/***********************/
/* AddPeriodicFunction */
/***********************/
bool CLIPSCPPEnv::AddPeriodicFunction(
  char *periodicFunctionName,
  int priority,
  CLIPSCPPPeriodicFunction *periodicFunction)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::AddPeriodicFunction(theEnv,periodicFunctionName,CLIPSCPPPeriodicCallback,
                                priority,periodicFunction);
#else
   return __AddPeriodicFunction(theEnv,periodicFunctionName,CLIPSCPPPeriodicCallback,
                                priority,periodicFunction);
#endif
  }

/**************************/
/* RemovePeriodicFunction */
/**************************/
bool CLIPSCPPEnv::RemovePeriodicFunction(
  char *periodicFunctionName)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::RemovePeriodicFunction(theEnv,periodicFunctionName);
#else
   return __RemovePeriodicFunction(theEnv,periodicFunctionName);
#endif
  }
  
/***************************/
/* EnablePeriodicFunctions */
/***************************/
bool CLIPSCPPEnv::EnablePeriodicFunctions(
  bool value)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::EnablePeriodicFunctions(theEnv,value);
#else
   return __EnablePeriodicFunctions(theEnv,value);
#endif
  }

/********************/
/* InputBufferCount */
/********************/
size_t CLIPSCPPEnv::InputBufferCount()
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::InputBufferCount(theEnv);
#else
   return __InputBufferCount(theEnv);
#endif
  }

/*******************/
/* GetInputBuffer: */
/*******************/
const char *CLIPSCPPEnv::GetInputBuffer()
  {
#ifndef CLIPS_DLL_WRAPPER
   return GetCommandString(theEnv);
#else
   return __GetCommandString(theEnv);
#endif
  }

/*******************/
/* SetInputBuffer: */
/*******************/
void CLIPSCPPEnv::SetInputBuffer(
  const char *command)
  {
#ifndef CLIPS_DLL_WRAPPER
   return SetCommandString(theEnv,command);
#else
   return __SetCommandString(theEnv,command);
#endif
  }

/*******************************/
/* InputBufferContainsCommand: */
/*******************************/
bool CLIPSCPPEnv::InputBufferContainsCommand()
  {
#ifndef CLIPS_DLL_WRAPPER
   if (CommandCompleteAndNotEmpty(theEnv)) return true;
   else return false;
#else
   if ( __CommandCompleteAndNotEmpty(theEnv)) return true;
   else return false;
#endif
  }

/********************/
/* AppendToDribble: */
/********************/
void CLIPSCPPEnv::AppendToDribble(
  const char *command)
  {
#ifndef CLIPS_DLL_WRAPPER
   AppendDribble(theEnv,command);
#else
   __AppendDribble(theEnv,command);
#endif
  }
  
/***************************/
/* SetParserErrorCallback: */
/***************************/
void CLIPSCPPEnv::SetParserErrorCallback(
  CLIPSCPPParserErrorFunction *callback)
  {
#ifndef CLIPS_DLL_WRAPPER
   ::SetParserErrorCallback(theEnv,CLIPSCPPParserErrorCallback,callback);
#else
   __SetParserErrorCallback(theEnv,CLIPSCPPParserErrorCallback,callback);
#endif
  }

/***********************/
/* GetParsingFileName: */
/***********************/
char *CLIPSCPPEnv::GetParsingFileName()
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::GetParsingFileName(theEnv);
#else
   return __GetParsingFileName(theEnv);
#endif
  }
  
/***********************/
/* SetParsingFileName: */
/***********************/
void CLIPSCPPEnv::SetParsingFileName(
  const char *fileName)
  {
#ifndef CLIPS_DLL_WRAPPER
   ::SetParsingFileName(theEnv,fileName);
#else
   __SetParsingFileName(theEnv,fileName);
#endif
  }
  
/**********************/
/* PrintRouterExists: */
/**********************/
bool CLIPSCPPEnv::PrintRouterExists(
  const char *logicalName)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::PrintRouterExists(theEnv,logicalName);
#else
   return __PrintRouterExists(theEnv,logicalName);
#endif
  }

/*##################################*/
/* CLIPSCPPPeriodicFunction Methods */
/*##################################*/

/************/
/* Callback */
/************/
void CLIPSCPPPeriodicFunction::Callback(
  CLIPSCPPEnv *theCPPEnv)
  { 
  }
  
/*#####################################*/
/* CLIPSCPPParserErrorFunction Methods */
/*#####################################*/

/************/
/* Callback */
/************/
void CLIPSCPPParserErrorFunction::Callback(
  CLIPSCPPEnv *theCPPEnv,
  const char *fileName,
  const char *warningString,
  const char *errorString,
  long lineNumber)
  { 
   // TBD This is what got called.
  }

/*########################*/
/* CLIPSCPPRouter Methods */
/*########################*/

const char *CLIPSCPPRouter::STDOUT = "stdout";
const char *CLIPSCPPRouter::STDIN = "stdin";
const char *CLIPSCPPRouter::STDWRN = "stdwrn";
const char *CLIPSCPPRouter::STDERR = "stderr";

/*********/
/* Query */
/*********/
bool CLIPSCPPRouter::Query(
  CLIPSCPPEnv *theCPPEnv,
  const char *logicalName)
  { 
   return false;
  }
  
/*********/
/* Write */
/*********/
void CLIPSCPPRouter::Write(
  CLIPSCPPEnv *theCPPEnv,
  const char *logicalName,
  const char *printString)
  {
  }
  
/********/
/* Read */
/********/
int CLIPSCPPRouter::Read(
  CLIPSCPPEnv *theCPPEnv,
  const char *logicalName)
  {
   return -1;
  }
  
/**********/
/* Unread */
/**********/
int CLIPSCPPRouter::Unread(
  CLIPSCPPEnv *theCPPEnv,
  int character,
  const char *logicalName)
  {
   return -1;
  }

/********/
/* Exit */
/********/
void CLIPSCPPRouter::Exit(
  CLIPSCPPEnv *theCPPEnv,
  bool failure)
  {
  }

/*####################*/
/* DataObject Methods */
/*####################*/

/**************/
/* DataObject */
/**************/
DataObject::DataObject() : theValue (new VoidValue())
  {
  }

/**************/
/* DataObject */
/**************/
DataObject::DataObject(Value * v) : theValue (v)
  { }

/**************/
/* DataObject */
/**************/
DataObject::DataObject(const DataObject& v) : theValue(NULL)
  { 
   theValue = v.theValue->clone();
  }
  
/***************/
/* ~DataObject */
/***************/
DataObject::~DataObject()
  { 
   delete theValue; 
  }

/**************/
/* Operator = */
/**************/
DataObject& DataObject::operator = (
  const DataObject& s)
  {
   if (this == &s) return *this;
   
   delete theValue;
   
   theValue = s.theValue->clone();

   return *this;
  }
  
/***************/
/* Operator << */
/***************/
std::ostream& CLIPS::operator<< (std::ostream& o, const DataObject& s)
  {
   s.print(o);
   return o;
  }

/***************/
/* Operator << */
/***************/
std::ostream& CLIPS::operator<< (std::ostream& o, const DataObject* s)
  {
   s->print(o);
   return o;
  }

/*********/
/* print */
/*********/
std::ostream& DataObject::print (std::ostream& o) const
  { return std::cout << theValue; }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType DataObject::GetCLIPSType()
  {
   if (theValue == NULL)
     { return CPP_UNKNOWN_TYPE; }

   return theValue->GetCLIPSType();
  }

/*****************/
/* GetCLIPSValue */
/*****************/
Value *DataObject::GetCLIPSValue()
  {
   return theValue;
  }

/*###############*/
/* Value Methods */
/*###############*/
    
/*********/
/* Value */
/*********/
Value::Value()
  {
  }

/**********/
/* ~Value */
/**********/
Value::~Value()
  {  
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType Value::GetCLIPSType()
  {
   return CPP_UNKNOWN_TYPE;
  }
  
/***********/
/* Value = */
/***********/
/*
Value& Value::operator = (
  const Value& v)
  {
   return *this;
  }
*/  
/***************/
/* Operator << */
/***************/
std::ostream& CLIPS::operator<< (std::ostream& o, const Value& s)
  {
   s.print(o);
   return o;
  }

/***************/
/* Operator << */
/***************/
std::ostream& CLIPS::operator<< (std::ostream& o, const Value* s)
  {
   s->print(o);
   return o;
  }

/*###################*/
/* VoidValue Methods */
/*###################*/
    
/*************/
/* VoidValue */
/*************/
VoidValue::VoidValue()
  {
  }

/*************/
/* VoidValue */
/*************/
VoidValue::VoidValue(const VoidValue& v)
  { 
   this->operator=(v); 
  }
  
/**************/
/* ~VoidValue */
/**************/
VoidValue::~VoidValue()
  { 
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType VoidValue::GetCLIPSType()
  {
   return CPP_VOID_TYPE;
  }

/***************/
/* VoidValue = */
/***************/
/*
VoidValue& VoidValue::operator = (
  const VoidValue& v)
  {
   return *this;
  }
*/
/*********/
/* print */
/*********/
std::ostream& VoidValue::print (std::ostream& o) const
  {
   return o << "<void>";
  }
  
/*********/
/* clone */
/*********/
VoidValue *VoidValue::clone() const
  { 
   return new VoidValue(*this); 
  }
  
/*#####################*/
/* StringValue Methods */
/*#####################*/
    
/***************/
/* StringValue */
/***************/
StringValue::StringValue()
  { 
  }

/***************/
/* StringValue */
/***************/
StringValue::StringValue(
  const char *initialString)
  {
   theString.assign(initialString);
  }

/***************/
/* StringValue */
/***************/
StringValue::StringValue( const StringValue& v)
  { 
   this->operator=(v); 
  }
  
/****************/
/* ~StringValue */
/****************/
StringValue::~StringValue()
  { 
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType StringValue::GetCLIPSType()
  {
   return CPP_STRING_TYPE;
  }

/******************/
/* GetStringValue */
/******************/
std::string *StringValue::GetStringValue()
  {
   return &this->theString;
  }

/*****************/
/* StringValue = */
/*****************/
/*
StringValue& StringValue::operator = (
  const StringValue& v)
  {
   if (this == &v) return *this;
   
   theString = v.theString; 

   return *this;
  }
*/
/*********/
/* print */
/*********/
std::ostream& StringValue::print (std::ostream& o) const
  { 
   return o << '\"' << theString.c_str() << '\"'; 
  }
  
/*********/
/* clone */
/*********/
StringValue *StringValue::clone() const
  {
   return new StringValue(*this); 
  }

/*#####################*/
/* SymbolValue Methods */
/*#####################*/

/***************/
/* SymbolValue */
/***************/
SymbolValue::SymbolValue()
  {
   /* theString = new string(""); */
  }

/***************/
/* SymbolValue */
/***************/
SymbolValue::SymbolValue(
  const char *initialString)
  {
   theString.assign(initialString);
  }

/***************/
/* SymbolValue */
/***************/
SymbolValue::SymbolValue( const SymbolValue& v)
  { this->operator=(v); }

/****************/
/* ~SymbolValue */
/****************/
SymbolValue::~SymbolValue()
  {
   /* delete theString; */
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType SymbolValue::GetCLIPSType()
  {
   return CPP_SYMBOL_TYPE;
  }

/******************/
/* GetSymbolValue */
/******************/
std::string *SymbolValue::GetSymbolValue()
  {
   return &this->theString;
  }

/*****************/
/* SymbolValue = */
/*****************/
/*
SymbolValue& SymbolValue::operator = (
  const SymbolValue& v)
  {
   if (this == &v) return *this;
   
   return *this;
  }
*/
/*********/
/* print */
/*********/
std::ostream& SymbolValue::print (std::ostream& o) const
  { return o << theString.c_str(); }
      
/*********/
/* clone */
/*********/
SymbolValue *SymbolValue::clone() const
  { return new SymbolValue(*this); }

/*###########################*/
/* InstanceNameValue Methods */
/*###########################*/

/*********************/
/* InstanceNameValue */
/*********************/
InstanceNameValue::InstanceNameValue()
  { }

/*********************/
/* InstanceNameValue */
/*********************/
InstanceNameValue::InstanceNameValue(
  const char *initialString)
  {
   theString.assign(initialString);
  }

/*********************/
/* InstanceNameValue */
/*********************/
InstanceNameValue::InstanceNameValue( const InstanceNameValue& v)
  { this->operator=(v); }

/**********************/
/* ~InstanceNameValue */
/**********************/
InstanceNameValue::~InstanceNameValue()
  { }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType InstanceNameValue::GetCLIPSType()
  {
   return CPP_INSTANCE_NAME_TYPE;
  }

/************************/
/* GetInstanceNameValue */
/************************/
std::string *InstanceNameValue::GetInstanceNameValue()
  {
   return &this->theString;
  }

/***********************/
/* InstanceNameValue = */
/***********************/
/*
InstanceNameValue& InstanceNameValue::operator = (
  const InstanceNameValue& v)
  {
   if (this == &v) return *this;
   
   return *this;
  }
*/  
/*********/
/* print */
/*********/
std::ostream& InstanceNameValue::print (std::ostream& o) const
  { return o << '[' << theString.c_str() << ']'; }
    
/*********/
/* clone */
/*********/
InstanceNameValue *InstanceNameValue::clone() const
  { return new InstanceNameValue(*this); }

/*######################*/
/* IntegerValue Methods */
/*######################*/

/****************/
/* IntegerValue */
/****************/
IntegerValue::IntegerValue() : theInteger (0)
  { }

/****************/
/* IntegerValue */
/****************/
IntegerValue::IntegerValue(
  long long initialValue) : theInteger(initialValue)
  {
  }

/****************/
/* IntegerValue */
/****************/
IntegerValue::IntegerValue( const IntegerValue& v)
  { this->operator=(v); }

/*****************/
/* ~IntegerValue */
/*****************/
IntegerValue::~IntegerValue()
  { }
  
/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType IntegerValue::GetCLIPSType()
  {
   return CPP_INTEGER_TYPE;
  }

/*******************/
/* GetIntegerValue */
/*******************/
long long IntegerValue::GetIntegerValue()
  {
   return this->theInteger;
  }

/*****************/
/* GetFloatValue */
/*****************/
double IntegerValue::GetFloatValue()
  {
   return (double) this->theInteger;
  }

/******************/
/* IntegerValue = */
/******************/
/*
IntegerValue& IntegerValue::operator = (
  const IntegerValue& v)
  {
   if (this == &v) return *this;
   
   return *this;
  }
*/  
/*********/
/* print */
/*********/
std::ostream& IntegerValue::print (std::ostream& o) const
  { return o << theInteger; }
    
/*********/
/* clone */
/*********/
IntegerValue *IntegerValue::clone() const
  { return new IntegerValue(*this); }
  
/*####################*/
/* FloatValue Methods */
/*####################*/

/**************/
/* FloatValue */
/**************/
FloatValue::FloatValue() : theFloat (0.0)
  { }

/**************/
/* FloatValue */
/**************/
FloatValue::FloatValue(
  double initialValue) : theFloat(initialValue) /* TBD combine with prior using default? */
  {
  }

/**************/
/* FloatValue */
/**************/
FloatValue::FloatValue( const FloatValue& v)
  { this->operator=(v); }

/***************/
/* ~FloatValue */
/***************/
FloatValue::~FloatValue()
  { }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType FloatValue::GetCLIPSType()
  {
   return CPP_FLOAT_TYPE;
  }

/*******************/
/* GetIntegerValue */
/*******************/
long long FloatValue::GetIntegerValue()
  {
   return (long long) this->theFloat;
  }

/*****************/
/* GetFloatValue */
/*****************/
double FloatValue::GetFloatValue()
  {
   return this->theFloat;
  }

/****************/
/* FloatValue = */
/****************/
/*
FloatValue& FloatValue::operator = (
  const FloatValue& v)
  {
   if (this == &v) return *this;
   
   return *this;
  }
*/  
/*********/
/* print */
/*********/
std::ostream& FloatValue::print (std::ostream& o) const
  { return o << theFloat; }
    
/*********/
/* clone */
/*********/
FloatValue *FloatValue::clone() const
  { return new FloatValue(*this); }

/*##########################*/
/* FactAddressValue Methods */
/*##########################*/

/********************/
/* FactAddressValue */
/********************/
FactAddressValue::FactAddressValue(
  Fact *theFact) : theFactAddress(theFact)
  {
#ifndef CLIPS_DLL_WRAPPER
   IncrementFactReferenceCount(theFact);
#else
   __IncrementFactReferenceCount(theFact);
#endif
  }

/********************/
/* FactAddressValue */
/********************/
FactAddressValue::FactAddressValue( const FactAddressValue& v) : theFactAddress(NULL)
  { 
   this->operator=(v); 
  }

/*********************/
/* ~FactAddressValue */
/*********************/
FactAddressValue::~FactAddressValue()
  {   
#ifndef CLIPS_DLL_WRAPPER
   ::DecrementFactReferenceCount(theFactAddress);
#else
   __DecrementFactReferenceCount(theFactAddress);
#endif
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType FactAddressValue::GetCLIPSType()
  {
   return CPP_FACT_ADDRESS_TYPE;
  }

/**********************/
/* FactAddressValue = */
/**********************/
FactAddressValue& FactAddressValue::operator = (
  const FactAddressValue& v)
  {
   if (this == &v) return *this;

   if (theFactAddress != NULL)
     { 
#ifndef CLIPS_DLL_WRAPPER
      ::DecrementFactReferenceCount(theFactAddress);
#else
      __DecrementFactReferenceCount(theFactAddress);
#endif
     }
        
   theFactAddress = v.theFactAddress;
     
#ifndef CLIPS_DLL_WRAPPER
   IncrementFactReferenceCount(theFactAddress);
#else
   __IncrementFactReferenceCount(theFactAddress);
#endif
   
   return *this;
  }

/****************/
/* GetFactIndex */
/****************/
long long FactAddressValue::GetFactIndex() const
  {  
#ifndef CLIPS_DLL_WRAPPER
   return FactIndex(theFactAddress);
#else
   return __FactIndex(theFactAddress);
#endif

  }

/*********/
/* print */
/*********/
std::ostream& FactAddressValue::print (std::ostream& o) const
  {  
   return o << "<Fact-" << GetFactIndex() << ">";
  }
    
/*********/
/* clone */
/*********/
FactAddressValue *FactAddressValue::clone() const
  { return new FactAddressValue(*this); }

/***************/
/* GetFactSlot */
/***************/
DataObject FactAddressValue::GetFactSlot(char *slotName) const
  {  
   CLIPSValue theCV;
   bool rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = ::GetFactSlot(theFactAddress,slotName,&theCV);
#else
   rv = __GetFactSlot(theFactAddress,slotName,&theCV);
#endif
   
   if (! rv)
      {
       std::string excStr = "Slot ";
       excStr.append(slotName);
       excStr.append(" is invalid");
       
       throw std::logic_error(excStr); 
      }

   return ConvertDataObject(&theCV);
  }

/***********************/
/* GetFactAddressValue */
/***********************/
Fact *FactAddressValue::GetFactAddressValue()
  { return this->theFactAddress; }

/*##############################*/
/* InstanceAddressValue Methods */
/*##############################*/

/************************/
/* InstanceAddressValue */
/************************/
InstanceAddressValue::InstanceAddressValue(
  Instance *theInstance) : theInstanceAddress(theInstance)
  {
#ifndef CLIPS_DLL_WRAPPER
   IncrementInstanceReferenceCount(theInstance);
#else
   __IncrementInstanceReferenceCount(theInstance);
#endif
  }

/************************/
/* InstanceAddressValue */
/************************/
InstanceAddressValue::InstanceAddressValue( const InstanceAddressValue& v) : theInstanceAddress(NULL)
  { 
   this->operator=(v); 
  }

/*************************/
/* ~InstanceAddressValue */
/*************************/
InstanceAddressValue::~InstanceAddressValue()
  {   
#ifndef CLIPS_DLL_WRAPPER
   ::DecrementInstanceReferenceCount(theInstanceAddress);
#else
   __DecrementInstanceReferenceCount(theInstanceAddress);
#endif
  }

/**************************/
/* InstanceAddressValue = */
/**************************/
InstanceAddressValue& InstanceAddressValue::operator = (
  const InstanceAddressValue& v)
  {
   if (this == &v) return *this;

   if (theInstanceAddress != NULL)
     { 
#ifndef CLIPS_DLL_WRAPPER
      ::DecrementInstanceReferenceCount(theInstanceAddress);
#else
      __DecrementInstanceReferenceCount(theInstanceAddress);
#endif

     }
        
   theInstanceAddress = v.theInstanceAddress;
     
#ifndef CLIPS_DLL_WRAPPER
   ::IncrementInstanceReferenceCount(theInstanceAddress);
#else
   __IncrementInstanceReferenceCount(theInstanceAddress);
#endif
   
   return *this;
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType InstanceAddressValue::GetCLIPSType()
  {
   return CPP_INSTANCE_ADDRESS_TYPE;
  }

/*******************/
/* GetInstanceName */
/*******************/
const char *InstanceAddressValue::GetInstanceName() const
  {  
#ifndef CLIPS_DLL_WRAPPER
   return InstanceName(theInstanceAddress);
#else
   return __InstanceName(theInstanceAddress);
#endif
  }

/*****************/
/* DirectGetSlot */
/*****************/
DataObject InstanceAddressValue::DirectGetSlot(char *slotName) const
  {  
   CLIPSValue theCV;
   bool rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = ::DirectGetSlot(theInstanceAddress,slotName,&theCV);
#else
   rv = __DirectGetSlot(theInstanceAddress,slotName,&theCV);
#endif

   if (! rv)
      {
       std::string excStr = "Slot ";
       excStr.append(slotName);
       excStr.append(" is invalid");
       
       throw std::logic_error(excStr); 
      }
   
   return ConvertDataObject(&theCV);
  }

/*********/
/* print */
/*********/
std::ostream& InstanceAddressValue::print (std::ostream& o) const
  {  
   return o << "<Instance-" << GetInstanceName() << ">";
  }
    
/*********/
/* clone */
/*********/
InstanceAddressValue *InstanceAddressValue::clone() const
  { return new InstanceAddressValue(*this); }

/***************************/
/* GetInstanceAddressValue */
/***************************/
Instance *InstanceAddressValue::GetInstanceAddressValue()
  { return this->theInstanceAddress; }
  
/*#########################*/
/* MultifieldValue Methods */
/*#########################*/

/*******************/
/* MultifieldValue */
/*******************/
MultifieldValue::MultifieldValue()
  { 
   /* std::cout << "MultifieldValue::MultifieldValue()" << std::endl; */
  }

/*******************/
/* MultifieldValue */
/*******************/
MultifieldValue::MultifieldValue(size_t size)
  {
   /* std::cout << "MultifieldValue::MultifieldValue(" << size << ")" << std::endl; */
   theMultifield.reserve(size);
  }

/*******************/
/* MultifieldValue */
/*******************/
MultifieldValue::MultifieldValue( const MultifieldValue& v)
  { 
   /* std::cout << "MultifieldValue::MultifieldValue(MultifieldValue)" << std::endl; */
   this->operator=(v);
  }

/********************/
/* ~MultifieldValue */
/********************/
MultifieldValue::~MultifieldValue()
  { 
   size_t i;
   
   for (i = 0; i < theMultifield.size(); i++)
     { delete theMultifield[i]; }
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType MultifieldValue::GetCLIPSType()
  {
   return CPP_MULTIFIELD_TYPE;
  }
 
/**********************/
/* GetMultifieldValue */
/**********************/
std::vector<Value *> *MultifieldValue::GetMultifieldValue()
  {
   return &this->theMultifield;
  }
 
/*******/
/* add */
/*******/
void MultifieldValue::add(
  Value *theValue)
  {
   theMultifield.push_back(theValue);
  }
  
/*********************/
/* MultifieldValue = */
/*********************/
MultifieldValue& MultifieldValue::operator = (
  const MultifieldValue& v)
  {
   size_t i;
   Value *theValue;

   if (this == &v) return *this;

   for (i = 0; i < theMultifield.size(); i++)
     { delete theMultifield[i]; }
      
   theMultifield = v.theMultifield; 
   theMultifield.reserve(v.theMultifield.capacity());

   for (i = 0; i < v.theMultifield.size(); i++)
     {  
      theValue = v.theMultifield[i];
      theMultifield[i] = theValue->clone(); 
     }
   
   return *this;
  }

/*********/
/* print */
/*********/
std::ostream& MultifieldValue::print (std::ostream& o) const
  {
   size_t i;
   bool first = true;
   
   o << "("; 
   
   for (i = 0; i < theMultifield.size(); i++)
     { 
      if (first)
        { o << theMultifield[i]; }
      else
        { o << " " << theMultifield[i]; }
      first = false;
     }

   return o << ")";    
  }
    
/*********/
/* clone */
/*********/
MultifieldValue *MultifieldValue::clone() const
  { 
   return new MultifieldValue(*this); 
  }
  
/*##############################*/
/* ExternalAddressValue Methods */
/*##############################*/

/************************/
/* ExternalAddressValue */
/************************/
ExternalAddressValue::ExternalAddressValue(
  CLIPSExternalAddress *theExternal) : theExternalAddress(theExternal)
  {
/* TBD
#ifndef CLIPS_DLL_WRAPPER
   IncrementExternalAddressCount(theExternal);
#else
   __IncrementExternalAddressCount(theExternal);
#endif
*/
  }

/************************/
/* ExternalAddressValue */
/************************/
ExternalAddressValue::ExternalAddressValue( const ExternalAddressValue& v) : theExternalAddress(NULL)
  { 
   this->operator=(v); 
  }

/*************************/
/* ~ExternalAddressValue */
/*************************/
ExternalAddressValue::~ExternalAddressValue()
  {  
/* TBD 
#ifndef CLIPS_DLL_WRAPPER
   ::DecrementExternalAddressReferenceCount(theExternalAddress);
#else
   __DecrementExternalAddressCount(theExternalAddress);
#endif
*/
  }

/**************************/
/* ExternalAddressValue = */
/**************************/
ExternalAddressValue& ExternalAddressValue::operator = (
  const ExternalAddressValue& v)
  {
   if (this == &v) return *this;

   if (theExternalAddress != NULL)
     { 
/* TBD
#ifndef CLIPS_DLL_WRAPPER
      ::DecrementExternalAddressReferenceCount(theExternalAddress);
#else
      __DecrementExternalReferenceCount(theExternalAddress);
#endif
*/
     }
        
   theExternalAddress = v.theExternalAddress;
/* TBD
#ifndef CLIPS_DLL_WRAPPER
   IncrementExternalAddressCount(theExternalAddress);
#else
   __IncrementExternalAddressCount(theExternalAddress);
#endif
*/   
   return *this;
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSCPPType ExternalAddressValue::GetCLIPSType()
  {
   return CPP_EXTERNAL_ADDRESS_TYPE;
  }

/*********/
/* print */
/*********/
std::ostream& ExternalAddressValue::print (std::ostream& o) const
  {  
   return o << "<ExternalAddress-" << this->GetExternalAddressValue()->contents << ">";
  }
    
/*********/
/* clone */
/*********/
ExternalAddressValue *ExternalAddressValue::clone() const
  { return new ExternalAddressValue(*this); }

/***************************/
/* GetExternalAddressValue */
/***************************/
CLIPSExternalAddress *ExternalAddressValue::GetExternalAddressValue() const
  { return this->theExternalAddress; }

/*###################################*/
/* Static PeriodicFunction Functions */
/*###################################*/

static void CLIPSCPPPeriodicCallback(
  Environment *theEnv,
  void *context)
  {
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPPeriodicFunction *thePF = (CLIPSCPPPeriodicFunction *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPPeriodicFunction *thePF = (CLIPSCPPPeriodicFunction *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   thePF->Callback(theCPPEnv);
  }
  
/*######################################*/
/* Static ParserErrorCallback Functions */
/*######################################*/

static void CLIPSCPPParserErrorCallback(
  Environment *theEnv,
  const char *fileName,
  const char *warningString,
  const char *errorString,
  long lineNumber,
  void *context) 
  {
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPParserErrorFunction *thePEF = (CLIPSCPPParserErrorFunction *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPParserErrorFunction *thePEF = (CLIPSCPPParserErrorFunction *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   thePEF->Callback(theCPPEnv,fileName,warningString,errorString,lineNumber);
  }

/*#########################*/
/* Static Router Functions */
/*#########################*/
  
/*****************/
/* CLIPSCPPQuery */
/*****************/
static bool CLIPSCPPQuery(
  Environment *theEnv,
  const char *logicalName,
  void *context)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return theRouter->Query(theCPPEnv,logicalName);
  }

/*****************/
/* CLIPSCPPWrite */
/*****************/
static void CLIPSCPPWrite(
  Environment *theEnv,
  const char *logicalName,
  const char *printString,
  void *context)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   theRouter->Write(theCPPEnv,logicalName,printString);
  }

/****************/
/* CLIPSCPPRead */
/****************/
static int CLIPSCPPRead(
  Environment *theEnv,
  const char *logicalName,
  void *context)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return(theRouter->Read(theCPPEnv,logicalName));
  }
  
/******************/
/* CLIPSCPPUnread */
/******************/
static int CLIPSCPPUnread(
  Environment *theEnv,
  const char *logicalName,
  int character,
  void *context)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return theRouter->Unread(theCPPEnv,character,logicalName);
  }
  
/*****************/
/* CLIPSCPPExit */
/*****************/
static void CLIPSCPPExit(
  Environment *theEnv,
  int exitCode,
  void *context)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) context;
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   theRouter->Exit(theCPPEnv,(exitCode == EXIT_FAILURE));
  }

/*########################*/
/* CLIPSCPPModule Methods */
/*########################*/

/******************/
/* CLIPSCPPModule */
/******************/
CLIPSCPPModule::CLIPSCPPModule()
  {
  }

/******************/
/* CLIPSCPPModule */
/******************/
CLIPSCPPModule::CLIPSCPPModule(
  const char *initialString)
  {
   moduleName.assign(initialString);
  }

/******************/
/* GetModuleName */
/******************/
std::string *CLIPSCPPModule::GetModuleName()
  {
   return &this->moduleName;
  }

/*******************/
/* ~CLIPSCPPModule */
/*******************/
CLIPSCPPModule::~CLIPSCPPModule()
  { 
  }

/*#######################*/
/* CLIPSCPPFocus Methods */
/*#######################*/

/*****************/
/* CLIPSCPPFocus */
/*****************/
CLIPSCPPFocus::CLIPSCPPFocus()
  {
  }

/*****************/
/* CLIPSCPPFocus */
/*****************/
CLIPSCPPFocus::CLIPSCPPFocus(
  const char *initialString)
  {
   moduleName.assign(initialString);
  }

/******************/
/* GetModuleName */
/******************/
std::string *CLIPSCPPFocus::GetModuleName()
  {
   return &this->moduleName;
  }

/******************/
/* ~CLIPSCPPFocus */
/******************/
CLIPSCPPFocus::~CLIPSCPPFocus()
  { 
  }

/*############################*/
/* CLIPSCPPFocusStack Methods */
/*############################*/

/**********************/
/* CLIPSCPPFocusStack */
/**********************/
CLIPSCPPFocusStack::CLIPSCPPFocusStack()
  {
  }

CLIPSCPPFocusStack::CLIPSCPPFocusStack(size_t size)
  {
   stack.reserve(size);
  }

/***********************/
/* ~CLIPSCPPFocusStack */
/***********************/
CLIPSCPPFocusStack::~CLIPSCPPFocusStack()
  { 
   size_t i;
   
   for (i = 0; i < stack.size(); i++)
     { delete stack[i]; }
  }

/************/
/* GetStack */
/************/
std::vector<CLIPSCPPFocus *> *CLIPSCPPFocusStack::GetStack()
  {
   return &this->stack;
  }
 
/*******/
/* add */
/*******/
void CLIPSCPPFocusStack::add(
  CLIPSCPPFocus *theValue)
  {
   stack.push_back(theValue);
  }

/*############################*/
/* CLIPSCPPActivation Methods */
/*############################*/

/**********************/
/* CLIPSCPPActivation */
/**********************/
CLIPSCPPActivation::CLIPSCPPActivation()
  {
  }

/**********************/
/* CLIPSCPPActivation */
/**********************/
CLIPSCPPActivation::CLIPSCPPActivation(
  const char *ruleName,
  int salience,
  const char *basis)
  {
   this->ruleName.assign(ruleName);
   this->salience = salience;
   this->basis.assign(basis);
  }

/***************/
/* GetRuleName */
/***************/
std::string *CLIPSCPPActivation::GetRuleName()
  {
   return &this->ruleName;
  }

/***************/
/* GetSalience */
/***************/
int CLIPSCPPActivation::GetSalience()
  {
   return this->salience;
  }

/************/
/* GetBasis */
/************/
std::string *CLIPSCPPActivation::GetBasis()
  {
   return &this->basis;
  }

/***********************/
/* ~CLIPSCPPActivation */
/***********************/
CLIPSCPPActivation::~CLIPSCPPActivation()
  { 
  }

/*########################*/
/* CLIPSCPPAgenda Methods */
/*########################*/

/******************/
/* CLIPSCPPAgenda */
/******************/
CLIPSCPPAgenda::CLIPSCPPAgenda()
  {
  }

CLIPSCPPAgenda::CLIPSCPPAgenda(size_t size)
  {
   activations.reserve(size);
  }

/*******************/
/* ~CLIPSCPPAgenda */
/*******************/
CLIPSCPPAgenda::~CLIPSCPPAgenda()
  { 
   size_t i;
   
   for (i = 0; i < activations.size(); i++)
     { delete activations[i]; }
  }

/******************/
/* GetActivations */
/******************/
std::vector<CLIPSCPPActivation *> *CLIPSCPPAgenda::GetActivations()
  {
   return &this->activations;
  }
 
/*******/
/* add */
/*******/
void CLIPSCPPAgenda::add(
  CLIPSCPPActivation *theValue)
  {
   activations.push_back(theValue);
  }
  
/*****************/
/* GetFocusStack */
/*****************/
CLIPSCPPFocusStack *CLIPSCPPEnv::GetFocusStack()
  {
   CLIPSCPPFocusStack *theCPPFS;
   CLIPSCPPFocus *theCPPF;
   size_t moduleCount = 0;
   FocalModule *theFocus;

   for (theFocus = EngineData(theEnv)->CurrentFocus;
        theFocus != NULL;
        theFocus = theFocus->next)
     { moduleCount++; }

   theCPPFS = new CLIPSCPPFocusStack(moduleCount);

   for (theFocus = EngineData(theEnv)->CurrentFocus;
        theFocus != NULL;
        theFocus = theFocus->next)
     {
      theCPPF = new CLIPSCPPFocus(theFocus->theModule->header.name->contents);
      theCPPFS->add(theCPPF);
     }

   return theCPPFS;
  }

/*****************/
/* GetModuleList */
/*****************/
std::vector<CLIPSCPPModule> *CLIPSCPPEnv::GetModuleList()
  {
   vector<CLIPSCPPModule> * theList;
   Defmodule *theDefmodule;
   size_t defmoduleCount = 0;

#ifndef CLIPS_DLL_WRAPPER
   for (theDefmodule = GetNextDefmodule(theEnv,NULL);
        theDefmodule != NULL;
        theDefmodule = GetNextDefmodule(theEnv,theDefmodule))
     { defmoduleCount++; }

   theList = new vector<CLIPSCPPModule>();
   theList->reserve(defmoduleCount);
   
   for (theDefmodule = GetNextDefmodule(theEnv,NULL);
        theDefmodule != NULL;
        theDefmodule = GetNextDefmodule(theEnv,theDefmodule))
     { theList->push_back(CLIPSCPPModule(DefmoduleName(theDefmodule))); }
#else
   for (theDefmodule = __GetNextDefmodule(theEnv,NULL);
        theDefmodule != NULL;
        theDefmodule = __GetNextDefmodule(theEnv,theDefmodule))
     { defmoduleCount++; }

   theList = new vector<CLIPSCPPModule>();
   theList->reserve(defmoduleCount);
   
   for (theDefmodule = __GetNextDefmodule(theEnv,NULL);
        theDefmodule != NULL;
        theDefmodule = __GetNextDefmodule(theEnv,theDefmodule))
     { theList->push_back(CLIPSCPPModule(__DefmoduleName(theDefmodule))); }
#endif

   return theList;
  }

/*************/
/* GetAgenda */
/*************/
CLIPSCPPAgenda *CLIPSCPPEnv::GetAgenda(
  const char *moduleName)
  {
   CLIPSCPPAgenda *theCPPAgenda;
   Activation *theActivation;
   CLIPSCPPActivation *theCPPActivation;
   size_t activationCount = 0;
   Defmodule *theModule;
   struct defruleModule *theModuleItem;
   char bindingsBuffer[1024]; // TBD Replace

#ifndef CLIPS_DLL_WRAPPER
   theModule = FindDefmodule(theEnv,moduleName);
   if (theModule == NULL) return NULL;

   SaveCurrentModule(theEnv);
   SetCurrentModule(theEnv,theModule);

   theModuleItem = (struct defruleModule *)
                   GetModuleItem(theEnv,NULL,DefruleData(theEnv)->DefruleModuleIndex);

   RestoreCurrentModule(theEnv);

   if (theModuleItem == NULL) return NULL;

   /*==================================*/
   /* Count the number of activations. */
   /*==================================*/
   
   for (theActivation = theModuleItem->agenda;
        theActivation != NULL;
        theActivation = GetNextActivation(theEnv,theActivation))
     { activationCount++; }

   theCPPAgenda = new CLIPSCPPAgenda(activationCount); 

   /*================================*/
   /* Add activations to the agenda. */
   /*================================*/

   for (theActivation = theModuleItem->agenda;
        theActivation != NULL;
        theActivation = GetNextActivation(theEnv,theActivation))
     {
      GetActivationBasisPPForm(theEnv,bindingsBuffer,1024,theActivation);

      theCPPActivation = new CLIPSCPPActivation(theActivation->theRule->header.name->contents,
                                                theActivation->salience,
                                                bindingsBuffer);

      theCPPAgenda->add(theCPPActivation); 
     }
#else
   theModule = __FindDefmodule(theEnv,moduleName);
   if (theModule == NULL) return NULL;

   __SaveCurrentModule(theEnv);
   __SetCurrentModule(theEnv,theModule);

   theModuleItem = (struct defruleModule *)
                   __GetModuleItem(theEnv,NULL,DefruleData(theEnv)->DefruleModuleIndex);

   __RestoreCurrentModule(theEnv);

   if (theModuleItem == NULL) return NULL;
   
   /*==================================*/
   /* Count the number of activations. */
   /*==================================*/
   
   for (theActivation = theModuleItem->agenda;
        theActivation != NULL;
        theActivation = __GetNextActivation(theEnv,theActivation))
     { activationCount++; }

   theCPPAgenda = new CLIPSCPPAgenda(activationCount); 

   /*================================*/
   /* Add activations to the agenda. */
   /*================================*/

   for (theActivation = theModuleItem->agenda;
        theActivation != NULL;
        theActivation = __GetNextActivation(theEnv,theActivation))
     {
      __GetActivationBasisPPForm(theEnv,bindingsBuffer,1024,theActivation);

      theCPPActivation = new CLIPSCPPActivation(theActivation->theRule->header.name->contents,
                                                theActivation->salience,
                                                bindingsBuffer);

      theCPPAgenda->add(theCPPActivation); 
     }


#endif
   return theCPPAgenda;
  }

/*##############################*/
/* CLIPSCPPFactInstance Methods */
/*##############################*/

/************************/
/* CLIPSCPPFactInstance */
/************************/
CLIPSCPPFactInstance::CLIPSCPPFactInstance()
  {
  }

CLIPSCPPFactInstance::CLIPSCPPFactInstance(
  unsigned long long theTypeAddress,
  const char *theName,
  const char *theRelationName,
  std::vector<CLIPSCPPSlotValue> theSlotValues)
  {
   this->typeAddress = theTypeAddress;
   this->name.assign(theName);
   this->relationName.assign(theRelationName);
   this->slotValues = theSlotValues;
  }

/*************************/
/* ~CLIPSCPPFactInstance */
/*************************/
CLIPSCPPFactInstance::~CLIPSCPPFactInstance()
  { 
  }

/******************/
/* GetTypeAddress */
/******************/
unsigned long long CLIPSCPPFactInstance::GetTypeAddress()
  {
   return this->typeAddress;
  }

/***********/
/* GetName */
/***********/
std::string *CLIPSCPPFactInstance::GetName()
  {
   return &this->name;
  }

/*******************/
/* GetRelationName */
/*******************/
std::string *CLIPSCPPFactInstance::GetRelationName()
  {
   return &this->relationName;
  }

/*****************/
/* GetSlotValues */
/*****************/
std::vector<CLIPSCPPSlotValue> *CLIPSCPPFactInstance::GetSlotValues()
  {
   return &this->slotValues;
  }

/*###########################*/
/* CLIPSCPPSlotValue Methods */
/*###########################*/

/*********************/
/* CLIPSCPPSlotValue */
/*********************/
CLIPSCPPSlotValue::CLIPSCPPSlotValue()
  {
  }

CLIPSCPPSlotValue::CLIPSCPPSlotValue(
  const char *theSlotName,
  const char *theSlotValue,
  bool isDefaultValue)
  {
   this->slotName.assign(theSlotName);
   this->slotValue.assign(theSlotValue);
   this->isDefault = isDefaultValue;
  }

/**********************/
/* ~CLIPSCPPSlotValue */
/**********************/
CLIPSCPPSlotValue::~CLIPSCPPSlotValue()
  { 
  }

/***************/
/* GetSlotName */
/***************/
std::string *CLIPSCPPSlotValue::GetSlotName()
  {
   return &this->slotName;
  }

/*************/
/* IsDefault */
/*************/
bool CLIPSCPPSlotValue::IsDefault()
  {
   return this->isDefault;
  }

/****************/
/* GetSlotValue */
/****************/
std::string *CLIPSCPPSlotValue::GetSlotValue() // TBD Return string not pointer to string
  {
   return &this->slotValue;    // TBD http://stackoverflow.com/questions/7945638/should-i-use-pointer-to-stdstring
  }

/*****************/
/* GetFactScopes */
/*****************/
void CLIPSCPPEnv::GetFactScopes(
  std::unordered_map<unsigned long long,vector<bool>>& scopes)
  {
   Defmodule *theModule;
   size_t moduleCount = 0, whichBit;
   struct deftemplateModule *theModuleItem;
   Deftemplate *theDeftemplate;
   CLIPSBitMap *theScopeMap;
   size_t theDeftemplateIndex;

   scopes.clear();

#ifndef CLIPS_DLL_WRAPPER
   /*==============================*/
   /* Count the number of modules. */
   /*==============================*/

   for (theModule = GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theEnv,theModule))
     { moduleCount++; }

   /*===========================================================*/
   /* Iterate over each module creating the deftemplate scopes. */
   /*===========================================================*/

   for (theModule = GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theEnv,theModule))
     {
      theModuleItem = (struct deftemplateModule *) 
                      GetModuleItem(theEnv,theModule,DeftemplateData(theEnv)->DeftemplateModuleIndex);

      for (theDeftemplate = (Deftemplate *) theModuleItem->header.firstItem;
           theDeftemplate != NULL;
           theDeftemplate = (Deftemplate *) GetNextDeftemplate(theEnv,theDeftemplate))
        { 
         if (theDeftemplate->factList == NULL) continue;

         theDeftemplateIndex = (size_t) theDeftemplate;

         theScopeMap = (CLIPSBitMap *) CreateDeftemplateScopeMap(theEnv,theDeftemplate);
         scopes[theDeftemplateIndex] = vector<bool>(moduleCount);

         for (whichBit = 0; whichBit < moduleCount; whichBit++)
           {
            if (TestBitMap(theScopeMap->contents,whichBit))
              { scopes[theDeftemplateIndex][whichBit] = true; }
           }
        }
     }
#else
   /*==============================*/
   /* Count the number of modules. */
   /*==============================*/

   for (theModule = __GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = __GetNextDefmodule(theEnv,theModule))
     { moduleCount++; }

   /*===========================================================*/
   /* Iterate over each module creating the deftemplate scopes. */
   /*===========================================================*/

   for (theModule = __GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = __GetNextDefmodule(theEnv,theModule))
     {
      theModuleItem = (struct deftemplateModule *) 
                      __GetModuleItem(theEnv,theModule,DeftemplateData(theEnv)->DeftemplateModuleIndex);

      for (theDeftemplate = (Deftemplate *) theModuleItem->header.firstItem;
           theDeftemplate != NULL;
           theDeftemplate = __GetNextDeftemplate(theEnv,theDeftemplate))
        { 
         if (theDeftemplate->factList == NULL) continue;

         theDeftemplateIndex = (size_t) theDeftemplate;

         theScopeMap = (CLIPSBitMap *) __CreateDeftemplateScopeMap(theEnv,theDeftemplate);
         scopes[theDeftemplateIndex] = vector<bool>(moduleCount);

         for (whichBit = 0; whichBit < moduleCount; whichBit++)
           {
            if (TestBitMap(theScopeMap->contents,whichBit))
              { scopes[theDeftemplateIndex][whichBit] = true; }
           }
        }
     }
#endif
  }

/***************/
/* GetFactList */
/***************/
vector<CLIPSCPPFactInstance> *CLIPSCPPEnv::GetFactList()
  {
   vector<CLIPSCPPFactInstance> *theCPPFactList;
   Fact *theFact;
   size_t factCount = 0;
   CLIPSValue slotNames, temp;
   UDFValue slotValue, defaultValue;
   size_t i;
   char factNameBuffer[32]; 

#ifndef CLIPS_DLL_WRAPPER
   /*============================*/
   /* Count the number of facts. */
   /*============================*/
   
   for (theFact = GetNextFact(theEnv,NULL);
        theFact != NULL;
        theFact = GetNextFact(theEnv,theFact))
     { factCount++; }

   theCPPFactList = new vector<CLIPSCPPFactInstance>(); 
   theCPPFactList->reserve(factCount);

   /*=============================*/
   /* Add facts to the fact list. */
   /*=============================*/

   for (theFact = GetNextFact(theEnv,NULL);
        theFact != NULL;
        theFact = GetNextFact(theEnv,theFact))
     {
      vector<CLIPSCPPSlotValue> theCPPSlotValues;

      /*===================================*/
      /* Determine the number of slots and */
      /* create a vector to contain them.  */
      /*===================================*/

      FactSlotNames(theFact,&slotNames);

      theCPPSlotValues.reserve(slotNames.multifieldValue->length);

      for (i = 0; i < slotNames.multifieldValue->length; i++)
        {
         const char *theCSlotName, *theCSlotValue;
         bool defaulted = false;

         theCSlotName = slotNames.multifieldValue->contents[i].lexemeValue->contents;

         FactSlotValue(theEnv,theFact,theCSlotName,&temp);
         CLIPSToUDFValue(&temp,&slotValue);
         
         if (DeftemplateSlotDefaultP(FactDeftemplate(theFact),theCSlotName) == STATIC_DEFAULT)
           {
            DeftemplateSlotDefaultValue(FactDeftemplate(theFact),theCSlotName,&temp);
            CLIPSToUDFValue(&temp,&defaultValue);
                             
            if (DOsEqual(&slotValue,&defaultValue))
              { defaulted = true; }
           }

         theCSlotValue = DataObjectToString(theEnv,&slotValue);
         theCPPSlotValues.push_back(CLIPSCPPSlotValue(theCSlotName,theCSlotValue,defaulted));
        }

      sprintf(factNameBuffer,"f-%lld", FactIndex(theFact));

      theCPPFactList->push_back(CLIPSCPPFactInstance((size_t) theFact->whichDeftemplate,
                                                     factNameBuffer,
                                                     theFact->whichDeftemplate->header.name->contents,
                                                     theCPPSlotValues)); 
     }
#else
   /*============================*/
   /* Count the number of facts. */
   /*============================*/
   
   for (theFact = __GetNextFact(theEnv,NULL);
        theFact != NULL;
        theFact = __GetNextFact(theEnv,theFact))
     { factCount++; }

   theCPPFactList = new vector<CLIPSCPPFactInstance>(); 
   theCPPFactList->reserve(factCount);

   /*=============================*/
   /* Add facts to the fact list. */
   /*=============================*/

   for (theFact = __GetNextFact(theEnv,NULL);
        theFact != NULL;
        theFact = __GetNextFact(theEnv,theFact))
     {
      vector<CLIPSCPPSlotValue> theCPPSlotValues;

      /*===================================*/
      /* Determine the number of slots and */
      /* create a vector to contain them.  */
      /*===================================*/

      __FactSlotNames(theFact,&slotNames);

      theCPPSlotValues.reserve(slotNames.multifieldValue->length);

      for (i = 0; i < slotNames.multifieldValue->length; i++)
        {
         const char *theCSlotName, *theCSlotValue;
         bool defaulted = false;

         theCSlotName = slotNames.multifieldValue->contents[i].lexemeValue->contents;

         __FactSlotValue(theEnv,theFact,theCSlotName,&temp);
         __CLIPSToUDFValue(&temp,&slotValue);
         
         if (__DeftemplateSlotDefaultP(__FactDeftemplate(theFact),theCSlotName) == STATIC_DEFAULT)
           {
            __DeftemplateSlotDefaultValue(__FactDeftemplate(theFact),theCSlotName,&temp);
            __CLIPSToUDFValue(&temp,&defaultValue);
                             
            if (__DOsEqual(&slotValue,&defaultValue))
              { defaulted = true; }
           }

         theCSlotValue = __DataObjectToString(theEnv,&slotValue);
         theCPPSlotValues.push_back(CLIPSCPPSlotValue(theCSlotName,theCSlotValue,defaulted));
        }

      sprintf(factNameBuffer,"f-%lld", __FactIndex(theFact));

      theCPPFactList->push_back(CLIPSCPPFactInstance((size_t) theFact->whichDeftemplate,
                                                     factNameBuffer,
                                                     theFact->whichDeftemplate->header.name->contents,
                                                     theCPPSlotValues)); 
     }
#endif

   return theCPPFactList;
  }

/*********************/
/* GetInstanceScopes */
/*********************/
void CLIPSCPPEnv::GetInstanceScopes(
  std::unordered_map<unsigned long long,vector<bool>>& scopes)
  {
   Defmodule *theModule;
   size_t moduleCount = 0, whichBit;
   struct defclassModule *theModuleItem;
   Defclass *theDefclass;
   CLIPSBitMap *theScopeMap;
   size_t theDefclassIndex;

   scopes.clear();

#ifndef CLIPS_DLL_WRAPPER
   /*==============================*/
   /* Count the number of modules. */
   /*==============================*/

   for (theModule = GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theEnv,theModule))
     { moduleCount++; }

   /*========================================================*/
   /* Iterate over each module creating the defclass scopes. */
   /*========================================================*/

   for (theModule = GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = GetNextDefmodule(theEnv,theModule))
     {
      theModuleItem = (struct defclassModule *) 
                      GetModuleItem(theEnv,theModule,DefclassData(theEnv)->DefclassModuleIndex);

      for (theDefclass = (Defclass *) theModuleItem->header.firstItem;
           theDefclass != NULL;
           theDefclass = (Defclass *) GetNextDefclass(theEnv,theDefclass))
        { 
         if (theDefclass->instanceList == NULL) continue;

         theDefclassIndex = (size_t) theDefclass;

         theScopeMap = (CLIPSBitMap *) CreateClassScopeMap(theEnv,theDefclass);
         scopes[theDefclassIndex] = vector<bool>(moduleCount);

         for (whichBit = 0; whichBit < moduleCount; whichBit++)
           {
            if (TestBitMap(theScopeMap->contents,whichBit))
              { scopes[theDefclassIndex][whichBit] = true; }
           }
        }
     }
#else
   /*==============================*/
   /* Count the number of modules. */
   /*==============================*/

   for (theModule = __GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = __GetNextDefmodule(theEnv,theModule))
     { moduleCount++; }

   /*===========================================================*/
   /* Iterate over each module creating the deftemplate scopes. */
   /*===========================================================*/

   for (theModule = __GetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = __GetNextDefmodule(theEnv,theModule))
     {
      theModuleItem = (struct defclassModule *) 
                      __GetModuleItem(theEnv,theModule,DefclassData(theEnv)->DefclassModuleIndex);

      for (theDefclass = (Defclass *) theModuleItem->header.firstItem;
           theDefclass != NULL;
           theDefclass = __GetNextDefclass(theEnv,theDefclass))
        { 
         if (theDefclass->instanceList == NULL) continue;

         theDefclassIndex = (size_t) theDefclass;

         theScopeMap = (CLIPSBitMap *) __CreateClassScopeMap(theEnv,theDefclass);
         scopes[theDefclassIndex] = vector<bool>(moduleCount);

         for (whichBit = 0; whichBit < moduleCount; whichBit++)
           {
            if (TestBitMap(theScopeMap->contents,whichBit))
              { scopes[theDefclassIndex][whichBit] = true; }
           }
        }
     }
#endif
  }

/*******************/
/* GetInstanceList */
/*******************/
vector<CLIPSCPPFactInstance> *CLIPSCPPEnv::GetInstanceList()
  {
   vector<CLIPSCPPFactInstance> *theCPPInstanceList;
   Instance *theInstance;
   size_t instanceCount = 0;
   CLIPSValue slotNames, temp;
   UDFValue slotValue, defaultValue;
   size_t i;
   Defclass *theClass; 

#ifndef CLIPS_DLL_WRAPPER
   /*================================*/
   /* Count the number of instances. */
   /*================================*/
   
   for (theInstance = GetNextInstance(theEnv,NULL);
        theInstance != NULL;
        theInstance = GetNextInstance(theEnv,theInstance))
     { instanceCount++; }

   theCPPInstanceList = new vector<CLIPSCPPFactInstance>(); 
   theCPPInstanceList->reserve(instanceCount);

   /*=====================================*/
   /* Add instances to the instance list. */
   /*=====================================*/

   for (theInstance = GetNextInstance(theEnv,NULL);
        theInstance != NULL;
        theInstance = GetNextInstance(theEnv,theInstance))
     {
      vector<CLIPSCPPSlotValue> theCPPSlotValues;

      /*===================================*/
      /* Determine the number of slots and */
      /* create a vector to contain them.  */
      /*===================================*/

      theClass = InstanceClass(theInstance);
      ClassSlots(theClass,&slotNames,true);

      theCPPSlotValues.reserve(slotNames.multifieldValue->length);

      for (i = 0; i < slotNames.multifieldValue->length; i++)
        {
         const char *theCSlotName, *theCSlotValue;
         bool defaulted = false;

         theCSlotName = slotNames.multifieldValue->contents[i].lexemeValue->contents;

         DirectGetSlot(theInstance,theCSlotName,&temp);
         CLIPSToUDFValue(&temp,&slotValue);
         
         if (SlotDefaultP(theEnv,theClass,theCSlotName) == STATIC_DEFAULT)
           {
            SlotDefaultValue(theClass,theCSlotName,&temp);
            CLIPSToUDFValue(&temp,&defaultValue);
                             
            if (DOsEqual(&slotValue,&defaultValue))
              { defaulted = true; }
           }

         theCSlotValue = DataObjectToString(theEnv,&slotValue);
         theCPPSlotValues.push_back(CLIPSCPPSlotValue(theCSlotName,theCSlotValue,defaulted));
        }

      theCPPInstanceList->push_back(CLIPSCPPFactInstance((size_t) theClass,
                                                     InstanceName(theInstance),
                                                     DefclassName(theClass),
                                                     theCPPSlotValues)); 
     }
#else
   /*============================*/
   /* Count the number of facts. */
   /*============================*/
   
   for (theInstance = __GetNextInstance(theEnv,NULL);
        theInstance != NULL;
        theInstance = __GetNextInstance(theEnv,theInstance))
     { instanceCount++; }

   theCPPInstanceList = new vector<CLIPSCPPFactInstance>(); 
   theCPPInstanceList->reserve(instanceCount);

   /*=====================================*/
   /* Add instances to the instance list. */
   /*=====================================*/

   for (theInstance = __GetNextInstance(theEnv,NULL);
        theInstance != NULL;
        theInstance = __GetNextInstance(theEnv,theInstance))
     {
      vector<CLIPSCPPSlotValue> theCPPSlotValues;

      /*===================================*/
      /* Determine the number of slots and */
      /* create a vector to contain them.  */
      /*===================================*/

      theClass = __InstanceClass(theInstance);
      __ClassSlots(theClass,&slotNames,true);

      theCPPSlotValues.reserve(slotNames.multifieldValue->length);

      for (i = 0; i < slotNames.multifieldValue->length; i++)
        {
         const char *theCSlotName, *theCSlotValue;
         bool defaulted = false;

         theCSlotName = slotNames.multifieldValue->contents[i].lexemeValue->contents;

         __DirectGetSlot(theInstance,theCSlotName,&temp);
         __CLIPSToUDFValue(&temp,&slotValue);
         
         if (__SlotDefaultP(theEnv,theClass,theCSlotName) == STATIC_DEFAULT)
           {
            __SlotDefaultValue(theClass,theCSlotName,&temp);
            __CLIPSToUDFValue(&temp,&defaultValue);
                             
            if (__DOsEqual(&slotValue,&defaultValue))
              { defaulted = true; }
           }

         theCSlotValue = __DataObjectToString(theEnv,&slotValue);
         theCPPSlotValues.push_back(CLIPSCPPSlotValue(theCSlotName,theCSlotValue,defaulted));
        }

      theCPPInstanceList->push_back(CLIPSCPPFactInstance((size_t) theClass,
                                                         __InstanceName(theInstance),
                                                         __DefclassName(theClass),
                                                         theCPPSlotValues)); 
     }
#endif

   return theCPPInstanceList;
  }

/************/
/* Evaluate */
/************/
DataObject CLIPSCPPUserFunction::Evaluate(
  CLIPSCPPEnv *theEnv,
  std::vector<DataObject> arguments)
  {
   return DataObject(&VoidValue());
  }

/*******************/
/* AddUserFunction */
/*******************/
bool CLIPSCPPEnv::AddUserFunction(
  char *functionName,
  CLIPSCPPUserFunction *udf)
  {
   return AddUserFunction(functionName,"*",0,UNBOUNDED,NULL,udf);
  }

/*******************/
/* AddUserFunction */
/*******************/
bool CLIPSCPPEnv::AddUserFunction(
  char *functionName,
  char *returnTypes,
  unsigned short minArgs,
  unsigned short maxArgs,
  char *restrictions,
  CLIPSCPPUserFunction *udf)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::AddUDF(theEnv,functionName,returnTypes,minArgs,maxArgs,restrictions,
                   CLIPSCPPUserFunctionCallback,"CLIPSCPPUserFunctionCallback",udf);
#else
   return __AddUDF(theEnv,functionName,returnTypes,minArgs,maxArgs,restrictions,
                   CLIPSCPPUserFunctionCallback,"CLIPSCPPUserFunctionCallback",udf);
#endif
  }

/**********************/
/* RemoveUserFunction */
/**********************/
bool CLIPSCPPEnv::RemoveUserFunction(
  char *functionName)
  {
   CLIPSCPPUserFunction *udf;
#ifndef CLIPS_DLL_WRAPPER
   udf = (CLIPSCPPUserFunction *) GetUDFContext(theEnv,functionName);
   if (udf != NULL) delete udf;

   return ::RemoveUDF(theEnv,functionName);
#else
   udf = (CLIPSCPPUserFunction *) __GetUDFContext(theEnv,functionName);
   if (udf != NULL) delete udf;

   return __RemoveUDF(theEnv,functionName);
#endif
  }
  
/********************************/
/* CLIPSCPPUserFunctionCallback */
/********************************/
static void CLIPSCPPUserFunctionCallback(
  Environment *theEnv,
  UDFContext *theUDFContext,
  UDFValue *result)
  {
   int i, argCount;
   vector<DataObject> arguments;
   UDFValue theArg;
   CLIPSValue theValue;
   DataObject rv;
   CLIPSCPPUserFunction *udf;
   CLIPSCPPEnv *theCPPEnv;
   
#ifndef CLIPS_DLL_WRAPPER
   theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);

   udf = (CLIPSCPPUserFunction *) theUDFContext->context;

   argCount = UDFArgumentCount(theUDFContext);
   arguments.reserve(argCount);

   for (i = 1; i <= argCount; i++)
     {
      UDFNthArgument(theUDFContext,i,ANY_TYPE_BITS,&theArg);
      NormalizeMultifield(theEnv,&theArg);
      theValue.value = theArg.value;
      arguments.push_back(ConvertDataObject(&theValue));
     }

   rv = udf->Evaluate(theCPPEnv,arguments);
   ConvertToCLIPSValue(theEnv,rv,&theValue);
   CLIPSToUDFValue(&theValue,result);
#else
   theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);

   udf = (CLIPSCPPUserFunction *) theUDFContext->context;

   argCount = __UDFArgumentCount(theUDFContext);
   arguments.reserve(argCount);

   for (i = 1; i <= argCount; i++)
     {
      __UDFNthArgument(theUDFContext,i,ANY_TYPE_BITS,&theArg);
      __NormalizeMultifield(theEnv,&theArg);
      theValue.value = theArg.value;
      arguments.push_back(ConvertDataObject(&theValue));
     }

   rv = udf->Evaluate(theCPPEnv,arguments);
   ConvertToCLIPSValue(theEnv,rv,&theValue);
   __CLIPSToUDFValue(&theValue,result);
#endif
  }
 