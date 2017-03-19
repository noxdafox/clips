#include <iostream>
#include <stdexcept>

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
#include "constant.h"
#include "commline.h"
#include "cstrcpsr.h"
#include "engine.h"
#include "envrnbld.h"
#include "factmngr.h"
#include "fileutil.h"
#include "inscom.h"
#include "router.h"
#include "strngfun.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "watch.h"

/*##################*/
/* Static Functions */
/*##################*/

static bool CLIPSCPPQuery(Environment *,const char *,void *);
static void CLIPSCPPPrint(Environment *,const char *,const char *,void *);
static int CLIPSCPPGetc(Environment *,const char *,void *);
static int CLIPSCPPUngetc(Environment *,const char *,int,void *);
static void CLIPSCPPExit(Environment *,int,void *);
static Value *ConvertSingleFieldValue(Environment *,int,void *);
static DataObject ConvertDataObject(Environment *,CLIPSValue *);

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
     
   return ConvertDataObject(theEnv,&rv);
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
   return new FactAddressValue(theEnv,rv);
  }

/*********************/
/* ConvertDataObject */
/*********************/
static DataObject ConvertDataObject(
  Environment *theEnv,
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
        return DataObject(ConvertSingleFieldValue(theEnv,theCV->header->type,theCV->value));
     
      case MULTIFIELD_TYPE:
        Multifield *theList = theCV->multifieldValue;
        size_t mfLength = theCV->multifieldValue->length, i; 
        
        MultifieldValue *theMultifield = new MultifieldValue(mfLength);
        
        for (i = 0; i < mfLength; i++)
         { theMultifield->add(ConvertSingleFieldValue(theEnv,theList->contents[i].header->type,theList->contents[i].value)); }

        return DataObject(theMultifield);
     }
     
   return DataObject();
  }
  
/****************************/
/* ConvertSingleFieldValue: */
/****************************/
static Value *ConvertSingleFieldValue(
  Environment *theEnv,
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
        return new FactAddressValue(theEnv,(Fact *) value);

      case INSTANCE_ADDRESS_TYPE:
        return new InstanceAddressValue(theEnv,(Instance *) value);
     }

   return new VoidValue();
  }
  
/*********/
/* Watch */
/*********/
int CLIPSCPPEnv::Watch(
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
int CLIPSCPPEnv::Unwatch(
  char *item)
  {
#ifndef CLIPS_DLL_WRAPPER
   return ::UnwatchString(theEnv,item);
#else
   return __UnwatchString(theEnv,item);
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
                                  CLIPSCPPPrint,CLIPSCPPGetc,CLIPSCPPUngetc,
                                  CLIPSCPPExit,router);
#else
   return __AddRouter(theEnv,routerName,priority,CLIPSCPPQuery,
                                    CLIPSCPPPrint,CLIPSCPPGetc,CLIPSCPPUngetc,
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

/*########################*/
/* CLIPSCPPRouter Methods */
/*########################*/

const char *CLIPSCPPRouter::STANDARD_OUTPUT = STDOUT;
const char *CLIPSCPPRouter::STANDARD_INPUT = STDIN;
const char *CLIPSCPPRouter::WARNING = WWARNING;
const char *CLIPSCPPRouter::ERROR = WERROR;

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
/* Print */
/*********/
void CLIPSCPPRouter::Print(
  CLIPSCPPEnv *theCPPEnv,
  const char *logicalName,
  const char *printString)
  {
  }
  
/********/
/* Getc */
/********/
int CLIPSCPPRouter::Getc(
  CLIPSCPPEnv *theCPPEnv,
  const char *logicalName)
  {
   return -1;
  }
  
/**********/
/* Ungetc */
/**********/
int CLIPSCPPRouter::Ungetc(
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
  int exitCode)
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
  Environment *theEnv,Fact *theFact) : theEnvironment(theEnv), theFactAddress(theFact)
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
        
   theEnvironment = v.theEnvironment;
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
   int rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = ::GetFactSlot(theFactAddress,slotName,&theCV);
#else
   rv = __GetFactSlot(theFactAddress,slotName,&theCV);
#endif
   
   if (! rv)
      {
       std::string excStr = "GetFactSlot: Invalid slot name ";
       excStr.append(slotName);
       
       throw std::logic_error(excStr); 
      }

   return ConvertDataObject(theEnvironment,&theCV);
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
  Environment *theEnv,Instance *theInstance) : theEnvironment(theEnv), theInstanceAddress(theInstance)
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
        
   theEnvironment = v.theEnvironment;
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
   return __InstanceName(theEnvironment,theInstanceAddress);
#endif
  }

/*****************/
/* DirectGetSlot */
/*****************/
DataObject InstanceAddressValue::DirectGetSlot(char *slotName) const
  {  
   CLIPSValue theCV;
   
#ifndef CLIPS_DLL_WRAPPER
   ::DirectGetSlot(theInstanceAddress,slotName,&theCV);
#else
   __DirectGetSlot(theInstanceAddress,slotName,&theCV);
#endif
   
   return ConvertDataObject(theEnvironment,&theCV);
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
/* CLIPSCPPPrint */
/*****************/
static void CLIPSCPPPrint(
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
   
   theRouter->Print(theCPPEnv,logicalName,printString);
  }

/*****************/
/* CLIPSCPPGetc */
/*****************/
static int CLIPSCPPGetc(
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
   
   return(theRouter->Getc(theCPPEnv,logicalName));
  }
  
/*****************/
/* CLIPSCPPUngetc */
/*****************/
static int CLIPSCPPUngetc(
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
   
   return theRouter->Ungetc(theCPPEnv,character,logicalName);
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
   
   theRouter->Exit(theCPPEnv,exitCode);
  }

