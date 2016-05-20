#include <iostream>
#include <stdexcept>

#include "clipscpp.h"

using namespace CLIPS;
using std::vector;
using std::string;

#ifdef CLIPS_DLL_WRAPPER
#include <windows.h>
#endif

#ifdef CLIPS_CLR_WRAPPER
#include "clips.h"
#else
extern "C"
  {
   #include "clips.h"
  }
#endif

#ifdef CLIPS_DLL_WRAPPER
extern "C"
  {
   #include "CLIPSDLL.h"
  }
#endif

/*##################*/
/* Static Functions */
/*##################*/

static int CLIPSCPPQuery(void *,const char *);
static int CLIPSCPPPrint(void *,const char *,const char *);
static int CLIPSCPPGetc(void *,const char *);
static int CLIPSCPPUngetc(void *,int,const char *);
static int CLIPSCPPExit(void *,int);
static Value *ConvertSingleFieldValue(void *,int,void *);
static DataObject ConvertDataObject(void *,DATA_OBJECT *);

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
   EnvClear(theEnv);
#else
   __EnvClear(theEnv);
#endif
  }

/********/
/* Load */
/********/
int CLIPSCPPEnv::Load(
  char *theFile)
  {
#ifndef CLIPS_DLL_WRAPPER
   return EnvLoad(theEnv,theFile);
#else
   return __EnvLoad(theEnv,theFile);
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
   EnvReset(theEnv);
#else
   __EnvReset(theEnv);
#endif
  }

/*******/
/* Run */
/*******/
long long CLIPSCPPEnv::Run(
  long long runLimit)
  {
#ifndef CLIPS_DLL_WRAPPER
   return EnvRun(theEnv,runLimit);
#else
   return __EnvRun(theEnv,runLimit);
#endif
  }

/*********/
/* Build */
/*********/
bool CLIPSCPPEnv::Build(
  char *buildString)
  {   
#ifndef CLIPS_DLL_WRAPPER
   if (EnvBuild(theEnv,buildString))
     { return true; }
   else
     { return false; }
#else
   if (__EnvBuild(theEnv,buildString))
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
   DATA_OBJECT rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rc = EnvEval(theEnv,evalString,&rv);
#else
   rc = __EnvEval(theEnv,evalString,&rv);
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
    return EnvGetHaltExecution(theEnv);
#else
    return __EnvGetHaltExecution(theEnv);
#endif
}

/********************/
/* SetHaltExecution */
/********************/
void CLIPSCPPEnv::SetHaltExecution(
   int value)
{
#ifndef CLIPS_DLL_WRAPPER
    EnvSetHaltExecution(theEnv,value);
#else
    __EnvSetHaltExecution(theEnv,value);
#endif
}

/**********************/
/* GetEvaluationError */
/**********************/
int CLIPSCPPEnv::GetEvaluationError()
{
#ifndef CLIPS_DLL_WRAPPER
    return EnvGetEvaluationError(theEnv);
#else
    return __EnvGetEvaluationError(theEnv);
#endif
}

/**********************/
/* SetEvaluationError */
/**********************/
void CLIPSCPPEnv::SetEvaluationError(
    int value)
{
#ifndef CLIPS_DLL_WRAPPER
    EnvSetEvaluationError(theEnv, value);
#else
    __EnvSetEvaluationError(theEnv, value);
#endif
}

/****************/
/* GetHaltRules */
/****************/
int CLIPSCPPEnv::GetHaltRules()
{
#ifndef CLIPS_DLL_WRAPPER
    return EnvGetHaltRules(theEnv);
#else
    return __EnvGetHaltRules(theEnv);
#endif
}

/****************/
/* SetHaltRules */
/****************/
void CLIPSCPPEnv::SetHaltRules(
    int value)
{
#ifndef CLIPS_DLL_WRAPPER
    EnvSetHaltRules(theEnv, value);
#else
    __EnvSetHaltRules(theEnv, value);
#endif
}

/****************/
/* AssertString */
/****************/
FactAddressValue *CLIPSCPPEnv::AssertString(
  char *factString)
  {
   void *rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = EnvAssertString(theEnv,factString);
#else
   rv = __EnvAssertString(theEnv,factString);
#endif
     
   if (rv == NULL) return(NULL);
   return new FactAddressValue(theEnv,rv);
  }

/*********************/
/* ConvertDataObject */
/*********************/
static DataObject ConvertDataObject(
  void *theEnv,
  DATA_OBJECT *theDO)
  {
   DataObject tv;
   
   switch (GetpType(theDO))
     {
      case RVOID:
      case STRING:        
      case SYMBOL:
      case INSTANCE_NAME:
      case INTEGER:
      case FLOAT:
      case FACT_ADDRESS:
      case INSTANCE_ADDRESS:
        return DataObject(ConvertSingleFieldValue(theEnv,GetpType(theDO),GetpValue(theDO)));
     
      case MULTIFIELD:
        struct multifield *theList = (struct multifield *) DOPToPointer(theDO);;
        size_t mfLength = GetpDOLength(theDO), i; 
        
        MultifieldValue *theMultifield = new MultifieldValue(mfLength);
        
        for (i = (unsigned) GetpDOBegin(theDO); i <= (unsigned) GetpDOEnd(theDO); i++)
         { theMultifield->add(ConvertSingleFieldValue(theEnv,GetMFType(theList,i),GetMFValue(theList,i))); }

        return DataObject(theMultifield);
     }
     
   return DataObject();
  }
  
/****************************/
/* ConvertSingleFieldValue: */
/****************************/
static Value *ConvertSingleFieldValue(
  void *theEnv,
  int type,
  void  *value)
  {
   switch(type)
     {
      case RVOID:
        return new VoidValue();

      case SYMBOL:
        return new SymbolValue(ValueToString(value));
        
      case STRING:
        return new StringValue(ValueToString(value));
        
      case INSTANCE_NAME:
        return new InstanceNameValue(ValueToString(value));

      case INTEGER:
        return new IntegerValue(ValueToLong(value));

      case FLOAT:
        return new FloatValue(ValueToDouble(value));

      case FACT_ADDRESS:
        return new FactAddressValue(theEnv,value);

      case INSTANCE_ADDRESS:
        return new InstanceAddressValue(theEnv,value);
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
   return EnvWatch(theEnv,item);
#else
   return __EnvWatch(theEnv,item);
#endif
  }

/***********/
/* Unwatch */
/***********/
int CLIPSCPPEnv::Unwatch(
  char *item)
  {
#ifndef CLIPS_DLL_WRAPPER
   return EnvUnwatch(theEnv,item);
#else
   return __EnvUnwatch(theEnv,item);
#endif
  }
  
/*************/
/* AddRouter */
/*************/
int CLIPSCPPEnv::AddRouter(
  char *routerName,
  int priority,
  CLIPSCPPRouter *router)
  {
#ifndef CLIPS_DLL_WRAPPER
   return EnvAddRouterWithContext(theEnv,routerName,priority,CLIPSCPPQuery,
                                  CLIPSCPPPrint,CLIPSCPPGetc,CLIPSCPPUngetc,
                                  CLIPSCPPExit,router);
#else
   return __EnvAddRouterWithContext(theEnv,routerName,priority,CLIPSCPPQuery,
                                    CLIPSCPPPrint,CLIPSCPPGetc,CLIPSCPPUngetc,
                                    CLIPSCPPExit,router);
#endif
  }

/********************/
/* InputBufferCount */
/********************/
size_t CLIPSCPPEnv::InputBufferCount()
  {
#ifndef CLIPS_DLL_WRAPPER
   return EnvInputBufferCount(theEnv);
#else
   return __EnvInputBufferCount(theEnv);
#endif
  }

/*******************/
/* getInputBuffer: */
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
/* setInputBuffer: */
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

/*########################*/
/* CLIPSCPPRouter Methods */
/*########################*/

const char *CLIPSCPPRouter::STANDARD_OUTPUT = STDOUT;
const char *CLIPSCPPRouter::STANDARD_INPUT = STDIN;
const char *CLIPSCPPRouter::WARNING = WWARNING;
const char *CLIPSCPPRouter::ERROR = WERROR;
const char *CLIPSCPPRouter::TRACE = WTRACE;
const char *CLIPSCPPRouter::DIALOG = WDIALOG;
const char *CLIPSCPPRouter::PROMPT = WPROMPT;
const char *CLIPSCPPRouter::DISPLAY = WDISPLAY;

/*********/
/* Query */
/*********/
int CLIPSCPPRouter::Query(
  CLIPSCPPEnv *theCPPEnv,
  const char *logicalName)
  { 
   return FALSE;
  }
  
/*********/
/* Print */
/*********/
int CLIPSCPPRouter::Print(
  CLIPSCPPEnv *theCPPEnv,
  const char *logicalName,
  const char *printString)
  {
   return FALSE;
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
int CLIPSCPPRouter::Exit(
  CLIPSCPPEnv *theCPPEnv,
  int exitCode)
  {
   return FALSE;
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
CLIPSType DataObject::GetCLIPSType()
  {
   if (theValue == NULL)
     { return UNKNOWN_TYPE; }

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
CLIPSType Value::GetCLIPSType()
  {
   return UNKNOWN_TYPE;
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
CLIPSType VoidValue::GetCLIPSType()
  {
   return VOID_TYPE;
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
CLIPSType StringValue::GetCLIPSType()
  {
   return STRING_TYPE;
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
CLIPSType SymbolValue::GetCLIPSType()
  {
   return SYMBOL_TYPE;
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
CLIPSType InstanceNameValue::GetCLIPSType()
  {
   return INSTANCE_NAME_TYPE;
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
CLIPSType IntegerValue::GetCLIPSType()
  {
   return INTEGER_TYPE;
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
CLIPSType FloatValue::GetCLIPSType()
  {
   return FLOAT_TYPE;
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
  void *theEnv,void *theFact) : theEnvironment(theEnv), theFactAddress(theFact)
  {
#ifndef CLIPS_DLL_WRAPPER
   EnvIncrementFactCount(theEnvironment,theFact);
#else
   __EnvIncrementFactCount(theEnvironment,theFact);
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
   EnvDecrementFactCount(theEnvironment,theFactAddress);
#else
   __EnvDecrementFactCount(theEnvironment,theFactAddress);
#endif
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSType FactAddressValue::GetCLIPSType()
  {
   return FACT_ADDRESS_TYPE;
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
      EnvDecrementFactCount(theEnvironment,theFactAddress);
#else
      __EnvDecrementFactCount(theEnvironment,theFactAddress);
#endif
     }
        
   theEnvironment = v.theEnvironment;
   theFactAddress = v.theFactAddress;
     
#ifndef CLIPS_DLL_WRAPPER
   EnvIncrementFactCount(theEnvironment,theFactAddress);
#else
   __EnvIncrementFactCount(theEnvironment,theFactAddress);
#endif
   
   return *this;
  }

/****************/
/* GetFactIndex */
/****************/
long long FactAddressValue::GetFactIndex() const
  {  
#ifndef CLIPS_DLL_WRAPPER
   return EnvFactIndex(theEnvironment,theFactAddress);
#else
   return __EnvFactIndex(theEnvironment,theFactAddress);
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
   DATA_OBJECT theDO;
   int rv;
   
#ifndef CLIPS_DLL_WRAPPER
   rv = EnvGetFactSlot(theEnvironment,theFactAddress,slotName,&theDO);
#else
   rv = __EnvGetFactSlot(theEnvironment,theFactAddress,slotName,&theDO);
#endif
   
   if (! rv)
      {
       std::string excStr = "GetFactSlot: Invalid slot name ";
       excStr.append(slotName);
       
       throw std::logic_error(excStr); 
      }

   return ConvertDataObject(theEnvironment,&theDO);
  }

/***********************/
/* GetFactAddressValue */
/***********************/
void *FactAddressValue::GetFactAddressValue()
  { return this->theFactAddress; }

/*##############################*/
/* InstanceAddressValue Methods */
/*##############################*/

/************************/
/* InstanceAddressValue */
/************************/
InstanceAddressValue::InstanceAddressValue(
  void *theEnv,void *theInstance) : theEnvironment(theEnv), theInstanceAddress(theInstance)
  {
#ifndef CLIPS_DLL_WRAPPER
   EnvIncrementInstanceCount(theEnvironment,theInstance);
#else
   __EnvIncrementInstanceCount(theEnvironment,theInstance);
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
   EnvDecrementInstanceCount(theEnvironment,theInstanceAddress);
#else
   __EnvDecrementInstanceCount(theEnvironment,theInstanceAddress);
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
      EnvDecrementInstanceCount(theEnvironment,theInstanceAddress);
#else
      __EnvDecrementInstanceCount(theEnvironment,theInstanceAddress);
#endif

     }
        
   theEnvironment = v.theEnvironment;
   theInstanceAddress = v.theInstanceAddress;
     
#ifndef CLIPS_DLL_WRAPPER
   EnvIncrementInstanceCount(theEnvironment,theInstanceAddress);
#else
   __EnvIncrementInstanceCount(theEnvironment,theInstanceAddress);
#endif
   
   return *this;
  }

/****************/
/* GetCLIPSType */
/****************/
CLIPSType InstanceAddressValue::GetCLIPSType()
  {
   return INSTANCE_ADDRESS_TYPE;
  }

/*******************/
/* GetInstanceName */
/*******************/
const char *InstanceAddressValue::GetInstanceName() const
  {  
#ifndef CLIPS_DLL_WRAPPER
   return EnvGetInstanceName(theEnvironment,theInstanceAddress);
#else
   return __EnvGetInstanceName(theEnvironment,theInstanceAddress);
#endif
  }

/*****************/
/* DirectGetSlot */
/*****************/
DataObject InstanceAddressValue::DirectGetSlot(char *slotName) const
  {  
   DATA_OBJECT theDO;
   
#ifndef CLIPS_DLL_WRAPPER
   EnvDirectGetSlot(theEnvironment,theInstanceAddress,slotName,&theDO);
#else
   __EnvDirectGetSlot(theEnvironment,theInstanceAddress,slotName,&theDO);
#endif
   
   return ConvertDataObject(theEnvironment,&theDO);
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
void *InstanceAddressValue::GetInstanceAddressValue()
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
CLIPSType MultifieldValue::GetCLIPSType()
  {
   return MULTIFIELD_TYPE;
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
static int CLIPSCPPQuery(
  void *theEnv,
  const char *logicalName)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) __GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return(theRouter->Query(theCPPEnv,logicalName));
  }

/*****************/
/* CLIPSCPPPrint */
/*****************/
static int CLIPSCPPPrint(
  void *theEnv,
  const char *logicalName,
  const char *printString)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) __GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return(theRouter->Print(theCPPEnv,logicalName,printString));
  }

/*****************/
/* CLIPSCPPGetc */
/*****************/
static int CLIPSCPPGetc(
  void *theEnv,
  const char *logicalName)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) __GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return(theRouter->Getc(theCPPEnv,logicalName));
  }
  
/*****************/
/* CLIPSCPPUngetc */
/*****************/
static int CLIPSCPPUngetc(
  void *theEnv,
  int character,
  const char *logicalName)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) __GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return(theRouter->Ungetc(theCPPEnv,character,logicalName));
  }
  
/*****************/
/* CLIPSCPPExit */
/*****************/
static int CLIPSCPPExit(
  void *theEnv,
  int exitCode)
  { 
#ifndef CLIPS_DLL_WRAPPER
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) GetEnvironmentContext(theEnv);
#else
   CLIPSCPPRouter *theRouter = (CLIPSCPPRouter *) __GetEnvironmentRouterContext(theEnv);
   CLIPSCPPEnv *theCPPEnv = (CLIPSCPPEnv *) __GetEnvironmentContext(theEnv);
#endif
   
   return(theRouter->Exit(theCPPEnv,exitCode));
  }

