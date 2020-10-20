#pragma once
#include <string>
#include <iostream>
#include <vector>
#include <unordered_map>

#include "envrnmnt.h"

namespace CLIPS
{

enum CLIPSCPPType 
  { 
    CPP_FLOAT_TYPE, 
    CPP_INTEGER_TYPE, 
    CPP_SYMBOL_TYPE, 
    CPP_STRING_TYPE, 
    CPP_MULTIFIELD_TYPE, 
    CPP_EXTERNAL_ADDRESS_TYPE, 
    CPP_FACT_ADDRESS_TYPE, 
    CPP_INSTANCE_ADDRESS_TYPE, 
    CPP_INSTANCE_NAME_TYPE, 
    CPP_VOID_TYPE,
    CPP_UNKNOWN_TYPE };

class CLIPSCPPRouter;
class CLIPSCPPPeriodicFunction;
class CLIPSCPPParserErrorFunction;
class CLIPSCPPFocus;
class CLIPSCPPModule;
class CLIPSCPPFocusStack;
class CLIPSCPPActivation;
class CLIPSCPPAgenda;
class CLIPSCPPFactInstance;
class CLIPSCPPSlotValue;
class CLIPSCPPUserFunction;

class DataObject; // TBD Change to PrimitiveValue
class FactAddressValue;
class InstanceAddressValue;

class CLIPSCPPEnv
  {
   private:
      ::Environment *theEnv;

   public:
      CLIPSCPPEnv();
      ~CLIPSCPPEnv();
      
      void Clear();
      int Load(char *);
      void LoadFromString(char *);
      bool Build(char *);
      
      void Reset();
      long long Run(long long runLimit = -1);
      
      FactAddressValue *AssertString(char *);
      InstanceAddressValue *MakeInstance(char *);
      
      DataObject Eval(char *);
      
      bool Watch(char *);
      bool Unwatch(char *);

      bool AddUserFunction(char *,CLIPSCPPUserFunction *);
      bool AddUserFunction(char *,char *,unsigned short,unsigned short,char *,CLIPSCPPUserFunction *);
      bool RemoveUserFunction(char *);
      
      bool AddRouter(char *,int,CLIPSCPPRouter *);
      bool DeleteRouter(char *);
      bool ActivateRouter(char *);
      bool DeactivateRouter(char *);
      void Write(const char *,const char *);
      void Write(const char *);
      void Writeln(const char *,const char *);
      void Writeln(const char *);
      
      void SetParserErrorCallback(CLIPSCPPParserErrorFunction *);

      void CommandLoop();
      void CommandLoopOnceThenBatch();
      int GetHaltExecution();
      void SetHaltExecution(bool);
      void SetHaltCommandLoopBatch(bool);
      int GetEvaluationError();
      void SetEvaluationError(bool);
      int GetHaltRules();
      void SetHaltRules(bool);
      int ChangeDirectory(char *);
      bool ValidWatchItem(char *);
      bool GetWatchItem(char *);
      void SetWatchItem(char *,bool);
      bool AddPeriodicFunction(char *,int,CLIPSCPPPeriodicFunction *);
      bool RemovePeriodicFunction(char *);
      bool EnablePeriodicFunctions(bool);
      size_t InputBufferCount();
      const char *GetInputBuffer();
      void SetInputBuffer(const char *);
      bool InputBufferContainsCommand();
      void AppendToDribble(const char *);
      void PrintBanner();
      void PrintPrompt();
      void CLIPSCPPEnv::GetFactScopes(std::unordered_map<unsigned long long,std::vector<bool>>&);
	  std::vector<CLIPSCPPFactInstance> *GetFactList();
      void CLIPSCPPEnv::GetInstanceScopes(std::unordered_map<unsigned long long,std::vector<bool>>&);
	  std::vector<CLIPSCPPFactInstance> *GetInstanceList();
	  std::vector<CLIPSCPPModule> *GetModuleList();
      CLIPSCPPFocusStack *GetFocusStack();
      CLIPSCPPAgenda *GetAgenda(const char *);
      bool GetAgendaChanged();
      void SetAgendaChanged(bool);
      bool GetFocusChanged();
      void SetFocusChanged(bool);
      bool GetFactListChanged();
      void SetFactListChanged(bool);
      bool GetInstancesChanged();
      void SetInstancesChanged(bool);
      InstanceAddressValue * FindInstanceByName(char *);
      char *GetParsingFileName();
      void SetParsingFileName(const char *);
      bool PrintRouterExists(const char *);
  };

class CLIPSCPPUserFunction
  {
   public:
     virtual DataObject Evaluate(CLIPSCPPEnv *,std::vector<DataObject>);
     const static unsigned short UNBOUNDED = USHRT_MAX;
  };

class CLIPSCPPRouter
  {
   public:
      virtual bool Query(CLIPSCPPEnv *,const char *);
      virtual void Write(CLIPSCPPEnv *,const char *,const char *);
      virtual int Read(CLIPSCPPEnv *,const char *);
      virtual int Unread(CLIPSCPPEnv *,int,const char *);
      virtual void Exit(CLIPSCPPEnv *,bool);

      const static char *STDOUT;
      const static char *STDIN;
      const static char *STDWRN;
      const static char *STDERR;
  };

class CLIPSCPPFocus
  {
   public:
     CLIPSCPPFocus();
     CLIPSCPPFocus(const char *);
     virtual ~CLIPSCPPFocus();

     std::string *GetModuleName();

   private:
     std::string moduleName;
  };

class CLIPSCPPFocusStack
  {
   public:
     CLIPSCPPFocusStack();
     CLIPSCPPFocusStack(size_t);
     virtual ~CLIPSCPPFocusStack();
     
     void add(CLIPSCPPFocus *);
     std::vector<CLIPSCPPFocus *> *GetStack();

   private:
     std::vector<CLIPSCPPFocus *> stack; // TBD CLIPSCPPFocus
  };

class CLIPSCPPModule
  {
   public:
     CLIPSCPPModule();
     CLIPSCPPModule(const char *);
     virtual ~CLIPSCPPModule();

     std::string *GetModuleName();

   private:
     std::string moduleName;
  };

class CLIPSCPPActivation
  {
   public:
     CLIPSCPPActivation();
     CLIPSCPPActivation(const char *,int,const char *);
     virtual ~CLIPSCPPActivation();

     int GetSalience();
     std::string *GetRuleName();
     std::string *GetBasis();

   private:
     int salience;
     std::string ruleName;
     std::string basis;
  };

class CLIPSCPPAgenda
  {
   public:
     CLIPSCPPAgenda();
     CLIPSCPPAgenda(size_t);
     virtual ~CLIPSCPPAgenda();
     
     void add(CLIPSCPPActivation *);
	  std::vector<CLIPSCPPActivation *> *GetActivations();

   private:
     std::vector<CLIPSCPPActivation *> activations; // TBD CLIPSCPPActivation
  };

class CLIPSCPPSlotValue
  {
   public:
     CLIPSCPPSlotValue();
     CLIPSCPPSlotValue(const char *,const char *,bool);
     virtual ~CLIPSCPPSlotValue();

     bool IsDefault();
     std::string *GetSlotName();
     std::string *GetSlotValue();

   private:
     std::string slotName;
     std::string slotValue;
     bool isDefault;
 
  };

class CLIPSCPPFactInstance
  {
   public:
     CLIPSCPPFactInstance();
     CLIPSCPPFactInstance(unsigned long long,const char *,const char *,std::vector<CLIPSCPPSlotValue>);
     virtual ~CLIPSCPPFactInstance();

     unsigned long long GetTypeAddress();
     std::string *GetName();
     std::string *GetRelationName();
     std::vector<CLIPSCPPSlotValue> *GetSlotValues();

   private:
     unsigned long long typeAddress;
     std::string name;
     std::string relationName;
     std::vector<CLIPSCPPSlotValue> slotValues;
  };

class CLIPSCPPPeriodicFunction
  {
   public:
      virtual void Callback(CLIPSCPPEnv *);
  };

class CLIPSCPPParserErrorFunction
  {
   public:
      virtual void Callback(CLIPSCPPEnv *,const char *,const char *,const char *,long);
  };
      
class Value
  {
   public:
     Value();
     Value(const Value& v);
     virtual ~Value();
     /* virtual Value& operator= (const Value& v); */
     friend std::ostream& operator<< (std::ostream& o, const Value& s);
     friend std::ostream& operator<< (std::ostream& o, const Value* s);
     virtual std::ostream& print(std::ostream& o) const = 0;
     virtual Value *clone() const = 0; 
     virtual CLIPSCPPType GetCLIPSType();
  };

class VoidValue : public Value
  {  
   public:
     VoidValue();
     VoidValue(const VoidValue& v);
     virtual ~VoidValue();
     /* virtual VoidValue& operator= (const VoidValue& s); */
     virtual std::ostream& print(std::ostream& o) const;
     virtual VoidValue *clone() const; 
	 CLIPSCPPType GetCLIPSType();
  };

class StringValue : public Value
  {  
   public:
     StringValue();
     StringValue(const char *);
     StringValue(const StringValue& v);
     virtual ~StringValue();
     /* virtual StringValue& operator= (const StringValue& v); */
     virtual std::ostream& print(std::ostream& o) const;
     virtual StringValue *clone() const; 
	 CLIPSCPPType GetCLIPSType();
	 std::string *GetStringValue();

   private:
     std::string theString;
  };

class SymbolValue : public Value
  { 
   public:
     SymbolValue();
     SymbolValue(const char *);
     SymbolValue(const SymbolValue& v);
     virtual ~SymbolValue();
     /* virtual SymbolValue& operator= (const SymbolValue& v); */
     virtual std::ostream& print(std::ostream& o) const;
     virtual SymbolValue *clone() const; 
	 CLIPSCPPType GetCLIPSType();
	 std::string *GetSymbolValue();
  
   private:
     std::string theString;
  };

class InstanceNameValue : public Value
  { 
   public:
     InstanceNameValue();
     InstanceNameValue(const char *);
     InstanceNameValue(const InstanceNameValue& v);
     virtual ~InstanceNameValue();
     /* virtual InstanceNameValue& operator= (const InstanceNameValue& v); */
     virtual std::ostream& print(std::ostream& o) const;
     virtual InstanceNameValue *clone() const; 
	 CLIPSCPPType GetCLIPSType();
	 std::string *GetInstanceNameValue();
  
   private:
     std::string theString;
  };

class IntegerValue : public Value
  { 
   public:
     IntegerValue();
     IntegerValue(long long);
     IntegerValue(const IntegerValue& v);
     virtual ~IntegerValue();
     /* virtual IntegerValue& operator= (const IntegerValue& v); */
     virtual std::ostream& print(std::ostream& o) const;
     virtual IntegerValue *clone() const; 
	 CLIPSCPPType GetCLIPSType();
	 long long GetIntegerValue();
	 double GetFloatValue();
  
   private:
     long long theInteger;
  };

class FloatValue : public Value
  { 
   public:
     FloatValue();
     FloatValue(double);
     FloatValue(const FloatValue& v);
     virtual ~FloatValue();
     virtual std::ostream& print(std::ostream& o) const;
     virtual FloatValue *clone() const; 
     CLIPSCPPType GetCLIPSType();
     long long GetIntegerValue();
     double GetFloatValue();
  
   private:
     double theFloat;
  };

class FactAddressValue : public Value
  { 
   public:
     FactAddressValue(Fact *);
     FactAddressValue(const FactAddressValue& v);
     virtual ~FactAddressValue();
     virtual FactAddressValue& operator= (const FactAddressValue& v);
     virtual std::ostream& print(std::ostream& o) const;
     virtual FactAddressValue *clone() const; 
     virtual DataObject GetFactSlot(char *) const;
     CLIPSCPPType GetCLIPSType();
     virtual long long GetFactIndex() const;
     Fact *GetFactAddressValue();
     void Retain();
     void Release();
  
   private:
     Fact *theFactAddress;
  };
 
class InstanceAddressValue : public Value
  { 
   public:
     InstanceAddressValue(Instance *);
     InstanceAddressValue(const InstanceAddressValue& v);
     virtual ~InstanceAddressValue();
     virtual InstanceAddressValue& operator= (const InstanceAddressValue& v);
     virtual std::ostream& print(std::ostream& o) const;
     virtual InstanceAddressValue *clone() const; 
     CLIPSCPPType GetCLIPSType();
     virtual const char *GetInstanceName() const;
     virtual DataObject DirectGetSlot(char *) const;
     Instance *GetInstanceAddressValue();
     void Retain();
     void Release();
  
   private:
     Instance *theInstanceAddress;
  };
  
class MultifieldValue : public Value
  {  
   public:
     MultifieldValue();
     MultifieldValue(size_t);
     MultifieldValue(const MultifieldValue& v);
     virtual ~MultifieldValue();
     virtual MultifieldValue& operator= (const MultifieldValue& v);
     virtual std::ostream& print(std::ostream& o) const;
     virtual MultifieldValue *clone() const; 
     void add(Value *);
     CLIPSCPPType GetCLIPSType();
     std::vector<Value *> *GetMultifieldValue(); // TBD Change Value * to Value

   private:
     std::vector<Value *> theMultifield;
  };

class ExternalAddressValue : public Value
  { 
   public:
     ExternalAddressValue(CLIPSExternalAddress *);
     ExternalAddressValue(const ExternalAddressValue& v);
     virtual ~ExternalAddressValue();
     virtual ExternalAddressValue& operator= (const ExternalAddressValue& v);
     virtual std::ostream& print(std::ostream& o) const;
     virtual ExternalAddressValue *clone() const; 
     CLIPSCPPType GetCLIPSType();
     CLIPSExternalAddress *GetExternalAddressValue() const;
  
   private:
     CLIPSExternalAddress *theExternalAddress;
  };

class DataObject
  {
   public:
     DataObject();
     DataObject(Value *v);
     DataObject(const DataObject& v);
     virtual ~DataObject();
     virtual DataObject& operator= (const DataObject& s);
     friend std::ostream& operator<< (std::ostream& o, const DataObject& s);
     friend std::ostream& operator<< (std::ostream& o, const DataObject* s);
     virtual std::ostream& print(std::ostream& o) const;
	 CLIPSCPPType GetCLIPSType();
	 Value *GetCLIPSValue();
     static DataObject Void();
     static DataObject String();
     static DataObject String(char *);
     static DataObject Symbol();
     static DataObject Symbol(char *);
     static DataObject InstanceName();
     static DataObject InstanceName(char *);
     static DataObject Multifield();
     static DataObject Multifield(size_t);
     static DataObject Integer();
     static DataObject Integer(long long);
     static DataObject Float();
     static DataObject Float(double);
     static DataObject FactAddress(Fact *);
     static DataObject InstanceAddress(Instance *);

   private:
     Value *theValue; // TBD Change Value * to Value
  };

inline DataObject DataObject::Void()
  { return DataObject(); }

inline DataObject DataObject::String()
  { return DataObject(new StringValue()); }

inline DataObject DataObject::String(
  char *initialString)
  { return DataObject(new StringValue(initialString)); }

inline DataObject DataObject::Symbol()
  { return DataObject(new SymbolValue()); }

inline DataObject DataObject::Symbol(
  char *initialString)
  { return DataObject(new SymbolValue(initialString)); }

inline DataObject DataObject::InstanceName()
  { return DataObject(new InstanceNameValue()); }

inline DataObject DataObject::InstanceName(
  char *initialString)
  { return DataObject(new InstanceNameValue(initialString)); }

inline DataObject DataObject::Multifield()
  { return DataObject(new MultifieldValue()); }

inline DataObject DataObject::Multifield(size_t size)
  { return DataObject(new MultifieldValue(size)); }

inline DataObject DataObject::Integer()
  { return DataObject(new IntegerValue(0)); }

inline DataObject DataObject::Integer(long long theInteger)
  { return DataObject(new IntegerValue(theInteger)); }

inline DataObject DataObject::Float()
  { return DataObject(new FloatValue(0.0)); }

inline DataObject DataObject::Float(double theFloat)
  { return DataObject(new FloatValue(theFloat)); }

inline DataObject DataObject::FactAddress(Fact *theFact)
  { return DataObject(new FactAddressValue(theFact)); }

inline DataObject DataObject::InstanceAddress(Instance *theInstance)
  { return DataObject(new InstanceAddressValue(theInstance)); }
}
