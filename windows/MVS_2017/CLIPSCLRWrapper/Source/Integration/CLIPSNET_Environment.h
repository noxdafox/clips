#pragma once

#include "clipscpp.h"
#include "CLIPSNET_Agenda.h"
#include "CLIPSNET_Exceptions.h"
#include "CLIPSNET_FactInstance.h"
#include "CLIPSNET_FocusStack.h"
#include "CLIPSNET_Module.h"
#include "CLIPSNET_Values.h"
#include "CLIPSNET_Router.h"
#include "CLIPSNET_ParserErrorCallback.h"
#include "CLIPSNET_PeriodicCallback.h"
#include "CLIPSNET_UserFunction.h"

using namespace System;
using namespace System::Collections;
using namespace CLIPS;

namespace CLIPSNET
  {
   ref class Router;
   ref class CaptureRouter;
   ref class Environment;

   //public delegate void PeriodicCallbackDelegate();
   //public delegate void ParserErrorCallbackDelegate();

   /*######################################*/
   /* DelegatePeriodicCallback declaration */
   /*######################################*/
   /*
   ref class DelegatePeriodicCallback : PeriodicCallback
     {
      public:
        DelegatePeriodicCallback();
        void Callback (void) override;

      event PeriodicCallbackDelegate ^ CallbackEvents;
     };
     */
   /*#########################################*/
   /* DelegateParserErrorCallback declaration */
   /*#########################################*/
   /*
   ref class DelegateParserErrorCallback : ParserErrorCallback
     {
      public:
        DelegateParserErrorCallback();
        void Callback (String ^,String ^,String ^,long) override;

      event ParserErrorCallbackDelegate ^ CallbackEvents;
     };
     */
  
   public ref class LoadParserErrorCallback : ParserErrorCallback
     {
      public:
        LoadParserErrorCallback(Environment ^);
        void LoadParserErrorCallback::Callback(String ^,String ^,String ^,long) override;

      private:
        Environment ^ env;
     };

   /*###################*/
   /* Environment class */
   /*###################*/

   public ref class Environment 
     {
      public:
        Environment();
        ~Environment();

        //event PeriodicCallbackDelegate ^ PeriodicCallbackEvent;
        //event ParserErrorCallbackDelegate ^ ParserErrorCallbackEvent;
        
        void Clear();
        void Load(String ^);
        void LoadFromString(String ^);
        void LoadFromResource(String ^,String ^);
        void Build(String ^);

        void Reset();
        long long Run();
        long long Run(long long);
        
        FactAddressValue ^ AssertString(String ^);
        InstanceAddressValue ^ MakeInstance(String ^);

        FactAddressValue ^ FindFact(String ^);
        FactAddressValue ^ FindFact(String ^,String ^,String ^);
        List<FactAddressValue ^> ^ FindAllFacts(String ^);
        List<FactAddressValue ^> ^ FindAllFacts(String ^,String ^,String ^);

        InstanceAddressValue ^ FindInstance(String ^);
        InstanceAddressValue ^ FindInstance(String ^,String ^,String ^);
        List<InstanceAddressValue ^> ^ FindAllInstances(String ^);
        List<InstanceAddressValue ^> ^ FindAllInstances(String ^,String ^,String ^);
        
        PrimitiveValue ^ Eval(String ^);
        
        void Watch(String ^);
        void Unwatch(String ^);
        bool GetWatchItem(String ^);
        void SetWatchItem(String ^,bool);
        
        void AddUserFunction(String ^,UserFunction ^);
        void AddUserFunction(String ^,String ^,unsigned short,unsigned short,String ^,UserFunction ^);
        void RemoveUserFunction(String ^);
        
        void AddRouter(Router ^);
        void DeleteRouter(Router ^);
        void ActivateRouter(Router ^);
        void DeactivateRouter(Router ^);
        void Write(String ^,String ^);
        void Write(String ^);
        void WriteLine(String ^,String ^);
        void WriteLine(String ^);
        
        void CommandLoop();

        void SetParserErrorCallback(ParserErrorCallback ^);
        
        void AddPeriodicCallback(String ^,int ,PeriodicCallback ^);

        bool GetHaltExecution();
        void SetHaltExecution(bool);
        void SetHaltCommandLoopBatch(bool);
        bool GetHaltRules();
        void SetHaltRules(bool);
        bool GetEvaluationError();
        void SetEvaluationError(bool);
        bool ChangeDirectory(String ^);
        void RemovePeriodicCallback(String ^);
        bool EnablePeriodicFunctions(bool);
        size_t InputBufferCount();
        String ^ GetInputBuffer();
        void SetInputBuffer(String ^);
        bool InputBufferContainsCommand();
        void AppendToDribble(String ^);
        void CommandLoopOnceThenBatch();
        void PrintBanner();
        void PrintPrompt();
        Dictionary<unsigned long long,BitArray ^> ^ GetFactScopes();
        List<FactInstance ^> ^ GetFactList();
        Dictionary<unsigned long long,BitArray ^> ^ GetInstanceScopes();
        List<FactInstance ^> ^ GetInstanceList();
		List<Module ^> ^ GetModuleList();
        FocusStack ^ GetFocusStack();
        Agenda ^ GetAgenda(String ^);
        Agenda ^ GetAgenda(Focus ^);
        bool GetAgendaChanged();
        void SetAgendaChanged(bool);
        bool GetFocusChanged();
        void SetFocusChanged(bool);
        bool GetFactListChanged();
        void SetFactListChanged(bool);
        bool GetInstancesChanged();
        void SetInstancesChanged(bool);
        virtual String^ ToString() override;
        void CallNextPrintRouter(Router ^,String ^,String ^);
        CaptureRouter ^ CaptureStart();
        void CaptureEnd(CaptureRouter ^,bool);
        InstanceAddressValue ^ FindInstanceByName(String ^);
        void AddError(String ^,long,String ^);

        static const String ^ FACTS = gcnew String("facts");
        static const String ^ RULES = gcnew String("rules");
        static const String ^ DEFFUNCTIONS = gcnew String("deffunctions");
        static const String ^ COMPILATIONS = gcnew String("compilations");
        static const String ^ INSTANCES = gcnew String("instances");
        static const String ^ SLOTS = gcnew String("slots");
        static const String ^ ACTIVATIONS = gcnew String("activations");
        static const String ^ STATISTICS = gcnew String("statistics");
        static const String ^ FOCUS = gcnew String("focus");
        static const String ^ GENERIC_FUNCTIONS = gcnew String("generic-functions");
        static const String ^ METHODS = gcnew String("methods");
        static const String ^ GLOBALS = gcnew String("globals");
        static const String ^ MESSAGES = gcnew String("messages");
        static const String ^ MESSAGE_HANDLERS = gcnew String("message-handlers");
        static const String ^ NONE = gcnew String("none");
        static const String ^ ALL = gcnew String("all");

      protected:
        !Environment();
        void CheckForErrors(String ^);
        void StringLoader(String ^);

      private:
        CLIPSCPPEnv *m_Env;
        LoadParserErrorCallback ^ errorCallback;
        List<CLIPSLineError ^> ^ errorList;
     };
  };