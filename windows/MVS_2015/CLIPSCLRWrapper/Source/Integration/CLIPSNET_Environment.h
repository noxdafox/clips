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
        int Load(String ^);
        void LoadFromString(String ^);
        bool LoadFromResource(String ^,String ^);
        bool Build(String ^);

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
        
        bool Watch(String ^);
        bool Unwatch(String ^);
        
        void AddUserFunction(String ^,UserFunction ^);
        void AddUserFunction(String ^,String ^,unsigned short,unsigned short,String ^,UserFunction ^);
        void RemoveUserFunction(String ^);
        
        void AddRouter(String ^,int ,Router ^);
        void DeleteRouter(String ^);
        bool ActivateRouter(Router ^);
        bool DeactivateRouter(Router ^);
        void Printout(String ^,String ^);
        void Print(String ^);
        void PrintLn(String ^);
        
        void CommandLoop();

        void AddParserErrorCallback(ParserErrorCallback ^);
        
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
        bool GetWatchItem(String ^);
        void SetWatchItem(String ^,bool);
        virtual String^ ToString() override;
        void CallNextPrintRouter(Router ^,String ^,String ^);
        CaptureRouter ^ CaptureStart();
        void CaptureEnd(CaptureRouter ^,bool);
        InstanceAddressValue ^ FindInstanceByName(String ^);
        void AddError(String ^,long,String ^);

      protected:
        !Environment();
        void CheckForErrors(String ^);
        void StringLoader(String ^);

      private:
        CLIPSCPPEnv *m_Env;
        List<CLIPSLineError ^> ^ errorList;
     };
  };