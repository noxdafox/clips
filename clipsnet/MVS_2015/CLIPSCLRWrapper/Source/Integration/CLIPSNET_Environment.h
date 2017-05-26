#pragma once

#include "clipscpp.h"
#include "CLIPSNET_Agenda.h"
#include "CLIPSNET_FactInstance.h"
#include "CLIPSNET_FocusStack.h"
#include "CLIPSNET_Module.h"
#include "CLIPSNET_Values.h"
#include "CLIPSNET_Router.h"
#include "CLIPSNET_PeriodicCallback.h"

using namespace System;
using namespace System::Collections;
using namespace CLIPS;

namespace CLIPSNET
  {
   public delegate void PeriodicCallbackDelegate();

   /*######################################*/
   /* DelegatePeriodicCallback declaration */
   /*######################################*/
   ref class DelegatePeriodicCallback : PeriodicCallback
     {
      public:
        DelegatePeriodicCallback();
        void Callback (void) override;

      event PeriodicCallbackDelegate ^ CallbackEvents;
     };

   /*###################*/
   /* Environment class */
   /*###################*/

   public ref class Environment 
     {
      public:
        Environment();
        ~Environment();

        event PeriodicCallbackDelegate ^ PeriodicCallbackEvent;

        void CommandLoop();
        long long Run();
        long long Run(long long);
        void Reset();
        bool Build(String ^);
        bool GetHaltExecution();
        void SetHaltExecution(bool);
        void SetHaltCommandLoopBatch(bool);
        bool GetHaltRules();
        void SetHaltRules(bool);
        bool GetEvaluationError();
        void SetEvaluationError(bool);
        int Load(String ^);
        bool ChangeDirectory(String ^);
        void LoadFromString(String ^);
        bool LoadFromResource(String ^,String ^);
        PrimitiveValue ^ Eval(String ^);
        void AddRouter(String ^,int ,Router ^);
        void DeleteRouter(String ^);
        void AddPeriodicCallback(String ^,int ,PeriodicCallback ^);
        void RemovePeriodicCallback(String ^);
        bool EnablePeriodicFunctions(bool);
        FactAddressValue ^ AssertString(String ^);
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

      protected:
        !Environment();

      private:
        CLIPSCPPEnv *m_Env;
     };
  };