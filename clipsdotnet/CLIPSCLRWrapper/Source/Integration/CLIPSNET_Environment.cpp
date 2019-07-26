
#include "CLIPSNET_Environment.h"

using namespace System;
using namespace System::Text;
using namespace System::Reflection;
using namespace System::IO;
using namespace CLIPS;

namespace CLIPSNET
  {	 
   /***************/
   /* Environment */
   /***************/
   Environment::Environment() : m_Env( new CLIPSCPPEnv ) {}
   
   /****************/
   /* ~Environment */
   /****************/
   Environment::~Environment()
     {
      this->!Environment(); 
     }
     
   /****************/
   /* !Environment */
   /****************/
   Environment::!Environment()
     {
      delete m_Env; 
     }
 
   /************/
   /* ToString */
   /************/
   String ^ Environment::ToString()
     {
      long long envLong = (long long) m_Env;

      return gcnew String("<Environment-" + envLong + ">");
     }
    
   /***************/
   /* CommandLoop */
   /***************/
   void Environment::CommandLoop()
     { return m_Env->CommandLoop(); }

   /*******/
   /* Run */
   /*******/
   long long Environment::Run()
     { return m_Env->Run(-1LL); }

   /*******/
   /* Run */
   /*******/
   long long Environment::Run(
     long long runLimit)
     { return m_Env->Run(runLimit); }

   /*********/
   /* Reset */
   /*********/
   void Environment::Reset()
     { return m_Env->Reset(); }

   /********/
   /* Load */
   /********/
   int Environment::Load(
     String ^ fileName)
     {
      array<Byte>^ ebFileName = Encoding::UTF8->GetBytes(fileName);
      pin_ptr<Byte> pbFileName = &ebFileName[0];

      return m_Env->Load((char *) pbFileName);
     }
     
   /******************/
   /* LoadFromString */
   /******************/
   void Environment::LoadFromString(
     String ^ loadString)
     {
      array<Byte>^ ebLoadString = Encoding::UTF8->GetBytes(loadString);
      pin_ptr<Byte> pbLoadString = &ebLoadString[0];

      m_Env->LoadFromString((char *) pbLoadString);
     }

   /********************/
   /* LoadFromResource */
   /********************/
   bool Environment::LoadFromResource(
     String ^ assemblyName,
     String ^ resourceName)
     {
      Assembly ^ assembly = System::Reflection::Assembly::Load(assemblyName);
      Stream ^ stream = assembly->GetManifestResourceStream(resourceName);
      if (stream == nullptr) return false;
      StreamReader ^ reader = gcnew StreamReader(stream);
      String ^ resourceContent = reader->ReadToEnd();

      this->LoadFromString(resourceContent);
      return true;
     }
     
   /*********/
   /* Build */
   /*********/
   bool Environment::Build(
     String ^ buildString)
     {
      array<Byte>^ ebBuildString = Encoding::UTF8->GetBytes(buildString);
      pin_ptr<Byte> pbBuildString = &ebBuildString[0];

      return m_Env->Build((char *) pbBuildString);
     }
     
   /********/
   /* Eval */
   /********/
   PrimitiveValue ^ Environment::Eval(
     String ^ evalString)
     {
      array<Byte>^ ebEvalString = Encoding::UTF8->GetBytes(evalString);
      pin_ptr<Byte> pbEvalString = &ebEvalString[0];
      PrimitiveValue ^ rv;

      rv = DataObjectToPrimitiveValue(m_Env->Eval((char *) pbEvalString));

      return rv;
     }
     
   /****************/
   /* AssertString */
   /****************/
   FactAddressValue ^ Environment::AssertString(
     String ^ factString)
     {
      array<Byte>^ ebFactString = Encoding::UTF8->GetBytes(factString);
      pin_ptr<Byte> pbFactString = &ebFactString[0];
      CLIPS::FactAddressValue *frv;

      frv = m_Env->AssertString((char *) pbFactString);
      if (frv == NULL) return (nullptr);
      
      return gcnew FactAddressValue(frv);
     }
     
   /*************/
   /* AddRouter */
   /*************/
   void Environment::AddRouter(
	  String ^ routerName,
	  int priority,
	  Router ^ theRouter)
     { 
      array<Byte>^ ebRouterName = Encoding::UTF8->GetBytes(routerName);
      pin_ptr<Byte> pbRouterName = &ebRouterName[0];

      m_Env->AddRouter((char *) pbRouterName,priority,(CLIPS::CLIPSCPPRouter *) theRouter->RouterBridge());
     }
     
   /********************/
   /* InputBufferCount */
   /********************/
   size_t Environment::InputBufferCount()
     { 
      return m_Env->InputBufferCount();
     }   

   /********************/
   /* GetHaltExecution */
   /********************/
   bool Environment::GetHaltExecution()
     { 
      if (m_Env->GetHaltExecution() == 0)
        { return false; }
      else
        { return true; }
     }

   /********************/
   /* SetHaltExecution */
   /********************/
   void Environment::SetHaltExecution(bool value)
     { m_Env->SetHaltExecution(value); }

   /**********************/
   /* GetEvaluationError */
   /**********************/
   bool Environment::GetEvaluationError()
     {
      if (m_Env->GetEvaluationError() == 0)
        { return false; }
      else
        { return true; }
     }
     
   /**********************/
   /* SetEvaluationError */
   /**********************/
   void Environment::SetEvaluationError(bool value)
     {
      m_Env->SetEvaluationError(value);
     }

   /****************/
   /* GetHaltRules */
   /****************/
   bool Environment::GetHaltRules()
     {
      if (m_Env->GetHaltRules() == 0)
        { return false; }
      else
       { return true; }
     }
     
   /****************/
   /* SetHaltRules */
   /****************/
   void Environment::SetHaltRules(bool value)
     {
      m_Env->SetHaltRules(value);
     }
  };