
#include "CLIPSNET_Environment.h"
#include "CLIPSNET_Exceptions.h"
#include "CLIPSNET_Router.h"

using namespace System;
using namespace System::Text;
using namespace System::Reflection;
using namespace System::IO;
using namespace CLIPS;

namespace CLIPSNET
  {	 
  /* TBD Use delegates rather than create instance */
  /*
   DelegatePeriodicCallback::DelegatePeriodicCallback() 
     {
     
     }

   void DelegatePeriodicCallback::Callback()
     {
      CallbackEvents();
     }
    */ 
    /*
   DelegateParserErrorCallback::DelegateParserErrorCallback() 
     {
     
     }
     
   void DelegateParserErrorCallback::Callback(
     String ^ fileName,
     String ^ warningString,
     String ^ errorString,
     long lineNumber)
     {
      CallbackEvents();
     }
     */
     
   String ^ Environment::CharStarToString(
     const char *theString)
     {
      if (theString == NULL)
        { return nullptr; }

      return gcnew String(theString,0, (int) strlen(theString), UTF8Encoding::UTF8);
     }

   /***************************/
   /* LoadParserErrorCallback */
   /***************************/
   LoadParserErrorCallback::LoadParserErrorCallback(
     Environment ^ theEnv) 
     {
      env = theEnv;
     }

   void LoadParserErrorCallback::Callback(
     String ^ fileName,
     String ^ warningString,
     String ^ errorString,
     long lineNumber)
     {
      if (errorString == nullptr) 
        { return; }
      
      env->AddError(fileName,lineNumber,errorString);
     }
   
   /*************/
   /* AddError: */
   /*************/
   void Environment::AddError(
     String ^ fileName,
     long lineNumber,
     String ^ message)
     {
      errorList->Add(gcnew CLIPSLineError(fileName,lineNumber,message));
     }
     
   /******************/
   /* CheckForErrors */
   /******************/
   void Environment::CheckForErrors(
     String ^ function)
     {
      if (errorList->Count == 0)
        { return; }
        
      String ^ exceptionString;
         
      if (errorList->Count == 1)
        { exceptionString = "\n" + function + " encountered 1 error:\n"; }
      else
        { exceptionString = "\n" + function + " encountered " + errorList->Count + " errors:\n"; }
           
      for each (CLIPSLineError ^ theError in errorList)
        {
         exceptionString = exceptionString + "\n" + 
                                                  theError->FileName + " (Line " +
                                                  theError->LineNumber + ") : " +
                                                  theError->Message;
        }
           
      CLIPSLoadException ^ e = gcnew CLIPSLoadException(exceptionString,errorList);
      errorList->Clear();
      throw e;
     }

   /***************/
   /* Environment */
   /***************/
   Environment::Environment() : m_Env( new CLIPSCPPEnv ) 
     {
      errorCallback = gcnew LoadParserErrorCallback(this);
      errorList = gcnew List<CLIPSLineError ^>();
     }
   
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

   /****************************/
   /* CommandLoopOnceThenBatch */
   /****************************/
   void Environment::CommandLoopOnceThenBatch()
     { 
      return m_Env->CommandLoopOnceThenBatch(); 
     }

   /***************/
   /* PrintBanner */
   /***************/
   void Environment::PrintBanner()
     { return m_Env->PrintBanner(); }

   /***************/
   /* PrintPrompt */
   /****************/
   void Environment::PrintPrompt()
     { return m_Env->PrintPrompt(); }

   /****************/
   /* CaptureStart */
   /****************/
   CaptureRouter ^ Environment::CaptureStart()
     {
      this->SetParserErrorCallback(errorCallback);
      errorList->Clear();
      return gcnew CaptureRouter(this,gcnew array<String ^> (1) { Router::STDERR });
     }

   /**************/
   /* CaptureEnd */
   /**************/
   void Environment::CaptureEnd(
     CaptureRouter ^ commandCapture,
     bool throwError)
     {
      String ^ error = commandCapture->Output;

      this->SetParserErrorCallback(nullptr);

      this->DeleteRouter(commandCapture);

      if (throwError && (! String::IsNullOrEmpty(error)))
        { throw gcnew CLIPSException(error); }
     }
     
   /*********/
   /* Clear */
   /*********/
   void Environment::Clear()
     { 
      CaptureRouter ^ commandCapture = CaptureStart();

      m_Env->Clear();

      CaptureEnd(commandCapture,true);
     }

   /********/
   /* Load */
   /********/
   void Environment::Load(
     String ^ fileName)
     {
      if (fileName == nullptr)
        { throw gcnew System::ArgumentNullException("fileName"); }

      if (fileName->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","fileName"); }

      array<Byte>^ ebFileName = Encoding::UTF8->GetBytes(fileName);
      
      CaptureRouter ^ commandCapture = CaptureStart();

      if (ebFileName->Length)
        {
         pin_ptr<Byte> pbFileName = &ebFileName[0];
         if (m_Env->Load((char *) pbFileName) != 0)
           {
            CaptureEnd(commandCapture,false);
            throw gcnew System::IO::FileNotFoundException(
                             "Could not load file '" + fileName + "'.",
                             fileName);
           }
        }

      CaptureEnd(commandCapture,false);

      CheckForErrors("Load");
     }

   /****************/
   /* StringLoader */
   /****************/
   void Environment::StringLoader(
     String ^ loadString)
     {
      array<Byte>^ ebLoadString = Encoding::UTF8->GetBytes(loadString);

      if (ebLoadString->Length)
        {
         pin_ptr<Byte> pbLoadString = &ebLoadString[0];
         m_Env->LoadFromString((char *) pbLoadString);
        }
     }
          
   /******************/
   /* LoadFromString */
   /******************/
   void Environment::LoadFromString(
     String ^ loadString)
     {
      if (loadString == nullptr)
        { throw gcnew System::ArgumentNullException("loadString"); }

      char *oldName = m_Env->GetParsingFileName();

      m_Env->SetParsingFileName("<String>");

      CaptureRouter ^ commandCapture = CaptureStart();

      this->StringLoader(loadString);

      CaptureEnd(commandCapture,false);

      m_Env->SetParsingFileName(oldName);

      CheckForErrors("LoadFromString");
     }

   /********************/
   /* LoadFromResource */
   /********************/
   void Environment::LoadFromResource(
     String ^ assemblyName,
     String ^ resourceName)
     {
      Assembly ^ assembly = System::Reflection::Assembly::Load(assemblyName);
      Stream ^ stream = assembly->GetManifestResourceStream(resourceName);
      if (stream == nullptr)
        {
         throw gcnew System::IO::FileNotFoundException(
                          "Could not load file '" + resourceName + "'.",
                          resourceName);
        }

      StreamReader ^ reader = gcnew StreamReader(stream);
      String ^ resourceContent = reader->ReadToEnd();
      
      char *oldName = m_Env->GetParsingFileName();

      array<Byte>^ ebResourceName = Encoding::UTF8->GetBytes(resourceName);

      if (ebResourceName->Length)
        {
         pin_ptr<Byte> pbResourceName = &ebResourceName[0];
         m_Env->SetParsingFileName((char *) pbResourceName);
        }

      CaptureRouter ^ commandCapture = CaptureStart();

      this->StringLoader(resourceContent);
      
      CaptureEnd(commandCapture,false);

      m_Env->SetParsingFileName(oldName);

      CheckForErrors("LoadFromResource");
     }
     
   /*********/
   /* Build */
   /*********/
   void Environment::Build(
     String ^ buildString)
     {
      if (buildString == nullptr)
        { throw gcnew System::ArgumentNullException("buildString"); }
      
      array<Byte>^ ebBuildString = Encoding::UTF8->GetBytes(buildString);
      
      CaptureRouter ^ commandCapture = CaptureStart();

      if (ebBuildString->Length)
        {
         pin_ptr<Byte> pbBuildString = &ebBuildString[0];
         m_Env->Build((char *) pbBuildString);
        }

      CaptureEnd(commandCapture,true);
     }

   /*********/
   /* Reset */
   /*********/
   void Environment::Reset()
     {
      CaptureRouter ^ commandCapture = CaptureStart();

      m_Env->Reset(); 
      
      CaptureEnd(commandCapture,true);
     }

   /*******/
   /* Run */
   /*******/
   long long Environment::Run()
     { 
      long long rv;

      CaptureRouter ^ commandCapture = CaptureStart();

      rv = m_Env->Run(-1LL); 

      CaptureEnd(commandCapture,true);

      return rv;
     }

   /*******/
   /* Run */
   /*******/
   long long Environment::Run(
     long long runLimit)
     { 
      long long rv;

      CaptureRouter ^ commandCapture = CaptureStart();

      rv = m_Env->Run(runLimit); 

      CaptureEnd(commandCapture,true);

      return rv;
     }

   /****************/
   /* AssertString */
   /****************/
   FactAddressValue ^ Environment::AssertString(
     String ^ factString)
     {
      FactAddressValue ^ fav;
      if (factString == nullptr)
        { throw gcnew System::ArgumentNullException("factString"); }

      if (factString->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","factString"); }

      CLIPS::FactAddressValue *frv = NULL;
      array<Byte>^ ebFactString = Encoding::UTF8->GetBytes(factString);
      
      CaptureRouter ^ commandCapture = CaptureStart();

      if (ebFactString->Length)
        {
         pin_ptr<Byte> pbFactString = &ebFactString[0];
         frv = m_Env->AssertString((char *) pbFactString);
        }

      CaptureEnd(commandCapture,true);

      if (frv == NULL) return nullptr;
      
      fav = gcnew FactAddressValue(frv);

      delete frv;

      return fav;
     }

   /****************/
   /* MakeInstance */
   /****************/
   InstanceAddressValue ^ Environment::MakeInstance(
     String ^ instanceString)
     {
      InstanceAddressValue ^ iav;

      if (instanceString == nullptr)
        { throw gcnew System::ArgumentNullException("instanceString"); }
              
      if (instanceString->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","instanceString"); }

      CLIPS::InstanceAddressValue *irv = NULL;
      array<Byte>^ ebInstanceString = Encoding::UTF8->GetBytes(instanceString);
      
      CaptureRouter ^ commandCapture = CaptureStart();

      if (ebInstanceString->Length)
        {
         pin_ptr<Byte> pbInstanceString = &ebInstanceString[0];
         irv = m_Env->MakeInstance((char *) pbInstanceString);
        }

      CaptureEnd(commandCapture,true);

      if (irv == NULL) return nullptr;

      iav = gcnew InstanceAddressValue(irv);

      delete irv;

      return iav;
     }

   /************/
   /* FindFact */
   /************/
   FactAddressValue ^ Environment::FindFact(
     String ^ deftemplate)
     {
      return FindFact("?f",deftemplate,"TRUE");
     }

   FactAddressValue ^ Environment::FindFact(
     String ^ variable,
     String ^ deftemplate,
     String ^ condition)
     {      
      if (variable == nullptr)
        { throw gcnew System::ArgumentNullException("variable"); }
        
      if (deftemplate == nullptr)
        { throw gcnew System::ArgumentNullException("deftemplate"); }

      if (condition == nullptr)
        { throw gcnew System::ArgumentNullException("condition"); }
      
      String ^ query = "(find-fact " + 
                         "((" + variable + " " + deftemplate + ")) " + condition + ")";

      PrimitiveValue ^ pv = Eval(query);

      if (! pv->IsMultifield())
        { return nullptr; }

      MultifieldValue ^ mv = (MultifieldValue ^) pv;

      if (mv->Count == 0)
        { return nullptr; }

      return (FactAddressValue ^) mv[0];
     }

   /****************/
   /* FindAllFacts */
   /****************/
   List<FactAddressValue ^> ^ Environment::FindAllFacts(
     String ^ deftemplate)
     {
      return FindAllFacts("?f",deftemplate,"TRUE");
     }

   List<FactAddressValue ^> ^ Environment::FindAllFacts(
     String ^ variable,
     String ^ deftemplate,
     String ^ condition)
     { 
      if (variable == nullptr)
        { throw gcnew System::ArgumentNullException("variable"); }
        
      if (deftemplate == nullptr)
        { throw gcnew System::ArgumentNullException("deftemplate"); }

      if (condition == nullptr)
        { throw gcnew System::ArgumentNullException("condition"); }

      String ^ query = "(find-all-facts " + 
                         "((" + variable + " " + deftemplate + ")) " + condition + ")";

      PrimitiveValue ^ pv = Eval(query);

      if (! pv->IsMultifield())
        { return nullptr; }

      MultifieldValue ^ mv = (MultifieldValue ^) pv;
     
      List<FactAddressValue ^> ^ rv = gcnew List<FactAddressValue ^>(mv->Count);
      
      for each (FactAddressValue ^ theValue in mv)
        { rv->Add(theValue); }

      return rv;
     }
     
   /****************/
   /* FindInstance */
   /****************/
   InstanceAddressValue ^ Environment::FindInstance(
     String ^ defclass)
     {
      return FindInstance("?i",defclass,"TRUE");
     }

   InstanceAddressValue ^ Environment::FindInstance(
     String ^ variable,
     String ^ defclass,
     String ^ condition)
     {       
      if (variable == nullptr)
        { throw gcnew System::ArgumentNullException("variable"); }
        
      if (defclass == nullptr)
        { throw gcnew System::ArgumentNullException("defclass"); }

      if (condition == nullptr)
        { throw gcnew System::ArgumentNullException("condition"); }

      String ^ query = "(find-instance " + 
                         "((" + variable + " " + defclass + ")) " + condition + ")";

      PrimitiveValue ^ pv = Eval(query);

      if (! pv->IsMultifield())
        { return nullptr; }

      MultifieldValue ^ mv = (MultifieldValue ^) pv;

      if (mv->Count == 0)
        { return nullptr; }

      return ((InstanceNameValue ^) mv[0])->GetInstance(this);
     }

   /********************/
   /* FindAllInstances */
   /********************/
   List<InstanceAddressValue ^> ^ Environment::FindAllInstances(
     String ^ defclass)
     {
      return FindAllInstances("?i",defclass,"TRUE");
     }

   List<InstanceAddressValue ^> ^ Environment::FindAllInstances(
     String ^ variable,
     String ^ defclass,
     String ^ condition)
     {    
      if (variable == nullptr)
        { throw gcnew System::ArgumentNullException("variable"); }
        
      if (defclass == nullptr)
        { throw gcnew System::ArgumentNullException("defclass"); }

      if (condition == nullptr)
        { throw gcnew System::ArgumentNullException("condition"); }

      String ^ query = "(find-all-instances " + 
                         "((" + variable + " " + defclass + ")) " + condition + ")";

      PrimitiveValue ^ pv = Eval(query);

      if (! pv->IsMultifield())
        { return nullptr; }

      MultifieldValue ^ mv = (MultifieldValue ^) pv;
     
      List<InstanceAddressValue ^> ^ rv = gcnew List<InstanceAddressValue ^>(mv->Count);
      
      for each (InstanceNameValue ^ theValue in mv)
        { rv->Add(theValue->GetInstance(this)); }

      return rv;
     }

   /********/
   /* Eval */
   /********/
   PrimitiveValue ^ Environment::Eval(
     String ^ evalString)
     {    
      if (evalString == nullptr)
        { throw gcnew System::ArgumentNullException("evalString"); }
        
      PrimitiveValue ^ rv;
      array<Byte>^ ebEvalString = Encoding::UTF8->GetBytes(evalString);

      CaptureRouter ^ commandCapture = CaptureStart();

      try
        {
         if (ebEvalString->Length)
           {
            pin_ptr<Byte> pbEvalString = &ebEvalString[0];
            rv = DataObjectToPrimitiveValue(m_Env->Eval((char *) pbEvalString));
           }
         else
           { rv = gcnew VoidValue(); }
        }
      catch (Exception ^)
        {
        }

      CaptureEnd(commandCapture,true);

      return rv;
     }
 
   /*********/
   /* Watch */
   /*********/
   void Environment::Watch(
     String ^ watchItem)
     {
      if (watchItem == nullptr)
        { throw gcnew System::ArgumentNullException("watchItem"); }

      array<Byte>^ ebItem = Encoding::UTF8->GetBytes(watchItem);
      if (ebItem->Length)
        {
         pin_ptr<Byte> pbItem = &ebItem[0];
         if (! m_Env->Watch((char *) pbItem))
           { 
            throw gcnew System::ArgumentException(
               "Watch item '"+ watchItem + "' is invalid.",
               "watchItem");
           }
        }
     }
     
   /***********/
   /* Unwatch */
   /***********/
   void Environment::Unwatch(
     String ^ watchItem)
     {
      if (watchItem == nullptr)
        { throw gcnew System::ArgumentNullException("watchItem"); }

      array<Byte>^ ebItem = Encoding::UTF8->GetBytes(watchItem);
      if (ebItem->Length)
        {
         pin_ptr<Byte> pbItem = &ebItem[0];
         if (! m_Env->Unwatch((char *) pbItem))
           {
            throw gcnew System::ArgumentException(
               "Watch item '"+ watchItem + "' is invalid.",
               "watchItem");
           }
        }
     }
      
   /****************/
   /* GetWatchItem */
   /****************/
   bool Environment::GetWatchItem(
     String ^ watchItem)
     {
      if (watchItem == nullptr)
        { throw gcnew System::ArgumentNullException("watchItem"); }

      if (watchItem->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","watchItem"); }

      array<Byte>^ ebItem = Encoding::UTF8->GetBytes(watchItem);
      if (ebItem->Length)
        {
         pin_ptr<Byte> pbItem = &ebItem[0];
         
         if (! m_Env->ValidWatchItem((char *) pbItem))
           {
            throw gcnew System::ArgumentException(
               "Watch item '"+ watchItem + "' is invalid.",
               "watchItem");
           }

         return m_Env->GetWatchItem((char *) pbItem);
        }

      return false;
     }
    
   /****************/
   /* SetWatchItem */
   /****************/
   void Environment::SetWatchItem(
     String ^ watchItem,
     bool newValue)
     {
      if (watchItem == nullptr)
        { throw gcnew System::ArgumentNullException("watchItem"); }

      if (watchItem->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","watchItem"); }

      array<Byte>^ ebItem = Encoding::UTF8->GetBytes(watchItem);
      if (ebItem->Length)
        {
         pin_ptr<Byte> pbItem = &ebItem[0];

         if (! m_Env->ValidWatchItem((char *) pbItem))
           {
            throw gcnew System::ArgumentException(
               "Watch item '"+ watchItem + "' is invalid.",
               "watchItem");
           }

         m_Env->SetWatchItem((char *) pbItem,newValue);
        }
     }

   /*******************/
   /* AddUserFunction */
   /*******************/
   void Environment::AddUserFunction(
	  String ^ functionName,
	  UserFunction ^ theFunction)
     { 
      AddUserFunction(functionName,nullptr,0,UserFunction::UNBOUNDED,nullptr,theFunction);
     }

   /*******************/
   /* AddUserFunction */
   /*******************/
   void Environment::AddUserFunction(
	  String ^ functionName,
      String ^ returnTypes,
      unsigned short minArgs,
      unsigned short maxArgs,
      String ^ argTypes,
	  UserFunction ^ callback)
     { 
      bool rv;

      if (functionName == nullptr)
        { throw gcnew System::ArgumentNullException("functionName"); }
      
      if (functionName->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","functionName"); }

      if (callback == nullptr)
        { throw gcnew System::ArgumentNullException("callback"); }

      char *cFunctionName = NULL;
      char *cReturnTypes = NULL;
      char *cArgTypes = NULL;
      array<Byte>^ ebFunctionName = Encoding::UTF8->GetBytes(functionName);

      if (maxArgs == UserFunction::UNBOUNDED)
        { maxArgs = CLIPSCPPUserFunction::UNBOUNDED; }

      if (ebFunctionName->Length)
        {
         pin_ptr<Byte> pbFunctionName = &ebFunctionName[0];
         cFunctionName = (char *) pbFunctionName;
        }

     if (returnTypes != nullptr)
       {
        array<Byte>^ ebReturnTypes = Encoding::UTF8->GetBytes(returnTypes);
        if (ebReturnTypes->Length)
          {
           pin_ptr<Byte> pbReturnTypes = &ebReturnTypes[0];
           cReturnTypes = (char *) pbReturnTypes;
          }   
       }

     if (argTypes != nullptr)
       {
        array<Byte>^ ebArgTypes = Encoding::UTF8->GetBytes(argTypes);
        if (ebArgTypes->Length)
          {
           pin_ptr<Byte> pbArgTypes = &ebArgTypes[0];
           cArgTypes = (char *) pbArgTypes;
          }   
       }
        
      rv = m_Env->AddUserFunction(cFunctionName,cReturnTypes,minArgs,maxArgs,cArgTypes,
                                  (CLIPS::CLIPSCPPUserFunction *) callback->UserFunctionBridge());

      if (! rv)
        { 
         throw gcnew System::ArgumentException(
           "Function '" + functionName + "' already exists.","functionName"); 
        }
     }

   /**********************/
   /* RemoveUserFunction */
   /**********************/
   void Environment::RemoveUserFunction(
	 String ^ functionName)
     { 
      bool rv = false;

      if (functionName == nullptr)
        { throw gcnew System::ArgumentNullException("functionName"); }
              
      if (functionName->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","functionName"); }

      array<Byte>^ ebFunctionName = Encoding::UTF8->GetBytes(functionName);
      if (ebFunctionName->Length)
        {
         pin_ptr<Byte> pbFunctionName = &ebFunctionName[0];
         m_Env->RemoveUserFunction((char *) pbFunctionName);
        }

      if (! rv)
        { 
         throw gcnew System::ArgumentException(
           "Function '" + functionName + "' does not exist.","functionName"); 
        }
     }
     
   /*************/
   /* AddRouter */
   /*************/
   void Environment::AddRouter(
	 Router ^ theRouter)
     { 
      bool rv = false;

      if (theRouter == nullptr)
        { throw gcnew System::ArgumentNullException("theRouter"); }

      array<Byte>^ ebRouterName = Encoding::UTF8->GetBytes(theRouter->Name);
      if (ebRouterName->Length)
        {
         pin_ptr<Byte> pbRouterName = &ebRouterName[0];
         rv = m_Env->AddRouter((char *) pbRouterName,theRouter->Priority,(CLIPS::CLIPSCPPRouter *) theRouter->RouterBridge());
        }

      if (! rv)
        { 
         throw gcnew System::ArgumentException(
           "Router named '" + theRouter->Name + "' already exists."); 
        }

     }

   /****************/
   /* DeleteRouter */
   /****************/
   void Environment::DeleteRouter(
	 Router ^ theRouter)
     { 
      bool rv = false;

      if (theRouter == nullptr)
        { throw gcnew System::ArgumentNullException("theRouter"); }

      array<Byte>^ ebRouterName = Encoding::UTF8->GetBytes(theRouter->Name);
      if (ebRouterName->Length)
        {
         pin_ptr<Byte> pbRouterName = &ebRouterName[0];
         rv = m_Env->DeleteRouter((char *) pbRouterName);
        }

      if (! rv)
        { 
         throw gcnew System::ArgumentException(
           "Router named '" + theRouter->Name + "' does not exist."); 
        }
     }

   /*******************/
   /* ActivateRouter: */
   /*******************/
   void Environment::ActivateRouter(
     Router ^ theRouter)
     {
      bool rv = false;

      if (theRouter == nullptr)
        { throw gcnew System::ArgumentNullException("theRouter"); }

      array<Byte>^ ebRouterName = Encoding::UTF8->GetBytes(theRouter->Name);

      if (ebRouterName->Length)
        {
         pin_ptr<Byte> pbRouterName = &ebRouterName[0];
         rv = m_Env->ActivateRouter((char *) pbRouterName);
        }
  
      if (! rv)
        { 
         throw gcnew System::ArgumentException(
           "Router named '" + theRouter->Name + "' does not exist."); 
        }
     }

   /*********************/
   /* DeactivateRouter: */
   /*********************/
   void Environment::DeactivateRouter(
     Router ^ theRouter)
     {
      bool rv = false;

      if (theRouter == nullptr)
        { throw gcnew System::ArgumentNullException("theRouter"); }

      array<Byte>^ ebRouterName = Encoding::UTF8->GetBytes(theRouter->Name);

      if (ebRouterName->Length)
        {
         pin_ptr<Byte> pbRouterName = &ebRouterName[0];
         rv= m_Env->DeactivateRouter((char *) pbRouterName);
        }

      if (! rv)
        { 
         throw gcnew System::ArgumentException(
           "Router named '" + theRouter->Name + "' does not exist."); 
        }
     }

   /*********/
   /* Write */
   /*********/
   void Environment::Write(
     String ^ logicalName,
     String ^ printString)
     {
      if (logicalName == nullptr)
        { throw gcnew System::ArgumentNullException("logicalName"); }

      if (logicalName->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","logicalName"); }
        
      if (printString == nullptr)
        { throw gcnew System::ArgumentNullException("printString"); }

      array<Byte>^ ebLogicalName = Encoding::UTF8->GetBytes(logicalName);
      array<Byte>^ ebPrintString = Encoding::UTF8->GetBytes(printString);

      if ((ebPrintString->Length != 0) && (ebLogicalName->Length != 0))
        {
         pin_ptr<Byte> pbLogicalName = &ebLogicalName[0];
         pin_ptr<Byte> pbPrintString = &ebPrintString[0];

         if (! m_Env->PrintRouterExists((char *) pbLogicalName))
           {
            throw gcnew System::ArgumentException(
              "No print router for logicalName '" + logicalName +"' exists.");
           }

         m_Env->Write((char *) pbLogicalName,(char *) pbPrintString);
        }
     }

   /*********/
   /* Write */
   /*********/
   void Environment::Write(
     String ^ printString)
     {
      if (printString == nullptr)
        { throw gcnew System::ArgumentNullException("printString"); }

      String ^ logicalName = Router::STDOUT;
      Write(logicalName,printString);
     }

   /*************/
   /* WriteLine */
   /*************/
   void Environment::WriteLine(
     String ^ logicalName,
     String ^ printString)
     {
      if (logicalName == nullptr)
        { throw gcnew System::ArgumentNullException("logicalName"); }

      if (logicalName->Length == 0)
        { throw gcnew System::ArgumentException("String cannot have zero length.","logicalName"); }
        
      if (printString == nullptr)
        { throw gcnew System::ArgumentNullException("printString"); }

      printString += "\n";

      array<Byte>^ ebLogicalName = Encoding::UTF8->GetBytes(logicalName);
      array<Byte>^ ebPrintString = Encoding::UTF8->GetBytes(printString);

      if ((ebPrintString->Length != 0) && (ebLogicalName->Length != 0))
        {
         pin_ptr<Byte> pbLogicalName = &ebLogicalName[0];
         pin_ptr<Byte> pbPrintString = &ebPrintString[0];

         if (! m_Env->PrintRouterExists((char *) pbLogicalName))
           {
            throw gcnew System::ArgumentException(
              "No print router for logicalName '" + logicalName +"' exists.");
           }

         m_Env->Write((char *) pbLogicalName,(char *) pbPrintString);
        }
     }

   /*************/
   /* WriteLine */
   /*************/
   void Environment::WriteLine(
     String ^ printString)
     {
      if (printString == nullptr)
        { throw gcnew System::ArgumentNullException("printString"); }

      String ^ logicalName = Router::STDOUT;

      WriteLine(logicalName,printString);
     }

   /***********************/
   /* AddPeriodicCallback */
   /***********************/
   void Environment::AddPeriodicCallback(
	  String ^ callbackName,
	  int priority,
	  PeriodicCallback ^ theCallback)
     { 
      array<Byte>^ ebCallbackName = Encoding::UTF8->GetBytes(callbackName);
      if (ebCallbackName->Length)
        {
         pin_ptr<Byte> pbCallbackName = &ebCallbackName[0];
         m_Env->AddPeriodicFunction((char *) pbCallbackName,priority,(CLIPS::CLIPSCPPPeriodicFunction *) theCallback->PeriodicCallbackBridge());
        }
      else
        { m_Env->AddPeriodicFunction("",priority,(CLIPS::CLIPSCPPPeriodicFunction *) theCallback->PeriodicCallbackBridge()); }
     }

   /**************************/
   /* RemovePeriodicCallback */
   /**************************/
   void Environment::RemovePeriodicCallback(
	  String ^ callbackName)
     { 
      array<Byte>^ ebCallbackName = Encoding::UTF8->GetBytes(callbackName);
      if (ebCallbackName->Length)
        {
         pin_ptr<Byte> pbCallbackName = &ebCallbackName[0];
         m_Env->DeleteRouter((char *) pbCallbackName);
        }
      else
        { m_Env->RemovePeriodicFunction(""); }
     }

   /***************************/
   /* EnablePeriodicFunctions */
   /***************************/
   bool Environment::EnablePeriodicFunctions(
     bool value)
     {
      return m_Env->EnablePeriodicFunctions(value);
     }
     
   /**************************/
   /* SetParserErrorCallback */
   /**************************/
   void Environment::SetParserErrorCallback(
	  ParserErrorCallback ^ theCallback)
     {
      if (theCallback == nullptr)
        { m_Env->SetParserErrorCallback(NULL); }
      else
        { m_Env->SetParserErrorCallback((CLIPS::CLIPSCPPParserErrorFunction *) theCallback->ParserErrorCallbackBridge()); }
     }

   /********************/
   /* InputBufferCount */
   /********************/
   size_t Environment::InputBufferCount()
     { 
      return m_Env->InputBufferCount();
     }   
   
   /*******************/
   /* getInputBuffer: */
   /*******************/
   String ^ Environment::GetInputBuffer()
     {
      const char *inputBuffer;

      inputBuffer = m_Env->GetInputBuffer();
      if (inputBuffer == NULL)
        { return gcnew String(""); }
      
      return gcnew String(inputBuffer,0, (int) strlen(inputBuffer), UTF8Encoding::UTF8);
     }

   /*******************/
   /* setInputBuffer: */
   /*******************/
   void Environment::SetInputBuffer(
     String ^ commandString)
     {
      array<Byte>^ ebCommandString = Encoding::UTF8->GetBytes(commandString);    

      if (ebCommandString->Length)
        {  
         pin_ptr<Byte> pbCommandString = &ebCommandString[0];
         m_Env->SetInputBuffer((char *) pbCommandString);
        }
      else
        { m_Env->SetInputBuffer(""); }   
     }

   /*******************************/
   /* InputBufferContainsCommand: */
   /*******************************/
   bool Environment::InputBufferContainsCommand()
     {
      return m_Env->InputBufferContainsCommand();
     }
   
   /********************/
   /* AppendToDribble: */
   /********************/
   void Environment::AppendToDribble(
     String ^ commandString)
     {
      array<Byte>^ ebCommandString = Encoding::UTF8->GetBytes(commandString);    

      if (ebCommandString->Length)
        {  
         pin_ptr<Byte> pbCommandString = &ebCommandString[0];
         m_Env->AppendToDribble((char *) pbCommandString);
        }
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


   /***************************/
   /* SetHaltCommandLoopBatch */
   /***************************/
   void Environment::SetHaltCommandLoopBatch(bool value)
     { m_Env->SetHaltCommandLoopBatch(value); }

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

   /********************/
   /* ChangeDirectory: */
   /********************/
   bool Environment::ChangeDirectory(
     String ^ directoryString)
     {
      array<Byte>^ ebDirectoryString = Encoding::UTF8->GetBytes(directoryString);    
      int rv;

      if (ebDirectoryString->Length)
        {  
         pin_ptr<Byte> pbDirectoryString = &ebDirectoryString[0];
         rv = m_Env->ChangeDirectory((char *) pbDirectoryString);
        }
      else
        { rv =  m_Env->ChangeDirectory(""); }   

      if (rv == 0) return false;
      else return true;
     }
   
   /*****************/
   /* GetFactScopes */
   /*****************/
   Dictionary<unsigned long long,BitArray ^> ^ Environment::GetFactScopes()
     {
      Dictionary<unsigned long long,BitArray ^> ^ scopes;
      std::unordered_map<unsigned long long,std::vector<bool>> theCPPScopes;
      std::unordered_map<unsigned long long,std::vector<bool>>::iterator it;

      m_Env->GetFactScopes(theCPPScopes);

      scopes = gcnew Dictionary<unsigned long long,BitArray ^>();

      for (it = theCPPScopes.begin(); it != theCPPScopes.end(); it++)
        {
         unsigned long long key = it->first;
         std::vector<bool> value = it->second;
         size_t i;

         BitArray ^ theArray = gcnew BitArray((int) it->second.size());

         for (i = 0; i < it->second.size(); i++)
           {
            if (it->second[i])
              { theArray->Set((int) i,true); }
           }

         scopes->Add(it->first,theArray);
        }

      return scopes;
     }

   /***************/
   /* GetFactList */
   /***************/
   List<FactInstance ^> ^ Environment::GetFactList()
     {
      List<FactInstance ^> ^ theList;
      std::vector<CLIPSCPPFactInstance> *v = m_Env->GetFactList();
      size_t i, j;

      theList = gcnew List<FactInstance ^>();

      for (i = 0; i < v->size(); i++)
        {
         const char *theCName, *theCRelationName;
         List<SlotValue ^> ^ theSlots = gcnew List<SlotValue ^>();
         unsigned long long theTypeAddress;
         std::vector<CLIPSCPPSlotValue> *theCPPSlots;

         theTypeAddress = v->at(i).GetTypeAddress();
         theCName = v->at(i).GetName()->c_str();
         theCRelationName = v->at(i).GetRelationName()->c_str();
         theCPPSlots = v->at(i).GetSlotValues();

         for (j = 0; j < theCPPSlots->size(); j++)
           {
            bool isDefault;
            const char *theCSlotName, *theCSlotValue;

            theCSlotName = theCPPSlots->at(j).GetSlotName()->c_str();
            theCSlotValue = theCPPSlots->at(j).GetSlotValue()->c_str();
            
            isDefault = theCPPSlots->at(j).IsDefault();

            theSlots->Add(gcnew SlotValue(CharStarToString(theCSlotName),
                                          CharStarToString(theCSlotValue),
                                          isDefault));
           }

         theList->Add(gcnew FactInstance(theTypeAddress,
                                         CharStarToString(theCName),
                                         CharStarToString(theCRelationName),
                                         theSlots));
        }

      delete v;

      return theList;
     }

   /*********************/
   /* GetInstanceScopes */
   /*********************/
   Dictionary<unsigned long long,BitArray ^> ^ Environment::GetInstanceScopes()
     {
      Dictionary<unsigned long long,BitArray ^> ^ scopes;
      std::unordered_map<unsigned long long,std::vector<bool>> theCPPScopes;
      std::unordered_map<unsigned long long,std::vector<bool>>::iterator it;

      m_Env->GetInstanceScopes(theCPPScopes);

      scopes = gcnew Dictionary<unsigned long long,BitArray ^>();

      for (it = theCPPScopes.begin(); it != theCPPScopes.end(); it++)
        {
         unsigned long long key = it->first;
         std::vector<bool> value = it->second;
         size_t i;

         BitArray ^ theArray = gcnew BitArray((int) it->second.size());

         for (i = 0; i < it->second.size(); i++)
           {
            if (it->second[i])
              { theArray->Set((int) i,true); }
           }

         scopes->Add(it->first,theArray);
        }

      return scopes;
     }

   /*******************/
   /* GetInstanceList */
   /*******************/
   List<FactInstance ^> ^ Environment::GetInstanceList()
     {
      List<FactInstance ^> ^ theList;
      std::vector<CLIPSCPPFactInstance> *v = m_Env->GetInstanceList();
      size_t i, j;

      theList = gcnew List<FactInstance ^>();

      for (i = 0; i < v->size(); i++)
        {
         const char *theCName, *theCRelationName;
         List<SlotValue ^> ^ theSlots = gcnew List<SlotValue ^>();
         unsigned long long theTypeAddress;
         std::vector<CLIPSCPPSlotValue> *theCPPSlots;

         theTypeAddress = v->at(i).GetTypeAddress();
         theCName = v->at(i).GetName()->c_str();
         theCRelationName = v->at(i).GetRelationName()->c_str();
         theCPPSlots = v->at(i).GetSlotValues();

         for (j = 0; j < theCPPSlots->size(); j++)
           {
            bool isDefault;
            const char *theCSlotName, *theCSlotValue;

            theCSlotName = theCPPSlots->at(j).GetSlotName()->c_str();
            theCSlotValue = theCPPSlots->at(j).GetSlotValue()->c_str();
            
            isDefault = theCPPSlots->at(j).IsDefault();

            theSlots->Add(gcnew SlotValue(CharStarToString(theCSlotName),
                                          CharStarToString(theCSlotValue),
                                          isDefault));
           }

         theList->Add(gcnew FactInstance(theTypeAddress,
                                         CharStarToString(theCName),
                                         CharStarToString(theCRelationName),
                                         theSlots));
        }

      delete v;

      return theList;
     }
 
   /*****************/
   /* GetModuleList */
   /*****************/
   List<Module ^> ^ Environment::GetModuleList()
     {
      List<Module ^> ^ theList;
      std::vector<CLIPSCPPModule> *v = m_Env->GetModuleList();
      size_t i;
      const char *theCString;

	  theList = gcnew List<Module ^>();

      for (i = 0; i < v->size(); i++)
        {
         theCString = v->at(i).GetModuleName()->c_str();
         theList->Add(gcnew Module(CharStarToString(theCString)));
        }

      delete v;

      return theList;
     }

   /*****************/
   /* GetFocusStack */
   /*****************/
   FocusStack ^ Environment::GetFocusStack()
     {
      CLIPSCPPFocusStack *theCPPStack = m_Env->GetFocusStack();
      CLIPSCPPFocus *theCPPFocus;
      FocusStack ^ theStack;
      size_t i;
      std::vector<CLIPSCPPFocus *> *v;
      std::string *theCPPString;
      const char *theCString;
      List<Focus ^> ^ theList;

      v = theCPPStack->GetStack();

      theList = gcnew List<Focus ^>;

      for (i = 0; i < v->size(); i++)
        {
         theCPPFocus = v->at(i);
         theCPPString = theCPPFocus->GetModuleName();
   	     theCString = theCPPString->c_str();
         theList->Add(gcnew Focus(CharStarToString(theCString)));
        }

      theStack = gcnew FocusStack(theList);

      delete theCPPStack;

      return theStack;
     }

   /*************/
   /* GetAgenda */
   /*************/
   Agenda ^ Environment::GetAgenda(
     String ^ moduleName)
     {
      array<Byte>^ ebModuleNameString = Encoding::UTF8->GetBytes(moduleName);
      pin_ptr<Byte> pbModuleNameString;
      CLIPSCPPActivation *theCPPActivation;
      CLIPSCPPAgenda *theCPPAgenda;
      Agenda ^ theAgenda;
      size_t i;
      std::vector<CLIPSCPPActivation *> *v;
      std::string *theCPPRuleName, *theCPPBasis;
      const char *theCRuleName, *theCBasis;
      List<Activation ^> ^ theList;

      if (ebModuleNameString->Length == 0) return nullptr;
      pbModuleNameString = &ebModuleNameString[0];

      theCPPAgenda = m_Env->GetAgenda((char *) pbModuleNameString);

      v = theCPPAgenda->GetActivations();

      theList = gcnew List<Activation ^>;

      for (i = 0; i < v->size(); i++)
        {
         theCPPActivation = v->at(i);
         theCPPRuleName = theCPPActivation->GetRuleName();
		 theCRuleName = theCPPRuleName->c_str();
         theCPPBasis = theCPPActivation->GetBasis();
		 theCBasis = theCPPBasis->c_str();
         theList->Add(gcnew Activation(CharStarToString(theCRuleName),theCPPActivation->GetSalience(),CharStarToString(theCBasis)));
        }

      theAgenda = gcnew Agenda(theList);

      delete theCPPAgenda;

      return theAgenda;    
     }

   Agenda ^ Environment::GetAgenda(
     Focus ^ theFocus)
     {
      return GetAgenda(theFocus->ModuleName);
     }
 
   bool Environment::GetAgendaChanged()
     {
      return m_Env->GetAgendaChanged();
     }

   void Environment::SetAgendaChanged(
     bool newValue)
     {
      m_Env->SetAgendaChanged(newValue);
     }

   bool Environment::GetFocusChanged()
     {
      return m_Env->GetFocusChanged();
     }

   void Environment::SetFocusChanged(
     bool newValue)
     {
      m_Env->SetFocusChanged(newValue);
     }

   bool Environment::GetFactListChanged()
     {
      return m_Env->GetFactListChanged();
     }

   void Environment::SetFactListChanged(
     bool newValue)
     {
      m_Env->SetFactListChanged(newValue);
     }

   bool Environment::GetInstancesChanged()
     {
      return m_Env->GetInstancesChanged();
     }

   void Environment::SetInstancesChanged(
     bool newValue)
     {
      m_Env->SetFactListChanged(newValue);
     }

   /************************/
   /* CallNextPrintRouter: */
   /************************/
   void Environment::CallNextPrintRouter(
     Router ^ theRouter,
     String ^ logName,
     String ^ printString)
     {
      DeactivateRouter(theRouter);
      Write(logName,printString);
      ActivateRouter(theRouter);
     }

   /***********************/
   /* FindInstanceByName: */
   /***********************/
   InstanceAddressValue ^ Environment::FindInstanceByName(
     String ^ theInstanceName)
     {
      array<Byte>^ ebInstanceName = Encoding::UTF8->GetBytes(theInstanceName);
      
      if (ebInstanceName->Length)
        {
         pin_ptr<Byte> pbInstanceName = &ebInstanceName[0];

         CLIPS::InstanceAddressValue *ins = m_Env->FindInstanceByName((char *) pbInstanceName);

         InstanceAddressValue ^ rv =  gcnew InstanceAddressValue(ins);

         delete ins;
         
         return rv;
        }

      return nullptr;
     }
  };
