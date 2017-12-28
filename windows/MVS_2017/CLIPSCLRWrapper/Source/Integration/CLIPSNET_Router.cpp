
#include "CLIPSNET_Router.h"

using namespace System;
using namespace System::Text;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*###################*/
   /* Utility functions */
   /*###################*/
   /*
   String ^ CharStarToString(
     const char *theString)
     {
      if (theString == NULL)
        { return nullptr; }

      return gcnew String(theString,0, (int) strlen(theString), UTF8Encoding::UTF8);
     }
    */
   /*####################################*/
   /* CLIPSCPPRouterBridge class methods */
   /*####################################*/

   CLIPSCPPRouterBridge::CLIPSCPPRouterBridge() {}
 
   CLIPSCPPRouterBridge::CLIPSCPPRouterBridge(msclr::gcroot<Router^> the_Router) 
     { m_Router = the_Router; }

   CLIPSCPPRouterBridge::~CLIPSCPPRouterBridge() {}

   bool CLIPSCPPRouterBridge::Query(
     CLIPSCPPEnv *theCPPEnv,
     const char *logicalName)
     {
      String ^ cliLogicalName = Environment::CharStarToString(logicalName);
      
      return m_Router->Query(cliLogicalName);
     }

   void CLIPSCPPRouterBridge::Write(
     CLIPSCPPEnv *theCPPEnv,
     const char *logicalName,
     const char *printString)
     {
      String ^ cliLogicalName = Environment::CharStarToString(logicalName);
      String ^ cliPrintString = Environment::CharStarToString(printString);

      m_Router->Write(cliLogicalName,cliPrintString);
     }

   int CLIPSCPPRouterBridge::Read(
     CLIPSCPPEnv *theCPPEnv,
     const char *logicalName)
     {
      String ^ cliLogicalName = Environment::CharStarToString(logicalName);

      return m_Router->Read(cliLogicalName);
     }

   int CLIPSCPPRouterBridge::Unread(
     CLIPSCPPEnv *theCPPEnv,
	 int theChar,
     const char *logicalName)
     {
      String ^ cliLogicalName = Environment::CharStarToString(logicalName);

      return m_Router->Unread(cliLogicalName,theChar);
     }

   void CLIPSCPPRouterBridge::Exit(
     CLIPSCPPEnv *theCPPEnv,
     bool failure)
     {
      m_Router->Exit(failure);
     }

   /*######################*/
   /* Router class methods */
   /*######################*/

   Router::Router() : Router(gcnew String("Router" + RouterNameIndex++),0)
     { 
     }

   Router::Router(
     String ^ theRouterName) : Router(theRouterName,0)
     {
     }
     
   Router::Router(
     int thePriority) : Router(gcnew String("Router" + RouterNameIndex++),thePriority)
     { 
     }

   Router::Router(
     String ^ theRouterName,
     int thePriority)
     {
      m_RouterBridge = new CLIPSCPPRouterBridge(this);
      routerName = theRouterName;
      priority = thePriority;
     }

   Router::~Router()
     { this->!Router(); }

   bool Router::Query(String ^ logicalName)
     { return false; }

   void Router::Write(String ^ logicalName,String ^printString)
     { }

   int Router::Read(String ^ logicalName)
     { return -1; }

   int Router::Unread(String ^ logicalName,int theChar)
     { return -1; }

   void Router::Exit(bool failure)
     { }

   CLIPSCPPRouterBridge *Router::RouterBridge()
     { return m_RouterBridge; }

   Router::!Router()
     { delete m_RouterBridge; }

   /*##########################*/
   /* BaseRouter class methods */
   /*##########################*/
   
   BaseRouter::BaseRouter(
     CLIPSNET::Environment ^ theEnv,
     array<String ^> ^ theQueryNames) : BaseRouter(theEnv, theQueryNames, 0)
     {
     }

   BaseRouter::BaseRouter(
     CLIPSNET::Environment ^ theEnv,
     array<String ^> ^ theQueryNames,
     int thePriority) : BaseRouter(theEnv, theQueryNames, thePriority, 
                                   gcnew String("BaseRouter" + BaseRouterNameIndex++))
     {
     }

   BaseRouter::BaseRouter(
     CLIPSNET::Environment ^ theEnv,
     array<String ^> ^ theQueryNames,
     int thePriority,
     String ^ theRouterName) : Router(theRouterName,thePriority)
     {
      clips = theEnv;
      queryNames = theQueryNames;
      clips->AddRouter(this);
     }

   BaseRouter::~BaseRouter()
     {
      this->!BaseRouter();
     }

   BaseRouter::!BaseRouter()
     {
     }

   bool BaseRouter::Query(String ^ logicalName)
     {
      for each (String ^ name in queryNames)
        {
         if (name->Equals(logicalName))
           { return true; }
        }

      return false;
     }

   /*#############################*/
   /* CaptureRouter class methods */
   /*#############################*/
   CaptureRouter::CaptureRouter(
     CLIPSNET::Environment ^ theEnv,
     array<String ^> ^ theCaptureNames) : BaseRouter(theEnv,theCaptureNames,30)
     {
      captureString = gcnew String("");
     }

   CaptureRouter::CaptureRouter(
     CLIPSNET::Environment ^ theEnv,
     array<String ^> ^ theCaptureNames,
     bool shouldForwardOutput) : BaseRouter(theEnv, theCaptureNames, 30)
     {
      captureString = gcnew String("");
      forwardOutput = shouldForwardOutput;
     }

   CaptureRouter::~CaptureRouter()
     {
      this->!CaptureRouter();
     }

   CaptureRouter::!CaptureRouter()
     {
     }

   void CaptureRouter::Clear()
     {
      captureString = gcnew String("");
     }

   void CaptureRouter::Write(
     String ^ logicalName,
     String ^ printString)
     {
      captureString = captureString + printString;
      if (forwardOutput)
        {
         clips->CallNextPrintRouter(this,logicalName,printString);
        }
     }
  };