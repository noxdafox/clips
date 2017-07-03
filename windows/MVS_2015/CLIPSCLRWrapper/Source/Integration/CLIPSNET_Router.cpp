
#include "CLIPSNET_Router.h"

using namespace System;
using namespace System::Text;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*###################*/
   /* Utility functions */
   /*###################*/
   
   String ^ CharStarToString(
     const char *theString)
     {
      if (theString == NULL)
        { return nullptr; }

      return gcnew String(theString,0, (int) strlen(theString), UTF8Encoding::UTF8);
     }
    
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
      String ^ cliLogicalName = CharStarToString(logicalName);

      return m_Router->Query(cliLogicalName);
     }

   void CLIPSCPPRouterBridge::Print(
     CLIPSCPPEnv *theCPPEnv,
     const char *logicalName,
     const char *printString)
     {
      String ^ cliLogicalName = CharStarToString(logicalName);
      String ^ cliPrintString = CharStarToString(printString);

      m_Router->Print(cliLogicalName,cliPrintString);
     }

   int CLIPSCPPRouterBridge::Getc(
     CLIPSCPPEnv *theCPPEnv,
     const char *logicalName)
     {
      String ^ cliLogicalName = CharStarToString(logicalName);

      return m_Router->Getc(cliLogicalName);
     }

   int CLIPSCPPRouterBridge::Ungetc(
     CLIPSCPPEnv *theCPPEnv,
	 int theChar,
     const char *logicalName)
     {
      String ^ cliLogicalName = CharStarToString(logicalName);

      return m_Router->Ungetc(cliLogicalName,theChar);
     }

   /*######################*/
   /* Router class methods */
   /*######################*/

   Router::Router() 
     { 
      m_RouterBridge = new CLIPSCPPRouterBridge(this); 
     }

   Router::Router(
     String ^ theRouterName)
     {
      m_RouterBridge = new CLIPSCPPRouterBridge(this);
      routerName = theRouterName;
     }

   Router::~Router()
     { this->!Router(); }

   bool Router::Query(String ^ logicalName)
     { return false; }

   void Router::Print(String ^ logicalName,String ^printString)
     { }

   int Router::Getc(String ^ logicalName)
     { return -1; }

   int Router::Ungetc(String ^ logicalName,int theChar)
     { return -1; }

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
     String ^ theRouterName) : Router(theRouterName)
     {
      clips = theEnv;
      queryNames = theQueryNames;
      priority = thePriority;
      clips->AddRouter(theRouterName,priority,this);
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

   void CaptureRouter::Print(
     String ^ logicalName,
     String ^ printString)
     {
      //captureString = captureString->Concat(printString);
      captureString = captureString + printString;
      if (forwardOutput)
        {
         clips->CallNextPrintRouter(this,logicalName,printString);
        }
     }
  };