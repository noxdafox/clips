
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

   int CLIPSCPPRouterBridge::Query(
     CLIPSCPPEnv *theCPPEnv,
     const char *logicalName)
     {
      String ^ cliLogicalName = CharStarToString(logicalName);

      return m_Router->Query(cliLogicalName);
     }

   int CLIPSCPPRouterBridge::Print(
     CLIPSCPPEnv *theCPPEnv,
     const char *logicalName,
     const char *printString)
     {
      String ^ cliLogicalName = CharStarToString(logicalName);
      String ^ cliPrintString = CharStarToString(printString);

      m_Router->Print(cliLogicalName,cliPrintString);

	   return true;
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
     { m_RouterBridge = new CLIPSCPPRouterBridge(this); }

   Router::~Router()
     { this->!Router(); }

   bool Router::Query(String ^ logicalName)
     { return false; }

   void Router::Print(String ^ logicalName,String ^printString)
     { }

   int Router::Getc(String ^ logicalName)
     { return -1; }

   int Router::Ungetc(String ^ logicalName,int theChar)
     { return 0; }

   CLIPSCPPRouterBridge *Router::RouterBridge()
     { return m_RouterBridge; }

   Router::!Router()
     { delete m_RouterBridge; }
  };