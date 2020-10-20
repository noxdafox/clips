
#include "CLIPSNET_ParserErrorCallback.h"

using namespace System;
using namespace System::Text;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*#################################################*/
   /* CLIPSCPPParserErrorCallbackBridge class methods */
   /*#################################################*/

   CLIPSCPPParserErrorCallbackBridge::CLIPSCPPParserErrorCallbackBridge() {}
   
   CLIPSCPPParserErrorCallbackBridge::CLIPSCPPParserErrorCallbackBridge(msclr::gcroot<ParserErrorCallback^> the_PEC) 
     { m_ParserErrorCallback = the_PEC; }
   
   CLIPSCPPParserErrorCallbackBridge::~CLIPSCPPParserErrorCallbackBridge() {}

   void CLIPSCPPParserErrorCallbackBridge::Callback(
     CLIPSCPPEnv *theCPPEnv,
     const char *fileName,
     const char *warningString,
     const char *errorString,
     long lineNumber)
     {
      String ^ sFileName;
      String ^ sWarningString;
      String ^ sErrorString;

      if (fileName == NULL)
        { sFileName = nullptr; }
      else
        { sFileName = gcnew String(fileName); }

      if (warningString == NULL)
        { sWarningString = nullptr; }
      else
        { sWarningString = gcnew String(warningString); }

      if (errorString == NULL)
        { sErrorString = nullptr; }
      else
        { sErrorString = gcnew String(errorString); }

      m_ParserErrorCallback->Callback(gcnew String(sFileName),
                                      gcnew String(sWarningString),
                                      gcnew String(sErrorString),lineNumber); 
     }

   /*###################################*/
   /* ParserErrorCallback class methods */
   /*###################################*/

   ParserErrorCallback::ParserErrorCallback() 
     { m_ParserErrorCallbackBridge = new CLIPSCPPParserErrorCallbackBridge(this); }

   ParserErrorCallback::~ParserErrorCallback()
     { this->!ParserErrorCallback(); }

   void ParserErrorCallback::Callback(
     String ^ fileName,
     String ^ warningString,
     String ^ errorString,
     long lineNumber)
     { }

   CLIPSCPPParserErrorCallbackBridge *ParserErrorCallback::ParserErrorCallbackBridge()
     { return m_ParserErrorCallbackBridge; }

   ParserErrorCallback::!ParserErrorCallback()
     { delete m_ParserErrorCallbackBridge; }
  };