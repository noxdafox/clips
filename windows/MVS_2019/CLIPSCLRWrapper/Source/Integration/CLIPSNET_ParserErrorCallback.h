#pragma once

#include "clipscpp.h"
#include <msclr\gcroot.h>

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   class CLIPSCPPParserErrorCallbackBridge;

   /*#######################################*/
   /* ParserErrorCallback class declaration */
   /*#######################################*/

   public ref class ParserErrorCallback
     {
      public:
        ParserErrorCallback();
        ~ParserErrorCallback();
        virtual void Callback(String ^,String ^,String ^,long);
        CLIPSCPPParserErrorCallbackBridge * ParserErrorCallbackBridge();

      protected:
        !ParserErrorCallback();

      private:
        CLIPSCPPParserErrorCallbackBridge *m_ParserErrorCallbackBridge;
  };

   /*###############################################*/
   /* CLIPSCPPParserErrorCallbackBridge declaration */
   /*###############################################*/

   class CLIPSCPPParserErrorCallbackBridge : private CLIPSCPPParserErrorFunction
     {
	   public:
         CLIPSCPPParserErrorCallbackBridge();
         CLIPSCPPParserErrorCallbackBridge(msclr::gcroot<ParserErrorCallback^>);
         ~CLIPSCPPParserErrorCallbackBridge();

		 void Callback(CLIPSCPPEnv *,const char *,const char *,const char *,long);

      private:
         msclr::gcroot<ParserErrorCallback^> m_ParserErrorCallback;
     };
  };