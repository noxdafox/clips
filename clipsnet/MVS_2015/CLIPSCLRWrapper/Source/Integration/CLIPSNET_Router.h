#pragma once

#include "clipscpp.h"
#include <msclr\gcroot.h>

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   class CLIPSCPPRouterBridge;

   /*##########################*/
   /* Router class declaration */
   /*##########################*/

   public ref class Router
     {
      public:
        Router();
        ~Router();
       virtual bool Query(String ^ logicalName);
       virtual void Print(String ^ logicalName, String ^ printString);
       virtual int Getc(String ^ logicalName);
		virtual int Ungetc(String ^ logicalName,int theChar);
		CLIPSCPPRouterBridge * RouterBridge();

      const static String ^STANDARD_OUTPUT = gcnew String(CLIPSCPPRouter::STANDARD_OUTPUT);
      const static String ^STANDARD_INPUT = gcnew String(CLIPSCPPRouter::STANDARD_INPUT);
      const static String ^WARNING = gcnew String(CLIPSCPPRouter::WARNING);
      const static String ^ERROR = gcnew String(CLIPSCPPRouter::ERROR);
      const static String ^TRACE = gcnew String(CLIPSCPPRouter::TRACE);
      const static String ^DIALOG = gcnew String(CLIPSCPPRouter::DIALOG);
      const static String ^PROMPT = gcnew String(CLIPSCPPRouter::PROMPT);
      const static String ^DISPLAY = gcnew String(CLIPSCPPRouter::DISPLAY);

      protected:
        !Router();

      private:
        CLIPSCPPRouterBridge *m_RouterBridge;
  };

   /*##################################*/
   /* CLIPSCPPRouterBridge declaration */
   /*##################################*/

   class CLIPSCPPRouterBridge : private CLIPSCPPRouter
     {
	   public:
         CLIPSCPPRouterBridge();
         CLIPSCPPRouterBridge(msclr::gcroot<Router^>);
         ~CLIPSCPPRouterBridge();

		 bool Query(CLIPSCPPEnv *,const char *);
		 void Print(CLIPSCPPEnv *,const char *,const char *);
		 int Getc(CLIPSCPPEnv *,const char *);
		 int Ungetc(CLIPSCPPEnv *,int,const char *);

      private:
         msclr::gcroot<Router^> m_Router;
     };

  };