#pragma once

#include "clipscpp.h"
#include "CLIPSNET_Environment.h"
#include <msclr\gcroot.h>

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   class CLIPSCPPRouterBridge;
   ref class Environment;

   /*##########################*/
   /* Router class declaration */
   /*##########################*/

   public ref class Router
     {
      public:
        Router();
        Router(String ^);
        ~Router();
        virtual bool Query(String ^ logicalName);
        virtual void Print(String ^ logicalName, String ^ printString);
        virtual int Getc(String ^ logicalName);
        virtual int Ungetc(String ^ logicalName,int theChar);
        CLIPSCPPRouterBridge * RouterBridge();

        property String ^ Name
          {
           String ^ get() { return routerName; };
          }

        static String ^STANDARD_OUTPUT = gcnew String(CLIPSCPPRouter::STANDARD_OUTPUT);
        static String ^STANDARD_INPUT = gcnew String(CLIPSCPPRouter::STANDARD_INPUT);
        static String ^WARNING = gcnew String(CLIPSCPPRouter::WARNING);
        static String ^ERROR = gcnew String(CLIPSCPPRouter::ERROR);

      protected:
        !Router();

      private:
        CLIPSCPPRouterBridge *m_RouterBridge;
        String ^ routerName;
  };

   /*##############################*/
   /* BaseRouter class declaration */
   /*##############################*/

   public ref class BaseRouter : Router
     {
      public:
        BaseRouter(CLIPSNET::Environment ^,array<String ^> ^);
        BaseRouter(CLIPSNET::Environment ^,array<String ^> ^,int);
        BaseRouter(CLIPSNET::Environment ^,array<String ^> ^,int,String ^);
        ~BaseRouter();
        virtual bool Query(String ^ logicalName) override;

        property int Priority
          {
           int get() { return priority; };
          }

      protected:
        !BaseRouter();
        CLIPSNET::Environment ^ clips;

      private:
        static int BaseRouterNameIndex = 0;
        int priority;
        array<String ^> ^ queryNames;
     };

   /*#################################*/
   /* CaptureRouter class declaration */
   /*#################################*/

   public ref class CaptureRouter : BaseRouter
     {
      public:
        CaptureRouter(CLIPSNET::Environment ^, array<String ^> ^);
        CaptureRouter(CLIPSNET::Environment ^, array<String ^> ^,bool);
        ~CaptureRouter();
        void Clear();
        virtual void Print(String ^ logicalName,String ^ printString) override;

        property String ^ Output
          {
           String ^ get() { return captureString; };
          }

      protected:
        !CaptureRouter();
      
      private:
        String ^ captureString;
        bool forwardOutput = false;
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