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
        Router(int);
        Router(String ^,int);
        ~Router();
        virtual bool Query(String ^ logicalName);
        virtual void Write(String ^ logicalName, String ^ printString);
        virtual int Read(String ^ logicalName);
        virtual int Unread(String ^ logicalName,int theChar);
        virtual void Exit(bool);
        CLIPSCPPRouterBridge * RouterBridge();

        property String ^ Name
          {
           String ^ get() { return routerName; };
          }
        
        property int Priority
	       {
            int get() { return priority; };
            void set(int value) { priority = value; }
	       }

        static String ^STDIN = gcnew String(CLIPSCPPRouter::STDIN);
        static String ^STDOUT = gcnew String(CLIPSCPPRouter::STDOUT);
        static String ^STDWRN = gcnew String(CLIPSCPPRouter::STDWRN);
        static String ^STDERR = gcnew String(CLIPSCPPRouter::STDERR);

      protected:
        !Router();

      private:
        static int RouterNameIndex = 0;
        int priority;
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

      protected:
        !BaseRouter();
        CLIPSNET::Environment ^ clips;

      private:
        static int BaseRouterNameIndex = 0;
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
        virtual void Write(String ^ logicalName,String ^ printString) override;

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
		 void Write(CLIPSCPPEnv *,const char *,const char *);
		 int Read(CLIPSCPPEnv *,const char *);
		 int Unread(CLIPSCPPEnv *,int,const char *);
		 void Exit(CLIPSCPPEnv *,bool);

      private:
         msclr::gcroot<Router^> m_Router;
     };

  };