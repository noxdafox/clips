#pragma once

#include "clipscpp.h"
#include <msclr\gcroot.h>

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   class CLIPSCPPPeriodicCallbackBridge;

   /*####################################*/
   /* PeriodicCallback class declaration */
   /*####################################*/

   public ref class PeriodicCallback
     {
      public:
        PeriodicCallback();
        ~PeriodicCallback();
        virtual void Callback(void);
        CLIPSCPPPeriodicCallbackBridge * PeriodicCallbackBridge();

      protected:
        !PeriodicCallback();

      private:
        CLIPSCPPPeriodicCallbackBridge *m_PeriodicCallbackBridge;
  };

   /*############################################*/
   /* CLIPSCPPPeriodicCallbackBridge declaration */
   /*############################################*/

   class CLIPSCPPPeriodicCallbackBridge : private CLIPSCPPPeriodicFunction
     {
	   public:
         CLIPSCPPPeriodicCallbackBridge();
         CLIPSCPPPeriodicCallbackBridge(msclr::gcroot<PeriodicCallback^>);
         ~CLIPSCPPPeriodicCallbackBridge();

		 void Callback(CLIPSCPPEnv *);

      private:
         msclr::gcroot<PeriodicCallback^> m_PeriodicCallback;
     };

  };