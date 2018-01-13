
#include "CLIPSNET_PeriodicCallback.h"

using namespace System;
using namespace System::Text;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*##############################################*/
   /* CLIPSCPPPeriodicCallbackBridge class methods */
   /*##############################################*/

   CLIPSCPPPeriodicCallbackBridge::CLIPSCPPPeriodicCallbackBridge() {}
   
   CLIPSCPPPeriodicCallbackBridge::CLIPSCPPPeriodicCallbackBridge(msclr::gcroot<PeriodicCallback^> the_PC) 
     { m_PeriodicCallback = the_PC; }
   
   CLIPSCPPPeriodicCallbackBridge::~CLIPSCPPPeriodicCallbackBridge() {}

   void CLIPSCPPPeriodicCallbackBridge::Callback(
     CLIPSCPPEnv *theCPPEnv)
     {
      m_PeriodicCallback->Callback();
     }

   /*################################*/
   /* PeriodicCallback class methods */
   /*################################*/

   PeriodicCallback::PeriodicCallback() 
     { m_PeriodicCallbackBridge = new CLIPSCPPPeriodicCallbackBridge(this); }

   PeriodicCallback::~PeriodicCallback()
     { this->!PeriodicCallback(); }

   void PeriodicCallback::Callback()
     { }

   CLIPSCPPPeriodicCallbackBridge *PeriodicCallback::PeriodicCallbackBridge()
     { return m_PeriodicCallbackBridge; }

   PeriodicCallback::!PeriodicCallback()
     { delete m_PeriodicCallbackBridge; }
  };