#pragma once

#include "clipscpp.h"
#include <msclr\gcroot.h>
#include "CLIPSNET_Values.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   class CLIPSCPPUserFunctionBridge;

   /*################################*/
   /* UserFunction class declaration */
   /*################################*/

   public ref class UserFunction
     {
      public:
        UserFunction();
        ~UserFunction();
        virtual PrimitiveValue ^ Evaluate(List<PrimitiveValue ^> ^);
        CLIPSCPPUserFunctionBridge * UserFunctionBridge();
        static unsigned short UNBOUNDED = USHRT_MAX;

      protected:
        !UserFunction();

      private:
        CLIPSCPPUserFunctionBridge *m_UserFunctionBridge;
  };

   /*########################################*/
   /* CLIPSCPPUserFunctionBridge declaration */
   /*########################################*/

   class CLIPSCPPUserFunctionBridge : private CLIPSCPPUserFunction
     {
      public:
        CLIPSCPPUserFunctionBridge();
        CLIPSCPPUserFunctionBridge(msclr::gcroot<UserFunction^>);
        ~CLIPSCPPUserFunctionBridge();

        DataObject Evaluate(CLIPSCPPEnv *,std::vector<DataObject>);

      private:
        msclr::gcroot<UserFunction^> m_UserFunction;
     };

  };