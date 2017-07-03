#pragma once

#include "clipscpp.h"

using namespace System;
//using namespace System::Collections;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*######################*/
   /* CLIPSException class */
   /*######################*/

   public ref class CLIPSException : System::Exception
     {
      public:
        CLIPSException();
        CLIPSException(String ^);
        ~CLIPSException();


      protected:
        !CLIPSException();
     };
  };