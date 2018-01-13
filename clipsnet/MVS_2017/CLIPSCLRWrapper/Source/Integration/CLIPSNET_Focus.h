#pragma once

#include "clipscpp.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*#########################*/
   /* Focus class declaration */
   /*#########################*/

   public ref class Focus
     {
      public:
        Focus(String ^ moduleName);
        virtual String ^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 

        property String ^ ModuleName
	       {
	        String ^ get() { return moduleName; };
           void set(String ^ value) { moduleName = value; }
	       }

        ~Focus();

      protected:
        ! Focus();

      private:

        String ^ moduleName;
     };
  };