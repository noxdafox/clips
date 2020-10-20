#pragma once

#include "clipscpp.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*##########################*/
   /* Module class declaration */
   /*##########################*/

   public ref class Module
     {
      public:
        Module(String ^ moduleName);
        virtual String ^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 

        property String ^ ModuleName
	       {
	        String ^ get() { return moduleName; };
            void set(String ^ value) { moduleName = value; }
	       }

        ~Module();

      protected:
        ! Module();

      private:

        String ^ moduleName;
     };
  };