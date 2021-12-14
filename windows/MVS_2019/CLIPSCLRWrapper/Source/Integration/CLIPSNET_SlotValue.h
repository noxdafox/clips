#pragma once

#include "clipscpp.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*##########################*/
   /* Agenda class declaration */
   /*##########################*/

   public ref class SlotValue
     {
      public:
        SlotValue(String ^ slotName,String ^ contents,bool isDefault);

        virtual String ^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 

        property bool IsDefault
	       {
	        bool get() { return isDefault; };
            void set(bool value) { isDefault = value; }
	       }

        property String ^ SlotName
	       {
	        String ^ get() { return slotName; };
            void set(String ^ value) { slotName = value; }
	       }

        property String ^ Contents
	       {
	        String ^ get() { return contents; };
            void set(String ^ value) { contents = value; }
	       }


        ~SlotValue();

      protected:
        ! SlotValue();

      private:

        bool isDefault;
        String ^ slotName;
        String ^ contents;
     };
  };