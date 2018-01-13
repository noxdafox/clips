#pragma once

#include "clipscpp.h"
#include "CLIPSNET_Activation.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*##########################*/
   /* Agenda class declaration */
   /*##########################*/

   public ref class Agenda
     {
      public:
        Agenda();
        Agenda(List<Activation ^> ^);
        ~Agenda();
        

        property int Count
	       {
	        int get() { return activations->Count; };
	       }

        property Activation ^ default[int] 
          {
           Activation ^ get(int index) 
	          { return activations[index]; }
          }

        List<Activation ^> ^ GetActivations();

        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 

      protected:
        !Agenda();

      private:
        List<Activation ^> ^ activations;
     };


  };