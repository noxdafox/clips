#pragma once

#include "clipscpp.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*##########################*/
   /* Agenda class declaration */
   /*##########################*/

   public ref class Activation
     {
      public:
        Activation(String ^ ruleName,int salience,String ^ basis);

        virtual String ^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 

        property int Salience
	       {
            int get() { return salience; };
            void set(int value) { salience = value; }
	       }

        property String ^ RuleName
	       {
            String ^ get() { return ruleName; };
            void set(String ^ value) { ruleName = value; }
	       }

        property String ^ Basis
	       {
            String ^ get() { return basis; };
            void set(String ^ value) { basis = value; }
	       }

        ~Activation();

      protected:
        ! Activation();

      private:

        int salience;
        String ^ ruleName;
        String ^ basis;
     };
  };