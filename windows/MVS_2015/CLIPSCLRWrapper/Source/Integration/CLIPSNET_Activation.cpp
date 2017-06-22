
#include "CLIPSNET_Activation.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*##########################*/
   /* Activation class methods */
   /*##########################*/

  Activation::Activation(
     String ^ ruleName,
     int salience,
     String ^ basis) 
     {
      this->ruleName = ruleName;
      this->salience = salience;
      this->basis = basis;  
     }

   String ^ Activation::ToString()
     { 
      String ^ theString;

      theString = gcnew String("(");

      theString = theString->Concat(theString,salience);
      theString = theString->Concat(theString," ");
      theString = theString->Concat(ruleName);
      theString = theString->Concat(theString,": ");
      theString = theString->Concat(basis);

	   return theString;
     }

   int Activation::GetHashCode()
     { 
      int value = salience;

      if (ruleName != nullptr)
        { value += ruleName->GetHashCode(); }
      
      if (basis != nullptr)
        { value += basis->GetHashCode(); }

      return value;
     }

   bool Activation::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      if (! this->salience.Equals(((Activation ^ ) obj)->Salience))
        { return false; }

      if (this->ruleName == nullptr)
        { 
         if (((Activation ^) obj)->RuleName != nullptr) 
           { return false; }
        }
      else 
        { 
        if (! this->ruleName->Equals(((Activation ^) obj)->RuleName))
          { return false; }
        }

     if (this->basis == nullptr)
       {
        if (((Activation ^) obj)->Basis != nullptr) 
          { return false; }
       }
     else
       {
        if (! this->basis->Equals(((Activation ^) obj)->Basis))
          { return false; }
       }
       
      return true;
     }

   Activation::~Activation()
     { this->!Activation(); }

   Activation::!Activation()
     { }
  };