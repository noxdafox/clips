
#include "CLIPSNET_Agenda.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*######################*/
   /* Agenda class methods */
   /*######################*/

   Agenda::Agenda() 
     {  
	   List<Activation ^> ^ theList;

      theList = gcnew List<Activation ^>;

      activations = theList;
     }

  Agenda::Agenda(List<Activation ^> ^ theList) 
     {  
      activations = theList;
     }

   Agenda::~Agenda()
     { this->!Agenda(); }

   Agenda::!Agenda()
     { }

   List<Activation ^> ^Agenda::GetActivations()
     { return this->activations; }

   int Agenda::GetHashCode()
     {
      if (activations == nullptr) return 0;

      return activations->GetHashCode();
     }

   bool Agenda::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }

      List<Activation ^> ^ list1 = this->GetActivations();
      List<Activation ^> ^ list2 = ((Agenda ^) obj)->GetActivations();

      if (list1->Count != list2->Count) 
        { return false; }

      for (int i = 0; i < this->activations->Count; i++)
        {
         if (! list1[i]->Equals(list2[i]))
           { return false; }
	     }
    
      return true;
     }

  };