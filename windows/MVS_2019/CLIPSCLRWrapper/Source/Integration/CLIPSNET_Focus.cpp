
#include "CLIPSNET_Focus.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*#####################*/
   /* Focus class methods */
   /*#####################*/

  Focus::Focus(
     String ^ moduleName) 
     {
      this->moduleName = moduleName;  
     }

   String ^ Focus::ToString()
     { 
	   return moduleName;
     }

   int Focus::GetHashCode()
     { 
      if (moduleName == nullptr) return 0;
      
      return moduleName->GetHashCode();
     }

   bool Focus::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      if (this->moduleName == nullptr)
        { 
         if (((Focus ^) obj)->ModuleName != nullptr) 
           { return false; }
        }
      else 
        { 
        if (! this->moduleName->Equals(((Focus ^) obj)->ModuleName))
          { return false; }
        }

      return true;
     }

   Focus::~Focus()
     { this->!Focus(); }

   Focus::!Focus()
     { }
  };