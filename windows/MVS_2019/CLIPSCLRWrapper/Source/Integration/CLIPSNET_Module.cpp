
#include "CLIPSNET_Module.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*######################*/
   /* Module class methods */
   /*######################*/

  Module::Module(
     String ^ moduleName) 
     {
      this->moduleName = moduleName;  
     }

   String ^ Module::ToString()
     { 
	   return moduleName;
     }

   int Module::GetHashCode()
     { 
      if (moduleName == nullptr) return 0;
      
      return moduleName->GetHashCode();
     }

   bool Module::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      if (this->moduleName == nullptr)
        { 
         if (((Module ^) obj)->ModuleName != nullptr) 
           { return false; }
        }
      else 
        { 
        if (! this->moduleName->Equals(((Module ^) obj)->ModuleName))
          { return false; }
        }

      return true;
     }

   Module::~Module()
     { this->!Module(); }

   Module::!Module()
     { }
  };