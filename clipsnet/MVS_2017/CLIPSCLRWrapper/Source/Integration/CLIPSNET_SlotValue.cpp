
#include "CLIPSNET_SlotValue.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*##########################*/
   /* SlotValue class methods */
   /*##########################*/

  SlotValue::SlotValue(
     String ^ slotName,
     String ^ contents,
     bool isDefault) 
     {
      this->slotName = slotName;
      this->contents = contents;
      this->isDefault = isDefault;  
     }

   String ^ SlotValue::ToString()
     { 
      String ^ theString;

      theString = gcnew String("(");

      theString = theString->Concat(theString,slotName);
      theString = theString->Concat(theString," ");
      theString = theString->Concat(contents);
      theString = theString->Concat(theString,")");

	  return theString;
     }

   int SlotValue::GetHashCode()
     { 
      int value = 0;

      if (slotName != nullptr)
        { value += slotName->GetHashCode(); }
      
      if (contents != nullptr)
        { value += contents->GetHashCode(); }

      return value;
     }

   bool SlotValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      if (! this->isDefault.Equals(((SlotValue ^ ) obj)->IsDefault))
        { return false; }

      if (this->slotName == nullptr)
        { 
         if (((SlotValue ^) obj)->slotName != nullptr) 
           { return false; }
        }
      else 
        { 
         if (! this->slotName->Equals(((SlotValue ^) obj)->SlotName))
           { return false; }
        }

     if (this->contents == nullptr)
       {
        if (((SlotValue ^) obj)->Contents != nullptr) 
          { return false; }
       }
     else
       {
        if (! this->contents->Equals(((SlotValue ^) obj)->Contents))
          { return false; }
       }
       
      return true;
     }

   SlotValue::~SlotValue()
     { this->!SlotValue(); }

   SlotValue::!SlotValue()
     { }
  };