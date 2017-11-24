
#include "CLIPSNET_FactInstance.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*############################*/
   /* FactInstance class methods */
   /*############################*/

   FactInstance::FactInstance() 
     {  
	  List<SlotValue ^> ^ theList;

      theList = gcnew List<SlotValue ^>;

      slotValues = theList;
     }

  FactInstance::FactInstance(
     unsigned long long theTypeAddress,
     String ^ theName,
     String ^ theRelationName,
     List<SlotValue ^> ^ theList) 
     {  
      typeAddress = theTypeAddress;
      name = theName;
      relationName = theRelationName;
      slotValues = theList;
     }

   FactInstance::~FactInstance()
     { this->!FactInstance(); }

   FactInstance::!FactInstance()
     { }

   List<SlotValue ^> ^FactInstance::GetSlotValues()
     { return this->slotValues; }

   int FactInstance::GetHashCode()
     {
      if (slotValues == nullptr) return 0;

      return slotValues->GetHashCode();
     }

   bool FactInstance::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }

      List<SlotValue ^> ^ list1 = this->GetSlotValues();
      List<SlotValue ^> ^ list2 = ((FactInstance ^) obj)->GetSlotValues();

      if (list1->Count != list2->Count) 
        { return false; }

      for (int i = 0; i < this->slotValues->Count; i++)
        {
         if (! list1[i]->Equals(list2[i]))
           { return false; }
	     }
    
      return true;
     }

   void FactInstance::OnPropertyChanged(String ^ name)
     {
      PropertyChanged(this, gcnew PropertyChangedEventArgs(name));
     }
     
   /*******************/
   /* SearchForString */
   /*******************/
   bool FactInstance::SearchForString(
     String ^ searchString)
     {
      if (relationName->ToLower()->Contains(searchString->ToLower()))
        { return true; }

      if (name->ToLower()->Contains(searchString->ToLower()))
        { return true; }

      for (int i = 0; i < this->slotValues->Count; i++)
        {
         String ^ svString;

         svString = gcnew String(relationName);

         svString = svString->Concat(" ");
         svString = svString->Concat(this->slotValues[i]->SlotName);
         svString = svString->Concat(" ");
         svString = svString->Concat(this->slotValues[i]->Contents);

         if (svString->ToLower()->Contains(searchString->ToLower()))
           { return true; }
        }

      return false;
     }


   /*******************/
   /* searchForString */
   /*******************/
   /*
   public boolean searchForString(
     String searchString)
     {
      if (relationName.toLowerCase().contains(searchString.toLowerCase()))
        { return true; }
        
      if (name.toLowerCase().contains(searchString.toLowerCase()))
        { return true; }
        
      for (SlotValue theSV : slotValues)
        {
         String svString = relationName + " " + theSV.getSlotName() + " " + theSV.getSlotValue();

         if (svString.toLowerCase().contains(searchString.toLowerCase()))
           { return true; }
        }

      return false;
     }
   */
  };