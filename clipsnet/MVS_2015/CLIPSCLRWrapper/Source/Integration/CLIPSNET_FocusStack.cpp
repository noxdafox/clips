
#include "CLIPSNET_FocusStack.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*##########################*/
   /* FocusStack class methods */
   /*##########################*/

   FocusStack::FocusStack() 
     {  
	   List<Focus ^> ^ theList;

      theList = gcnew List<Focus ^>;

      stack = theList;
     }

  FocusStack::FocusStack(List<Focus ^> ^ theList) 
     {  
      stack = theList;
     }

   FocusStack::~FocusStack()
     { this->!FocusStack(); }

   FocusStack::!FocusStack()
     { }

   List<Focus ^> ^ FocusStack::GetStack()
     { return this->stack; }

   int FocusStack::GetHashCode()
     {
      if (stack == nullptr) return 0;

      return stack->GetHashCode();
     }

   bool FocusStack::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }

      List<Focus ^> ^ list1 = this->GetStack();
      List<Focus ^> ^ list2 = ((FocusStack ^) obj)->GetStack();

      if (list1->Count != list2->Count) 
        { return false; }

      for (int i = 0; i < this->stack->Count; i++)
        {
         if (! list1[i]->Equals(list2[i]))
           { return false; }
	     }
    
      return true;
     }

   System::Collections::IEnumerator ^ FocusStack::GetEnumerator()
     { return gcnew FSEnumerator(stack); }

   /*######################*/
   /* FSEnumerator Methods */
   /*######################*/

   FSEnumerator::FSEnumerator(List<Focus ^> ^list)
     { 
      theList = list;
      cur = -1;
     }

   void FSEnumerator::Reset()
     {
      cur = -1;
     }

   bool FSEnumerator::MoveNext()
     {
      cur++;
      return (cur < theList->Count);
     }

  };