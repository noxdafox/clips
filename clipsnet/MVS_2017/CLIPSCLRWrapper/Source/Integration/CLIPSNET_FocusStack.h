#pragma once

#include "clipscpp.h"
#include "CLIPSNET_Focus.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*##############################*/
   /* FocusStack class declaration */
   /*##############################*/

   public ref class FocusStack: System::Collections::IEnumerable
     {
      public:
        FocusStack();
        FocusStack(List<Focus ^> ^);
        ~FocusStack();
        
        property int Count
	       {
	        int get() { return stack->Count; };
	       }

        property Focus ^ default[int] 
          {
           Focus ^ get(int index) 
	          { return stack[index]; }
          }

        List<Focus ^> ^ GetStack();

        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        
        virtual System::Collections::IEnumerator ^ GetEnumerator();

      protected:
        !FocusStack();

      private:
        List<Focus ^> ^ stack;
     };

   public ref class FSEnumerator : public System::Collections::IEnumerator // TBD Private
     {
      private:
        List<Focus ^> ^theList;
        int cur;

      public:
        FSEnumerator(List<Focus ^> ^ list);
        virtual property Object ^ Current
          {
           Object ^ get()
             {
              try 
                { return theList[cur]; }
              catch (IndexOutOfRangeException ^)
                { throw gcnew InvalidOperationException(); }
             }
          }
        virtual void Reset();
        virtual bool MoveNext();
     };

  };