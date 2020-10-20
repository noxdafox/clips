#pragma once

#include "clipscpp.h"
#include "CLIPSNET_SlotValue.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace CLIPS;
using namespace System::ComponentModel;

namespace CLIPSNET
  {
   /*################################*/
   /* FactInstance class declaration */
   /*################################*/

   public ref class FactInstance : INotifyPropertyChanged
     {
      public:
        FactInstance();
        FactInstance(unsigned long long,String ^,String ^,List<SlotValue ^> ^);
        ~FactInstance();

        property unsigned long long TypeAddress
	       {
            unsigned long long get() { return typeAddress; };
            void set(unsigned long long value) 
              {
               if (value != typeAddress)
                 {
                  typeAddress = value; 
                  OnPropertyChanged("TypeAddress");
                 }
              }
	       }
        
        property String ^ Name
	       {
            String ^ get() { return name; };
            void set(String ^ value) 
              { 
               if (value != name)
                 {
                  name = value;
                  OnPropertyChanged("Name");
                 } 
              }
	       }

        property String ^ RelationName
	       {
            String ^ get() { return relationName; };
            void set(String ^ value) 
              {
               if (value != relationName) 
                 {
                  relationName = value;
                  OnPropertyChanged("RelationName");
                 }
              }
	       }

        property int SlotCount
	       {
	        int get() { return slotValues->Count; };
	       }

        property SlotValue ^ default[int] 
          {
           SlotValue ^ get(int index) 
	          { return slotValues[index]; }
          }

        List<SlotValue ^> ^ GetSlotValues();

        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        
        bool SearchForString(String ^);

        virtual event PropertyChangedEventHandler ^ PropertyChanged;

      protected:
        !FactInstance();
        void OnPropertyChanged(String ^ name);

      private:
        unsigned long long typeAddress;
        String ^ name;
        String ^ relationName;
        List<SlotValue ^> ^ slotValues;
     };
  };