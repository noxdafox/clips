#pragma once
#include "clipscpp.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace CLIPS;

namespace CLIPSNET
  {
   ref class Environment;
   ref class InstanceAddressValue;

   public enum class CLIPSNETType 
     { FLOAT, 
       INTEGER, 
       SYMBOL, 
       STRING, 
       MULTIFIELD, 
       EXTERNAL_ADDRESS, 
       FACT_ADDRESS, 
       INSTANCE_ADDRESS, 
       INSTANCE_NAME, 
       VOID,
       UNKNOWN };

   /*##################################*/
   /* PrimitiveValue Class declaration */
   /*##################################*/
 
   public ref class PrimitiveValue abstract 
     {
      protected:
        PrimitiveValue();
        ~PrimitiveValue();

      public:
        virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType();
        bool PrimitiveValue::IsVoid();
        bool PrimitiveValue::IsLexeme();
        bool PrimitiveValue::IsSymbol();
        bool PrimitiveValue::IsString();
        bool PrimitiveValue::IsInstanceName();
        bool PrimitiveValue::IsNumber();
        bool PrimitiveValue::IsFloat();
        bool PrimitiveValue::IsInteger();
        bool PrimitiveValue::IsFactAddress();
        bool PrimitiveValue::IsInstance();
        bool PrimitiveValue::IsInstanceAddress();
        bool PrimitiveValue::IsMultifield();
        bool PrimitiveValue::IsExternalAddress();
     };

   /*################################*/
   /* PVEnumerator Class declaration */
   /*################################*/

   public ref class PVEnumerator : public System::Collections::IEnumerator
     {
      private:
        List<PrimitiveValue ^> ^theList;
        int cur;

      public:
        PVEnumerator(List<PrimitiveValue ^> ^ list);
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

   /*#############################*/
   /* VoidValue Class declaration */
   /*#############################*/

   public ref class VoidValue : PrimitiveValue
     {
      public:
        VoidValue();
        ~VoidValue();
        virtual String ^ ToString() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;
     };

   /*###############################*/
   /* LexemeValue Class declaration */
   /*###############################*/

   public ref class LexemeValue abstract : PrimitiveValue
     {
      private:
        String ^ value;

      protected:
        LexemeValue();
        LexemeValue(String ^ theString);
        ~LexemeValue();

      public:
        virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 

        property String ^ Value
          {
           String ^ get() { return value; };
          }
    };

   /*###############################*/
   /* StringValue Class declaration */
   /*###############################*/

   public ref class StringValue : LexemeValue
     {
      public:
        StringValue();
        StringValue(String ^ theString);
        ~StringValue();

      public:
        virtual String ^ ToString() override;
        virtual CLIPSNETType CLIPSType() override;
     };

   /*###############################*/
   /* SymbolValue Class declaration */
   /*###############################*/

   public ref class SymbolValue : LexemeValue
     {
      public:
        SymbolValue();
        SymbolValue(String ^ theString);
        ~SymbolValue();

      public:
        virtual String^ ToString() override;
        virtual CLIPSNETType CLIPSType() override;
     };

   /*#####################################*/
   /* InstanceNameValue Class declaration */
   /*#####################################*/

   public ref class InstanceNameValue : LexemeValue
     {
      public:
        InstanceNameValue();
        InstanceNameValue(String ^ theString);
        ~InstanceNameValue();

      public:
        CLIPSNET::InstanceAddressValue ^ GetInstance(CLIPSNET::Environment ^);
        virtual String^ ToString() override;
        virtual CLIPSNETType CLIPSType() override;
     };

   /*###############################*/
   /* NumberValue Class declaration */
   /*###############################*/

   public ref class NumberValue abstract : PrimitiveValue
     {
      private:
        double dValue;
        long long lValue;

      public:
        NumberValue();
        NumberValue(double theDouble);
        NumberValue(long long theLong);
        ~NumberValue();
        
        static operator long long (NumberValue ^ val) 
          { return val->lValue; }
 
        static operator double (NumberValue ^ val) 
          { return val->dValue; }
     };

   /*################################*/
   /* IntegerValue Class declaration */
   /*################################*/

   public ref class IntegerValue : NumberValue
     {
      public:
        IntegerValue();
        IntegerValue(long long theLong);
        IntegerValue(double theDouble);
        ~IntegerValue();

      public:
        virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;

        property long long Value
          {
           long long get() { return (long long) this; };
          }
     };

   /*###############################*/
   /* FloatValue Class declaration */
   /*###############################*/

   public ref class FloatValue : NumberValue
     {
      public:
        FloatValue();
        FloatValue(long long theLong);
        FloatValue(double theDouble);
        ~FloatValue();

      public:
        virtual String ^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;

        property double Value
          {
           double get() { return (double) this; };
          }
     };

   /*###################################*/
   /* MultifieldValue Class declaration */
   /*###################################*/

   public ref class MultifieldValue : PrimitiveValue, System::Collections::IEnumerable
     {
      private:
        List<PrimitiveValue ^> ^ listValue;

      public:
        MultifieldValue();
        MultifieldValue(List<PrimitiveValue ^> ^theList);
        ~MultifieldValue();
        
        property int Count
          {
           int get() { return listValue->Count; };
          }

        property PrimitiveValue ^ default[int] 
          {
           PrimitiveValue ^ get(int index) 
             { return listValue[index]; }
          }

        property List<PrimitiveValue ^> ^ Value
          {
           List<PrimitiveValue ^> ^ get() { return listValue; };
          }
 
        virtual String ^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual System::Collections::IEnumerator ^ GetEnumerator();
        virtual CLIPSNETType CLIPSType() override;
     };

   /*####################################*/
   /* FactAddressValue Class declaration */
   /*####################################*/

   public ref class FactAddressValue : PrimitiveValue
     {
      private:
        CLIPS::FactAddressValue *m_factAddressValue;

      protected:
        !FactAddressValue();

      public:
        FactAddressValue(CLIPS::FactAddressValue *);
        ~FactAddressValue();
        PrimitiveValue ^ GetSlotValue(String ^);

        virtual String ^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;

        property PrimitiveValue ^ default[String ^] 
          {
           PrimitiveValue ^ get(String ^ slotName) 
             { return GetSlotValue(slotName); }
          }

        property long long FactIndex
          {
           long long get() 
             { 
              if (m_factAddressValue != NULL)
                { return m_factAddressValue->GetFactIndex(); }

              return 0;
             };
          }

        property CLIPS::FactAddressValue * Value
          {
           CLIPS::FactAddressValue * get() { return m_factAddressValue; };
          }
        // TBD retain release
     };

   /*########################################*/
   /* InstanceAddressValue Class declaration */
   /*########################################*/

   public ref class InstanceAddressValue : PrimitiveValue
     {
      private:
        CLIPS::InstanceAddressValue *m_instanceAddressValue;

      protected:
        !InstanceAddressValue();

      public:
        InstanceAddressValue(CLIPS::InstanceAddressValue *);
        ~InstanceAddressValue();
        PrimitiveValue ^ GetSlotValue(String ^);

        virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;

        property String ^ InstanceName
          {
           String ^ get()
             { return gcnew String(m_instanceAddressValue->GetInstanceName()); }
          }

        property PrimitiveValue ^ default[String ^] 
          {
           PrimitiveValue ^ get(String ^ slotName) 
             { return GetSlotValue(slotName); }
          }

        property CLIPS::InstanceAddressValue * Value
          {
           CLIPS::InstanceAddressValue * get() { return m_instanceAddressValue; };
          }
        // TBD retain release
     };
     
   /*########################################*/
   /* ExternalAddressValue Class declaration */
   /*########################################*/

   public ref class ExternalAddressValue : PrimitiveValue
     {
      private:
        CLIPS::ExternalAddressValue *m_externalAddressValue;

      protected:
        !ExternalAddressValue();

      public:
        ExternalAddressValue(CLIPS::ExternalAddressValue *);
        ~ExternalAddressValue();

        virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;

        property CLIPS::ExternalAddressValue * Value
          {
           CLIPS::ExternalAddressValue * get() { return m_externalAddressValue; };
          }
        // TBD retain release
     };

   /*###########*/
   /* Functions */
   /*###########*/

   PrimitiveValue ^ DataObjectToPrimitiveValue (DataObject);
   DataObject PrimitiveValueToDataObject (PrimitiveValue ^ thePV);
  };


