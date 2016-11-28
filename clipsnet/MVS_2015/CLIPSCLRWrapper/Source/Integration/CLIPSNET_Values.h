#pragma once
#include "clipscpp.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace CLIPS;

namespace CLIPSNET
  {
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
        bool PrimitiveValue::IsInstanceAddress();
        bool PrimitiveValue::IsMultifield();
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
     	  virtual String^ ToString() override;
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
	     String ^ GetLexemeValue();
		  virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
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
	     String ^ GetStringValue();
        virtual String^ ToString() override;
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
	     String ^ GetSymbolValue();
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
	     String ^ GetInstanceNameValue();
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
        
        double GetFloatValue(); 
        long long GetIntegerValue(); 
        static operator long long ( NumberValue ^ val ) 
          { return val->lValue; }
        static operator double ( NumberValue ^ val ) 
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
	     virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;
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

        List<PrimitiveValue ^> ^ GetMultifieldValue();
 
	     virtual String^ ToString() override;
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
        FactAddressValue();
        FactAddressValue(CLIPS::FactAddressValue *);
        ~FactAddressValue();
        long long GetFactIndex();
        PrimitiveValue ^ GetFactSlot(String ^);

        CLIPS::FactAddressValue *GetFactAddressValue();
        virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;
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
	     InstanceAddressValue();
	     InstanceAddressValue(CLIPS::InstanceAddressValue *);
        ~InstanceAddressValue();
        String ^ GetInstanceName();
        PrimitiveValue ^ DirectGetSlot(String ^);

        CLIPS::InstanceAddressValue *GetInstanceAddressValue();
        virtual String^ ToString() override;
        virtual int GetHashCode() override;
        virtual bool Equals(Object ^ obj) override; 
        virtual CLIPSNETType CLIPSType() override;
	  };

   /*###########*/
   /* Functions */
   /*###########*/

   PrimitiveValue ^ DataObjectToPrimitiveValue (DataObject);
  };


