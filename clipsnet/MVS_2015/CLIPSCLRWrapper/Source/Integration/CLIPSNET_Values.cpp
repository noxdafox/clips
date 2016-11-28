
#include "CLIPSNET_Values.h"

#include "basetsd.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*###################*/
   /* Utility Functions */
   /*###################*/

   PrimitiveValue ^ SingleFieldToPrimitiveValue(
     CLIPSCPPType theType,
	  Value *theValue)
     {
	  std::string *theCPPString;
	  const char *theCString;
	  CLIPS::StringValue *theStringValue;
	  CLIPS::SymbolValue *theSymbolValue;
	  CLIPS::InstanceNameValue *theInstanceNameValue;
	  CLIPS::IntegerValue *theIntegerValue;
	  CLIPS::FloatValue *theFloatValue;
	  CLIPS::FactAddressValue *theFactAddressValue;
	  CLIPS::InstanceAddressValue *theInstanceAddressValue;
	  PrimitiveValue ^ rv = nullptr;

	  switch (theType)
	    {
		 case CPP_STRING_TYPE:
		   theStringValue = (CLIPS::StringValue *) theValue;
		   theCPPString = theStringValue->GetStringValue();
		   theCString = theCPPString->c_str();
		   rv = gcnew StringValue(gcnew String(theCString));
		   break;

		 case CPP_SYMBOL_TYPE:
		   theSymbolValue = (CLIPS::SymbolValue *) theValue;
		   theCPPString = theSymbolValue->GetSymbolValue();
		   theCString = theCPPString->c_str();
		   rv = gcnew SymbolValue(gcnew String(theCString));
		   break;

		 case CPP_INSTANCE_NAME_TYPE:
		   theInstanceNameValue = (CLIPS::InstanceNameValue *) theValue;
		   theCPPString = theInstanceNameValue->GetInstanceNameValue();
		   theCString = theCPPString->c_str();
		   rv = gcnew InstanceNameValue(gcnew String(theCString));
		   break;

		 case CPP_INTEGER_TYPE:
		   theIntegerValue = (CLIPS::IntegerValue *) theValue;
		   rv = gcnew IntegerValue(theIntegerValue->GetIntegerValue());
		   break;

		 case CPP_FLOAT_TYPE:
		   theFloatValue = (CLIPS::FloatValue *) theValue;
		   rv = gcnew FloatValue(theFloatValue->GetFloatValue());
		   break;

		 case CPP_FACT_ADDRESS_TYPE:
		   theFactAddressValue = (CLIPS::FactAddressValue *) theValue;
		   rv = gcnew FactAddressValue(theFactAddressValue);
		   break;

		 case CPP_INSTANCE_ADDRESS_TYPE:
		   theInstanceAddressValue = (CLIPS::InstanceAddressValue *) theValue;
		   rv = gcnew InstanceAddressValue(theInstanceAddressValue);
		   break;

		 case CPP_VOID_TYPE:
		   rv = gcnew VoidValue();
		   break;
		}

	  return rv;
	 }

   PrimitiveValue ^ DataObjectToPrimitiveValue (DataObject theDO)
     {
	  CLIPSCPPType theType = theDO.GetCLIPSType();
	  List<PrimitiveValue ^> ^ theList;
	  CLIPS::MultifieldValue *theMultifieldValue;
	  std::vector<Value *> *theMultifield;
	  Value *theValue;
	  PrimitiveValue ^ rv = nullptr;

     switch (theType)
	    {
		 case CPP_STRING_TYPE:
		 case CPP_SYMBOL_TYPE:
		 case CPP_INSTANCE_NAME_TYPE:
		 case CPP_INTEGER_TYPE:
		 case CPP_FLOAT_TYPE:
		 case CPP_FACT_ADDRESS_TYPE:
		 case CPP_INSTANCE_ADDRESS_TYPE:
		 case CPP_VOID_TYPE:
		   rv = SingleFieldToPrimitiveValue(theType,theDO.GetCLIPSValue());
		   break;

		 case CPP_MULTIFIELD_TYPE:
		   theList = gcnew List<PrimitiveValue ^>;
		   theMultifieldValue = (CLIPS::MultifieldValue *) theDO.GetCLIPSValue();
		   theMultifield = theMultifieldValue->GetMultifieldValue();	
		   for (std::vector<Value *>::size_type i = 0; i != theMultifield->size(); i++) 
           {
            theValue = theMultifield->at(i);
            theList->Add(SingleFieldToPrimitiveValue(theValue->GetCLIPSType(),theValue));
           }
		   rv = gcnew MultifieldValue(theList);
         break;
	    }

	  return rv;
	 }

   /*########################*/
   /* PrimitiveValue Methods */
   /*########################*/

   PrimitiveValue::PrimitiveValue()
     { }

   PrimitiveValue::~PrimitiveValue()
     { }
	 
   String ^ PrimitiveValue::ToString()
     { return gcnew String(""); }

   int PrimitiveValue::GetHashCode()
     { return 0; }
   
   bool PrimitiveValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      return (this == obj);
     }
     
   CLIPSNETType PrimitiveValue::CLIPSType()
     { return CLIPSNETType::UNKNOWN; }

   bool PrimitiveValue::IsVoid()
     { return (CLIPSType() == CLIPSNETType::VOID); }

   bool PrimitiveValue::IsSymbol()
     { return (CLIPSType() == CLIPSNETType::SYMBOL); }

   bool PrimitiveValue::IsString()
     { return (CLIPSType() == CLIPSNETType::STRING); }

   bool PrimitiveValue::IsInstanceName()
     { return (CLIPSType() == CLIPSNETType::INSTANCE_NAME); }

   bool PrimitiveValue::IsLexeme()
     { return (IsSymbol() || IsString() || IsInstanceName()); }

   bool PrimitiveValue::IsInteger()
     { return (CLIPSType() == CLIPSNETType::INTEGER); }

   bool PrimitiveValue::IsFloat()
     { return (CLIPSType() == CLIPSNETType::FLOAT); }

   bool PrimitiveValue::IsNumber()
     { return (IsInteger() || IsFloat()); }

   bool PrimitiveValue::IsFactAddress()
     { return (CLIPSType() == CLIPSNETType::FACT_ADDRESS); }

   bool PrimitiveValue::IsInstanceAddress()
     { return (CLIPSType() == CLIPSNETType::INSTANCE_ADDRESS); }

   bool PrimitiveValue::IsMultifield()
     { return (CLIPSType() == CLIPSNETType::MULTIFIELD); }

   /*######################*/
   /* PVEnumerator Methods */
   /*######################*/

   PVEnumerator::PVEnumerator(List<PrimitiveValue ^> ^list)
     { 
      theList = list;
      cur = -1;
     }

   void PVEnumerator::Reset()
     {
      cur = -1;
     }

   bool PVEnumerator::MoveNext()
     {
      cur++;
      return (cur < theList->Count);
     }

   /*###################*/
   /* VoidValue Methods */
   /*###################*/

   VoidValue::VoidValue() 
     { }

   VoidValue::~VoidValue()
     { }

   String ^ VoidValue::ToString()
     { return gcnew String("");  }
  
   bool VoidValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      return true;
     }

   CLIPSNETType VoidValue::CLIPSType()
     { return CLIPSNETType::VOID; }

   /*#####################*/
   /* LexemeValue Methods */
   /*#####################*/

   LexemeValue::LexemeValue() : value ( nullptr)
     { }

   LexemeValue::LexemeValue(String ^ theString) : value (theString)
     { }

   LexemeValue::~LexemeValue()
     { }

   String ^ LexemeValue::GetLexemeValue()
     { return this->value; }

   String ^ LexemeValue::ToString()
     {
	  if (value != nullptr)
	    { return value->ToString(); }
		
	  return gcnew String(""); 
	 }

   int LexemeValue::GetHashCode()
     {
      return value->GetHashCode();
     }

   bool LexemeValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      return value->Equals(((LexemeValue ^) obj)->GetLexemeValue());
     }

   /*#####################*/
   /* StringValue Methods */
   /*#####################*/

   StringValue::StringValue() : LexemeValue(gcnew String(""))
     { }

   StringValue::StringValue(String ^ theString) : LexemeValue (theString)
     { }

   StringValue::~StringValue()
     { }

   String ^ StringValue::GetStringValue()
     { return this->GetLexemeValue(); }

   String ^ StringValue::ToString()
     { return gcnew String("\"" + LexemeValue::ToString() +"\""); }
     
   CLIPSNETType StringValue::CLIPSType()
     { return CLIPSNETType::STRING; }

   /*#####################*/
   /* SymbolValue Methods */
   /*#####################*/

   SymbolValue::SymbolValue() : LexemeValue(gcnew String(""))
     { }

   SymbolValue::SymbolValue(String ^ theString) : LexemeValue (theString)
     { }

   SymbolValue::~SymbolValue()
     { }

   String ^ SymbolValue::GetSymbolValue()
     { return this->GetLexemeValue(); }

   String ^ SymbolValue::ToString()
     { return gcnew String(LexemeValue::ToString()); }
     
   CLIPSNETType SymbolValue::CLIPSType()
     { return CLIPSNETType::SYMBOL; }

   /*###########################*/
   /* InstanceNameValue Methods */
   /*###########################*/

   InstanceNameValue::InstanceNameValue() : LexemeValue(gcnew String(""))
     { }

   InstanceNameValue::InstanceNameValue(String ^ theString) : LexemeValue (theString)
     { }

   InstanceNameValue::~InstanceNameValue()
     { }

   String ^ InstanceNameValue::GetInstanceNameValue()
     { return this->GetLexemeValue(); }

   String ^ InstanceNameValue::ToString()
     { return gcnew String("[" + LexemeValue::ToString() + "]"); }
     
   CLIPSNETType InstanceNameValue::CLIPSType()
     { return CLIPSNETType::INSTANCE_NAME; }

   /*#####################*/
   /* NumberValue Methods */
   /*#####################*/

   NumberValue::NumberValue() 
     {
      dValue = 0.0;
      lValue = 0;
     }

   NumberValue::NumberValue(double theDouble)
     {
      dValue = theDouble;
      lValue = (long long) theDouble;
     }

   NumberValue::NumberValue(long long theLong)
     {
      dValue = (double) theLong;
      lValue = theLong;
     }

   NumberValue::~NumberValue()
     { }

   double NumberValue::GetFloatValue()
     { return this->dValue; }

   long long NumberValue::GetIntegerValue()
     { return this->lValue; }

   /*######################*/
   /* IntegerValue Methods */
   /*######################*/

   IntegerValue::IntegerValue() : NumberValue()
     { }

   IntegerValue::IntegerValue(double theDouble) : NumberValue (theDouble)
     { }

   IntegerValue::IntegerValue(long long theLong) : NumberValue (theLong)
     { }

   IntegerValue::~IntegerValue()
     { }
 
   String ^ IntegerValue::ToString()
     { return this->GetIntegerValue().ToString(); }

   int IntegerValue::GetHashCode()
     { return GetIntegerValue().GetHashCode(); }

   bool IntegerValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      return this->GetIntegerValue().Equals(((IntegerValue ^) obj)->GetIntegerValue());
     }
     
   CLIPSNETType IntegerValue::CLIPSType()
     { return CLIPSNETType::INTEGER; }

   /*####################*/
   /* FloatValue Methods */
   /*####################*/

   FloatValue::FloatValue() : NumberValue()
     { }

   FloatValue::FloatValue(double theDouble) : NumberValue (theDouble)
     { }

   FloatValue::FloatValue(long long theLong) : NumberValue (theLong)
     { }

   FloatValue::~FloatValue()
     { }
 
   String ^ FloatValue::ToString()
     { return this->GetFloatValue().ToString(); }

   int FloatValue::GetHashCode()
     { return GetFloatValue().GetHashCode(); }

   bool FloatValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      return this->GetFloatValue().Equals(((FloatValue ^) obj)->GetFloatValue());
     }
     
   CLIPSNETType FloatValue::CLIPSType()
     { return CLIPSNETType::FLOAT; }

   /*#########################*/
   /* MultifieldValue Methods */
   /*#########################*/

   MultifieldValue::MultifieldValue() 
     { this->listValue = nullptr; }
 
   MultifieldValue::MultifieldValue(List<PrimitiveValue ^> ^ theList)
     { this->listValue = theList; }

   MultifieldValue::~MultifieldValue()
     { }

   String ^ MultifieldValue::ToString()
     {
      bool first = true;
      String ^ theString;

      theString = gcnew String("(");

      for (int i = 0; i < this->listValue->Count; i++)
        {
	      if (first)
	        { first = false; }
         else
 	        { theString = theString->Concat(theString," "); }
	      theString = theString->Concat(theString,this->listValue[i]);
	     }
	  
      theString = theString->Concat(theString,")");

	   return theString;
     }

   List<PrimitiveValue ^> ^MultifieldValue::GetMultifieldValue()
     { return this->listValue; }

   int MultifieldValue::GetHashCode()
     {
      int hash = 17;
      for (int i = 0; i < this->listValue->Count; i++)
        {
	      hash = hash * 23 + ((PrimitiveValue ^) this->listValue[i])->GetHashCode();
	     }

      return hash;
     }

   bool MultifieldValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }

      List<PrimitiveValue ^> ^ list1 = this->GetMultifieldValue();
      List<PrimitiveValue ^> ^ list2 = ((MultifieldValue ^) obj)->GetMultifieldValue();

      if (list1->Count != list2->Count) 
        { return false; }

      for (int i = 0; i < this->listValue->Count; i++)
        {
         if (! list1[i]->Equals(list2[i]))
           { return false; }
	     }
    
      return true;
     }

   System::Collections::IEnumerator ^ MultifieldValue::GetEnumerator()
     { return gcnew PVEnumerator(listValue); }
     
   CLIPSNETType MultifieldValue::CLIPSType()
     { return CLIPSNETType::MULTIFIELD; }

   /*##########################*/
   /* FactAddressValue Methods */
   /*##########################*/

   FactAddressValue::FactAddressValue(
     CLIPS::FactAddressValue *theFactAddressValue) 
     { m_factAddressValue = theFactAddressValue->clone(); }

   FactAddressValue::FactAddressValue() 
     { m_factAddressValue = NULL; }

   FactAddressValue::~FactAddressValue()
     { this->!FactAddressValue();  }

   FactAddressValue::!FactAddressValue()
     {
	   if (m_factAddressValue != NULL)
	     { delete m_factAddressValue; }
     }
 
   CLIPS::FactAddressValue *FactAddressValue::GetFactAddressValue()
     { return this->m_factAddressValue; }

   long long FactAddressValue::GetFactIndex()
     {
      if (m_factAddressValue != NULL)
        { return m_factAddressValue->GetFactIndex(); }

      return 0;
     }

   PrimitiveValue ^ FactAddressValue::GetFactSlot(
     String ^ slotName)
     {
      array<Byte>^ ebSlotName = Encoding::UTF8->GetBytes(slotName);
      pin_ptr<Byte> pbSlotName = &ebSlotName[0];

      return DataObjectToPrimitiveValue(m_factAddressValue->GetFactSlot((char *) pbSlotName));
     }

   String ^ FactAddressValue::ToString()
     { return gcnew String("<Fact-" + this->GetFactIndex() + ">"); }

   int FactAddressValue::GetHashCode()
     {
      INT_PTR ptrValue = (INT_PTR) m_factAddressValue;
      return ptrValue.GetHashCode();
     }

   bool FactAddressValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }
      
      return (m_factAddressValue->GetFactAddressValue() == ((FactAddressValue ^) obj)->GetFactAddressValue()->GetFactAddressValue());
     }
     
   CLIPSNETType FactAddressValue::CLIPSType()
     { return CLIPSNETType::FACT_ADDRESS; }

   /*##############################*/
   /* InstanceAddressValue Methods */
   /*##############################*/

   InstanceAddressValue::InstanceAddressValue(
     CLIPS::InstanceAddressValue *theInstanceAddressValue) 
     { m_instanceAddressValue = theInstanceAddressValue->clone(); }

   InstanceAddressValue::InstanceAddressValue() 
     { m_instanceAddressValue = NULL; }

   InstanceAddressValue::~InstanceAddressValue()
     { this->!InstanceAddressValue(); }

   InstanceAddressValue::!InstanceAddressValue()
     {
      if (m_instanceAddressValue != NULL)
        { delete m_instanceAddressValue; }
     }

   CLIPS::InstanceAddressValue *InstanceAddressValue::GetInstanceAddressValue()
     { return this->m_instanceAddressValue; }

   String ^ InstanceAddressValue::GetInstanceName()
     { return gcnew String(m_instanceAddressValue->GetInstanceName()); }

   PrimitiveValue ^ InstanceAddressValue::DirectGetSlot(
     String ^ slotName)
     {
      array<Byte>^ ebSlotName = Encoding::UTF8->GetBytes(slotName);
      pin_ptr<Byte> pbSlotName = &ebSlotName[0];

      return DataObjectToPrimitiveValue(m_instanceAddressValue->DirectGetSlot((char *) pbSlotName));
     }

  String ^ InstanceAddressValue::ToString()
     { return gcnew String("<Instance-" + this->GetInstanceName() + ">"); }

   int InstanceAddressValue::GetHashCode()
     {
      INT_PTR ptrValue = (INT_PTR) m_instanceAddressValue;
      return ptrValue.GetHashCode();
     }

   bool InstanceAddressValue::Equals(Object ^ obj) 
     {
      if ((obj == nullptr) || 
          (GetType() != obj->GetType()))
        { return false; }

      return (m_instanceAddressValue->GetInstanceAddressValue() == ((InstanceAddressValue ^) obj)->GetInstanceAddressValue()->GetInstanceAddressValue());
     }
     
   CLIPSNETType InstanceAddressValue::CLIPSType()
     { return CLIPSNETType::INSTANCE_ADDRESS; }

  };