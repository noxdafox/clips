
#include "CLIPSNET_UserFunction.h"

using namespace System;
using namespace System::Text;
using namespace CLIPS;

namespace CLIPSNET
  {    
   /*##########################################*/
   /* CLIPSCPPUserFunctionBridge class methods */
   /*##########################################*/

   /******************************/
   /* CLIPSCPPUserFunctionBridge */
   /******************************/
   CLIPSCPPUserFunctionBridge::CLIPSCPPUserFunctionBridge() {}
   
   CLIPSCPPUserFunctionBridge::CLIPSCPPUserFunctionBridge(msclr::gcroot<UserFunction^> the_UserFunction) 
     { m_UserFunction = the_UserFunction; }
   
   CLIPSCPPUserFunctionBridge::~CLIPSCPPUserFunctionBridge() {}

   /************/
   /* Evaluate */
   /************/
   DataObject CLIPSCPPUserFunctionBridge::Evaluate(
     CLIPSCPPEnv *theCPPEnv,
     std::vector<DataObject> arguments)
     {
      PrimitiveValue ^ pvrv;
      List<PrimitiveValue ^> ^ theList;

      theList = gcnew List<PrimitiveValue ^>;

      for (std::vector<DataObject>::size_type i = 0; i != arguments.size(); i++) 
        { theList->Add(DataObjectToPrimitiveValue(arguments[i])); }

      pvrv = m_UserFunction->Evaluate(theList);

      return PrimitiveValueToDataObject(pvrv);
     }

   /*############################*/
   /* UserFunction class methods */
   /*############################*/

   UserFunction::UserFunction() 
     { m_UserFunctionBridge = new CLIPSCPPUserFunctionBridge(this); }

   UserFunction::~UserFunction()
     { this->!UserFunction(); }

   PrimitiveValue ^ UserFunction::Evaluate(
     List<PrimitiveValue ^> ^arguments)
     { 
      return gcnew VoidValue(); 
     }

   CLIPSCPPUserFunctionBridge *UserFunction::UserFunctionBridge()
     { return m_UserFunctionBridge; }

   UserFunction::!UserFunction()
     { delete m_UserFunctionBridge; }
  };