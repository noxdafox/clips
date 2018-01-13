#pragma once

#include "clipscpp.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace CLIPS;

namespace CLIPSNET
  {
   /*######################*/
   /* CLIPSException class */
   /*######################*/

   public ref class CLIPSException : System::Exception
     {
      public:
        CLIPSException();
        CLIPSException(String ^);
        ~CLIPSException();

      protected:
        !CLIPSException();
     };

   public ref class CLIPSLineError
     {
      public:
        CLIPSLineError(String ^,long,String ^);
        ~CLIPSLineError();
        
        property long LineNumber
	       {
            long get() { return lineNumber; };
	       }

        property String ^ FileName
	       {
            String ^ get() { return fileName; };
	       }

        property String ^ Message
	       {
            String ^ get() { return message; };
	       }

      protected:
        !CLIPSLineError();

      private:
        String ^ fileName;
        long lineNumber;
        String ^ message;
     };

   public ref class CLIPSLoadException : CLIPSException
     {
      public:
        CLIPSLoadException(String ^,List<CLIPSLineError ^> ^);
        ~CLIPSLoadException();

        property int Count
	       {
	        int get() { return errorList->Count; };
	       }

        property CLIPSLineError ^ default[int] 
          {
           CLIPSLineError ^ get(int index) 
	         { return errorList[index]; }
          }

        property List<CLIPSLineError ^> ^ LineErrors
	      {
           List<CLIPSLineError ^> ^ get() { return errorList; };
	      }
        
      protected:
        !CLIPSLoadException();

      private:
        List<CLIPSLineError ^> ^ errorList;
     };
  };