
#include "CLIPSNET_Exceptions.h"

using namespace System;
using namespace CLIPS;

namespace CLIPSNET
  {	 
   /******************/
   /* CLIPSException */
   /******************/
   CLIPSException::CLIPSException() : Exception()
     {
     }
   
   CLIPSException::CLIPSException(String ^ message) : Exception(message)
     {
     }

   /*******************/
   /* ~CLIPSException */
   /*******************/
   CLIPSException::~CLIPSException()
     {
      this->!CLIPSException(); 
     }
     
   /*******************/
   /* !CLIPSException */
   /*******************/
   CLIPSException::!CLIPSException()
     { 
     }
   
   /******************/
   /* CLIPSLineError */
   /******************/   
   CLIPSLineError::CLIPSLineError(
     String ^ theFileName,
     long theLineNumber, 
     String ^ theMessage)
     { 
      fileName = theFileName;
      lineNumber = theLineNumber;
      message = theMessage;
     }

   /*******************/
   /* ~CLIPSLineError */
   /*******************/
   CLIPSLineError::~CLIPSLineError()
     {
      this->!CLIPSLineError(); 
     }
     
   /*******************/
   /* !CLIPSLineError */
   /*******************/
   CLIPSLineError::!CLIPSLineError()
     { 
     }

   /**********************/
   /* CLIPSLoadException */
   /**********************/   
   CLIPSLoadException::CLIPSLoadException(
     String ^theMessage,
     List<CLIPSLineError ^> ^ theErrorList) : CLIPSException(theMessage)
     {
      errorList = theErrorList;
     }

   /***********************/
   /* ~CLIPSLoadException */
   /***********************/
   CLIPSLoadException::~CLIPSLoadException()
     {
      this->!CLIPSLoadException(); 
     }
     
   /***********************/
   /* !CLIPSLoadException */
   /***********************/
   CLIPSLoadException::!CLIPSLoadException()
     { 
     }
  };



  	  
