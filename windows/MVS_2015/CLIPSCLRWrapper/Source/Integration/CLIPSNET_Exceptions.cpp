
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
  };



  	  
