   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_genrcbin

#pragma once

#define _H_genrcbin

#include "genrcfun.h"

#define GENRCBIN_DATA 28

struct defgenericBinaryData
  { 
   DEFGENERIC *DefgenericArray;
   long ModuleCount;
   long GenericCount;
   long MethodCount;
   long RestrictionCount;
   long TypeCount;
   DEFGENERIC_MODULE *ModuleArray;
   DEFMETHOD *MethodArray;
   RESTRICTION *RestrictionArray;
   void **TypeArray;
  };
  
#define DefgenericBinaryData(theEnv) ((struct defgenericBinaryData *) GetEnvironmentData(theEnv,GENRCBIN_DATA))

#define GenericPointer(i) (((i) == -1L) ? NULL : (DEFGENERIC *) &DefgenericBinaryData(theEnv)->DefgenericArray[i])

   void                           SetupGenericsBload(void *);
   void                          *BloadDefgenericModuleReference(void *,int);

#endif /* _H_genrcbin */




