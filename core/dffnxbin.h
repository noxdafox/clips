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

#ifndef _H_dffnxbin

#pragma once

#define _H_dffnxbin

#if DEFFUNCTION_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "dffnxfun.h"

   void                           SetupDeffunctionsBload(void *);
   void                          *BloadDeffunctionModuleReference(void *,int);

#define DFFNXBIN_DATA 24

struct deffunctionBinaryData
  { 
   DEFFUNCTION *DeffunctionArray;
   long DeffunctionCount;
   long ModuleCount;
   DEFFUNCTION_MODULE *ModuleArray;
  };
  
#define DeffunctionBinaryData(theEnv) ((struct deffunctionBinaryData *) GetEnvironmentData(theEnv,DFFNXBIN_DATA))

#define DeffunctionPointer(i) (((i) == -1L) ? NULL : (DEFFUNCTION *) &DeffunctionBinaryData(theEnv)->DeffunctionArray[i])

#endif /* DEFFUNCTION_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) */

#endif /* _H_dffnxbin */




