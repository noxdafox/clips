   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_dffnxbin

#pragma once

#define _H_dffnxbin

#if DEFFUNCTION_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "dffnxfun.h"

   void                           SetupDeffunctionsBload(Environment *);
   void                          *BloadDeffunctionModuleReference(Environment *,int);

#define DFFNXBIN_DATA 24

struct deffunctionBinaryData
  {
   Deffunction *DeffunctionArray;
   long DeffunctionCount;
   long ModuleCount;
   DeffunctionModuleData *ModuleArray;
  };

#define DeffunctionBinaryData(theEnv) ((struct deffunctionBinaryData *) GetEnvironmentData(theEnv,DFFNXBIN_DATA))

#define DeffunctionPointer(i) (((i) == -1L) ? NULL : &DeffunctionBinaryData(theEnv)->DeffunctionArray[i])

#endif /* DEFFUNCTION_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) */

#endif /* _H_dffnxbin */




