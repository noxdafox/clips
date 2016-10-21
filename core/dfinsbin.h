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

#ifndef _H_dfinsbin

#pragma once

#define _H_dfinsbin

#if DEFINSTANCES_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "defins.h"

#define DFINSBIN_DATA 25

struct definstancesBinaryData
  {
   Definstances *DefinstancesArray;
   long DefinstancesCount;
   long ModuleCount;
   DEFINSTANCES_MODULE *ModuleArray;
  };

#define DefinstancesBinaryData(theEnv) ((struct definstancesBinaryData *) GetEnvironmentData(theEnv,DFINSBIN_DATA))

   void                           SetupDefinstancesBload(Environment *);
   void                          *BloadDefinstancesModuleRef(Environment *,int);

#endif /* DEFINSTANCES_CONSTRUCT && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) */

#endif /* _H_dfinsbin */



