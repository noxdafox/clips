   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*        FACT RETE ACCESS FUNCTIONS HEADER FILE       */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Removed INCREMENTAL_RESET compilation flag.    */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for hashing optimizations.             */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_factrete

#pragma once

#define _H_factrete

#include "evaluatn.h"

   bool                           FactPNGetVar1(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactPNGetVar2(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactPNGetVar3(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactJNGetVar1(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactJNGetVar2(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactJNGetVar3(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactSlotLength(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactJNCompVars1(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactJNCompVars2(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactPNCompVars1(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactPNConstant1(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactPNConstant2(Environment *,void *,DATA_OBJECT_PTR);
   bool                           FactStoreMultifield(Environment *,void *,DATA_OBJECT_PTR);
   unsigned short                 AdjustFieldPosition(Environment *,struct multifieldMarker *,
                                                      unsigned short,unsigned short,int *);

#endif


