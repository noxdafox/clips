   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_factrete

#pragma once

#define _H_factrete

#include "evaluatn.h"

   bool                           FactPNGetVar1(Environment *,void *,UDFValue *);
   bool                           FactPNGetVar2(Environment *,void *,UDFValue *);
   bool                           FactPNGetVar3(Environment *,void *,UDFValue *);
   bool                           FactJNGetVar1(Environment *,void *,UDFValue *);
   bool                           FactJNGetVar2(Environment *,void *,UDFValue *);
   bool                           FactJNGetVar3(Environment *,void *,UDFValue *);
   bool                           FactSlotLength(Environment *,void *,UDFValue *);
   bool                           FactJNCompVars1(Environment *,void *,UDFValue *);
   bool                           FactJNCompVars2(Environment *,void *,UDFValue *);
   bool                           FactPNCompVars1(Environment *,void *,UDFValue *);
   bool                           FactPNConstant1(Environment *,void *,UDFValue *);
   bool                           FactPNConstant2(Environment *,void *,UDFValue *);
   bool                           FactStoreMultifield(Environment *,void *,UDFValue *);
   unsigned short                 AdjustFieldPosition(Environment *,struct multifieldMarker *,
                                                      unsigned short,unsigned short,int *);

#endif


