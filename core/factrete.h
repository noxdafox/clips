   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_factrete

#define _H_factrete

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

   intBool                        FactPNGetVar1(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactPNGetVar2(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactPNGetVar3(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactJNGetVar1(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactJNGetVar2(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactJNGetVar3(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactSlotLength(void *,void *,DATA_OBJECT_PTR);
   int                            FactJNCompVars1(void *,void *,DATA_OBJECT_PTR);
   int                            FactJNCompVars2(void *,void *,DATA_OBJECT_PTR);
   int                            FactPNCompVars1(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactPNConstant1(void *,void *,DATA_OBJECT_PTR);
   intBool                        FactPNConstant2(void *,void *,DATA_OBJECT_PTR);
   int                            FactStoreMultifield(void *,void *,DATA_OBJECT_PTR);
   unsigned short                 AdjustFieldPosition(void *,struct multifieldMarker *,
                                                             unsigned short,unsigned short,int *);

#endif


