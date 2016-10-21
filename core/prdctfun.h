   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*            PREDICATE FUNCTIONS HEADER FILE          */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
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

#ifndef _H_prdctfun

#pragma once

#define _H_prdctfun

   void                           PredicateFunctionDefinitions(Environment *);
   void                           EqFunction(Environment *,UDFContext *,UDFValue *);
   void                           NeqFunction(Environment *,UDFContext *,UDFValue *);
   void                           StringpFunction(Environment *,UDFContext *,UDFValue *);
   void                           SymbolpFunction(Environment *,UDFContext *,UDFValue *);
   void                           LexemepFunction(Environment *,UDFContext *,UDFValue *);
   void                           NumberpFunction(Environment *,UDFContext *,UDFValue *);
   void                           FloatpFunction(Environment *,UDFContext *,UDFValue *);
   void                           IntegerpFunction(Environment *,UDFContext *,UDFValue *);
   void                           MultifieldpFunction(Environment *,UDFContext *,UDFValue *);
   void                           PointerpFunction(Environment *,UDFContext *,UDFValue *);
   void                           NotFunction(Environment *,UDFContext *,UDFValue *);
   void                           AndFunction(Environment *,UDFContext *,UDFValue *);
   void                           OrFunction(Environment *,UDFContext *,UDFValue *);
   void                           LessThanOrEqualFunction(Environment *,UDFContext *,UDFValue *);
   void                           GreaterThanOrEqualFunction(Environment *,UDFContext *,UDFValue *);
   void                           LessThanFunction(Environment *,UDFContext *,UDFValue *);
   void                           GreaterThanFunction(Environment *,UDFContext *,UDFValue *);
   void                           NumericEqualFunction(Environment *,UDFContext *,UDFValue *);
   void                           NumericNotEqualFunction(Environment *,UDFContext *,UDFValue *);
   void                           OddpFunction(Environment *,UDFContext *,UDFValue *);
   void                           EvenpFunction(Environment *,UDFContext *,UDFValue *);

#endif /* _H_prdctfun */



