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
   void                           EqFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           NeqFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           StringpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SymbolpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           LexemepFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           NumberpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           FloatpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           IntegerpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           MultifieldpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           PointerpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           NotFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AndFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           OrFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           LessThanOrEqualFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           GreaterThanOrEqualFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           LessThanFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           GreaterThanFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           NumericEqualFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           NumericNotEqualFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           OddpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           EvenpFunction(Environment *,UDFContext *,CLIPSValue *);

#endif /* _H_prdctfun */



