   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*************************************************************/

#ifndef _H_prdctfun

#pragma once

#define _H_prdctfun

   void                           PredicateFunctionDefinitions(Environment *);
   bool                           EqFunction(Environment *);
   bool                           NeqFunction(Environment *);
   bool                           StringpFunction(Environment *);
   bool                           SymbolpFunction(Environment *);
   bool                           LexemepFunction(Environment *);
   bool                           NumberpFunction(Environment *);
   bool                           FloatpFunction(Environment *);
   bool                           IntegerpFunction(Environment *);
   bool                           MultifieldpFunction(Environment *);
   bool                           PointerpFunction(Environment *);
   bool                           NotFunction(Environment *);
   bool                           AndFunction(Environment *);
   bool                           OrFunction(Environment *);
   bool                           LessThanOrEqualFunction(Environment *);
   bool                           GreaterThanOrEqualFunction(Environment *);
   bool                           LessThanFunction(Environment *);
   bool                           GreaterThanFunction(Environment *);
   bool                           NumericEqualFunction(Environment *);
   bool                           NumericNotEqualFunction(Environment *);
   bool                           OddpFunction(Environment *);
   bool                           EvenpFunction(Environment *);

#endif /* _H_prdctfun */



