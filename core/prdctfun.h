   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_prdctfun

#define _H_prdctfun

   void                           PredicateFunctionDefinitions(void *);
   intBool                        EqFunction(void *);
   intBool                        NeqFunction(void *);
   intBool                        StringpFunction(void *);
   intBool                        SymbolpFunction(void *);
   intBool                        LexemepFunction(void *);
   intBool                        NumberpFunction(void *);
   intBool                        FloatpFunction(void *);
   intBool                        IntegerpFunction(void *);
   intBool                        MultifieldpFunction(void *);
   intBool                        PointerpFunction(void *);
   intBool                        NotFunction(void *);
   intBool                        AndFunction(void *);
   intBool                        OrFunction(void *);
   intBool                        LessThanOrEqualFunction(void *);
   intBool                        GreaterThanOrEqualFunction(void *);
   intBool                        LessThanFunction(void *);
   intBool                        GreaterThanFunction(void *);
   intBool                        NumericEqualFunction(void *);
   intBool                        NumericNotEqualFunction(void *);
   intBool                        OddpFunction(void *);
   intBool                        EvenpFunction(void *);

#endif /* _H_prdctfun */



