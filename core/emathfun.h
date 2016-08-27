   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*          EXTENDED MATH FUNCTIONS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous extended math     */
/*   functions including cos, sin, tan, sec, csc, cot, acos, */
/*   asin, atan, asec, acsc, acot, cosh, sinh, tanh, sech,   */
/*   csch, coth, acosh, asinh, atanh, asech, acsch, acoth,   */
/*   mod, exp, log, log10, sqrt, pi, deg-rad, rad-deg,       */
/*   deg-grad, grad-deg, **, and round.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Renamed EX_MATH compiler flag to               */
/*            EXTENDED_MATH_FUNCTIONS.                       */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_emathfun

#pragma once

#define _H_emathfun

   void                           ExtendedMathFunctionDefinitions(Environment *);
#if EXTENDED_MATH_FUNCTIONS
   void                           CosFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SinFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           TanFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SecFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           CscFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           CotFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AcosFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AsinFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AtanFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AsecFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AcscFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AcotFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           CoshFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SinhFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           TanhFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SechFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           CschFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           CothFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AcoshFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AsinhFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AtanhFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AsechFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AcschFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           AcothFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           RoundFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           ModFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           ExpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           LogFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           Log10Function(Environment *,UDFContext *,CLIPSValue *);
   void                           SqrtFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           PiFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           DegRadFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           RadDegFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           DegGradFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           GradDegFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           PowFunction(Environment *,UDFContext *,CLIPSValue *);
#endif

#endif /* _H_emathfun */



