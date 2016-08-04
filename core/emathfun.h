   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*************************************************************/

#ifndef _H_emathfun

#pragma once

#define _H_emathfun

   void                           ExtendedMathFunctionDefinitions(Environment *);
#if EXTENDED_MATH_FUNCTIONS
   double                         CosFunction(Environment *);
   double                         SinFunction(Environment *);
   double                         TanFunction(Environment *);
   double                         SecFunction(Environment *);
   double                         CscFunction(Environment *);
   double                         CotFunction(Environment *);
   double                         AcosFunction(Environment *);
   double                         AsinFunction(Environment *);
   double                         AtanFunction(Environment *);
   double                         AsecFunction(Environment *);
   double                         AcscFunction(Environment *);
   double                         AcotFunction(Environment *);
   double                         CoshFunction(Environment *);
   double                         SinhFunction(Environment *);
   double                         TanhFunction(Environment *);
   double                         SechFunction(Environment *);
   double                         CschFunction(Environment *);
   double                         CothFunction(Environment *);
   double                         AcoshFunction(Environment *);
   double                         AsinhFunction(Environment *);
   double                         AtanhFunction(Environment *);
   double                         AsechFunction(Environment *);
   double                         AcschFunction(Environment *);
   double                         AcothFunction(Environment *);
   long long                      RoundFunction(Environment *);
   void                           ModFunction(Environment *,DATA_OBJECT_PTR);
   double                         ExpFunction(Environment *);
   double                         LogFunction(Environment *);
   double                         Log10Function(Environment *);
   double                         SqrtFunction(Environment *);
   double                         PiFunction(Environment *);
   double                         DegRadFunction(Environment *);
   double                         RadDegFunction(Environment *);
   double                         DegGradFunction(Environment *);
   double                         GradDegFunction(Environment *);
   double                         PowFunction(Environment *);
#endif

#endif /* _H_emathfun */



