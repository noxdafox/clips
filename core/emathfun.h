   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_emathfun

#define _H_emathfun

   void                           ExtendedMathFunctionDefinitions(void *theEnv);
#if EXTENDED_MATH_FUNCTIONS
   double                         CosFunction(void *);
   double                         SinFunction(void *);
   double                         TanFunction(void *);
   double                         SecFunction(void *);
   double                         CscFunction(void *);
   double                         CotFunction(void *);
   double                         AcosFunction(void *);
   double                         AsinFunction(void *);
   double                         AtanFunction(void *);
   double                         AsecFunction(void *);
   double                         AcscFunction(void *);
   double                         AcotFunction(void *);
   double                         CoshFunction(void *);
   double                         SinhFunction(void *);
   double                         TanhFunction(void *);
   double                         SechFunction(void *);
   double                         CschFunction(void *);
   double                         CothFunction(void *);
   double                         AcoshFunction(void *);
   double                         AsinhFunction(void *);
   double                         AtanhFunction(void *);
   double                         AsechFunction(void *);
   double                         AcschFunction(void *);
   double                         AcothFunction(void *);
   long long                      RoundFunction(void *);
   void                           ModFunction(void *,DATA_OBJECT_PTR);
   double                         ExpFunction(void *);
   double                         LogFunction(void *);
   double                         Log10Function(void *);
   double                         SqrtFunction(void *);
   double                         PiFunction(void *);
   double                         DegRadFunction(void *);
   double                         RadDegFunction(void *);
   double                         DegGradFunction(void *);
   double                         GradDegFunction(void *);
   double                         PowFunction(void *);
#endif

#endif /* _H_emathfun */



