   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*             BASIC MATH FUNCTIONS MODULE             */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_bmathfun

#pragma once

#define _H_bmathfun

#include "evaluatn.h"

   void                    BasicMathFunctionDefinitions(void *);
   void                    AdditionFunction(void *,DATA_OBJECT_PTR);
   void                    MultiplicationFunction(void *,DATA_OBJECT_PTR);
   void                    SubtractionFunction(void *,DATA_OBJECT_PTR);
   void                    DivisionFunction(void *,DATA_OBJECT_PTR);
   long long               DivFunction(void *);
   intBool                 SetAutoFloatDividendCommand(void *);
   intBool                 GetAutoFloatDividendCommand(void *);
   intBool                 EnvGetAutoFloatDividend(void *);
   intBool                 EnvSetAutoFloatDividend(void *,int);
   long long               IntegerFunction(void *);
   double                  FloatFunction(void *);
   void                    AbsFunction(void *,DATA_OBJECT_PTR);
   void                    MinFunction(void *,DATA_OBJECT_PTR);
   void                    MaxFunction(void *,DATA_OBJECT_PTR);

#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                 GetAutoFloatDividend(void);
   intBool                 SetAutoFloatDividend(int);

#endif

#endif




