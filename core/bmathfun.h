   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/06/16            */
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
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*************************************************************/

#ifndef _H_bmathfun

#pragma once

#define _H_bmathfun

#include "evaluatn.h"

   void                    BasicMathFunctionDefinitions(Environment *);
   void                    AdditionFunction(Environment *,DATA_OBJECT_PTR);
   void                    MultiplicationFunction(Environment *,DATA_OBJECT_PTR);
   void                    SubtractionFunction(Environment *,DATA_OBJECT_PTR);
   void                    DivisionFunction(Environment *,DATA_OBJECT_PTR);
   long long               DivFunction(Environment *);
   bool                    SetAutoFloatDividendCommand(Environment *);
   bool                    GetAutoFloatDividendCommand(Environment *);
   bool                    EnvGetAutoFloatDividend(Environment *);
   bool                    EnvSetAutoFloatDividend(Environment *,bool);
   long long               IntegerFunction(Environment *);
   double                  FloatFunction(Environment *);
   void                    AbsFunction(Environment *,DATA_OBJECT_PTR);
   void                    MinFunction(Environment *,DATA_OBJECT_PTR);
   void                    MaxFunction(Environment *,DATA_OBJECT_PTR);

#endif




