   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_bmathfun

#pragma once

#define _H_bmathfun

#include "evaluatn.h"

   void                    BasicMathFunctionDefinitions(Environment *);
   void                    AdditionFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    MultiplicationFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    SubtractionFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    DivisionFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    DivFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    SetAutoFloatDividendCommand(Environment *,UDFContext *,CLIPSValue *);
   void                    GetAutoFloatDividendCommand(Environment *,UDFContext *,CLIPSValue *);
   bool                    EnvGetAutoFloatDividend(Environment *);
   bool                    EnvSetAutoFloatDividend(Environment *,bool);
   void                    IntegerFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    FloatFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    AbsFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    MinFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    MaxFunction(Environment *,UDFContext *,CLIPSValue *);

#endif




