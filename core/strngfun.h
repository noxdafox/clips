   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*             STRING FUNCTIONS HEADER FILE            */
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
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added support for UTF-8 strings to str-length, */
/*            str-index, and sub-string functions.           */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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

#ifndef _H_strngfun

#pragma once

#define _H_strngfun

#include "evaluatn.h"

   bool                           EnvBuild(Environment *,const char *);
   bool                           EnvEval(Environment *,const char *,CLIPSValue *);
   void                           StringFunctionDefinitions(Environment *);
   void                           StrCatFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SymCatFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           StrLengthFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           UpcaseFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           LowcaseFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           StrCompareFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SubStringFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           StrIndexFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           EvalFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           BuildFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           StringToFieldFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           StringToField(Environment *,const char *,CLIPSValue *);

#endif /* _H_strngfun */






