   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
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
/*************************************************************/

#ifndef _H_strngfun

#pragma once

#define _H_strngfun

#include "evaluatn.h"

#if ALLOW_ENVIRONMENT_GLOBALS
   bool                           Build(const char *);
   bool                           Eval(const char *,DATA_OBJECT_PTR);
#endif

   bool                           EnvBuild(void *,const char *);
   bool                           EnvEval(void *,const char *,DATA_OBJECT_PTR);
   void                           StringFunctionDefinitions(void *);
   void                           StrCatFunction(void *,DATA_OBJECT_PTR);
   void                           SymCatFunction(void *,DATA_OBJECT_PTR);
   long long                      StrLengthFunction(void *);
   void                           UpcaseFunction(void *,DATA_OBJECT_PTR);
   void                           LowcaseFunction(void *,DATA_OBJECT_PTR);
   long long                      StrCompareFunction(void *);
   void                          *SubStringFunction(void *);
   void                           StrIndexFunction(void *,DATA_OBJECT_PTR);
   void                           EvalFunction(void *,DATA_OBJECT_PTR);
   bool                           BuildFunction(void *);
   void                           StringToFieldFunction(void *,DATA_OBJECT *);
   void                           StringToField(void *,const char *,DATA_OBJECT *);

#endif /* _H_strngfun */






