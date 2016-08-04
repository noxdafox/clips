   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*************************************************************/

#ifndef _H_strngfun

#pragma once

#define _H_strngfun

#include "evaluatn.h"

#if ALLOW_ENVIRONMENT_GLOBALS
   bool                           Build(const char *);
   bool                           Eval(const char *,DATA_OBJECT_PTR);
#endif

   bool                           EnvBuild(Environment *,const char *);
   bool                           EnvEval(Environment *,const char *,DATA_OBJECT_PTR);
   void                           StringFunctionDefinitions(Environment *);
   void                           StrCatFunction(Environment *,DATA_OBJECT_PTR);
   void                           SymCatFunction(Environment *,DATA_OBJECT_PTR);
   long long                      StrLengthFunction(Environment *);
   void                           UpcaseFunction(Environment *,DATA_OBJECT_PTR);
   void                           LowcaseFunction(Environment *,DATA_OBJECT_PTR);
   long long                      StrCompareFunction(Environment *);
   void                          *SubStringFunction(Environment *);
   void                           StrIndexFunction(Environment *,DATA_OBJECT_PTR);
   void                           EvalFunction(Environment *,DATA_OBJECT_PTR);
   bool                           BuildFunction(Environment *);
   void                           StringToFieldFunction(Environment *,DATA_OBJECT *);
   void                           StringToField(Environment *,const char *,DATA_OBJECT *);

#endif /* _H_strngfun */






