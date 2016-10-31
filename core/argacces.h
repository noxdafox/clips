   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*             ARGUMENT ACCESS HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides access routines for accessing arguments */
/*   passed to user or system functions defined using the    */
/*   DefineFunction protocol.                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added IllegalLogicalNameMessage function.      */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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

#ifndef _H_argacces

#pragma once

#define _H_argacces

#include "expressn.h"
#include "evaluatn.h"
#include "moduldef.h"

   const char                    *GetLogicalName(UDFContext *,const char *);
   const char                    *GetFileName(UDFContext *);
   const char                    *GetConstructName(UDFContext *,const char *,const char *);
   void                           ExpectedCountError(Environment *,const char *,int,int);
   void                           OpenErrorMessage(Environment *,const char *,const char *);
   bool                           CheckFunctionArgCount(Environment *,struct functionDefinition *,int);
   void                           ExpectedTypeError0(Environment *,const char *,int);
   void                           ExpectedTypeError1(Environment *,const char *,int,const char *);
   void                           ExpectedTypeError2(Environment *,const char *,int);
   Defmodule                     *GetModuleName(UDFContext *,int,bool *);
   void                          *GetFactOrInstanceArgument(UDFContext *,int,UDFValue *);
   void                           IllegalLogicalNameMessage(Environment *,const char *);

#endif






