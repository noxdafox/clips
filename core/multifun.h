   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*           MULTIFIELD FUNCTIONS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary Riley and Brian Dantes                          */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Moved ImplodeMultifield to multifld.c.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Fixed memory leaks when error occurred.        */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when DEFMODULE_CONSTRUCT   */
/*            compiler flag is set to 0.                     */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_multifun

#pragma once

#define _H_multifun

#include "evaluatn.h"

   void                    MultifieldFunctionDefinitions(Environment *);
#if MULTIFIELD_FUNCTIONS
   void                    DeleteFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    ReplaceFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    DeleteMemberFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    ReplaceMemberFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    InsertFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    ExplodeFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    ImplodeFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    SubseqFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    FirstFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    RestFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    NthFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    SubsetpFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    MemberFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    MultifieldPrognFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    ForeachFunction(Environment *,UDFContext *,CLIPSValue *);
   void                    GetMvPrognField(Environment *,UDFContext *,CLIPSValue *);
   void                    GetMvPrognIndex(Environment *,UDFContext *,CLIPSValue *);
   bool                    FindDOsInSegment(CLIPSValue *,int,CLIPSValue *,
                                            long *,long *,long *,int);
#endif
   bool                    ReplaceMultiValueField(Environment *,CLIPSValue *,
                                                  CLIPSValue *,
                                                  long,long,
                                                  CLIPSValue *,const char *);
   bool                    InsertMultiValueField(Environment *,CLIPSValue *,
                                                 CLIPSValue *,
                                                 long,CLIPSValue *,const char *);
   bool                    DeleteMultiValueField(Environment *,CLIPSValue *,CLIPSValue *,
                                                 long,long,const char *);

#endif /* _H_multifun */

