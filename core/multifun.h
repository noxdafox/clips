   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*************************************************************/

#ifndef _H_multifun

#pragma once

#define _H_multifun

#include "evaluatn.h"

   void                    MultifieldFunctionDefinitions(Environment *);
#if MULTIFIELD_FUNCTIONS
   void                    DeleteFunction(Environment *,DATA_OBJECT_PTR);
   void                    MVDeleteFunction(Environment *,DATA_OBJECT_PTR);
   void                    ReplaceFunction(Environment *,DATA_OBJECT_PTR);
   void                    MVReplaceFunction(Environment *,DATA_OBJECT_PTR);
   void                    DeleteMemberFunction(Environment *,DATA_OBJECT_PTR);
   void                    ReplaceMemberFunction(Environment *,DATA_OBJECT_PTR);
   void                    InsertFunction(Environment *,DATA_OBJECT_PTR);
   void                    ExplodeFunction(Environment *,DATA_OBJECT_PTR);
   void                   *ImplodeFunction(Environment *);
   void                    SubseqFunction(Environment *,DATA_OBJECT_PTR);
   void                    MVSubseqFunction(Environment *,DATA_OBJECT_PTR);
   void                    FirstFunction(Environment *,DATA_OBJECT_PTR);
   void                    RestFunction(Environment *,DATA_OBJECT_PTR);
   void                    NthFunction(Environment *,DATA_OBJECT_PTR);
   bool                    SubsetpFunction(Environment *);
   void                    MemberFunction(Environment *,DATA_OBJECT_PTR);
   void                    MultifieldPrognFunction(Environment *,DATA_OBJECT_PTR);
   void                    ForeachFunction(Environment *,DATA_OBJECT_PTR);
   void                    GetMvPrognField(Environment *,DATA_OBJECT_PTR);
   long                    GetMvPrognIndex(Environment *);
   bool                    FindDOsInSegment(DATA_OBJECT_PTR,int,DATA_OBJECT_PTR,
                                            long *,long *,long *,int);
#endif
   bool                    ReplaceMultiValueField(Environment *,struct dataObject *,
                                                  struct dataObject *,
                                                  long,long,
                                                  struct dataObject *,const char *);
   bool                    InsertMultiValueField(Environment *,struct dataObject *,
                                                 struct dataObject *,
                                                 long,struct dataObject *,const char *);
   bool                    DeleteMultiValueField(Environment *,struct dataObject *,struct dataObject *,
                                                 long,long,const char *);

#endif /* _H_multifun */

