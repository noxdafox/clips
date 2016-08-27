   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Added fact-set queries.                        */
/*                                                           */
/*      6.24: Corrected errors when compiling as a C++ file. */
/*            DR0868                                         */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed garbage collection algorithm.          */
/*                                                           */
/*            Fixes for run-time use of query functions.     */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_factqury

#pragma once

#define _H_factqury

#if FACT_SET_QUERIES

#include "factmngr.h"

typedef struct query_template
  {
   Deftemplate *templatePtr;
   struct query_template *chain, *nxt;
  } QUERY_TEMPLATE;

typedef struct query_soln
  {
   struct fact **soln;
   struct query_soln *nxt;
  } QUERY_SOLN;

typedef struct query_core
  {
   struct fact **solns;
   EXPRESSION *query,*action;
   QUERY_SOLN *soln_set,*soln_bottom;
   unsigned soln_size,soln_cnt;
   CLIPSValue *result;
  } QUERY_CORE;

typedef struct query_stack
  {
   QUERY_CORE *core;
   struct query_stack *nxt;
  } QUERY_STACK;

#define FACT_QUERY_DATA 63

struct factQueryData
  { 
   SYMBOL_HN *QUERY_DELIMETER_SYMBOL;
   QUERY_CORE *QueryCore;
   QUERY_STACK *QueryCoreStack;
   bool AbortQuery;
  };

#define FactQueryData(theEnv) ((struct factQueryData *) GetEnvironmentData(theEnv,FACT_QUERY_DATA))

#define QUERY_DELIMETER_STRING     "(QDS)"

   void                           SetupFactQuery(Environment *);
   void                           GetQueryFact(Environment *,UDFContext *,CLIPSValue *);
   void                           GetQueryFactSlot(Environment *,UDFContext *,CLIPSValue *);
   void                           AnyFacts(Environment *,UDFContext *,CLIPSValue *);
   void                           QueryFindFact(Environment *,UDFContext *,CLIPSValue *);
   void                           QueryFindAllFacts(Environment *,UDFContext *,CLIPSValue *);
   void                           QueryDoForFact(Environment *,UDFContext *,CLIPSValue *);
   void                           QueryDoForAllFacts(Environment *,UDFContext *,CLIPSValue *);
   void                           DelayedQueryDoForAllFacts(Environment *,UDFContext *,CLIPSValue *);

#endif /* FACT_SET_QUERIES */

#endif /* _H_factqury */
