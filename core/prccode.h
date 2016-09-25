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
/*                                                           */
/* Revision History:                                          */
/*                                                            */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859   */
/*                                                            */
/*            Changed name of variable log to logName         */
/*            because of Unix compiler warnings of shadowed   */
/*            definitions.                                    */
/*                                                            */
/*      6.24: Renamed BOOLEAN macro type to intBool.          */
/*                                                            */
/*            Added pragmas to remove compilation warnings.   */
/*                                                            */
/*      6.30: Updated ENTITY_RECORD definitions to include    */
/*            additional NULL initializers.                   */
/*                                                            */
/*            Added ReleaseProcParameters call.               */
/*                                                            */
/*            Added tracked memory calls.                     */
/*                                                            */
/*            Removed conditional code for unsupported        */
/*            compilers/operating systems (IBM_MCW,           */
/*            MAC_MCW, and IBM_TBC).                          */
/*                                                            */
/*            Added const qualifiers to remove C++            */
/*            deprecation warnings.                           */
/*                                                            */
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

#ifndef _H_prccode

#pragma once

#define _H_prccode

#include "expressn.h"
#include "evaluatn.h"
#include "moduldef.h"
#include "scanner.h"
#include "symbol.h"

typedef struct ProcParamStack
  {
   CLIPSValue *ParamArray;

#if DEFGENERIC_CONSTRUCT
   EXPRESSION *ParamExpressions;
#endif

   int ParamArraySize;
   CLIPSValue *WildcardValue;
   void (*UnboundErrFunc)(Environment *);
   struct ProcParamStack *nxt;
  } PROC_PARAM_STACK;

#define PROCEDURAL_PRIMITIVE_DATA 37

struct proceduralPrimitiveData
  {
   void *NoParamValue;
   CLIPSValue *ProcParamArray;
   int ProcParamArraySize;
   EXPRESSION *CurrentProcActions;
#if DEFGENERIC_CONSTRUCT
   EXPRESSION *ProcParamExpressions;
#endif
   PROC_PARAM_STACK *pstack;
   CLIPSValue *WildcardValue;
   CLIPSValue *LocalVarArray;
   void (*ProcUnboundErrFunc)(Environment *);
   ENTITY_RECORD ProcParameterInfo;
   ENTITY_RECORD ProcWildInfo;
   ENTITY_RECORD ProcGetInfo;
   ENTITY_RECORD ProcBindInfo;
#if ! DEFFUNCTION_CONSTRUCT
   ENTITY_RECORD DeffunctionEntityRecord;
#endif
#if ! DEFGENERIC_CONSTRUCT
   ENTITY_RECORD GenericEntityRecord;
#endif
   int Oldindex;
  };

#define ProceduralPrimitiveData(theEnv) ((struct proceduralPrimitiveData *) GetEnvironmentData(theEnv,PROCEDURAL_PRIMITIVE_DATA))

   void                           InstallProcedurePrimitives(Environment *);

#if (! BLOAD_ONLY) && (! RUN_TIME)

#if DEFFUNCTION_CONSTRUCT || OBJECT_SYSTEM
   EXPRESSION                    *ParseProcParameters(Environment *,const char *,struct token *,EXPRESSION *,
                                                             CLIPSLexeme **,int *,int *,bool *,
                                                             bool (*)(Environment *,const char *));
#endif
   EXPRESSION                    *ParseProcActions(Environment *,const char *,const char *,struct token *,EXPRESSION *,CLIPSLexeme *,
                                                          int (*)(Environment *,EXPRESSION *,void *),
                                                          int (*)(Environment *,EXPRESSION *,void *),
                                                          int *,void *);
   int                            ReplaceProcVars(Environment *,const char *,EXPRESSION *,EXPRESSION *,CLIPSLexeme *,
                                                         int (*)(Environment *,EXPRESSION *,void *),void *);
#if DEFGENERIC_CONSTRUCT
   EXPRESSION                    *GenProcWildcardReference(Environment *,int);
#endif
#endif

   void                           PushProcParameters(Environment *,EXPRESSION *,int,const char *,const char *,void (*)(Environment *));
   void                           PopProcParameters(Environment *);

#if DEFGENERIC_CONSTRUCT
   EXPRESSION                    *GetProcParamExpressions(Environment *);
#endif

   void                           EvaluateProcActions(Environment *,Defmodule *,EXPRESSION *,int,
                                                      CLIPSValue *,void (*)(Environment *));
   void                           PrintProcParamArray(Environment *,const char *);
   void                           GrabProcWildargs(Environment *,CLIPSValue *,int);

#endif /* _H_prccode */

