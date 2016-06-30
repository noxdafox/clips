   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
   DATA_OBJECT *ParamArray;

#if DEFGENERIC_CONSTRUCT
   EXPRESSION *ParamExpressions;
#endif

   int ParamArraySize;
   DATA_OBJECT *WildcardValue;
   void (*UnboundErrFunc)(void *);
   struct ProcParamStack *nxt;
  } PROC_PARAM_STACK;

#define PROCEDURAL_PRIMITIVE_DATA 37

struct proceduralPrimitiveData
  { 
   void *NoParamValue;
   DATA_OBJECT *ProcParamArray;
   int ProcParamArraySize;
   EXPRESSION *CurrentProcActions;
#if DEFGENERIC_CONSTRUCT
   EXPRESSION *ProcParamExpressions;
#endif
   PROC_PARAM_STACK *pstack;
   DATA_OBJECT *WildcardValue;
   DATA_OBJECT *LocalVarArray;
   void (*ProcUnboundErrFunc)(void *);
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

   void                           InstallProcedurePrimitives(void *);

#if (! BLOAD_ONLY) && (! RUN_TIME)

#if DEFFUNCTION_CONSTRUCT || OBJECT_SYSTEM
   EXPRESSION                    *ParseProcParameters(void *,const char *,struct token *,EXPRESSION *,
                                                             SYMBOL_HN **,int *,int *,int *,
                                                             int (*)(void *,const char *));
#endif
   EXPRESSION                    *ParseProcActions(void *,const char *,const char *,struct token *,EXPRESSION *,SYMBOL_HN *,
                                                          int (*)(void *,EXPRESSION *,void *),
                                                          int (*)(void *,EXPRESSION *,void *),
                                                          int *,void *);
   intBool                        ReplaceProcVars(void *,const char *,EXPRESSION *,EXPRESSION *,SYMBOL_HN *,
                                                         int (*)(void *,EXPRESSION *,void *),void *);
#if DEFGENERIC_CONSTRUCT
   EXPRESSION                    *GenProcWildcardReference(void *,int);
#endif
#endif

   void                           PushProcParameters(void *,EXPRESSION *,int,const char *,const char *,void (*)(void *));
   void                           PopProcParameters(void *);

#if DEFGENERIC_CONSTRUCT
   EXPRESSION                    *GetProcParamExpressions(void *);
#endif

   void                           EvaluateProcActions(void *,struct defmodule *,EXPRESSION *,int,
                                                             DATA_OBJECT *,void (*)(void *));
   void                           PrintProcParamArray(void *,const char *);
   void                           GrabProcWildargs(void *,DATA_OBJECT *,int);

#endif /* _H_prccode */

