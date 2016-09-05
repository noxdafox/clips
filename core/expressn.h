   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*               EXPRESSION HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains routines for creating, deleting,        */
/*   compacting, installing, and hashing expressions.        */
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
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed expression hashing value.              */
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

#ifndef _H_expressn

#pragma once

#define _H_expressn

struct expr;
struct exprHashNode;

typedef struct expr EXPRESSION;

#include "exprnops.h"

/******************************/
/* Expression Data Structures */
/******************************/

struct expr
   {
    unsigned short type;
    void *value;
    struct expr *argList;
    struct expr *nextArg;
   };

#define arg_list argList
#define next_arg nextArg

typedef struct exprHashNode
  {
   unsigned hashval;
   unsigned count;
   struct expr *exp;
   struct exprHashNode *next;
   long bsaveID;
  } EXPRESSION_HN;

#define EXPRESSION_HASH_SIZE 503

/*************************/
/* Type and Value Macros */
/*************************/

#define GetType(target)         ((target).type)
#define GetpType(target)        ((target)->type)
#define SetType(target,val)     ((target).type = (unsigned short) (val))
#define SetpType(target,val)    ((target)->type = (unsigned short) (val))
#define GetValue(target)        ((target).value)
#define GetpValue(target)       ((target)->value)
#define SetValue(target,val)    ((target).value = (val))
#define SetpValue(target,val)   ((target)->value = (val))

#define EnvGetType(theEnv,target)         ((target).type)
#define EnvGetpType(theEnv,target)        ((target)->type)
#define EnvSetType(theEnv,target,val)     ((target).type = (unsigned short) (val))
#define EnvSetpType(theEnv,target,val)    ((target)->type = (unsigned short) (val))
#define EnvGetValue(theEnv,target)        ((target).value)
#define EnvGetpValue(theEnv,target)       ((target)->value)
#define EnvSetValue(theEnv,target,val)    ((target).value = (void *) (val))
#define EnvSetpValue(theEnv,target,val)   ((target)->value = (val))

/********************/
/* ENVIRONMENT DATA */
/********************/

#ifndef _H_exprnpsr
#include "exprnpsr.h"
#endif

#define EXPRESSION_DATA 45

struct expressionData
  {
   struct FunctionDefinition *PTR_AND;
   struct FunctionDefinition *PTR_OR;
   struct FunctionDefinition *PTR_EQ;
   struct FunctionDefinition *PTR_NEQ;
   struct FunctionDefinition *PTR_NOT;
   EXPRESSION_HN **ExpressionHashTable;
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   long NumberOfExpressions;
   struct expr *ExpressionArray;
   long int ExpressionCount;
#endif
#if (! RUN_TIME)
   SAVED_CONTEXTS *svContexts;
   bool ReturnContext;
   bool BreakContext;
#endif
   bool SequenceOpMode;
  };

#define ExpressionData(theEnv) ((struct expressionData *) GetEnvironmentData(theEnv,EXPRESSION_DATA))

/********************/
/* Global Functions */
/********************/

   void                           ReturnExpression(Environment *,struct expr *);
   void                           ExpressionInstall(Environment *,struct expr *);
   void                           ExpressionDeinstall(Environment *,struct expr *);
   struct expr                   *PackExpression(Environment *,struct expr *);
   void                           ReturnPackedExpression(Environment *,struct expr *);
   void                           InitExpressionData(Environment *);
   void                           InitExpressionPointers(Environment *);
#if (! BLOAD_ONLY) && (! RUN_TIME)
   EXPRESSION                    *AddHashedExpression(Environment *,EXPRESSION *);
#endif
#if (! RUN_TIME)
   void                           RemoveHashedExpression(Environment *,EXPRESSION *);
#endif
#if BLOAD_AND_BSAVE || BLOAD_ONLY || BLOAD || CONSTRUCT_COMPILER
   long                           HashedExpressionIndex(Environment *,EXPRESSION *);
#endif

#endif




