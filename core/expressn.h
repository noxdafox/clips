   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/18/16            */
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
/*            Eval support for run time and bload only.      */
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
    union
      {
       void *value;
       CLIPSLexeme *lexemeValue;
       CLIPSFloat *floatValue;
       CLIPSInteger *integerValue;
       CLIPSBitMap *bitMapValue;
      };
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
   SAVED_CONTEXTS *svContexts;
   bool ReturnContext;
   bool BreakContext;
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
   void                           RemoveHashedExpression(Environment *,EXPRESSION *);
#if BLOAD_AND_BSAVE || BLOAD_ONLY || BLOAD || CONSTRUCT_COMPILER
   long                           HashedExpressionIndex(Environment *,EXPRESSION *);
#endif

#endif




