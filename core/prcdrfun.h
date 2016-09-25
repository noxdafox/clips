   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*          PROCEDURAL FUNCTIONS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
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
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for long long integers.                */
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

#ifndef _H_prcdrfun

#pragma once

#define _H_prcdrfun

#include "evaluatn.h"

typedef struct loopCounterStack
  {
   long long loopCounter;
   struct loopCounterStack *nxt;
  } LOOP_COUNTER_STACK;

#define PRCDRFUN_DATA 13

struct procedureFunctionData
  {
   bool ReturnFlag;
   bool BreakFlag;
   LOOP_COUNTER_STACK *LoopCounterStack;
   CLIPSValue *BindList;
  };

#define ProcedureFunctionData(theEnv) ((struct procedureFunctionData *) GetEnvironmentData(theEnv,PRCDRFUN_DATA))

   void                           ProceduralFunctionDefinitions(Environment *);
   void                           WhileFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           LoopForCountFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           GetLoopCount(Environment *,UDFContext *,CLIPSValue *);
   void                           IfFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           BindFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           PrognFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           ReturnFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           BreakFunction(Environment *,UDFContext *,CLIPSValue *);
   void                           SwitchFunction(Environment *,UDFContext *,CLIPSValue *);
   bool                           GetBoundVariable(Environment *,CLIPSValue *,CLIPSLexeme *);
   void                           FlushBindList(Environment *);

#endif /* _H_prcdrfun */






