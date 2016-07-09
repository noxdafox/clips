   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
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
   struct dataObject *BindList;
  };

#define ProcedureFunctionData(theEnv) ((struct procedureFunctionData *) GetEnvironmentData(theEnv,PRCDRFUN_DATA))

   void                           ProceduralFunctionDefinitions(void *);
   void                           WhileFunction(void *,DATA_OBJECT_PTR);
   void                           LoopForCountFunction(void *,DATA_OBJECT_PTR);
   long long                      GetLoopCount(void *);
   void                           IfFunction(void *,DATA_OBJECT_PTR);
   void                           BindFunction(void *,DATA_OBJECT_PTR);
   void                           PrognFunction(void *,DATA_OBJECT_PTR);
   void                           ReturnFunction(void *,DATA_OBJECT_PTR);
   void                           BreakFunction(void *);
   void                           SwitchFunction(void *,DATA_OBJECT_PTR);
   bool                           GetBoundVariable(void *,struct dataObject *,struct symbolHashNode *);
   void                           FlushBindList(void *);

#endif /* _H_prcdrfun */






