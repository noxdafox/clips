   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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

   void                           ProceduralFunctionDefinitions(Environment *);
   void                           WhileFunction(Environment *,DATA_OBJECT_PTR);
   void                           LoopForCountFunction(Environment *,DATA_OBJECT_PTR);
   long long                      GetLoopCount(Environment *);
   void                           IfFunction(Environment *,DATA_OBJECT_PTR);
   void                           BindFunction(Environment *,DATA_OBJECT_PTR);
   void                           PrognFunction(Environment *,DATA_OBJECT_PTR);
   void                           ReturnFunction(Environment *,DATA_OBJECT_PTR);
   void                           BreakFunction(Environment *);
   void                           SwitchFunction(Environment *,DATA_OBJECT_PTR);
   bool                           GetBoundVariable(Environment *,struct dataObject *,struct symbolHashNode *);
   void                           FlushBindList(Environment *);

#endif /* _H_prcdrfun */






