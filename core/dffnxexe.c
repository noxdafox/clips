   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/01/16             */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Deffunction Execution Routines                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.30: Changed garbage collection algorithm.          */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Added CLIPSBlockStart and CLIPSBlockEnd        */
/*            functions for garbage collection blocks.       */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFFUNCTION_CONSTRUCT

#include <stdio.h>
#include <string.h>

#include "constrct.h"
#include "envrnmnt.h"
#include "prcdrfun.h"
#include "prccode.h"
#include "prntutil.h"
#include "proflfun.h"
#include "router.h"
#include "utility.h"
#include "watch.h"

#include "dffnxexe.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define BEGIN_TRACE ">> "
#define END_TRACE   "<< "

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

   static void                    UnboundDeffunctionErr(Environment *);

#if DEBUGGING_FUNCTIONS
   static void                    WatchDeffunction(Environment *,const char *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/****************************************************
  NAME         : CallDeffunction
  DESCRIPTION  : Executes the body of a deffunction
  INPUTS       : 1) The deffunction
                 2) Argument expressions
                 3) Data object buffer to hold result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deffunction executed and result
                 stored in data object buffer
  NOTES        : Used in EvaluateExpression(theEnv,)
 ****************************************************/
void CallDeffunction(
  Environment *theEnv,
  Deffunction *dptr,
  Expression *args,
  UDFValue *returnValue)
  {
   bool oldce;
   Deffunction *previouslyExecutingDeffunction;
   CLIPSBlock gcBlock;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   returnValue->value = FalseSymbol(theEnv);
   EvaluationData(theEnv)->EvaluationError = false;
   if (EvaluationData(theEnv)->HaltExecution)
     return;

   CLIPSBlockStart(theEnv,&gcBlock);

   oldce = ExecutingConstruct(theEnv);
   SetExecutingConstruct(theEnv,true);
   previouslyExecutingDeffunction = DeffunctionData(theEnv)->ExecutingDeffunction;
   DeffunctionData(theEnv)->ExecutingDeffunction = dptr;
   EvaluationData(theEnv)->CurrentEvaluationDepth++;
   dptr->executing++;
   PushProcParameters(theEnv,args,CountArguments(args),DeffunctionName(dptr),
                      "deffunction",UnboundDeffunctionErr);
   if (EvaluationData(theEnv)->EvaluationError)
     {
      dptr->executing--;
      DeffunctionData(theEnv)->ExecutingDeffunction = previouslyExecutingDeffunction;
      EvaluationData(theEnv)->CurrentEvaluationDepth--;

      CLIPSBlockEnd(theEnv,&gcBlock,returnValue);
      CallPeriodicTasks(theEnv);

      SetExecutingConstruct(theEnv,oldce);
      return;
     }

#if DEBUGGING_FUNCTIONS
   if (dptr->trace)
     WatchDeffunction(theEnv,BEGIN_TRACE);
#endif

#if PROFILING_FUNCTIONS
   StartProfile(theEnv,&profileFrame,
                &dptr->header.usrData,
                ProfileFunctionData(theEnv)->ProfileConstructs);
#endif

   EvaluateProcActions(theEnv,dptr->header.whichModule->theModule,
                       dptr->code,dptr->numberOfLocalVars,
                       returnValue,UnboundDeffunctionErr);

#if PROFILING_FUNCTIONS
    EndProfile(theEnv,&profileFrame);
#endif

#if DEBUGGING_FUNCTIONS
   if (dptr->trace)
     WatchDeffunction(theEnv,END_TRACE);
#endif
   ProcedureFunctionData(theEnv)->ReturnFlag = false;

   dptr->executing--;
   PopProcParameters(theEnv);
   DeffunctionData(theEnv)->ExecutingDeffunction = previouslyExecutingDeffunction;
   EvaluationData(theEnv)->CurrentEvaluationDepth--;

   CLIPSBlockEnd(theEnv,&gcBlock,returnValue);
   CallPeriodicTasks(theEnv);

   SetExecutingConstruct(theEnv,oldce);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*******************************************************
  NAME         : UnboundDeffunctionErr
  DESCRIPTION  : Print out a synopis of the currently
                   executing deffunction for unbound
                   variable errors
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Error synopsis printed to WERROR
  NOTES        : None
 *******************************************************/
static void UnboundDeffunctionErr(
  Environment *theEnv)
  {
   EnvPrintRouter(theEnv,WERROR,"deffunction ");
   EnvPrintRouter(theEnv,WERROR,DeffunctionName(DeffunctionData(theEnv)->ExecutingDeffunction));
   EnvPrintRouter(theEnv,WERROR,".\n");
  }

#if DEBUGGING_FUNCTIONS

/***************************************************
  NAME         : WatchDeffunction
  DESCRIPTION  : Displays a message indicating when
                 a deffunction began and ended
                 execution
  INPUTS       : The beginning or end trace string
                 to print when deffunction starts
                 or finishes respectively
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch message printed
  NOTES        : None
 ***************************************************/
static void WatchDeffunction(
  Environment *theEnv,
  const char *tstring)
  {
   EnvPrintRouter(theEnv,WTRACE,"DFN ");
   EnvPrintRouter(theEnv,WTRACE,tstring);
   if (DeffunctionData(theEnv)->ExecutingDeffunction->header.whichModule->theModule != EnvGetCurrentModule(theEnv))
     {
      EnvPrintRouter(theEnv,WTRACE,DeffunctionModule(DeffunctionData(theEnv)->ExecutingDeffunction));;
      EnvPrintRouter(theEnv,WTRACE,"::");
     }
   EnvPrintRouter(theEnv,WTRACE,DeffunctionData(theEnv)->ExecutingDeffunction->header.name->contents);
   EnvPrintRouter(theEnv,WTRACE," ED:");
   PrintLongInteger(theEnv,WTRACE,(long long) EvaluationData(theEnv)->CurrentEvaluationDepth);
   PrintProcParamArray(theEnv,WTRACE);
  }

#endif
#endif
