   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/06/16            */
   /*                                                     */
   /*               FACT COMMANDS HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*************************************************************/

#ifndef _H_factcom

#pragma once

#define _H_factcom

#include "evaluatn.h"

   void                           FactCommandDefinitions(Environment *);
   void                           AssertCommand(Environment *,DATA_OBJECT_PTR);
   void                           RetractCommand(Environment *);
   void                           AssertStringFunction(Environment *,DATA_OBJECT_PTR);
   void                           FactsCommand(Environment *);
   void                           EnvFacts(Environment *,const char *,Defmodule *,long long,long long,long long);
   bool                           SetFactDuplicationCommand(Environment *);
   bool                           GetFactDuplicationCommand(Environment *);
   bool                           SaveFactsCommand(Environment *);
   bool                           LoadFactsCommand(Environment *);
   bool                           EnvSaveFacts(Environment *,const char *,int);
   bool                           EnvSaveFactsDriver(Environment *,const char *,int,struct expr *);
   bool                           EnvLoadFacts(Environment *,const char *);
   bool                           EnvLoadFactsFromString(Environment *,const char *,long);
   long long                      FactIndexFunction(Environment *);

#endif /* _H_factcom */


