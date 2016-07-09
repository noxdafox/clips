   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
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
/*************************************************************/

#ifndef _H_factcom

#pragma once

#define _H_factcom

#include "evaluatn.h"

   void                           FactCommandDefinitions(void *);
   void                           AssertCommand(void *,DATA_OBJECT_PTR);
   void                           RetractCommand(void *);
   void                           AssertStringFunction(void *,DATA_OBJECT_PTR);
   void                           FactsCommand(void *);
   void                           EnvFacts(void *,const char *,void *,long long,long long,long long);
   bool                           SetFactDuplicationCommand(void *);
   bool                           GetFactDuplicationCommand(void *);
   bool                           SaveFactsCommand(void *);
   bool                           LoadFactsCommand(void *);
   bool                           EnvSaveFacts(void *,const char *,int);
   bool                           EnvSaveFactsDriver(void *,const char *,int,struct expr *);
   bool                           EnvLoadFacts(void *,const char *);
   bool                           EnvLoadFactsFromString(void *,const char *,long);
   long long                      FactIndexFunction(void *);

#if ALLOW_ENVIRONMENT_GLOBALS

#if DEBUGGING_FUNCTIONS
   void                           Facts(const char *,void *,long long,long long,long long);
#endif
   bool                           LoadFacts(const char *);
   bool                           SaveFacts(const char *,int);
   bool                           LoadFactsFromString(const char *,int);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_factcom */


