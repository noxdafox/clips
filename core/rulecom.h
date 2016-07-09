   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*             DEFRULE COMMANDS HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the matches command. Also provides the  */
/*   the developer commands show-joins and rule-complexity.  */
/*   Also provides the initialization routine which          */
/*   registers rule commands found in other modules.         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES         */
/*            INCREMENTAL_RESET, and LOGICAL_DEPENDENCIES    */
/*            compilation flags.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Improvements to matches command.               */
/*                                                           */
/*            Add join-activity and join-activity-reset      */
/*            commands.                                      */
/*                                                           */
/*            Added get-beta-memory-resizing and             */
/*            set-beta-memory-resizing functions.            */
/*                                                           */
/*            Added timetag function.                        */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_rulecom

#pragma once

#define _H_rulecom

#include "evaluatn.h"

struct joinInformation
  {
   int whichCE;
   struct joinNode *theJoin;
   int patternBegin;
   int patternEnd;
   int marked;
   struct betaMemory *theMemory;
   struct joinNode *nextJoin;
  };

#define VERBOSE  0
#define SUCCINCT 1
#define TERSE    2

   bool                           EnvGetBetaMemoryResizing(void *);
   bool                           EnvSetBetaMemoryResizing(void *,bool);
   bool                           GetBetaMemoryResizingCommand(void *);
   bool                           SetBetaMemoryResizingCommand(void *);

   void                           EnvMatches(void *,void *,int,DATA_OBJECT *);
   void                           EnvJoinActivity(void *,void *,int,DATA_OBJECT *);
   void                           DefruleCommands(void *);
   void                           MatchesCommand(void *,DATA_OBJECT *);
   void                           JoinActivityCommand(void *,DATA_OBJECT *);
   long long                      TimetagFunction(void *);
   long                           EnvAlphaJoinCount(void *,void *);
   long                           EnvBetaJoinCount(void *,void *);
   struct joinInformation        *EnvCreateJoinArray(void *,long);
   void                           EnvFreeJoinArray(void *,struct joinInformation *,long);
   void                           EnvAlphaJoins(void *,void *,long,struct joinInformation *);
   void                           EnvBetaJoins(void *,void *,long,struct joinInformation *);
   void                           JoinActivityResetCommand(void *);
#if DEVELOPER
   void                           ShowJoinsCommand(void *);
   long                           RuleComplexityCommand(void *);
   void                           ShowAlphaHashTable(void *);
#endif

#if ALLOW_ENVIRONMENT_GLOBALS

#if DEBUGGING_FUNCTIONS
   void                           Matches(void *,int,DATA_OBJECT *);
   void                           JoinActivity(void *,int,DATA_OBJECT *);
#endif
   bool                           GetBetaMemoryResizing(void);
   bool                           SetBetaMemoryResizing(bool);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_rulecom */
