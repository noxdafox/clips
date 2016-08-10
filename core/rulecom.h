   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/06/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
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

   bool                           EnvGetBetaMemoryResizing(Environment *);
   bool                           EnvSetBetaMemoryResizing(Environment *,bool);
   bool                           GetBetaMemoryResizingCommand(Environment *);
   bool                           SetBetaMemoryResizingCommand(Environment *);

   void                           EnvMatches(Environment *,Defrule *,int,DATA_OBJECT *);
   void                           EnvJoinActivity(Environment *,Defrule *,int,DATA_OBJECT *);
   void                           DefruleCommands(Environment *);
   void                           MatchesCommand(Environment *,DATA_OBJECT *);
   void                           JoinActivityCommand(Environment *,DATA_OBJECT *);
   long long                      TimetagFunction(Environment *);
   long                           EnvAlphaJoinCount(Environment *,Defrule *);
   long                           EnvBetaJoinCount(Environment *,Defrule *);
   struct joinInformation        *EnvCreateJoinArray(Environment *,long);
   void                           EnvFreeJoinArray(Environment *,struct joinInformation *,long);
   void                           EnvAlphaJoins(Environment *,Defrule *,long,struct joinInformation *);
   void                           EnvBetaJoins(Environment *,Defrule *,long,struct joinInformation *);
   void                           JoinActivityResetCommand(Environment *);
#if DEVELOPER
   void                           ShowJoinsCommand(Environment *);
   long                           RuleComplexityCommand(Environment *);
   void                           ShowAlphaHashTable(Environment *);
#endif

#endif /* _H_rulecom */
