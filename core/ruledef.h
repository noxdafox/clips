   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                 DEFRULE HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defrule primitive functions such   */
/*   as allocating and deallocating, traversing, and finding */
/*   defrule data structures.                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed DYNAMIC_SALIENCE and                   */
/*            LOGICAL_DEPENDENCIES compilation flags.        */
/*                                                           */
/*            Removed CONFLICT_RESOLUTION_STRATEGIES         */
/*            compilation flag.                              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*            Added EnvGetDisjunctCount and                  */
/*            EnvGetNthDisjunct functions.                   */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_ruledef

#pragma once

#define _H_ruledef

#define GetDisjunctIndex(r) ((struct constructHeader *) r)->bsaveID

struct defrule;
struct defruleModule;

#include "constrct.h"
#include "expressn.h"
#include "network.h"
#include "ruledef.h"

struct defrule
  {
   struct constructHeader header;
   int salience;
   int localVarCnt;
   unsigned int complexity      : 11;
   unsigned int afterBreakpoint :  1;
   unsigned int watchActivation :  1;
   unsigned int watchFiring     :  1;
   unsigned int autoFocus       :  1;
   unsigned int executing       :  1;
   struct expr *dynamicSalience;
   struct expr *actions;
   struct joinNode *logicalJoin;
   struct joinNode *lastJoin;
   struct defrule *disjunct;
  };

#include "agenda.h"
#include "conscomp.h"
#include "constrnt.h"
#include "cstrccom.h"
#include "evaluatn.h"
#include "moduldef.h"
#include "symbol.h"

struct defruleModule
  {
   struct defmoduleItemHeader header;
   struct salienceGroup *groupings;
   struct activation *agenda;
  };

#ifndef ALPHA_MEMORY_HASH_SIZE
#define ALPHA_MEMORY_HASH_SIZE       63559L
#endif

#define DEFRULE_DATA 16

struct defruleData
  { 
   struct construct *DefruleConstruct;
   int DefruleModuleIndex;
   long long CurrentEntityTimeTag;
   struct alphaMemoryHash **AlphaMemoryTable;
   bool BetaMemoryResizingFlag;
   struct joinLink *RightPrimeJoins;
   struct joinLink *LeftPrimeJoins;

#if DEBUGGING_FUNCTIONS
    bool WatchRules;
    int DeletedRuleDebugFlags;
#endif
#if DEVELOPER && (! RUN_TIME) && (! BLOAD_ONLY)
    bool WatchRuleAnalysis;
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DefruleCodeItem;
#endif
  };

#define DefruleData(theEnv) ((struct defruleData *) GetEnvironmentData(theEnv,DEFRULE_DATA))

#define GetPreviousJoin(theJoin) \
   (((theJoin)->joinFromTheRight) ? \
    ((struct joinNode *) (theJoin)->rightSideEntryStructure) : \
    ((theJoin)->lastLevel))
#define GetPatternForJoin(theJoin) \
   (((theJoin)->joinFromTheRight) ? \
    NULL : \
    ((theJoin)->rightSideEntryStructure))

   void                           InitializeDefrules(void *);
   void                          *EnvFindDefrule(void *,const char *);
   void                          *EnvFindDefruleInModule(void *,const char *);
   void                          *EnvGetNextDefrule(void *,void *);
   struct defruleModule          *GetDefruleModuleItem(void *,struct defmodule *);
   bool                           EnvIsDefruleDeletable(void *,void *);
#if RUN_TIME
   void                           DefruleRunTimeInitialize(void *,struct joinLink *,struct joinLink *);
#endif
#if RUN_TIME || BLOAD_ONLY || BLOAD || BLOAD_AND_BSAVE
   void                           AddBetaMemoriesToJoin(void *,struct joinNode *);
#endif
   long                           EnvGetDisjunctCount(void *,void *);
   void                          *EnvGetNthDisjunct(void *,void *,long);
   const char                    *EnvDefruleModule(void *,void *);
   const char                    *EnvGetDefruleName(void *,void *);
   const char                    *EnvGetDefrulePPForm(void *,void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   const char                    *DefruleModule(void *);
   void                          *FindDefrule(const char *);
   const char                    *GetDefruleName(void *);
   const char                    *GetDefrulePPForm(void *);
   void                          *GetNextDefrule(void *);
   bool                           IsDefruleDeletable(void *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */


#endif /* _H_ruledef */


