   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*              RETE UTILITY HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed INCREMENTAL_RESET compilation flag.    */
/*                                                           */
/*            Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for join network changes.              */
/*                                                           */
/*            Support for using an asterick (*) to indicate  */
/*            that existential patterns are matched.         */
/*                                                           */
/*            Support for partial match changes.             */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Removed pseudo-facts used in not CEs.          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_reteutil

#pragma once

#define _H_reteutil

#include "evaluatn.h"
#include "match.h"
#include "network.h"

#define NETWORK_ASSERT  0
#define NETWORK_RETRACT 1

   void                           PrintPartialMatch(void *,const char *,struct partialMatch *);
   struct partialMatch           *CopyPartialMatch(void *,struct partialMatch *);
   struct partialMatch           *MergePartialMatches(void *,struct partialMatch *,struct partialMatch *);
   long int                       IncrementPseudoFactIndex(void);
   struct partialMatch           *GetAlphaMemory(void *,struct patternNodeHeader *,unsigned long);
   struct partialMatch           *GetLeftBetaMemory(struct joinNode *,unsigned long);
   struct partialMatch           *GetRightBetaMemory(struct joinNode *,unsigned long);
   void                           ReturnLeftMemory(void *,struct joinNode *);
   void                           ReturnRightMemory(void *,struct joinNode *);
   void                           DestroyBetaMemory(void *,struct joinNode *,int);
   void                           FlushBetaMemory(void *,struct joinNode *,int);
   bool                           BetaMemoryNotEmpty(struct joinNode *);
   void                           RemoveAlphaMemoryMatches(void *,struct patternNodeHeader *,struct partialMatch *,
                                                                  struct alphaMatch *); 
   void                           DestroyAlphaMemory(void *,struct patternNodeHeader *,bool);
   void                           FlushAlphaMemory(void *,struct patternNodeHeader *);
   void                           FlushAlphaBetaMemory(void *,struct partialMatch *);
   void                           DestroyAlphaBetaMemory(void *,struct partialMatch *);
   int                            GetPatternNumberFromJoin(struct joinNode *);
   struct multifieldMarker       *CopyMultifieldMarkers(void *,struct multifieldMarker *);
   struct partialMatch           *CreateAlphaMatch(void *,void *,struct multifieldMarker *,
                                                          struct patternNodeHeader *,unsigned long);
   void                           TraceErrorToRule(void *,struct joinNode *,const char *);
   void                           InitializePatternHeader(void *,struct patternNodeHeader *);
   void                           MarkRuleNetwork(void *,int);
   void                           TagRuleNetwork(void *,long *,long *,long *,long *);
   bool                           FindEntityInPartialMatch(struct patternEntity *,struct partialMatch *);
   unsigned long                  ComputeRightHashValue(void *,struct patternNodeHeader *);
   void                           UpdateBetaPMLinks(void *,struct partialMatch *,struct partialMatch *,struct partialMatch *,
                                                       struct joinNode *,unsigned long,int);
   void                           UnlinkBetaPMFromNodeAndLineage(void *,struct joinNode *,struct partialMatch *,int);
   void                           UnlinkNonLeftLineage(void *,struct joinNode *,struct partialMatch *,int);
   struct partialMatch           *CreateEmptyPartialMatch(void *);
   void                           MarkRuleJoins(struct joinNode *,int);
   void                           AddBlockedLink(struct partialMatch *,struct partialMatch *);
   void                           RemoveBlockedLink(struct partialMatch *);
   unsigned long                  PrintBetaMemory(void *,const char *,struct betaMemory *,bool,const char *,int);

#endif /* _H_reteutil */



