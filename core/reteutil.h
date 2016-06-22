   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_reteutil
#define _H_reteutil

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_network
#include "network.h"
#endif

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
   intBool                        BetaMemoryNotEmpty(struct joinNode *);
   void                           RemoveAlphaMemoryMatches(void *,struct patternNodeHeader *,struct partialMatch *,
                                                                  struct alphaMatch *); 
   void                           DestroyAlphaMemory(void *,struct patternNodeHeader *,int);
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
   int                            FindEntityInPartialMatch(struct patternEntity *,struct partialMatch *);
   unsigned long                  ComputeRightHashValue(void *,struct patternNodeHeader *);
   void                           UpdateBetaPMLinks(void *,struct partialMatch *,struct partialMatch *,struct partialMatch *,
                                                       struct joinNode *,unsigned long,int);
   void                           UnlinkBetaPMFromNodeAndLineage(void *,struct joinNode *,struct partialMatch *,int);
   void                           UnlinkNonLeftLineage(void *,struct joinNode *,struct partialMatch *,int);
   struct partialMatch           *CreateEmptyPartialMatch(void *);
   void                           MarkRuleJoins(struct joinNode *,int);
   void                           AddBlockedLink(struct partialMatch *,struct partialMatch *);
   void                           RemoveBlockedLink(struct partialMatch *);
   unsigned long                  PrintBetaMemory(void *,const char *,struct betaMemory *,int,const char *,int);

#endif /* _H_reteutil */



