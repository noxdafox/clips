   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                RETRACT HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Handles join network activity associated with   */
/*   with the removal of a data entity such as a fact or     */
/*   instance.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Rule with exists CE has incorrect activation.  */
/*            DR0867                                         */
/*                                                           */
/*      6.30: Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Removed pseudo-facts used in not CEs.          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_retract

#pragma once

#define _H_retract

#include "match.h"
#include "network.h"

struct rdriveinfo
  {
   struct partialMatch *link;
   struct joinNode *jlist;
   struct rdriveinfo *next;
  };

void                           NetworkRetract(void *,struct patternMatch *);
void                           ReturnPartialMatch(void *,struct partialMatch *);
void                           DestroyPartialMatch(void *,struct partialMatch *);
void                           FlushGarbagePartialMatches(void *);
void                           DeletePartialMatches(void *,struct partialMatch *);
void                           PosEntryRetractBeta(void *,struct partialMatch *,struct partialMatch *,int);
void                           PosEntryRetractAlpha(void *,struct partialMatch *,int);
intBool                        PartialMatchWillBeDeleted(void *,struct partialMatch *);

#endif /* _H_retract */



