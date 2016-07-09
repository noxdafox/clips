   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*          LOGICAL DEPENDENCIES HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provide support routines for managing truth      */
/*   maintenance using the logical conditional element.      */
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
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_lgcldpnd

#pragma once

#define _H_lgcldpnd

struct dependency
  {
   void *dPtr;
   struct dependency *next;
  };

#include "match.h"
#include "pattern.h"

   bool                           AddLogicalDependencies(void *,struct patternEntity *,bool);
   void                           RemoveEntityDependencies(void *,struct patternEntity *);
   void                           RemovePMDependencies(void *,struct partialMatch *);
   void                           DestroyPMDependencies(void *,struct partialMatch *);
   void                           RemoveLogicalSupport(void *,struct partialMatch *);
   void                           ForceLogicalRetractions(void *);
   void                           Dependencies(void *,struct patternEntity *);
   void                           Dependents(void *,struct patternEntity *);
   void                           DependenciesCommand(void *);
   void                           DependentsCommand(void *);
   void                           ReturnEntityDependencies(void *,struct patternEntity *);
   struct partialMatch           *FindLogicalBind(struct joinNode *,struct partialMatch *);

#endif /* _H_lgcldpnd */





