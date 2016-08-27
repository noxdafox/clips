   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
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

   bool                           AddLogicalDependencies(Environment *,struct patternEntity *,bool);
   void                           RemoveEntityDependencies(Environment *,struct patternEntity *);
   void                           RemovePMDependencies(Environment *,struct partialMatch *);
   void                           DestroyPMDependencies(Environment *,struct partialMatch *);
   void                           RemoveLogicalSupport(Environment *,struct partialMatch *);
   void                           ForceLogicalRetractions(Environment *);
   void                           Dependencies(Environment *,struct patternEntity *);
   void                           Dependents(Environment *,struct patternEntity *);
   void                           DependenciesCommand(Environment *,UDFContext *,CLIPSValue *);
   void                           DependentsCommand(Environment *,UDFContext *,CLIPSValue *);
   void                           ReturnEntityDependencies(Environment *,struct patternEntity *);
   struct partialMatch           *FindLogicalBind(struct joinNode *,struct partialMatch *);

#endif /* _H_lgcldpnd */





