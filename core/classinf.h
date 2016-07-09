   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/04/16            */
   /*                                                     */
   /*     CLASS INFO PROGRAMMATIC ACCESS HEADER FILE      */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                            */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Changed integer type/precision.                */
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

#ifndef _H_classinf

#pragma once

#define _H_classinf

#include "evaluatn.h"

   bool                           ClassAbstractPCommand(void *);
#if DEFRULE_CONSTRUCT
   bool                           ClassReactivePCommand(void *);
#endif
   void                          *ClassInfoFnxArgs(void *,const char *,bool *);
   void                           ClassSlotsCommand(void *,DATA_OBJECT *);
   void                           ClassSuperclassesCommand(void *,DATA_OBJECT *);
   void                           ClassSubclassesCommand(void *,DATA_OBJECT *);
   void                           GetDefmessageHandlersListCmd(void *,DATA_OBJECT *);
   void                           SlotFacetsCommand(void *,DATA_OBJECT *);
   void                           SlotSourcesCommand(void *,DATA_OBJECT *);
   void                           SlotTypesCommand(void *,DATA_OBJECT *);
   void                           SlotAllowedValuesCommand(void *,DATA_OBJECT *);
   void                           SlotAllowedClassesCommand(void *,DATA_OBJECT *);
   void                           SlotRangeCommand(void *,DATA_OBJECT *);
   void                           SlotCardinalityCommand(void *,DATA_OBJECT *);
   bool                           EnvClassAbstractP(void *,void *);
#if DEFRULE_CONSTRUCT
   bool                           EnvClassReactiveP(void *,void *);
#endif
   void                           EnvClassSlots(void *,void *,DATA_OBJECT *,bool);
   void                           EnvGetDefmessageHandlerList(void *,void *,DATA_OBJECT *,bool);
   void                           EnvClassSuperclasses(void *,void *,DATA_OBJECT *,bool);
   void                           EnvClassSubclasses(void *,void *,DATA_OBJECT *,bool);
   void                           ClassSubclassAddresses(void *,void *,DATA_OBJECT *,bool);
   void                           EnvSlotFacets(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotSources(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotTypes(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotAllowedValues(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotAllowedClasses(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotRange(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotCardinality(void *,void *,const char *,DATA_OBJECT *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           ClassAbstractP(void *);
#if DEFRULE_CONSTRUCT
   bool                           ClassReactiveP(void *);
#endif
   void                           ClassSlots(void *,DATA_OBJECT *,int);
   void                           ClassSubclasses(void *,DATA_OBJECT *,int);
   void                           ClassSuperclasses(void *,DATA_OBJECT *,int);
   void                           SlotAllowedValues(void *,const char *,DATA_OBJECT *);
   void                           SlotAllowedClasses(void *,const char *,DATA_OBJECT *);
   void                           SlotCardinality(void *,const char *,DATA_OBJECT *);
   void                           SlotFacets(void *,const char *,DATA_OBJECT *);
   void                           SlotRange(void *,const char *,DATA_OBJECT *);
   void                           SlotSources(void *,const char *,DATA_OBJECT *);
   void                           SlotTypes(void *,const char *,DATA_OBJECT *);
   void                           GetDefmessageHandlerList(void *,DATA_OBJECT *,int);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_classinf */





