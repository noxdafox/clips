   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_classinf

#pragma once

#define _H_classinf

#include "evaluatn.h"

   bool                           ClassAbstractPCommand(Environment *);
#if DEFRULE_CONSTRUCT
   bool                           ClassReactivePCommand(Environment *);
#endif
   Defclass                      *ClassInfoFnxArgs(Environment *,const char *,bool *);
   void                           ClassSlotsCommand(Environment *,DATA_OBJECT *);
   void                           ClassSuperclassesCommand(Environment *,DATA_OBJECT *);
   void                           ClassSubclassesCommand(Environment *,DATA_OBJECT *);
   void                           GetDefmessageHandlersListCmd(Environment *,DATA_OBJECT *);
   void                           SlotFacetsCommand(Environment *,DATA_OBJECT *);
   void                           SlotSourcesCommand(Environment *,DATA_OBJECT *);
   void                           SlotTypesCommand(Environment *,DATA_OBJECT *);
   void                           SlotAllowedValuesCommand(Environment *,DATA_OBJECT *);
   void                           SlotAllowedClassesCommand(Environment *,DATA_OBJECT *);
   void                           SlotRangeCommand(Environment *,DATA_OBJECT *);
   void                           SlotCardinalityCommand(Environment *,DATA_OBJECT *);
   bool                           EnvClassAbstractP(Environment *,Defclass *);
#if DEFRULE_CONSTRUCT
   bool                           EnvClassReactiveP(Environment *,Defclass *);
#endif
   void                           EnvClassSlots(Environment *,Defclass *,DATA_OBJECT *,bool);
   void                           EnvGetDefmessageHandlerList(Environment *,Defclass *,DATA_OBJECT *,bool);
   void                           EnvClassSuperclasses(Environment *,Defclass *,DATA_OBJECT *,bool);
   void                           EnvClassSubclasses(Environment *,Defclass *,DATA_OBJECT *,bool);
   void                           ClassSubclassAddresses(Environment *,Defclass *,DATA_OBJECT *,bool);
   void                           EnvSlotFacets(Environment *,Defclass *,const char *,DATA_OBJECT *);
   void                           EnvSlotSources(Environment *,Defclass *,const char *,DATA_OBJECT *);
   void                           EnvSlotTypes(Environment *,Defclass *,const char *,DATA_OBJECT *);
   void                           EnvSlotAllowedValues(Environment *,Defclass *,const char *,DATA_OBJECT *);
   void                           EnvSlotAllowedClasses(Environment *,Defclass *,const char *,DATA_OBJECT *);
   void                           EnvSlotRange(Environment *,Defclass *,const char *,DATA_OBJECT *);
   void                           EnvSlotCardinality(Environment *,Defclass *,const char *,DATA_OBJECT *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           ClassAbstractP(Defclass *);
#if DEFRULE_CONSTRUCT
   bool                           ClassReactiveP(Defclass *);
#endif
   void                           ClassSlots(Defclass *,DATA_OBJECT *,bool);
   void                           ClassSubclasses(Defclass *,DATA_OBJECT *,bool);
   void                           ClassSuperclasses(Defclass *,DATA_OBJECT *,bool);
   void                           SlotAllowedValues(Defclass *,const char *,DATA_OBJECT *);
   void                           SlotAllowedClasses(Defclass *,const char *,DATA_OBJECT *);
   void                           SlotCardinality(Defclass *,const char *,DATA_OBJECT *);
   void                           SlotFacets(Defclass *,const char *,DATA_OBJECT *);
   void                           SlotRange(Defclass *,const char *,DATA_OBJECT *);
   void                           SlotSources(Defclass *,const char *,DATA_OBJECT *);
   void                           SlotTypes(Defclass *,const char *,DATA_OBJECT *);
   void                           GetDefmessageHandlerList(Defclass *,DATA_OBJECT *,bool);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_classinf */





