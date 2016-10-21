   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_classinf

#pragma once

#define _H_classinf

#include "evaluatn.h"

   void                           ClassAbstractPCommand(Environment *,UDFContext *,UDFValue *);
#if DEFRULE_CONSTRUCT
   void                           ClassReactivePCommand(Environment *,UDFContext *,UDFValue *);
#endif
   Defclass                      *ClassInfoFnxArgs(UDFContext *,const char *,bool *);
   void                           ClassSlotsCommand(Environment *,UDFContext *,UDFValue *);
   void                           ClassSuperclassesCommand(Environment *,UDFContext *,UDFValue *);
   void                           ClassSubclassesCommand(Environment *,UDFContext *,UDFValue *);
   void                           GetDefmessageHandlersListCmd(Environment *,UDFContext *,UDFValue *);
   void                           SlotFacetsCommand(Environment *,UDFContext *,UDFValue *);
   void                           SlotSourcesCommand(Environment *,UDFContext *,UDFValue *);
   void                           SlotTypesCommand(Environment *,UDFContext *,UDFValue *);
   void                           SlotAllowedValuesCommand(Environment *,UDFContext *,UDFValue *);
   void                           SlotAllowedClassesCommand(Environment *,UDFContext *,UDFValue *);
   void                           SlotRangeCommand(Environment *,UDFContext *,UDFValue *);
   void                           SlotCardinalityCommand(Environment *,UDFContext *,UDFValue *);
   bool                           EnvClassAbstractP(Environment *,Defclass *);
#if DEFRULE_CONSTRUCT
   bool                           EnvClassReactiveP(Environment *,Defclass *);
#endif
   void                           EnvClassSlots(Environment *,Defclass *,CLIPSValue *,bool);
   void                           EnvGetDefmessageHandlerList(Environment *,Defclass *,CLIPSValue *,bool);
   void                           EnvClassSuperclasses(Environment *,Defclass *,CLIPSValue *,bool);
   void                           EnvClassSubclasses(Environment *,Defclass *,CLIPSValue *,bool);
   void                           ClassSubclassAddresses(Environment *,Defclass *,UDFValue *,bool);
   void                           EnvSlotFacets(Environment *,Defclass *,const char *,CLIPSValue *);
   void                           EnvSlotSources(Environment *,Defclass *,const char *,CLIPSValue *);
   void                           EnvSlotTypes(Environment *,Defclass *,const char *,CLIPSValue *);
   void                           EnvSlotAllowedValues(Environment *,Defclass *,const char *,CLIPSValue *);
   void                           EnvSlotAllowedClasses(Environment *,Defclass *,const char *,CLIPSValue *);
   void                           EnvSlotRange(Environment *,Defclass *,const char *,CLIPSValue *);
   void                           EnvSlotCardinality(Environment *,Defclass *,const char *,CLIPSValue *);

#endif /* _H_classinf */





