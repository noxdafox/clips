   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                                                     */
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
/*************************************************************/

#ifndef _H_classinf
#define _H_classinf

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

   intBool                        ClassAbstractPCommand(void *);
#if DEFRULE_CONSTRUCT
   intBool                        ClassReactivePCommand(void *);
#endif
   void                          *ClassInfoFnxArgs(void *,const char *,int *);
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
   intBool                        EnvClassAbstractP(void *,void *);
#if DEFRULE_CONSTRUCT
   intBool                        EnvClassReactiveP(void *,void *);
#endif
   void                           EnvClassSlots(void *,void *,DATA_OBJECT *,int);
   void                           EnvGetDefmessageHandlerList(void *,void *,DATA_OBJECT *,int);
   void                           EnvClassSuperclasses(void *,void *,DATA_OBJECT *,int);
   void                           EnvClassSubclasses(void *,void *,DATA_OBJECT *,int);
   void                           ClassSubclassAddresses(void *,void *,DATA_OBJECT *,int);
   void                           EnvSlotFacets(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotSources(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotTypes(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotAllowedValues(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotAllowedClasses(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotRange(void *,void *,const char *,DATA_OBJECT *);
   void                           EnvSlotCardinality(void *,void *,const char *,DATA_OBJECT *);

#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                        ClassAbstractP(void *);
#if DEFRULE_CONSTRUCT
   intBool                        ClassReactiveP(void *);
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





