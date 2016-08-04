   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*               INSTANCE FUNCTIONS MODULE             */
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
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Link error occurs for the SlotExistError       */
/*            function when OBJECT_SYSTEM is set to 0 in     */
/*            setup.h. DR0865                                */
/*                                                           */
/*            Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Moved EvaluateAndStoreInDataObject to          */
/*            evaluatn.c                                     */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed slot override default ?NONE bug.         */
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

#ifndef _H_insfun

#pragma once

#define _H_insfun

#include "evaluatn.h"
#include "moduldef.h"
#include "object.h"
#include "pattern.h"

typedef struct igarbage
  {
   Instance *ins;
   struct igarbage *nxt;
  } IGARBAGE;

#define INSTANCE_TABLE_HASH_SIZE 8191
#define InstanceSizeHeuristic(ins)      sizeof(Instance)

   void                           EnvIncrementInstanceCount(Environment *,Instance *);
   void                           EnvDecrementInstanceCount(Environment *,Instance *);
   void                           InitializeInstanceTable(Environment *);
   void                           CleanupInstances(Environment *);
   unsigned                       HashInstance(SYMBOL_HN *);
   void                           DestroyAllInstances(Environment *);
   void                           RemoveInstanceData(Environment *,Instance *);
   Instance                      *FindInstanceBySymbol(Environment *,SYMBOL_HN *);
   Instance                      *FindInstanceInModule(Environment *,SYMBOL_HN *,Defmodule *,
                                                       Defmodule *,bool);
   INSTANCE_SLOT                 *FindInstanceSlot(Environment *,Instance *,SYMBOL_HN *);
   int                            FindInstanceTemplateSlot(Environment *,Defclass *,SYMBOL_HN *);
   bool                           PutSlotValue(Environment *,Instance *,INSTANCE_SLOT *,DATA_OBJECT *,DATA_OBJECT *,const char *);
   bool                           DirectPutSlotValue(Environment *,Instance *,INSTANCE_SLOT *,DATA_OBJECT *,DATA_OBJECT *);
   bool                           ValidSlotValue(Environment *,DATA_OBJECT *,SlotDescriptor *,Instance *,const char *);
   Instance                      *CheckInstance(Environment *,const char *);
   void                           NoInstanceError(Environment *,const char *,const char *);
   void                           StaleInstanceAddress(Environment *,const char *,int);
   bool                           EnvGetInstancesChanged(Environment *);
   void                           EnvSetInstancesChanged(Environment *,bool);
   void                           PrintSlot(Environment *,const char *,SlotDescriptor *,Instance *,const char *);
   void                           PrintInstanceNameAndClass(Environment *,const char *,Instance *,bool);
   void                           PrintInstanceName(Environment *,const char *,Instance *);
   void                           PrintInstanceLongForm(Environment *,const char *,Instance *);
#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           DecrementObjectBasisCount(Environment *,Instance *);
   void                           IncrementObjectBasisCount(Environment *,Instance *);
   void                           MatchObjectFunction(Environment *,Instance *);
   bool                           NetworkSynchronized(Environment *,Instance *);
   bool                           InstanceIsDeleted(Environment *,Instance *);
#endif

#if ALLOW_ENVIRONMENT_GLOBALS

   void                           DecrementInstanceCount(Instance *);
   bool                           GetInstancesChanged(void);
   void                           IncrementInstanceCount(Instance *);
   void                           SetInstancesChanged(bool);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_insfun */







