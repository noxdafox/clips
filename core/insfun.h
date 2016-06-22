   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                INSTANCE FUNCTIONS MODULE            */
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
/*************************************************************/

#ifndef _H_insfun
#define _H_insfun

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_object
#include "object.h"
#endif

#ifndef _H_pattern
#include "pattern.h"
#endif

typedef struct igarbage
  {
   INSTANCE_TYPE *ins;
   struct igarbage *nxt;
  } IGARBAGE;

#define INSTANCE_TABLE_HASH_SIZE 8191
#define InstanceSizeHeuristic(ins)      sizeof(INSTANCE_TYPE)

   void                           EnvIncrementInstanceCount(void *,void *);
   void                           EnvDecrementInstanceCount(void *,void *);
   void                           InitializeInstanceTable(void *);
   void                           CleanupInstances(void *);
   unsigned                       HashInstance(SYMBOL_HN *);
   void                           DestroyAllInstances(void *);
   void                           RemoveInstanceData(void *,INSTANCE_TYPE *);
   INSTANCE_TYPE                 *FindInstanceBySymbol(void *,SYMBOL_HN *);
   INSTANCE_TYPE                 *FindInstanceInModule(void *,SYMBOL_HN *,struct defmodule *,
                                           struct defmodule *,unsigned);
   INSTANCE_SLOT                 *FindInstanceSlot(void *,INSTANCE_TYPE *,SYMBOL_HN *);
   int                            FindInstanceTemplateSlot(void *,DEFCLASS *,SYMBOL_HN *);
   int                            PutSlotValue(void *,INSTANCE_TYPE *,INSTANCE_SLOT *,DATA_OBJECT *,DATA_OBJECT *,const char *);
   int                            DirectPutSlotValue(void *,INSTANCE_TYPE *,INSTANCE_SLOT *,DATA_OBJECT *,DATA_OBJECT *);
   intBool                        ValidSlotValue(void *,DATA_OBJECT *,SLOT_DESC *,INSTANCE_TYPE *,const char *);
   INSTANCE_TYPE                 *CheckInstance(void *,const char *);
   void                           NoInstanceError(void *,const char *,const char *);
   void                           StaleInstanceAddress(void *,const char *,int);
   int                            EnvGetInstancesChanged(void *);
   void                           EnvSetInstancesChanged(void *,int);
   void                           PrintSlot(void *,const char *,SLOT_DESC *,INSTANCE_TYPE *,const char *);
   void                           PrintInstanceNameAndClass(void *,const char *,INSTANCE_TYPE *,intBool);
   void                           PrintInstanceName(void *,const char *,void *);
   void                           PrintInstanceLongForm(void *,const char *,void *);
#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           DecrementObjectBasisCount(void *,void *);
   void                           IncrementObjectBasisCount(void *,void *);
   void                           MatchObjectFunction(void *,void *);
   intBool                        NetworkSynchronized(void *,void *);
   intBool                        InstanceIsDeleted(void *,void *);
#endif

#if ALLOW_ENVIRONMENT_GLOBALS

   void                           DecrementInstanceCount(void *);
   int                            GetInstancesChanged(void);
   void                           IncrementInstanceCount(void *);
   void                           SetInstancesChanged(int);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_insfun */







