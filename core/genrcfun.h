   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*                                                           */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed IMPERATIVE_METHODS compilation flag.   */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when DEBUGGING_FUNCTIONS   */
/*            is set to 0 and PROFILING_FUNCTIONS is set to  */
/*            1.                                             */
/*                                                           */
/*            Fixed typing issue when OBJECT_SYSTEM          */
/*            compiler flag is set to 0.                     */
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

#ifndef _H_genrcfun

#pragma once

#define _H_genrcfun

typedef struct defgenericModule DEFGENERIC_MODULE;
typedef struct restriction RESTRICTION;
typedef struct defmethod Defmethod;
typedef struct defgeneric Defgeneric;

#include <stdio.h>

#include "conscomp.h"
#include "constrct.h"
#include "expressn.h"
#include "evaluatn.h"
#include "moduldef.h"
#include "symbol.h"

#if OBJECT_SYSTEM
#include "object.h"
#endif

struct defgenericModule
  {
   struct defmoduleItemHeader header;
  };

struct restriction
  {
   void **types;
   EXPRESSION *query;
   short tcnt;
  };

struct defmethod
  {
   struct constructHeader header;
   short index;
   unsigned busy;
   short restrictionCount;
   short minRestrictions;
   short maxRestrictions;
   short localVarCount;
   unsigned system : 1;
   unsigned trace : 1;
   RESTRICTION *restrictions;
   EXPRESSION *actions;
  };

struct defgeneric
  {
   struct constructHeader header;
   unsigned busy;
   bool trace;
   Defmethod *methods;
   short mcnt;
   short new_index;
  };

#define DEFGENERIC_DATA 27

struct defgenericData
  {
   struct construct *DefgenericConstruct;
   int DefgenericModuleIndex;
   ENTITY_RECORD GenericEntityRecord;
#if DEBUGGING_FUNCTIONS
   bool WatchGenerics;
   bool WatchMethods;
#endif
   Defgeneric *CurrentGeneric;
   Defmethod *CurrentMethod;
   UDFValue *GenericCurrentArgument;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   unsigned OldGenericBusySave;
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DefgenericCodeItem;
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
   struct token GenericInputToken;
#endif
  };

#define DefgenericData(theEnv) ((struct defgenericData *) GetEnvironmentData(theEnv,DEFGENERIC_DATA))
#define SaveBusyCount(gfunc)    (DefgenericData(theEnv)->OldGenericBusySave = gfunc->busy)
#define RestoreBusyCount(gfunc) (gfunc->busy = DefgenericData(theEnv)->OldGenericBusySave)

#if ! RUN_TIME
   bool                           ClearDefgenericsReady(Environment *);
   void                          *AllocateDefgenericModule(Environment *);
   void                           FreeDefgenericModule(Environment *,void *);
#else
   void                           DefgenericRunTimeInitialize(Environment *);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

   bool                           ClearDefmethods(Environment *);
   bool                           RemoveAllExplicitMethods(Environment *,Defgeneric *);
   void                           RemoveDefgeneric(Environment *,Defgeneric *);
   bool                           ClearDefgenerics(Environment *);
   void                           MethodAlterError(Environment *,Defgeneric *);
   void                           DeleteMethodInfo(Environment *,Defgeneric *,Defmethod *);
   void                           DestroyMethodInfo(Environment *,Defgeneric *,Defmethod *);
   bool                           MethodsExecuting(Defgeneric *);
#endif
#if ! OBJECT_SYSTEM
   bool                           SubsumeType(int,int);
#endif

   long                           FindMethodByIndex(Defgeneric *,long);
#if DEBUGGING_FUNCTIONS || PROFILING_FUNCTIONS
   void                           PrintMethod(Environment *,char *,size_t,Defmethod *);
#endif
#if DEBUGGING_FUNCTIONS
   void                           PreviewGeneric(Environment *,UDFContext *,UDFValue *);
#endif
   Defgeneric                    *CheckGenericExists(Environment *,const char *,const char *);
   long                           CheckMethodExists(Environment *,const char *,Defgeneric *,long);

#if ! OBJECT_SYSTEM
   const char                    *TypeName(Environment *,int);
#endif

   void                           PrintGenericName(Environment *,const char *,Defgeneric *);

#endif /* _H_genrcfun */

