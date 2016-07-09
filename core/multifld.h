   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                MULTIFIELD HEADER FILE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for creating and manipulating           */
/*   multifield values.                                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*            Moved ImplodeMultifield from multifun.c.       */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Used DataObjectToString instead of             */
/*            ValueToString in implode$ to handle            */
/*            print representation of external addresses.    */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed issue with StoreInMultifield when        */
/*            asserting void values in implied deftemplate   */
/*            facts.                                         */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_multifld

#pragma once

#define _H_multifld

struct field;
struct multifield;

#include "evaluatn.h"

struct field
  {
   unsigned short type;
   void *value;
  };

struct multifield
  {
   unsigned busyCount;
   long multifieldLength;
   struct multifield *next;
   struct field theFields[1];
  };

typedef struct multifield SEGMENT;
typedef struct multifield * SEGMENT_PTR;
typedef struct multifield * MULTIFIELD_PTR;
typedef struct field FIELD;
typedef struct field * FIELD_PTR;

#define GetMFLength(target)     (((struct multifield *) (target))->multifieldLength)
#define GetMFPtr(target,index)  (&(((struct field *) ((struct multifield *) (target))->theFields)[index-1]))
#define SetMFType(target,index,value)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type = (unsigned short) (value))
#define SetMFValue(target,index,val)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value = (void *) (val))
#define GetMFType(target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type)
#define GetMFValue(target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value)

#define EnvGetMFLength(theEnv,target)     (((struct multifield *) (target))->multifieldLength)
#define EnvGetMFPtr(theEnv,target,index)  (&(((struct field *) ((struct multifield *) (target))->theFields)[index-1]))
#define EnvSetMFType(theEnv,target,index,value)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type = (unsigned short) (value))
#define EnvSetMFValue(theEnv,target,index,val)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value = (void *) (val))
#define EnvGetMFType(theEnv,target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].type)
#define EnvGetMFValue(theEnv,target,index)  (((struct field *) ((struct multifield *) (target))->theFields)[index-1].value)

   void                          *CreateMultifield2(void *,long);
   void                           ReturnMultifield(void *,struct multifield *);
   void                           MultifieldInstall(void *,struct multifield *);
   void                           MultifieldDeinstall(void *,struct multifield *);
   struct multifield             *StringToMultifield(void *,const char *);
   void                          *EnvCreateMultifield(void *,long);
   void                           AddToMultifieldList(void *,struct multifield *);
   void                           FlushMultifields(void *);
   void                           DuplicateMultifield(void *,struct dataObject *,struct dataObject *);
   void                           PrintMultifield(void *,const char *,SEGMENT_PTR,long,long,bool);
   bool                           MultifieldDOsEqual(DATA_OBJECT_PTR,DATA_OBJECT_PTR);
   void                           StoreInMultifield(void *,DATA_OBJECT *,EXPRESSION *,bool);
   void                          *CopyMultifield(void *,struct multifield *);
   bool                           MultifieldsEqual(struct multifield *,struct multifield *);
   void                          *DOToMultifield(void *,DATA_OBJECT *);
   unsigned long                  HashMultifield(struct multifield *,unsigned long);
   struct multifield             *GetMultifieldList(void *);
   void                          *ImplodeMultifield(void *,DATA_OBJECT *);
   void                           EphemerateMultifield(void *,struct multifield *);

#if ALLOW_ENVIRONMENT_GLOBALS

   void                          *CreateMultifield(long);
   
#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_multifld */




