   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                 BSAVE HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used genstrncpy instead of strncpy.            */
/*                                                           */
/*            Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
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

#ifndef _H_bsave

#pragma once

#define _H_bsave

struct BinaryItem;

#include <stdio.h>

#include "expressn.h"

struct BinaryItem
  {
   const char *name;
   void (*findFunction)(Environment *);
   void (*bloadStorageFunction)(Environment *);
   void (*bloadFunction)(Environment *);
   void (*clearFunction)(Environment *);
   void (*expressionFunction)(Environment *,FILE *);
   void (*bsaveStorageFunction)(Environment *,FILE *);
   void (*bsaveFunction)(Environment *,FILE *);
   int priority;
   struct BinaryItem *next;
  };

#if BLOAD_AND_BSAVE
typedef struct bloadcntsv
  {
   long val;
   struct bloadcntsv *nxt;
  } BLOADCNTSV;
#endif

typedef struct bsave_expr
  {
   unsigned short type;
   long value,arg_list,next_arg;
  } BSAVE_EXPRESSION;

#define CONSTRUCT_HEADER_SIZE 20

#define BSAVE_DATA 39

struct bsaveData
  {
   struct BinaryItem *ListOfBinaryItems;
#if BLOAD_AND_BSAVE
   BLOADCNTSV *BloadCountSaveTop;
#endif
  };

#define BsaveData(theEnv) ((struct bsaveData *) GetEnvironmentData(theEnv,BSAVE_DATA))

   void                    InitializeBsaveData(Environment *);
   void                    BsaveCommand(Environment *,UDFContext *,UDFValue *);
#if BLOAD_AND_BSAVE
   bool                    EnvBsave(Environment *,const char *);
   void                    MarkNeededItems(Environment *,struct expr *);
   void                    SaveBloadCount(Environment *,long);
   void                    RestoreBloadCount(Environment *,long *);
#endif
   bool                    AddBinaryItem(Environment *,const char *,int,
                                         void (*)(Environment *),
                                         void (*)(Environment *,FILE *),
                                         void (*)(Environment *,FILE *),
                                         void (*)(Environment *,FILE *),
                                         void (*)(Environment *),
                                         void (*)(Environment *),
                                         void (*)(Environment *));


#endif /* _H_bsave */







