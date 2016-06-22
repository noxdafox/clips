   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_bsave
#define _H_bsave

struct BinaryItem;

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#ifndef _H_expressn
#include "expressn.h"
#endif

struct BinaryItem
  {
   const char *name;
   void (*findFunction)(void *);
   void (*bloadStorageFunction)(void *);
   void (*bloadFunction)(void *);
   void (*clearFunction)(void *);
   void (*expressionFunction)(void *,FILE *);
   void (*bsaveStorageFunction)(void *,FILE *);
   void (*bsaveFunction)(void *,FILE *);
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

   void                    InitializeBsaveData(void *);
   int                     BsaveCommand(void *);
#if BLOAD_AND_BSAVE
   intBool                 EnvBsave(void *,const char *);
   void                    MarkNeededItems(void *,struct expr *);
   void                    SaveBloadCount(void *,long);
   void                    RestoreBloadCount(void *,long *);
#endif
   intBool                 AddBinaryItem(void *,const char *,int,
                                                void (*)(void *),
                                                void (*)(void *,FILE *),
                                                void (*)(void *,FILE *),
                                                void (*)(void *,FILE *),
                                                void (*)(void *),
                                                void (*)(void *),
                                                void (*)(void *));

#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                 Bsave(const char *);

#endif

#endif /* _H_bsave */







