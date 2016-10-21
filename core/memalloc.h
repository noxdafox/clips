   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/06/16            */
   /*                                                     */
   /*            MEMORY ALLOCATION HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Memory allocation routines.                      */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed HaltExecution check from the           */
/*            EnvReleaseMem function. DR0863                 */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems.                   */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Removed genlongalloc/genlongfree functions.    */
/*                                                           */
/*            Added get_mem and rtn_mem macros.              */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Removed deallocating message parameter from    */
/*            EnvReleaseMem.                                 */
/*                                                           */
/*            Removed support for BLOCK_MEMORY.              */
/*                                                           */
/*      6.31: Fix for get_mem macro.                         */
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
/*************************************************************/

#ifndef _H_memalloc

#pragma once

#define _H_memalloc

#include <string.h>

struct chunkInfo;
struct blockInfo;
struct memoryPtr;

typedef bool OutOfMemoryFunction(Environment *,size_t);

#ifndef MEM_TABLE_SIZE
#define MEM_TABLE_SIZE 500
#endif

struct chunkInfo
  {
   struct chunkInfo *prevChunk;
   struct chunkInfo *nextFree;
   struct chunkInfo *lastFree;
   long int size;
  };

struct blockInfo
  {
   struct blockInfo *nextBlock;
   struct blockInfo *prevBlock;
   struct chunkInfo *nextFree;
   long int size;
  };

struct memoryPtr
  {
   struct memoryPtr *next;
  };

#if (MEM_TABLE_SIZE > 0)
/*
 * Normal memory management case
 */

#define get_struct(theEnv,type) \
  ((MemoryData(theEnv)->MemoryTable[sizeof(struct type)] == NULL) ? \
   ((struct type *) genalloc(theEnv,sizeof(struct type))) :\
   ((MemoryData(theEnv)->TempMemoryPtr = MemoryData(theEnv)->MemoryTable[sizeof(struct type)]),\
    MemoryData(theEnv)->MemoryTable[sizeof(struct type)] = MemoryData(theEnv)->TempMemoryPtr->next,\
    ((struct type *) MemoryData(theEnv)->TempMemoryPtr)))

#define rtn_struct(theEnv,type,struct_ptr) \
  (MemoryData(theEnv)->TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
   MemoryData(theEnv)->TempMemoryPtr->next = MemoryData(theEnv)->MemoryTable[sizeof(struct type)], \
   MemoryData(theEnv)->MemoryTable[sizeof(struct type)] = MemoryData(theEnv)->TempMemoryPtr)

#define rtn_sized_struct(theEnv,size,struct_ptr) \
  (MemoryData(theEnv)->TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
   MemoryData(theEnv)->TempMemoryPtr->next = MemoryData(theEnv)->MemoryTable[size], \
   MemoryData(theEnv)->MemoryTable[size] = MemoryData(theEnv)->TempMemoryPtr)

#define get_var_struct(theEnv,type,vsize) \
  ((((sizeof(struct type) + vsize) <  MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv)->MemoryTable[sizeof(struct type) + vsize] == NULL) : 1) ? \
   ((struct type *) genalloc(theEnv,(sizeof(struct type) + vsize))) :\
   ((MemoryData(theEnv)->TempMemoryPtr = MemoryData(theEnv)->MemoryTable[sizeof(struct type) + vsize]),\
    MemoryData(theEnv)->MemoryTable[sizeof(struct type) + vsize] = MemoryData(theEnv)->TempMemoryPtr->next,\
    ((struct type *) MemoryData(theEnv)->TempMemoryPtr)))

#define rtn_var_struct(theEnv,type,vsize,struct_ptr) \
  (MemoryData(theEnv)->TempSize = sizeof(struct type) + vsize, \
   ((MemoryData(theEnv)->TempSize < MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv)->TempMemoryPtr = (struct memoryPtr *) struct_ptr,\
     MemoryData(theEnv)->TempMemoryPtr->next = MemoryData(theEnv)->MemoryTable[MemoryData(theEnv)->TempSize], \
     MemoryData(theEnv)->MemoryTable[MemoryData(theEnv)->TempSize] =  MemoryData(theEnv)->TempMemoryPtr) : \
    (genfree(theEnv,struct_ptr,MemoryData(theEnv)->TempSize),(struct memoryPtr *) struct_ptr)))

#define get_mem(theEnv,size) \
  (((size <  MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv)->MemoryTable[size] == NULL) : 1) ? \
   ((void *) genalloc(theEnv,(size_t) (size))) :\
   ((MemoryData(theEnv)->TempMemoryPtr = MemoryData(theEnv)->MemoryTable[size]),\
    MemoryData(theEnv)->MemoryTable[size] = MemoryData(theEnv)->TempMemoryPtr->next,\
    ((void *) MemoryData(theEnv)->TempMemoryPtr)))

#define rtn_mem(theEnv,size,ptr) \
  (MemoryData(theEnv)->TempSize = size, \
   ((MemoryData(theEnv)->TempSize < MEM_TABLE_SIZE) ? \
    (MemoryData(theEnv)->TempMemoryPtr = (struct memoryPtr *) ptr,\
     MemoryData(theEnv)->TempMemoryPtr->next = MemoryData(theEnv)->MemoryTable[MemoryData(theEnv)->TempSize], \
     MemoryData(theEnv)->MemoryTable[MemoryData(theEnv)->TempSize] =  MemoryData(theEnv)->TempMemoryPtr) : \
    (genfree(theEnv,ptr,MemoryData(theEnv)->TempSize),(struct memoryPtr *) ptr)))

#else // MEM_TABLE_SIZE == 0
/*
 * Debug case (routes all memory management through genalloc/genfree to take advantage of
 * platform, memory debugging aids)
 */
#define get_struct(theEnv,type) ((struct type *) genalloc(theEnv,sizeof(struct type)))

#define rtn_struct(theEnv,type,struct_ptr) (genfree(theEnv,struct_ptr,sizeof(struct type)))

#define rtn_sized_struct(theEnv,size,struct_ptr) (genfree(theEnv,struct_ptr,size))

#define get_var_struct(theEnv,type,vsize) ((struct type *) genalloc(theEnv,(sizeof(struct type) + vsize)))

#define rtn_var_struct(theEnv,type,vsize,struct_ptr) (genfree(theEnv,struct_ptr,sizeof(struct type)+vsize))

#define get_mem(theEnv,size) ((struct type *) genalloc(theEnv,(size_t) (size)))

#define rtn_mem(theEnv,size,ptr) (genfree(theEnv,ptr,size))

#endif

#define GenCopyMemory(type,cnt,dst,src) \
   memcpy((void *) (dst),(void *) (src),sizeof(type) * (size_t) (cnt))

#define MEMORY_DATA 59

struct memoryData
  {
   long int MemoryAmount;
   long int MemoryCalls;
   bool ConserveMemory;
   OutOfMemoryFunction *OutOfMemoryCallback;
   struct memoryPtr *TempMemoryPtr;
   struct memoryPtr **MemoryTable;
   size_t TempSize;
  };

#define MemoryData(theEnv) ((struct memoryData *) GetEnvironmentData(theEnv,MEMORY_DATA))

   void                           InitializeMemory(Environment *);
   void                          *genalloc(Environment *,size_t);
   bool                           DefaultOutOfMemoryFunction(Environment *,size_t);
   OutOfMemoryFunction           *EnvSetOutOfMemoryFunction(Environment *,OutOfMemoryFunction *);
   void                           genfree(Environment *,void *,size_t);
   void                          *genrealloc(Environment *,void *,size_t,size_t);
   long                           EnvMemUsed(Environment *);
   long                           EnvMemRequests(Environment *);
   long                           UpdateMemoryUsed(Environment *,long int);
   long                           UpdateMemoryRequests(Environment *,long int);
   long                           EnvReleaseMem(Environment *,long);
   void                          *gm1(Environment *,size_t);
   void                          *gm2(Environment *,size_t);
   void                          *gm3(Environment *,size_t);
   void                           rm(Environment *,void *,size_t);
   void                           rm3(Environment *,void *,size_t);
   unsigned long                  PoolSize(Environment *);
   unsigned long                  ActualPoolSize(Environment *);
   void                          *RequestChunk(Environment *,size_t);
   int                            ReturnChunk(Environment *,void *,size_t);
   bool                           EnvSetConserveMemory(Environment *,bool);
   bool                           EnvGetConserveMemory(Environment *);
   void                           genmemcpy(char *,char *,unsigned long);

#endif /* _H_memalloc */






