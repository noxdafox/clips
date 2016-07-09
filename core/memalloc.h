   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
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
/*************************************************************/

#ifndef _H_memalloc

#pragma once

#define _H_memalloc

#include <string.h>

struct chunkInfo;
struct blockInfo;
struct memoryPtr;

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
    (genfree(theEnv,(void *) struct_ptr,MemoryData(theEnv)->TempSize),(struct memoryPtr *) struct_ptr)))

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
    (genfree(theEnv,(void *) ptr,MemoryData(theEnv)->TempSize),(struct memoryPtr *) ptr)))

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
   bool (*OutOfMemoryFunction)(void *,size_t);
   struct memoryPtr *TempMemoryPtr;
   struct memoryPtr **MemoryTable;
   size_t TempSize;
  };

#define MemoryData(theEnv) ((struct memoryData *) GetEnvironmentData(theEnv,MEMORY_DATA))

   void                           InitializeMemory(void *);
   void                          *genalloc(void *,size_t);
   bool                           DefaultOutOfMemoryFunction(void *,size_t);
   bool                         (*EnvSetOutOfMemoryFunction(void *,bool (*)(void *,size_t)))(void *,size_t);
   void                           genfree(void *,void *,size_t);
   void                          *genrealloc(void *,void *,size_t,size_t);
   long                           EnvMemUsed(void *);
   long                           EnvMemRequests(void *);
   long                           UpdateMemoryUsed(void *,long int);
   long                           UpdateMemoryRequests(void *,long int);
   long                           EnvReleaseMem(void *,long);
   void                          *gm1(void *,size_t);
   void                          *gm2(void *,size_t);
   void                          *gm3(void *,size_t);
   void                           rm(void *,void *,size_t);
   void                           rm3(void *,void *,size_t);
   unsigned long                  PoolSize(void *);
   unsigned long                  ActualPoolSize(void *);
   void                          *RequestChunk(void *,size_t);
   int                            ReturnChunk(void *,void *,size_t);
   bool                           EnvSetConserveMemory(void *,bool);
   bool                           EnvGetConserveMemory(void *);
   void                           genmemcpy(char *,char *,unsigned long);
   void                           ReturnAllBlocks(void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           GetConserveMemory(void);
   long int                       MemRequests(void);
   long int                       MemUsed(void);
   long int                       ReleaseMem(long);
   bool                           SetConserveMemory(bool);
   bool                         (*SetOutOfMemoryFunction(bool (*)(void *,size_t)))(void *,size_t);
 
#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_memalloc */






