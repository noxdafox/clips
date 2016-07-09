   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                 UTILITY HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules. Primarily these are the functions for    */
/*   handling periodic garbage collection and appending      */
/*   string data.                                            */
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
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added CopyString, DeleteString,                */
/*            InsertInString,and EnlargeString functions.    */
/*                                                           */
/*            Used genstrncpy function instead of strncpy    */
/*            function.                                      */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS.            */
/*                                                           */
/*            Support for tracked memory (allows memory to   */
/*            be freed if CLIPS is exited while executing).  */
/*                                                           */
/*            Added UTF-8 routines.                          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Added EnvAddPeriodicFunctionWithContext        */
/*            function.                                      */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_utility

#pragma once

#define _H_utility

#include "evaluatn.h"

struct callFunctionItem
  {
   const char *name;
   void (*func)(void *);
   int priority;
   struct callFunctionItem *next;
   bool environmentAware;
   void *context;
  };

struct callFunctionItemWithArg
  {
   const char *name;
   void (*func)(void *,void *);
   int priority;
   struct callFunctionItemWithArg *next;
   bool environmentAware;
   void *context;
  };
  
struct trackedMemory
  {
   void *theMemory;
   struct trackedMemory *next;
   struct trackedMemory *prev;
   size_t memSize;
  };

struct garbageFrame
  {
   bool dirty;
   bool topLevel;
   struct garbageFrame *priorFrame;
   struct ephemeron *ephemeralSymbolList;
   struct ephemeron *ephemeralFloatList;
   struct ephemeron *ephemeralIntegerList;
   struct ephemeron *ephemeralBitMapList;
   struct ephemeron *ephemeralExternalAddressList;
   struct multifield *ListOfMultifields;
   struct multifield *LastMultifield;
  };

#define UTILITY_DATA 55

struct utilityData
  { 
   struct callFunctionItem *ListOfCleanupFunctions;
   struct callFunctionItem *ListOfPeriodicFunctions;
   short GarbageCollectionLocks;
   bool PeriodicFunctionsEnabled;
   bool YieldFunctionEnabled;
   void (*YieldTimeFunction)(void);
   struct trackedMemory *trackList;
   struct garbageFrame MasterGarbageFrame;
   struct garbageFrame *CurrentGarbageFrame;
  };

#define UtilityData(theEnv) ((struct utilityData *) GetEnvironmentData(theEnv,UTILITY_DATA))

  /* Is c the start of a utf8 sequence? */
#define IsUTF8Start(ch) (((ch) & 0xC0) != 0x80)
#define IsUTF8MultiByteStart(ch) ((((unsigned char) ch) >= 0xC0) && (((unsigned char) ch) <= 0xF7))
#define IsUTF8MultiByteContinuation(ch) ((((unsigned char) ch) >= 0x80) && (((unsigned char) ch) <= 0xBF))

   void                           InitializeUtilityData(void *);
   bool                           AddCleanupFunction(void *,const char *,void (*)(void *),int);
   bool                           EnvAddPeriodicFunction(void *,const char *,void (*)(void *),int);
   bool                           EnvAddPeriodicFunctionWithContext(void *,const char *,void (*)(void *),int,void *);
   bool                           AddPeriodicFunction(const char *,void (*)(void),int);
   bool                           RemoveCleanupFunction(void *,const char *);
   bool                           EnvRemovePeriodicFunction(void *,const char *);
   char                          *CopyString(void *,const char *);
   void                           DeleteString(void *,char *);
   const char                    *AppendStrings(void *,const char *,const char *);
   const char                    *StringPrintForm(void *,const char *);
   char                          *AppendToString(void *,const char *,char *,size_t *,size_t *);
   char                          *InsertInString(void *,const char *,size_t,char *,size_t *,size_t *);
   char                          *AppendNToString(void *,const char *,char *,size_t,size_t *,size_t *);
   char                          *EnlargeString(void *,size_t,char *,size_t *,size_t *);
   char                          *ExpandStringWithChar(void *,int,char *,size_t *,size_t *,size_t);
   struct callFunctionItem       *AddFunctionToCallList(void *,const char *,int,void (*)(void *),
                                                               struct callFunctionItem *,bool);
   struct callFunctionItem       *AddFunctionToCallListWithContext(void *,const char *,int,void (*)(void *),
                                                                          struct callFunctionItem *,bool,void *);
   struct callFunctionItem       *RemoveFunctionFromCallList(void *,const char *,
                                                             struct callFunctionItem *,
                                                             bool *);
   void                           DeallocateCallList(void *,struct callFunctionItem *);
   struct callFunctionItemWithArg *AddFunctionToCallListWithArg(void *,const char *,int,void (*)(void *, void *),
                                                                       struct callFunctionItemWithArg *,bool);
   struct callFunctionItemWithArg *AddFunctionToCallListWithArgWithContext(void *,const char *,int,void (*)(void *, void *),
                                                                                  struct callFunctionItemWithArg *,bool,void *);
   struct callFunctionItemWithArg *RemoveFunctionFromCallListWithArg(void *,const char *,
                                                                            struct callFunctionItemWithArg *,
                                                                            bool *);
   void                           DeallocateCallListWithArg(void *,struct callFunctionItemWithArg *);
   struct callFunctionItem       *GetFunctionFromCallList(void *,const char *,struct callFunctionItem *);
   void                          *EnvGetPeriodicFunctionContext(void *,const char *);
   unsigned long                  ItemHashValue(void *,unsigned short,void *,unsigned long);
   void                           YieldTime(void *);
   void                           EnvIncrementGCLocks(void *);
   void                           EnvDecrementGCLocks(void *);
   bool                           EnablePeriodicFunctions(void *,bool);
   short                          EnableYieldFunction(void *,short);
   struct trackedMemory          *AddTrackedMemory(void *,void *,size_t);
   void                           RemoveTrackedMemory(void *,struct trackedMemory *);
   void                           UTF8Increment(const char *,size_t *);
   size_t                         UTF8Offset(const char *,size_t);
   size_t                         UTF8Length(const char *);
   size_t                         UTF8CharNum(const char *,size_t);
   void                           RestorePriorGarbageFrame(void *,struct garbageFrame *,struct garbageFrame *,struct dataObject *);
   void                           CallCleanupFunctions(void *);
   void                           CallPeriodicTasks(void *);
   void                           CleanCurrentGarbageFrame(void *,struct dataObject *);

#if ALLOW_ENVIRONMENT_GLOBALS

   void                           IncrementGCLocks(void);
   void                           DecrementGCLocks(void);
   bool                           RemovePeriodicFunction(const char *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_utility */




