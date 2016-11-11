   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/01/16            */
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
/*            Support for typed EXTERNAL_ADDRESS_TYPE.            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Added CLIPSBlockStart and CLIPSBlockEnd        */
/*            functions for garbage collection blocks.       */
/*                                                           */
/*            Added StringBuilder functions.                 */
/*                                                           */
/*************************************************************/

#ifndef _H_utility

#pragma once

#define _H_utility

#include <stdlib.h>

#include "evaluatn.h"
#include "moduldef.h"

typedef struct clipsBlock CLIPSBlock;
typedef struct stringBuilder StringBuilder;

struct voidCallFunctionItem
  {
   const char *name;
   void (*func)(Environment *);
   int priority;
   struct voidCallFunctionItem *next;
   void *context;
  };

struct boolCallFunctionItem
  {
   const char *name;
   bool (*func)(Environment *);
   int priority;
   struct boolCallFunctionItem *next;
   void *context;
  };

struct saveCallFunctionItem
  {
   const char *name;
   void (*func)(Environment *,Defmodule *,const char *);
   int priority;
   struct saveCallFunctionItem *next;
   void *context;
  };

struct callFunctionItemWithArg
  {
   const char *name;
   void (*func)(Environment *,void *);
   int priority;
   struct callFunctionItemWithArg *next;
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

struct clipsBlock
  {
   struct garbageFrame newGarbageFrame;
   struct garbageFrame *oldGarbageFrame;
   UDFValue *result;
  };

struct stringBuilder
  {
   Environment *sbEnv;
   char *stringBuffer;
   size_t bufferReset;
   size_t bufferPosition;
   size_t bufferMaximum;
  };

#define UTILITY_DATA 55

struct utilityData
  {
   struct voidCallFunctionItem *ListOfCleanupFunctions;
   struct voidCallFunctionItem *ListOfPeriodicFunctions;
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

   void                           InitializeUtilityData(Environment *);
   bool                           AddCleanupFunction(Environment *,const char *,void (*)(Environment *),int);
   bool                           EnvAddPeriodicFunction(Environment *,const char *,void (*)(Environment *),int);
   bool                           EnvAddPeriodicFunctionWithContext(Environment *,const char *,void (*)(Environment *),int,void *);
   bool                           AddPeriodicFunction(const char *,void (*)(void),int);
   bool                           RemoveCleanupFunction(Environment *,const char *);
   bool                           EnvRemovePeriodicFunction(Environment *,const char *);
   char                          *CopyString(Environment *,const char *);
   void                           DeleteString(Environment *,char *);
   const char                    *AppendStrings(Environment *,const char *,const char *);
   const char                    *StringPrintForm(Environment *,const char *);
   char                          *AppendToString(Environment *,const char *,char *,size_t *,size_t *);
   char                          *InsertInString(Environment *,const char *,size_t,char *,size_t *,size_t *);
   char                          *AppendNToString(Environment *,const char *,char *,size_t,size_t *,size_t *);
   char                          *EnlargeString(Environment *,size_t,char *,size_t *,size_t *);
   char                          *ExpandStringWithChar(Environment *,int,char *,size_t *,size_t *,size_t);
   struct voidCallFunctionItem   *AddVoidFunctionToCallList(Environment *,const char *,int,void (*)(Environment *),
                                                            struct voidCallFunctionItem *);
   struct boolCallFunctionItem   *AddBoolFunctionToCallList(Environment *,const char *,int,bool (*)(Environment *),
                                                            struct boolCallFunctionItem *);
   struct saveCallFunctionItem   *AddSaveFunctionToCallList(Environment *,const char *,int,
                                                            void (*)(Environment *,Defmodule *,const char *),
                                                            struct saveCallFunctionItem *);
   struct voidCallFunctionItem   *AddVoidFunctionToCallListWithContext(Environment *,const char *,int,void (*)(Environment *),
                                                                   struct voidCallFunctionItem *,void *);
   struct boolCallFunctionItem   *AddBoolFunctionToCallListWithContext(Environment *,const char *,int,bool (*)(Environment *),
                                                                   struct boolCallFunctionItem *,void *);
   struct saveCallFunctionItem   *AddSaveFunctionToCallListWithContext(Environment *,const char *,int,
                                                                       void (*)(Environment *,Defmodule *,const char *),
                                                                       struct saveCallFunctionItem *,void *);
   struct voidCallFunctionItem   *RemoveVoidFunctionFromCallList(Environment *,const char *,
                                                                 struct voidCallFunctionItem *,bool *);
   struct saveCallFunctionItem   *RemoveSaveFunctionFromCallList(Environment *,const char *,
                                                                 struct saveCallFunctionItem *,bool *);
   struct boolCallFunctionItem   *RemoveBoolFunctionFromCallList(Environment *,const char *,
                                                                 struct boolCallFunctionItem *,bool *);
   void                           DeallocateVoidCallList(Environment *,struct voidCallFunctionItem *);
   void                           DeallocateBoolCallList(Environment *,struct boolCallFunctionItem *);
   void                           DeallocateSaveCallList(Environment *,struct saveCallFunctionItem *);
   struct callFunctionItemWithArg *AddFunctionToCallListWithArg(Environment *,const char *,int,void (*)(Environment *, void *),
                                                                       struct callFunctionItemWithArg *);
   struct callFunctionItemWithArg *AddFunctionToCallListWithArgWithContext(Environment *,const char *,int,void (*)(Environment *, void *),
                                                                                  struct callFunctionItemWithArg *,void *);
   struct callFunctionItemWithArg *RemoveFunctionFromCallListWithArg(Environment *,const char *,
                                                                            struct callFunctionItemWithArg *,
                                                                            bool *);
   void                           DeallocateCallListWithArg(Environment *,struct callFunctionItemWithArg *);
   struct voidCallFunctionItem   *GetVoidFunctionFromCallList(Environment *,const char *,struct voidCallFunctionItem *);
   struct boolCallFunctionItem   *GetBoolFunctionFromCallList(Environment *,const char *,struct boolCallFunctionItem *);
   void                          *EnvGetPeriodicFunctionContext(Environment *,const char *);
   unsigned long                  ItemHashValue(Environment *,unsigned short,void *,unsigned long);
   void                           YieldTime(Environment *);
   void                           EnvIncrementGCLocks(Environment *);
   void                           EnvDecrementGCLocks(Environment *);
   bool                           EnablePeriodicFunctions(Environment *,bool);
   short                          EnableYieldFunction(Environment *,short);
   struct trackedMemory          *AddTrackedMemory(Environment *,void *,size_t);
   void                           RemoveTrackedMemory(Environment *,struct trackedMemory *);
   void                           UTF8Increment(const char *,size_t *);
   size_t                         UTF8Offset(const char *,size_t);
   size_t                         UTF8Length(const char *);
   size_t                         UTF8CharNum(const char *,size_t);
   void                           RestorePriorGarbageFrame(Environment *,struct garbageFrame *,struct garbageFrame *,UDFValue *);
   void                           CallCleanupFunctions(Environment *);
   void                           CallPeriodicTasks(Environment *);
   void                           CleanCurrentGarbageFrame(Environment *,UDFValue *);
   void                           CLIPSBlockStart(Environment *,CLIPSBlock *);
   void                           CLIPSBlockEnd(Environment *,CLIPSBlock *,UDFValue *);
   StringBuilder                 *EnvCreateStringBuilder(Environment *,size_t);
   void                           StringBuilderDispose(StringBuilder *);
   void                           StringBuilderAppend(StringBuilder *,const char *);
   void                           StringBuilderReset(StringBuilder *);
   char                          *StringBuilderCopy(StringBuilder *);

#endif /* _H_utility */




