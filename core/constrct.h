   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.50  10/01/16            */
   /*                                                     */
   /*                  CONSTRUCT MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed garbage collection algorithm.          */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added code for capturing errors/warnings       */
/*            (EnvSetParserErrorCallback).                   */
/*                                                           */
/*            Fixed issue with save function when multiple   */
/*            defmodules exist.                              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
/*                                                           */
/*            Added code to keep track of pointers to        */
/*            constructs that are contained externally to    */
/*            to constructs, DanglingConstructs.             */
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
/*      6.50: Modified EnvClear to return completion status. */
/*                                                           */
/*            File name/line count displayed for errors      */
/*            and warnings during load command.              */
/*                                                           */
/*************************************************************/

#ifndef _H_constrct

#pragma once

#define _H_constrct

typedef struct construct Construct;

#include "entities.h"
#include "userdata.h"
#include "moduldef.h"

typedef void ParserErrorFunction(Environment *,const char *,const char *,const char *,long);
typedef bool *BeforeResetFunction(Environment *);

#define CHS (ConstructHeader *)

struct construct
  {
   const char *constructName;
   const char *pluralName;
   bool (*parseFunction)(Environment *,const char *);
   FindConstructFunction *findFunction;
   CLIPSLexeme *(*getConstructNameFunction)(ConstructHeader *);
   const char *(*getPPFormFunction)(ConstructHeader *);
   struct defmoduleItemHeader *(*getModuleItemFunction)(ConstructHeader *);
   GetNextConstructFunction *getNextItemFunction;
   void (*setNextItemFunction)(ConstructHeader *,ConstructHeader *);
   IsConstructDeletableFunction *isConstructDeletableFunction;
   DeleteConstructFunction *deleteFunction;
   FreeConstructFunction *freeFunction;
   Construct *next;
  };

#define CONSTRUCT_DATA 42

struct constructData
  {
   bool ClearReadyInProgress;
   bool ClearInProgress;
   bool ResetReadyInProgress;
   bool ResetInProgress;
   short ClearReadyLocks;
   int DanglingConstructs;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   struct saveCallFunctionItem *ListOfSaveFunctions;
   bool PrintWhileLoading;
   bool LoadInProgress;
   bool WatchCompilations;
   bool CheckSyntaxMode;
   bool ParsingConstruct;
   char *ErrorString;
   char *WarningString;
   char *ParsingFileName;
   char *ErrorFileName;
   char *WarningFileName;
   long ErrLineNumber;
   long WrnLineNumber;
   int errorCaptureRouterCount;
   size_t MaxErrChars;
   size_t CurErrPos;
   size_t MaxWrnChars;
   size_t CurWrnPos;
   ParserErrorFunction *ParserErrorCallback;
#endif
   Construct *ListOfConstructs;
   struct voidCallFunctionItem *ListOfResetFunctions;
   struct voidCallFunctionItem *ListOfClearFunctions;
   struct boolCallFunctionItem *ListOfClearReadyFunctions;
   bool Executing;
   BeforeResetFunction *BeforeResetCallback;
  };

#define ConstructData(theEnv) ((struct constructData *) GetEnvironmentData(theEnv,CONSTRUCT_DATA))

   bool                           EnvClear(Environment *);
   void                           EnvReset(Environment *);
   bool                           EnvSave(Environment *,const char *);

   void                           InitializeConstructData(Environment *);
   bool                           AddSaveFunction(Environment *,const char *,void (*)(Environment *,Defmodule *,const char *),int);
   bool                           RemoveSaveFunction(Environment *,const char *);
   bool                           EnvAddResetFunction(Environment *,const char *,void (*)(Environment *),int);
   bool                           EnvRemoveResetFunction(Environment *,const char *);
   bool                           AddClearReadyFunction(Environment *,const char *,bool (*)(Environment *),int);
   bool                           RemoveClearReadyFunction(Environment *,const char *);
   bool                           EnvAddClearFunction(Environment *,const char *,void (*)(Environment *),int);
   bool                           EnvRemoveClearFunction(Environment *,const char *);
   void                           EnvIncrementClearReadyLocks(Environment *);
   void                           EnvDecrementClearReadyLocks(Environment *);
   Construct                     *AddConstruct(Environment *,const char *,const char *,
                                               bool (*)(Environment *,const char *),
                                               FindConstructFunction *,
                                               CLIPSLexeme *(*)(ConstructHeader *),
                                               const char *(*)(ConstructHeader *),
                                               struct defmoduleItemHeader *(*)(ConstructHeader *),
                                               GetNextConstructFunction *,
                                               void (*)(ConstructHeader *,ConstructHeader *),
                                               IsConstructDeletableFunction *,
                                               DeleteConstructFunction *,
                                               FreeConstructFunction *);
   bool                           RemoveConstruct(Environment *,const char *);
   void                           SetCompilationsWatch(Environment *,bool);
   bool                           GetCompilationsWatch(Environment *);
   void                           SetPrintWhileLoading(Environment *,bool);
   bool                           GetPrintWhileLoading(Environment *);
   void                           SetLoadInProgress(Environment *,bool);
   bool                           GetLoadInProgress(Environment *);
   bool                           ExecutingConstruct(Environment *);
   void                           SetExecutingConstruct(Environment *,bool);
   void                           InitializeConstructs(Environment *);
   BeforeResetFunction           *SetBeforeResetFunction(Environment *,BeforeResetFunction *);
   void                           ResetCommand(Environment *,UDFContext *,UDFValue *);
   void                           ClearCommand(Environment *,UDFContext *,UDFValue *);
   bool                           ClearReady(Environment *);
   Construct                     *FindConstruct(Environment *,const char *);
   void                           DeinstallConstructHeader(Environment *,ConstructHeader *);
   void                           DestroyConstructHeader(Environment *,ConstructHeader *);
   ParserErrorFunction           *EnvSetParserErrorCallback(Environment *,ParserErrorFunction *);

#endif /* _H_constrct */




