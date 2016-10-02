   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/01/16            */
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
/*************************************************************/

#ifndef _H_constrct

#pragma once

#define _H_constrct

struct constructHeader;
struct construct;

typedef void *FindConstructFunction(Environment *,const char *);
typedef void *GetNextConstructFunction(Environment *,void *);
typedef bool *IsConstructDeletableFunction(Environment *,void *);
typedef bool *DeleteConstructFunction(Environment *,void *);
typedef void *FreeConstructFunction(Environment *,void *);
typedef void ParserErrorFunction(Environment *,const char *,const char *,const char *,long);
typedef bool *BeforeResetFunction(Environment *);

#include "symbol.h"
#include "userdata.h"

typedef enum
  {
   DEFMODULE,
   DEFRULE,
   DEFTEMPLATE,
   DEFFACTS,
   DEFGLOBAL,
   DEFFUNCTION,
   DEFGENERIC,
   DEFMETHOD,
   DEFCLASS,
   DEFMESSAGE_HANDLER,
   DEFINSTANCES
  } ConstructType;


struct defmoduleItemHeader; // TBD Can this be removed?

struct constructHeader
  {
   ConstructType constructType;
   CLIPSLexeme *name;
   const char *ppForm;
   struct defmoduleItemHeader *whichModule;
   long bsaveID;
   struct constructHeader *next;
   struct userData *usrData;
  };

#include "moduldef.h"


#define CHS (struct constructHeader *)

struct construct
  {
   const char *constructName;
   const char *pluralName;
   bool (*parseFunction)(Environment *,const char *);
   FindConstructFunction *findFunction;
   CLIPSLexeme *(*getConstructNameFunction)(struct constructHeader *);
   const char *(*getPPFormFunction)(Environment *,struct constructHeader *);
   struct defmoduleItemHeader *(*getModuleItemFunction)(struct constructHeader *);
   GetNextConstructFunction *getNextItemFunction;
   void (*setNextItemFunction)(struct constructHeader *,struct constructHeader *);
   IsConstructDeletableFunction *isConstructDeletableFunction;
   DeleteConstructFunction *deleteFunction;
   FreeConstructFunction *freeFunction;
   struct construct *next;
  };

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif

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
   struct callFunctionItem *ListOfSaveFunctions;
   bool PrintWhileLoading;
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
   struct construct *ListOfConstructs;
   struct callFunctionItem *ListOfResetFunctions;
   struct callFunctionItem *ListOfClearFunctions;
   struct callFunctionItem *ListOfClearReadyFunctions;
   bool Executing;
   BeforeResetFunction *BeforeResetCallback;
  };

#define ConstructData(theEnv) ((struct constructData *) GetEnvironmentData(theEnv,CONSTRUCT_DATA))

   void                           EnvClear(Environment *);
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
   struct construct              *AddConstruct(Environment *,const char *,const char *,
                                               bool (*)(Environment *,const char *),
                                               FindConstructFunction *,
                                               CLIPSLexeme *(*)(struct constructHeader *),
                                               const char *(*)(Environment *,struct constructHeader *),
                                               struct defmoduleItemHeader *(*)(struct constructHeader *),
                                               GetNextConstructFunction *,
                                               void (*)(struct constructHeader *,struct constructHeader *),
                                               IsConstructDeletableFunction *,
                                               DeleteConstructFunction *,
                                               FreeConstructFunction *);
   bool                           RemoveConstruct(Environment *,const char *);
   void                           SetCompilationsWatch(Environment *,unsigned);
   unsigned                       GetCompilationsWatch(Environment *);
   void                           SetPrintWhileLoading(Environment *,bool);
   bool                           GetPrintWhileLoading(Environment *);
   bool                           ExecutingConstruct(Environment *);
   void                           SetExecutingConstruct(Environment *,bool);
   void                           InitializeConstructs(Environment *);
   BeforeResetFunction           *SetBeforeResetFunction(Environment *,BeforeResetFunction *);
   void                           ResetCommand(Environment *,UDFContext *,CLIPSValue *);
   void                           ClearCommand(Environment *,UDFContext *,CLIPSValue *);
   bool                           ClearReady(Environment *);
   struct construct              *FindConstruct(Environment *,const char *);
   void                           DeinstallConstructHeader(Environment *,struct constructHeader *);
   void                           DestroyConstructHeader(Environment *,struct constructHeader *);
   ParserErrorFunction           *EnvSetParserErrorCallback(Environment *,ParserErrorFunction *);

#endif /* _H_constrct */




