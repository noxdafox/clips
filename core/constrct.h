   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_constrct

#pragma once

#define _H_constrct

struct constructHeader;
struct construct;

#include "symbol.h"
#include "userdata.h"

struct defmoduleItemHeader; // TBD Can this be removed?

struct constructHeader
  {
   struct symbolHashNode *name;
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
   int (*parseFunction)(void *,const char *);
   void *(*findFunction)(void *,const char *);
   struct symbolHashNode *(*getConstructNameFunction)(struct constructHeader *);
   const char *(*getPPFormFunction)(void *,struct constructHeader *);
   struct defmoduleItemHeader *(*getModuleItemFunction)(struct constructHeader *);
   void *(*getNextItemFunction)(void *,void *);
   void (*setNextItemFunction)(struct constructHeader *,struct constructHeader *);
   intBool (*isConstructDeletableFunction)(void *,void *);
   int (*deleteFunction)(void *,void *);
   void (*freeFunction)(void *,void *);
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
   int ClearReadyInProgress;
   int ClearInProgress;
   int ResetReadyInProgress;
   int ResetInProgress;
   short ClearReadyLocks;
   int DanglingConstructs;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   struct callFunctionItem *ListOfSaveFunctions;
   intBool PrintWhileLoading;
   unsigned WatchCompilations;
   int CheckSyntaxMode;
   int ParsingConstruct;
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
   void (*ParserErrorCallback)(void *,const char *,const char *,const char *,long);
#endif
   struct construct *ListOfConstructs;
   struct callFunctionItem *ListOfResetFunctions;
   struct callFunctionItem *ListOfClearFunctions;
   struct callFunctionItem *ListOfClearReadyFunctions;
   int Executing;
   int (*BeforeResetFunction)(void *);
  };

#define ConstructData(theEnv) ((struct constructData *) GetEnvironmentData(theEnv,CONSTRUCT_DATA))

   void                           EnvClear(void *);
   void                           EnvReset(void *);
   int                            EnvSave(void *,const char *);

   void                           InitializeConstructData(void *);
   intBool                        AddSaveFunction(void *,const char *,void (*)(void *,void *,const char *),int);
   intBool                        RemoveSaveFunction(void *,const char *);
   intBool                        EnvAddResetFunction(void *,const char *,void (*)(void *),int);
   intBool                        EnvRemoveResetFunction(void *,const char *);
   intBool                        AddClearReadyFunction(void *,const char *,int (*)(void *),int);
   intBool                        RemoveClearReadyFunction(void *,const char *);
   intBool                        EnvAddClearFunction(void *,const char *,void (*)(void *),int);
   intBool                        EnvRemoveClearFunction(void *,const char *);
   void                           EnvIncrementClearReadyLocks(void *);
   void                           EnvDecrementClearReadyLocks(void *);
   struct construct              *AddConstruct(void *,const char *,const char *,
                                                      int (*)(void *,const char *),
                                                      void *(*)(void *,const char *),
                                                      SYMBOL_HN *(*)(struct constructHeader *),
                                                      const char *(*)(void *,struct constructHeader *),
                                                      struct defmoduleItemHeader *(*)(struct constructHeader *),
                                                      void *(*)(void *,void *),
                                                      void (*)(struct constructHeader *,struct constructHeader *),
                                                      intBool (*)(void *,void *),
                                                      int (*)(void *,void *),
                                                      void (*)(void *,void *));
   int                            RemoveConstruct(void *,const char *);
   void                           SetCompilationsWatch(void *,unsigned);
   unsigned                       GetCompilationsWatch(void *);
   void                           SetPrintWhileLoading(void *,intBool);
   intBool                        GetPrintWhileLoading(void *);
   int                            ExecutingConstruct(void *);
   void                           SetExecutingConstruct(void *,int);
   void                           InitializeConstructs(void *);
   int                          (*SetBeforeResetFunction(void *,int (*)(void *)))(void *);
   void                           ResetCommand(void *);
   void                           ClearCommand(void *);
   intBool                        ClearReady(void *);
   struct construct              *FindConstruct(void *,const char *);
   void                           DeinstallConstructHeader(void *,struct constructHeader *);
   void                           DestroyConstructHeader(void *,struct constructHeader *);
   void                         (*EnvSetParserErrorCallback(void *theEnv,
                                                                   void (*functionPtr)(void *,const char *,const char *,
                                                                                       const char *,long)))
                                            (void *,const char *,const char *,const char*,long);


#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                        AddClearFunction(const char *,void (*)(void),int);
   intBool                        AddResetFunction(const char *,void (*)(void),int);
   void                           Clear(void);
   void                           Reset(void);
   intBool                        RemoveClearFunction(const char *);
   intBool                        RemoveResetFunction(const char *);
#if (! RUN_TIME) && (! BLOAD_ONLY)
   int                            Save(const char *);
#endif

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_constrct */




