   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                DEFGLOBAL HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
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

#ifndef _H_globldef

#pragma once

#define _H_globldef

typedef struct defglobal Defglobal;

#include "conscomp.h"
#include "constrct.h"
#include "cstrccom.h"
#include "evaluatn.h"
#include "expressn.h"
#include "moduldef.h"
#include "symbol.h"

#define DEFGLOBAL_DATA 1

struct defglobalData
  {
   Construct *DefglobalConstruct;
   int DefglobalModuleIndex;
   bool ChangeToGlobals;
#if DEBUGGING_FUNCTIONS
   bool WatchGlobals;
#endif
   bool ResetGlobals;
   struct entityRecord GlobalInfo;
   struct entityRecord DefglobalPtrRecord;
   long LastModuleIndex;
   Defmodule *TheDefmodule;
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DefglobalCodeItem;
#endif
  };

struct defglobal
  {
   ConstructHeader header;
   unsigned int watch   : 1;
   unsigned int inScope : 1;
   long busyCount;
   UDFValue current;
   struct expr *initial;
  };

struct defglobalModule
  {
   struct defmoduleItemHeader header;
  };

#define DefglobalData(theEnv) ((struct defglobalData *) GetEnvironmentData(theEnv,DEFGLOBAL_DATA))

   void                           InitializeDefglobals(Environment *);
   Defglobal                     *EnvFindDefglobal(Environment *,const char *);
   Defglobal                     *EnvFindDefglobalInModule(Environment *,const char *);
   Defglobal                     *EnvGetNextDefglobal(Environment *,Defglobal *);
   void                           CreateInitialFactDefglobal(void);
   bool                           DefglobalIsDeletable(Defglobal *);
   struct defglobalModule        *GetDefglobalModuleItem(Environment *,Defmodule *);
   void                           QSetDefglobalValue(Environment *,Defglobal *,UDFValue *,bool);
   Defglobal                     *QFindDefglobal(Environment *,CLIPSLexeme *);
   void                           DefglobalValueForm(Defglobal *,char *,size_t);
   bool                           EnvGetGlobalsChanged(Environment *);
   void                           EnvSetGlobalsChanged(Environment *,bool);
   bool                           EnvGetDefglobalValue(Environment *,const char *,CLIPSValue *);
   bool                           EnvSetDefglobalValue(Environment *,const char *,CLIPSValue *);
   void                           UpdateDefglobalScope(Environment *);
   Defglobal                     *GetNextDefglobalInScope(Environment *,Defglobal *);
   bool                           QGetDefglobalValue(Environment *,Defglobal *,UDFValue *);
   const char                    *DefglobalModule(Defglobal *);
   const char                    *DefglobalName(Defglobal *);
   const char                    *DefglobalPPForm(Defglobal *);
#if RUN_TIME
   void                           DefglobalRunTimeInitialize(Environment *);
#endif

#endif /* _H_globldef */


