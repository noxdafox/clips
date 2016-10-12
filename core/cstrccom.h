   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*           CONSTRUCT COMMAND HEADER MODULE           */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added ConstructsDeletable function.            */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrccom

#pragma once

#define _H_cstrccom

typedef bool ConstructGetWatchFunction(Environment *,void *);
typedef void ConstructSetWatchFunction(Environment *,bool,void *);

#include "moduldef.h"
#include "constrct.h"

typedef void ConstructActionFunction(Environment *,struct constructHeader *,void *);

#if (! RUN_TIME)
   void                           AddConstructToModule(struct constructHeader *);
#endif
   bool                           DeleteNamedConstruct(Environment *,const char *,struct construct *);
   void                          *FindNamedConstructInModule(Environment *,const char *,struct construct *);
   void                          *FindNamedConstructInModuleOrImports(Environment *,const char *,struct construct *);
   void                           UndefconstructCommand(UDFContext *,const char *,struct construct *);
   bool                           PPConstruct(Environment *,const char *,const char *,struct construct *);
   CLIPSLexeme                   *GetConstructModuleCommand(UDFContext *,const char *,struct construct *);
   Defmodule                     *GetConstructModule(Environment *,const char *,struct construct *);
   bool                           Undefconstruct(Environment *,void *,struct construct *);
   void                           SaveConstruct(Environment *,Defmodule *,const char *,struct construct *);
   const char                    *GetConstructNameString(struct constructHeader *);
   const char                    *EnvGetConstructNameString(Environment *,struct constructHeader *);
   const char                    *GetConstructModuleName(struct constructHeader *);
   CLIPSLexeme                   *GetConstructNamePointer(struct constructHeader *);
   void                           GetConstructListFunction(UDFContext *,UDFValue *,struct construct *);
   void                           GetConstructList(Environment *,UDFValue *,struct construct *,
                                                   Defmodule *);
   void                           ListConstructCommand(UDFContext *,struct construct *);
   void                           ListConstruct(Environment *,struct construct *,const char *,Defmodule *);
   void                           SetNextConstruct(struct constructHeader *,struct constructHeader *);
   struct defmoduleItemHeader    *GetConstructModuleItem(struct constructHeader *);
   const char                    *GetConstructPPForm(Environment *,struct constructHeader *);
   void                           PPConstructCommand(UDFContext *,const char *,struct construct *);
   struct constructHeader        *GetNextConstructItem(Environment *,struct constructHeader *,int);
   struct defmoduleItemHeader    *GetConstructModuleItemByIndex(Environment *,Defmodule *,int);
   void                           FreeConstructHeaderModule(Environment *,struct defmoduleItemHeader *,
                                                                   struct construct *);
   long                           DoForAllConstructs(Environment *,
                                                     ConstructActionFunction *,
                                                     int,bool,void *);
   void                           DoForAllConstructsInModule(Environment *,Defmodule *,
                                                             ConstructActionFunction *,
                                                             int,int,void *);
   void                           InitializeConstructHeader(Environment *,const char *,ConstructType,
                                                            struct constructHeader *,CLIPSLexeme *);
   void                           SetConstructPPForm(Environment *,struct constructHeader *,const char *);
   void                          *LookupConstruct(Environment *,struct construct *,const char *,bool);
#if DEBUGGING_FUNCTIONS
   bool                           ConstructPrintWatchAccess(Environment *,struct construct *,const char *,
                                            EXPRESSION *,
                                            bool (*)(Environment *,void *),
                                            void (*)(Environment *,bool,void *));
   bool                           ConstructSetWatchAccess(Environment *,struct construct *,bool,
                                            EXPRESSION *,
                                            ConstructGetWatchFunction *,
                                            ConstructSetWatchFunction *);
#endif
   bool                           ConstructsDeletable(Environment *);

#endif



