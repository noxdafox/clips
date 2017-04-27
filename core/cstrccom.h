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

typedef bool ConstructGetWatchFunction(void *);
typedef void ConstructSetWatchFunction(void *,bool);

#include "moduldef.h"
#include "constrct.h"

typedef void ConstructActionFunction(Environment *,ConstructHeader *,void *);

#if (! RUN_TIME)
   void                           AddConstructToModule(ConstructHeader *);
#endif
   bool                           DeleteNamedConstruct(Environment *,const char *,Construct *);
   ConstructHeader               *FindNamedConstructInModule(Environment *,const char *,Construct *);
   ConstructHeader               *FindNamedConstructInModuleOrImports(Environment *,const char *,Construct *);
   void                           UndefconstructCommand(UDFContext *,const char *,Construct *);
   bool                           PPConstruct(Environment *,const char *,const char *,Construct *);
   CLIPSLexeme                   *GetConstructModuleCommand(UDFContext *,const char *,Construct *);
   Defmodule                     *GetConstructModule(Environment *,const char *,Construct *);
   bool                           Undefconstruct(Environment *,ConstructHeader *,Construct *);
   bool                           UndefconstructAll(Environment *,Construct *);
   void                           SaveConstruct(Environment *,Defmodule *,const char *,Construct *);
   const char                    *GetConstructNameString(ConstructHeader *);
   const char                    *GetConstructModuleName(ConstructHeader *);
   CLIPSLexeme                   *GetConstructNamePointer(ConstructHeader *);
   void                           GetConstructListFunction(UDFContext *,UDFValue *,Construct *);
   void                           GetConstructList(Environment *,UDFValue *,Construct *,
                                                   Defmodule *);
   void                           ListConstructCommand(UDFContext *,Construct *);
   void                           ListConstruct(Environment *,Construct *,const char *,Defmodule *);
   void                           SetNextConstruct(ConstructHeader *,ConstructHeader *);
   struct defmoduleItemHeader    *GetConstructModuleItem(ConstructHeader *);
   const char                    *GetConstructPPForm(ConstructHeader *);
   void                           PPConstructCommand(UDFContext *,const char *,Construct *);
   ConstructHeader               *GetNextConstructItem(Environment *,ConstructHeader *,unsigned);
   struct defmoduleItemHeader    *GetConstructModuleItemByIndex(Environment *,Defmodule *,unsigned);
   void                           FreeConstructHeaderModule(Environment *,struct defmoduleItemHeader *,
                                                                   Construct *);
   void                           DoForAllConstructs(Environment *,
                                                     ConstructActionFunction *,
                                                     unsigned,bool,void *);
   void                           DoForAllConstructsInModule(Environment *,Defmodule *,
                                                             ConstructActionFunction *,
                                                             unsigned,bool,void *);
   void                           InitializeConstructHeader(Environment *,const char *,ConstructType,
                                                            ConstructHeader *,CLIPSLexeme *);
   void                           SetConstructPPForm(Environment *,ConstructHeader *,const char *);
   ConstructHeader        *LookupConstruct(Environment *,Construct *,const char *,bool);
#if DEBUGGING_FUNCTIONS
   bool                           ConstructPrintWatchAccess(Environment *,Construct *,const char *,
                                            Expression *,
                                            ConstructGetWatchFunction *,
                                            ConstructSetWatchFunction *);
   bool                           ConstructSetWatchAccess(Environment *,Construct *,bool,
                                            Expression *,
                                            ConstructGetWatchFunction *,
                                            ConstructSetWatchFunction *);
#endif
   bool                           ConstructsDeletable(Environment *);

#endif



