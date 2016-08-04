   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*                DEFMODULE HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Defines basic defmodule primitive functions such */
/*   as allocating and deallocating, traversing, and finding */
/*   defmodule data structures.                              */
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
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*************************************************************/

#ifndef _H_moduldef

#pragma once

#define _H_moduldef

typedef struct defmodule Defmodule;
struct portItem;
struct defmoduleItemHeader;
struct moduleItem;

typedef void *AllocateModuleFunction(Environment *);
typedef void FreeModuleFunction(Environment *,void *);

#include <stdio.h>

#include "symbol.h"
#include "userdata.h"

/**********************************************************************/
/* defmodule                                                          */
/* ----------                                                         */
/* name: The name of the defmodule (stored as a reference in the      */
/*   table).                                                          */
/*                                                                    */
/* ppForm: The pretty print representation of the defmodule (used by  */
/*   the save and ppdefmodule commands).                              */
/*                                                                    */
/* itemsArray: An array of pointers to the module specific data used  */
/*   by each construct specified with the RegisterModuleItem          */
/*   function. The data pointer stored in the array is allocated by   */
/*   the allocateFunction in moduleItem data structure.               */
/*                                                                    */
/* importList: The list of items which are being imported by this     */
/*   module from other modules.                                       */
/*                                                                    */
/* next: A pointer to the next defmodule data structure.              */
/**********************************************************************/

struct defmodule
  {
   struct symbolHashNode *name;
   char *ppForm;
   struct defmoduleItemHeader **itemsArray;
   struct portItem *importList;
   struct portItem *exportList;
   bool visitedFlag;
   long bsaveID;
   struct userData *usrData;
   Defmodule *next;
  };

struct portItem
  {
   struct symbolHashNode *moduleName;
   struct symbolHashNode *constructType;
   struct symbolHashNode *constructName;
   struct portItem *next;
  };

struct defmoduleItemHeader
  {
   Defmodule *theModule;
   struct constructHeader *firstItem;
   struct constructHeader *lastItem;
  };

#define MIHS (struct defmoduleItemHeader *)

/**********************************************************************/
/* moduleItem                                                         */
/* ----------                                                         */
/* name: The name of the construct which can be placed in a module.   */
/*   For example, "defrule".                                          */
/*                                                                    */
/* allocateFunction: Used to allocate a data structure containing all */
/*   pertinent information related to a specific construct for a      */
/*   given module. For example, the deffacts construct stores a       */
/*   pointer to the first and last deffacts for each each module.     */
/*                                                                    */
/* freeFunction: Used to deallocate a data structure allocated by     */
/*   the allocateFunction. In addition, the freeFunction deletes      */
/*   all constructs of the specified type in the given module.        */
/*                                                                    */
/* bloadModuleReference: Used during a binary load to establish a     */
/*   link between the defmodule data structure and the data structure */
/*   containing all pertinent module information for a specific       */
/*   construct.                                                       */
/*                                                                    */
/* findFunction: Used to determine if a specified construct is in a   */
/*   specific module. The name is the specific construct is passed as */
/*   a string and the function returns a pointer to the specified     */
/*   construct if it exists.                                          */
/*                                                                    */
/* exportable: If true, then the specified construct type can be      */
/*   exported (and hence imported). If false, it can't be exported.   */
/*                                                                    */
/* next: A pointer to the next moduleItem data structure.             */
/**********************************************************************/

struct moduleItem
  {
   const char *name;
   int moduleIndex;
   void *(*allocateFunction)(Environment *);
   void  (*freeFunction)(Environment *,void *);
   void *(*bloadModuleReference)(Environment *,int);
   void  (*constructsToCModuleReference)(Environment *,FILE *,int,int,int);
   void *(*findFunction)(Environment *,const char *);
   struct moduleItem *next;
  };

typedef struct moduleStackItem
  {
   bool changeFlag;
   Defmodule *theModule;
   struct moduleStackItem *next;
  } MODULE_STACK_ITEM;

#define DEFMODULE_DATA 4

#include "conscomp.h" /* TBD Needed Headers? */
#include "constrct.h"
#include "evaluatn.h"
#include "modulpsr.h"
#include "utility.h"

struct defmoduleData
  {   
   struct moduleItem *LastModuleItem;
   struct callFunctionItem *AfterModuleChangeFunctions;
   MODULE_STACK_ITEM *ModuleStack;
   bool CallModuleChangeFunctions;
   Defmodule *ListOfDefmodules;
   Defmodule *CurrentModule;
   Defmodule *LastDefmodule;
   int NumberOfModuleItems;
   struct moduleItem *ListOfModuleItems;
   long ModuleChangeIndex;
   bool MainModuleRedefinable;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   struct portConstructItem *ListOfPortConstructItems;
   long NumberOfDefmodules;
   struct callFunctionItem *AfterModuleDefinedFunctions;
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DefmoduleCodeItem;
#endif
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   long BNumberOfDefmodules;
   long NumberOfPortItems;
   struct portItem *PortItemArray;
   Defmodule *DefmoduleArray;
#endif
  };
  
#define DefmoduleData(theEnv) ((struct defmoduleData *) GetEnvironmentData(theEnv,DEFMODULE_DATA))

   void                           InitializeDefmodules(Environment *);
   Defmodule                     *EnvFindDefmodule(Environment *,const char *);
   const char                    *EnvGetDefmoduleName(Environment *,Defmodule *);
   const char                    *EnvGetDefmodulePPForm(Environment *,Defmodule *);
   Defmodule                     *EnvGetNextDefmodule(Environment *,Defmodule *);
   void                           RemoveAllDefmodules(Environment *);
   int                            AllocateModuleStorage(void);
   int                            RegisterModuleItem(Environment *,const char *,
                                                     AllocateModuleFunction *,
                                                     FreeModuleFunction *,
                                                     void *(*)(Environment *,int),
                                                     void (*)(Environment *,FILE *,int,int,int),
                                                     FindConstructFunction *);
   void                          *GetModuleItem(Environment *,Defmodule *,int);
   void                           SetModuleItem(Environment *,Defmodule *,int,void *);
   Defmodule                     *EnvGetCurrentModule(Environment *);
   Defmodule                     *EnvSetCurrentModule(Environment *,Defmodule *);
   void                          *GetCurrentModuleCommand(Environment *);
   void                          *SetCurrentModuleCommand(Environment *);
   int                            GetNumberOfModuleItems(Environment *);
   void                           CreateMainModule(Environment *);
   void                           SetListOfDefmodules(Environment *,Defmodule *);
   struct moduleItem             *GetListOfModuleItems(Environment *);
   struct moduleItem             *FindModuleItem(Environment *,const char *);
   void                           SaveCurrentModule(Environment *);
   void                           RestoreCurrentModule(Environment *);
   void                           AddAfterModuleChangeFunction(Environment *,const char *,void (*)(Environment *),int);
   void                           IllegalModuleSpecifierMessage(Environment *);
   void                           AllocateDefmoduleGlobals(Environment *);

#if ALLOW_ENVIRONMENT_GLOBALS

   Defmodule                     *FindDefmodule(const char *);
   Defmodule                     *GetCurrentModule(void);
   const char                    *GetDefmoduleName(Defmodule *);
   const char                    *GetDefmodulePPForm(Defmodule *);
   Defmodule                     *GetNextDefmodule(Defmodule *);
   Defmodule                     *SetCurrentModule(Defmodule *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_moduldef */


