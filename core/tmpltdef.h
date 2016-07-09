   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*               DEFTEMPLATE HEADER FILE               */
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
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Added code for deftemplate run time            */
/*            initialization of hashed comparisons to        */
/*            constants.                                     */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for deftemplate slot facets.           */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*************************************************************/

#ifndef _H_tmpltdef

#pragma once

#define _H_tmpltdef

struct deftemplate;
struct templateSlot;
struct deftemplateModule;

#include "constrct.h"
#include "factbld.h"
#include "factmngr.h"
#include "expressn.h"

#include "conscomp.h"
#include "constrnt.h"
#include "cstrccom.h"
#include "evaluatn.h"
#include "moduldef.h"
#include "symbol.h"

struct deftemplate
  {
   struct constructHeader header;
   struct templateSlot *slotList;
   unsigned int implied       : 1;
   unsigned int watch         : 1;
   unsigned int inScope       : 1;
   unsigned short numberOfSlots;
   long busyCount;
   struct factPatternNode *patternNetwork;
   struct fact *factList;
   struct fact *lastFact;
  };

struct templateSlot
  {
   struct symbolHashNode *slotName;
   unsigned int multislot : 1;
   unsigned int noDefault : 1;
   unsigned int defaultPresent : 1;
   unsigned int defaultDynamic : 1;
   CONSTRAINT_RECORD *constraints;
   struct expr *defaultList;
   struct expr *facetList;
   struct templateSlot *next;
  };

struct deftemplateModule
  {
   struct defmoduleItemHeader header;
  };

#define DEFTEMPLATE_DATA 5

struct deftemplateData
  { 
   struct construct *DeftemplateConstruct;
   int DeftemplateModuleIndex;
   struct entityRecord DeftemplatePtrRecord;
#if DEBUGGING_FUNCTIONS
   int DeletedTemplateDebugFlags;
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DeftemplateCodeItem;
#endif
#if (! RUN_TIME) && (! BLOAD_ONLY)
   bool DeftemplateError;
#endif
  };

#define DeftemplateData(theEnv) ((struct deftemplateData *) GetEnvironmentData(theEnv,DEFTEMPLATE_DATA))

   void                           InitializeDeftemplates(void *);
   void                          *EnvFindDeftemplate(void *,const char *);
   void                          *EnvFindDeftemplateInModule(void *,const char *);
   void                          *EnvGetNextDeftemplate(void *,void *);
   bool                           EnvIsDeftemplateDeletable(void *,void *);
   void                          *EnvGetNextFactInTemplate(void *,void *,void *);
   struct deftemplateModule      *GetDeftemplateModuleItem(void *,struct defmodule *);
   void                           ReturnSlots(void *,struct templateSlot *);
   void                           IncrementDeftemplateBusyCount(void *,void *);
   void                           DecrementDeftemplateBusyCount(void *,void *);
   void                          *CreateDeftemplateScopeMap(void *,struct deftemplate *);
#if RUN_TIME
   void                           DeftemplateRunTimeInitialize(void *);
#endif
   const char                    *EnvDeftemplateModule(void *,void *);
   const char                    *EnvGetDeftemplateName(void *,void *);
   const char                    *EnvGetDeftemplatePPForm(void *,void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   const char                    *DeftemplateModule(void *);
   void                          *FindDeftemplate(const char *);
   const char                    *GetDeftemplateName(void *);
   const char                    *GetDeftemplatePPForm(void *);
   void                          *GetNextDeftemplate(void *);
   bool                           IsDeftemplateDeletable(void *);
   void                          *GetNextFactInTemplate(void *,void *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_tmpltdef */


