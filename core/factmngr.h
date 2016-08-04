   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*              FACTS MANAGER HEADER FILE              */
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
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            AssignFactSlotDefaults function does not       */
/*            properly handle defaults for multifield slots. */
/*            DR0869                                         */
/*                                                           */
/*            Support for ppfact command.                    */
/*                                                           */
/*      6.30: Callback function support for assertion,       */
/*            retraction, and modification of facts.         */
/*                                                           */
/*            Updates to fact pattern entity record.         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
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
/*            Removed unused global variables.               */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            JoinOperationInProgress mechanism.             */
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

#ifndef _H_factmngr

#pragma once

#define _H_factmngr

typedef struct fact Fact;

#include "conscomp.h"
#include "evaluatn.h"
#include "multifld.h"
#include "pattern.h"
#include "tmpltdef.h"

struct fact
  {
   struct patternEntity factHeader;
   Deftemplate *whichDeftemplate;
   void *list;
   long long factIndex;
   unsigned long hashValue;
   unsigned int garbage : 1;
   struct fact *previousFact;
   struct fact *nextFact;
   struct fact *previousTemplateFact;
   struct fact *nextTemplateFact;
   struct multifield theProposition;
  };

#include "facthsh.h"

#define FACTS_DATA 3

struct factsData
  {
   bool ChangeToFactList;
#if DEBUGGING_FUNCTIONS
   bool WatchFacts;
#endif
   Fact DummyFact;
   struct fact *GarbageFacts;
   struct fact *LastFact;
   struct fact *FactList;
   long long NextFactIndex;
   unsigned long NumberOfFacts;
   struct callFunctionItemWithArg *ListOfAssertFunctions;
   struct callFunctionItemWithArg *ListOfRetractFunctions;
   struct callFunctionItemWithArg *ListOfModifyFunctions;
   struct patternEntityRecord  FactInfo;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   Deftemplate *CurrentDeftemplate;
#endif
#if DEFRULE_CONSTRUCT && (! RUN_TIME) && DEFTEMPLATE_CONSTRUCT && CONSTRUCT_COMPILER
   struct CodeGeneratorItem *FactCodeItem;
#endif
   struct factHashEntry **FactHashTable;
   unsigned long FactHashTableSize;
   bool FactDuplication;
#if DEFRULE_CONSTRUCT
   struct fact             *CurrentPatternFact;
   struct multifieldMarker *CurrentPatternMarks;
#endif
   long LastModuleIndex;
  };
  
#define FactData(theEnv) ((struct factsData *) GetEnvironmentData(theEnv,FACTS_DATA))

   Fact                          *EnvAssert(Environment *,Fact *);
   Fact                          *EnvAssertString(Environment *,const char *);
   Fact                          *EnvCreateFact(Environment *,Deftemplate *);
   void                           EnvDecrementFactCount(Environment *,Fact *);
   long long                      EnvFactIndex(Environment *,Fact *);
   bool                           EnvGetFactSlot(Environment *,Fact *,const char *,DATA_OBJECT *);
   void                           PrintFactWithIdentifier(Environment *,const char *,Fact *);
   void                           PrintFact(Environment *,const char *,Fact *,bool,bool);
   void                           PrintFactIdentifierInLongForm(Environment *,const char *,Fact *);
   bool                           EnvRetract(Environment *,Fact *);
   void                           RemoveAllFacts(Environment *);
   struct fact                   *CreateFactBySize(Environment *,unsigned);
   void                           FactInstall(Environment *,Fact *);
   void                           FactDeinstall(Environment *,Fact *);
   Fact                          *EnvGetNextFact(Environment *,Fact *);
   Fact                          *GetNextFactInScope(Environment *,Fact *);
   void                           EnvGetFactPPForm(Environment *,char *,size_t,Fact *);
   bool                           EnvGetFactListChanged(Environment *);
   void                           EnvSetFactListChanged(Environment *,bool);
   unsigned long                  GetNumberOfFacts(Environment *);
   void                           InitializeFacts(Environment *);
   Fact                          *FindIndexedFact(Environment *,long long);
   void                           EnvIncrementFactCount(Environment *,Fact *);
   void                           PrintFactIdentifier(Environment *,const char *,Fact *);
   void                           DecrementFactBasisCount(Environment *,Fact *);
   void                           IncrementFactBasisCount(Environment *,Fact *);
   bool                           FactIsDeleted(Environment *,Fact *);
   void                           ReturnFact(Environment *,Fact *);
   void                           MatchFactFunction(Environment *,Fact *);
   bool                           EnvPutFactSlot(Environment *,Fact *,const char *,DATA_OBJECT *);
   bool                           EnvAssignFactSlotDefaults(Environment *,Fact *);
   bool                           CopyFactSlotValues(Environment *,Fact *,Fact *);
   bool                           DeftemplateSlotDefault(Environment *,Deftemplate *,
                                                         struct templateSlot *,DATA_OBJECT *,bool);
   bool                           EnvAddAssertFunction(Environment *,const char *,
                                                       void (*)(Environment *,void *),int);
   bool                           EnvAddAssertFunctionWithContext(Environment *,const char *,
                                                                  void (*)(Environment *,void *),int,void *);
   bool                           EnvRemoveAssertFunction(Environment *,const char *);
   bool                           EnvAddRetractFunction(Environment *,const char *,
                                                        void (*)(Environment *,void *),int);
   bool                           EnvAddRetractFunctionWithContext(Environment *,const char *,
                                                                   void (*)(Environment *,void *),int,void *);
   bool                           EnvRemoveRetractFunction(Environment *,const char *);
   bool                           EnvAddModifyFunction(Environment *,const char *,
                                                       void (*)(Environment *,void *,void *),int);
   bool                           EnvAddModifyFunctionWithContext(Environment *,const char *,
                                                                  void (*)(Environment *,void *,void *),int,void *);
   bool                           EnvRemoveModifyFunction(Environment *,const char *);


#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           AddAssertFunction(const char *,void (*)(void *,void *),int);
   bool                           AddModifyFunction(const char *,void (*)(void *,void *,void *),int);
   bool                           AddRetractFunction(const char *,void (*)(Environment *,void *),int);
   void                          *Assert(Fact *);
   void                          *AssertString(const char *);
   bool                           AssignFactSlotDefaults(Fact *);
   struct fact                   *CreateFact(Deftemplate *);
   void                           DecrementFactCount(Fact *);
   long long                      FactIndex(Fact *);
   bool                           GetFactListChanged(void);
   void                           GetFactPPForm(char *,unsigned,Fact *);
   bool                           GetFactSlot(Fact *,const char *,DATA_OBJECT *);
   Fact                          *GetNextFact(Fact *);
   void                           IncrementFactCount(Fact *);
   bool                           PutFactSlot(Fact *,const char *,DATA_OBJECT *);
   bool                           RemoveAssertFunction(const char *);
   bool                           RemoveModifyFunction(const char *);
   bool                           RemoveRetractFunction(const char *);
   bool                           Retract(Fact *);
   void                           SetFactListChanged(bool);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_factmngr */





