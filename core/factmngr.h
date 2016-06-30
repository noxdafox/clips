   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_factmngr

#pragma once

#define _H_factmngr

struct fact;

#include "conscomp.h"
#include "evaluatn.h"
#include "facthsh.h"
#include "multifld.h"
#include "pattern.h"
#include "tmpltdef.h"

struct fact
  {
   struct patternEntity factHeader;
   struct deftemplate *whichDeftemplate;
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
  
#define FACTS_DATA 3

struct factsData
  {
   int ChangeToFactList;
#if DEBUGGING_FUNCTIONS
   unsigned WatchFacts;
#endif
   struct fact DummyFact;
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
   struct deftemplate *CurrentDeftemplate;
#endif
#if DEFRULE_CONSTRUCT && (! RUN_TIME) && DEFTEMPLATE_CONSTRUCT && CONSTRUCT_COMPILER
   struct CodeGeneratorItem *FactCodeItem;
#endif
   struct factHashEntry **FactHashTable;
   unsigned long FactHashTableSize;
   intBool FactDuplication;
#if DEFRULE_CONSTRUCT
   struct fact             *CurrentPatternFact;
   struct multifieldMarker *CurrentPatternMarks;
#endif
   long LastModuleIndex;
  };
  
#define FactData(theEnv) ((struct factsData *) GetEnvironmentData(theEnv,FACTS_DATA))

   void                          *EnvAssert(void *,void *);
   void                          *EnvAssertString(void *,const char *);
   struct fact                   *EnvCreateFact(void *,void *);
   void                           EnvDecrementFactCount(void *,void *);
   long long                      EnvFactIndex(void *,void *);
   intBool                        EnvGetFactSlot(void *,void *,const char *,DATA_OBJECT *);
   void                           PrintFactWithIdentifier(void *,const char *,struct fact *);
   void                           PrintFact(void *,const char *,struct fact *,int,int);
   void                           PrintFactIdentifierInLongForm(void *,const char *,void *);
   intBool                        EnvRetract(void *,void *);
   void                           RemoveAllFacts(void *);
   struct fact                   *CreateFactBySize(void *,unsigned);
   void                           FactInstall(void *,struct fact *);
   void                           FactDeinstall(void *,struct fact *);
   void                          *EnvGetNextFact(void *,void *);
   void                          *GetNextFactInScope(void *theEnv,void *);
   void                           EnvGetFactPPForm(void *,char *,size_t,void *);
   int                            EnvGetFactListChanged(void *);
   void                           EnvSetFactListChanged(void *,int);
   unsigned long                  GetNumberOfFacts(void *);
   void                           InitializeFacts(void *);
   struct fact                   *FindIndexedFact(void *,long long);
   void                           EnvIncrementFactCount(void *,void *);
   void                           PrintFactIdentifier(void *,const char *,void *);
   void                           DecrementFactBasisCount(void *,void *);
   void                           IncrementFactBasisCount(void *,void *);
   intBool                        FactIsDeleted(void *,void *);
   void                           ReturnFact(void *,struct fact *);
   void                           MatchFactFunction(void *,void *);
   intBool                        EnvPutFactSlot(void *,void *,const char *,DATA_OBJECT *);
   intBool                        EnvAssignFactSlotDefaults(void *,void *);
   intBool                        CopyFactSlotValues(void *,void *,void *);
   intBool                        DeftemplateSlotDefault(void *,struct deftemplate *,
                                                                struct templateSlot *,DATA_OBJECT *,int);
   intBool                        EnvAddAssertFunction(void *,const char *,
                                                              void (*)(void *,void *),int);
   intBool                        EnvAddAssertFunctionWithContext(void *,const char *,
                                                                         void (*)(void *,void *),int,void *);
   intBool                        EnvRemoveAssertFunction(void *,const char *);
   intBool                        EnvAddRetractFunction(void *,const char *,
                                                                    void (*)(void *,void *),int);
   intBool                        EnvAddRetractFunctionWithContext(void *,const char *,
                                                                          void (*)(void *,void *),int,void *);
   intBool                        EnvRemoveRetractFunction(void *,const char *);
   intBool                        EnvAddModifyFunction(void *,const char *,
                                                              void (*)(void *,void *,void *),int);
   intBool                        EnvAddModifyFunctionWithContext(void *,const char *,
                                                                         void (*)(void *,void *,void *),int,void *);
   intBool                        EnvRemoveModifyFunction(void *,const char *);


#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                        AddAssertFunction(const char *,void (*)(void *,void *),int);
   intBool                        AddModifyFunction(const char *,void (*)(void *,void *,void *),int);
   intBool                        AddRetractFunction(const char *,void (*)(void *,void *),int);
   void                          *Assert(void *);
   void                          *AssertString(const char *);
   intBool                        AssignFactSlotDefaults(void *);
   struct fact                   *CreateFact(void *);
   void                           DecrementFactCount(void *);
   long long                      FactIndex(void *);
   int                            GetFactListChanged(void);
   void                           GetFactPPForm(char *,unsigned,void *);
   intBool                        GetFactSlot(void *,const char *,DATA_OBJECT *);
   void                          *GetNextFact(void *);
   void                           IncrementFactCount(void *);
   intBool                        PutFactSlot(void *,const char *,DATA_OBJECT *);
   intBool                        RemoveAssertFunction(const char *);
   intBool                        RemoveModifyFunction(const char *);
   intBool                        RemoveRetractFunction(const char *);
   intBool                        Retract(void *);
   void                           SetFactListChanged(int);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_factmngr */





