   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  11/01/16            */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Watch facts for modify command only prints     */
/*            changed slots.                                 */
/*                                                           */
/*            Modify command preserves fact id and address.  */
/*                                                           */
/*************************************************************/

#ifndef _H_factmngr

#pragma once

#define _H_factmngr

typedef struct factBuilder FactBuilder;
typedef struct factModifier FactModifier;

#include "entities.h"
#include "conscomp.h"
#include "tmpltdef.h"

typedef void ModifyCallFunction(Environment *,Fact *,Fact *,void *);
typedef struct modifyCallFunctionItem ModifyCallFunctionItem;

struct modifyCallFunctionItem
  {
   const char *name;
   ModifyCallFunction *func;
   int priority;
   ModifyCallFunctionItem *next;
   void *context;
  };

struct fact
  {
   union
     {
      struct patternEntity patternHeader;
      TypeHeader header;
     };
   Deftemplate *whichDeftemplate;
   void *list;
   long long factIndex;
   unsigned long hashValue;
   unsigned int garbage : 1;
   Fact *previousFact;
   Fact *nextFact;
   Fact *previousTemplateFact;
   Fact *nextTemplateFact;
   Multifield *basisSlots;
   Multifield theProposition;
  };

struct factBuilder
  {
   Environment *fbEnv;
   Deftemplate *fbDeftemplate;
   CLIPSValue *fbValueArray;
  };

struct factModifier
  {
   Environment *fmEnv;
   Fact *fmOldFact;
   CLIPSValue *fmValueArray;
   char *changeMap;
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
   Fact *GarbageFacts;
   Fact *LastFact;
   Fact *FactList;
   long long NextFactIndex;
   unsigned long NumberOfFacts;
   struct callFunctionItemWithArg *ListOfAssertFunctions;
   struct callFunctionItemWithArg *ListOfRetractFunctions;
   ModifyCallFunctionItem *ListOfModifyFunctions;
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
   Fact                    *CurrentPatternFact;
   struct multifieldMarker *CurrentPatternMarks;
#endif
   long LastModuleIndex;
  };

#define FactData(theEnv) ((struct factsData *) GetEnvironmentData(theEnv,FACTS_DATA))

   Fact                          *Assert(Environment *,Fact *);
   Fact                          *AssertDriver(Environment *,Fact *,long long,Fact *,Fact *,char *);
   Fact                          *AssertString(Environment *,const char *);
   Fact                          *CreateFact(Environment *,Deftemplate *);
   void                           DecrementFactReferenceCount(Environment *,Fact *);
   long long                      FactIndex(Environment *,Fact *);
   bool                           GetFactSlot(Environment *,Fact *,const char *,CLIPSValue *);
   void                           PrintFactWithIdentifier(Environment *,const char *,Fact *,const char *);
   void                           PrintFact(Environment *,const char *,Fact *,bool,bool,const char *);
   void                           PrintFactIdentifierInLongForm(Environment *,const char *,Fact *);
   bool                           Retract(Environment *,Fact *);
   bool                           RetractDriver(Environment *,Fact *,bool,char *);
   void                           RemoveAllFacts(Environment *);
   Fact                          *CreateFactBySize(Environment *,unsigned);
   void                           FactInstall(Environment *,Fact *);
   void                           FactDeinstall(Environment *,Fact *);
   Fact                          *GetNextFact(Environment *,Fact *);
   Fact                          *GetNextFactInScope(Environment *,Fact *);
   void                           FactPPForm(Fact *,char *,size_t);
   bool                           GetFactListChanged(Environment *);
   void                           SetFactListChanged(Environment *,bool);
   unsigned long                  GetNumberOfFacts(Environment *);
   void                           InitializeFacts(Environment *);
   Fact                          *FindIndexedFact(Environment *,long long);
   void                           IncrementFactReferenceCount(Environment *,Fact *);
   void                           PrintFactIdentifier(Environment *,const char *,Fact *);
   void                           DecrementFactBasisCount(Environment *,Fact *);
   void                           IncrementFactBasisCount(Environment *,Fact *);
   bool                           FactIsDeleted(Environment *,Fact *);
   void                           ReturnFact(Environment *,Fact *);
   void                           MatchFactFunction(Environment *,Fact *);
   bool                           PutFactSlot(Environment *,Fact *,const char *,CLIPSValue *);
   bool                           AssignFactSlotDefaults(Environment *,Fact *);
   bool                           CopyFactSlotValues(Environment *,Fact *,Fact *);
   bool                           DeftemplateSlotDefault(Environment *,Deftemplate *,
                                                         struct templateSlot *,UDFValue *,bool);
   bool                           AddAssertFunction(Environment *,const char *,
                                                    VoidCallFunctionWithArg *,int,void *);
   bool                           RemoveAssertFunction(Environment *,const char *);
   bool                           AddRetractFunction(Environment *,const char *,
                                                     VoidCallFunctionWithArg *,int,void *);
   bool                           RemoveRetractFunction(Environment *,const char *);
   FactBuilder                   *CreateFactBuilder(Environment *,const char *);
   bool                           FBPutSlot(FactBuilder *,const char *,CLIPSValue *);
   Fact                          *FBAssert(FactBuilder *);
   void                           FBDispose(FactBuilder *);
   void                           FBAbort(FactBuilder *);
   bool                           FBSetDeftemplate(FactBuilder *,const char *);
   bool                           FBPutSlotInteger(FactBuilder *,const char *,CLIPSInteger *);
   bool                           FBPutSlotFloat(FactBuilder *,const char *,CLIPSFloat *);
   bool                           FBPutSlotLexeme(FactBuilder *,const char *,CLIPSLexeme *);
   bool                           FBPutSlotFact(FactBuilder *,const char *,Fact *);
   bool                           FBPutSlotInstance(FactBuilder *,const char *,Instance *);
   bool                           FBPutSlotExternalAddress(FactBuilder *,const char *,CLIPSExternalAddress *);
   bool                           FBPutSlotMultifield(FactBuilder *,const char *,Multifield *);

   FactModifier                  *CreateFactModifier(Environment *,Fact *);
   bool                           FMPutSlot(FactModifier *,const char *,CLIPSValue *);
   Fact                          *FMModify(FactModifier *);
   void                           FMDispose(FactModifier *);
   void                           FMAbort(FactModifier *);
   bool                           FMSetFact(FactModifier *,Fact *);
   bool                           FMPutSlotInteger(FactModifier *,const char *,CLIPSInteger *);
   bool                           FMPutSlotFloat(FactModifier *,const char *,CLIPSFloat *);
   bool                           FMPutSlotLexeme(FactModifier *,const char *,CLIPSLexeme *);
   bool                           FMPutSlotFact(FactModifier *,const char *,Fact *);
   bool                           FMPutSlotInstance(FactModifier *,const char *,Instance *);
   bool                           FMPutSlotExternalAddress(FactModifier *,const char *,CLIPSExternalAddress *);
   bool                           FMPutSlotMultifield(FactModifier *,const char *,Multifield *);

   bool                           AddModifyFunction(Environment *,const char *,ModifyCallFunction *,int,void *);
   bool                           RemoveModifyFunction(Environment *,const char *);
   ModifyCallFunctionItem        *AddModifyFunctionToCallList(Environment *,const char *,int,
                                                              ModifyCallFunction *,ModifyCallFunctionItem *,void *);
   ModifyCallFunctionItem        *RemoveModifyFunctionFromCallList(Environment *,const char *,
                                                                   ModifyCallFunctionItem *,bool *);
   void                           DeallocateModifyCallList(Environment *,ModifyCallFunctionItem *);

#endif /* _H_factmngr */





