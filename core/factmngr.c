   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.50  11/01/16             */
   /*                                                     */
   /*                 FACT MANAGER MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides core routines for maintaining the fact  */
/*   list including assert/retract operations, data          */
/*   structure creation/deletion, printing, slot access,     */
/*   and other utility functions.                            */
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
/*            Added code to keep track of pointers to        */
/*            constructs that are contained externally to    */
/*            to constructs, DanglingConstructs.             */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
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
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*      6.50: Removed initial-fact support.                  */
/*                                                           */
/*            Modify command preserves fact id and address.  */
/*                                                           */
/*            Watch facts for modify command only prints     */
/*            changed slots.                                 */
/*                                                           */
/*************************************************************/

#include <stdio.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT

#include "commline.h"
#include "default.h"
#include "engine.h"
#include "factbin.h"
#include "factcmp.h"
#include "factcom.h"
#include "factfun.h"
#include "factmch.h"
#include "factqury.h"
#include "factrhs.h"
#include "lgcldpnd.h"
#include "memalloc.h"
#include "multifld.h"
#include "retract.h"
#include "prntutil.h"
#include "router.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "tmpltbsc.h"
#include "tmpltfun.h"
#include "tmpltutl.h"
#include "utility.h"
#include "watch.h"

#include "factmngr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ResetFacts(Environment *);
   static bool                    ClearFactsReady(Environment *);
   static void                    RemoveGarbageFacts(Environment *);
   static void                    DeallocateFactData(Environment *);

/**************************************************************/
/* InitializeFacts: Initializes the fact data representation. */
/*   Facts are only available when both the defrule and       */
/*   deftemplate constructs are available.                    */
/**************************************************************/
void InitializeFacts(
  Environment *theEnv)
  {
   struct patternEntityRecord factInfo =
      { { "FACT_ADDRESS_TYPE", FACT_ADDRESS_TYPE,1,0,0,
          (EntityPrintFunction *) PrintFactIdentifier,
          (EntityPrintFunction *) PrintFactIdentifierInLongForm,
          (bool (*)(void *,void *)) EnvRetract,
          NULL,
          (void *(*)(void *,void *)) EnvGetNextFact,
          (EntityBusyCountFunction *) EnvDecrementFactCount,
          (EntityBusyCountFunction *) EnvIncrementFactCount,
          NULL,NULL,NULL,NULL,NULL
        },
        (void (*)(Environment *,void *)) DecrementFactBasisCount,
        (void (*)(Environment *,void *)) IncrementFactBasisCount,
        (void (*)(Environment *,void *)) MatchFactFunction,
        NULL,
        (bool (*)(Environment *,void *)) FactIsDeleted
      };

   Fact dummyFact = { { { FACT_ADDRESS_TYPE }, NULL, NULL, 0, 0L },
                      NULL, NULL, -1L, 0, 1,
                      NULL, NULL, NULL, NULL, NULL, 
                      { {MULTIFIELD_TYPE } , 1, 0UL, NULL, { { { NULL } } } } };

   AllocateEnvironmentData(theEnv,FACTS_DATA,sizeof(struct factsData),DeallocateFactData);

   memcpy(&FactData(theEnv)->FactInfo,&factInfo,sizeof(struct patternEntityRecord));
   dummyFact.factHeader.theInfo = &FactData(theEnv)->FactInfo;
   memcpy(&FactData(theEnv)->DummyFact,&dummyFact,sizeof(struct fact));
   FactData(theEnv)->LastModuleIndex = -1;

   /*=========================================*/
   /* Initialize the fact hash table (used to */
   /* quickly determine if a fact exists).    */
   /*=========================================*/

   InitializeFactHashTable(theEnv);

   /*============================================*/
   /* Initialize the fact callback functions for */
   /* use with the reset and clear commands.     */
   /*============================================*/

   EnvAddResetFunction(theEnv,"facts",ResetFacts,60);
   AddClearReadyFunction(theEnv,"facts",ClearFactsReady,0);

   /*=============================*/
   /* Initialize periodic garbage */
   /* collection for facts.       */
   /*=============================*/

   AddCleanupFunction(theEnv,"facts",RemoveGarbageFacts,0);

   /*===================================*/
   /* Initialize fact pattern matching. */
   /*===================================*/

   InitializeFactPatterns(theEnv);

   /*==================================*/
   /* Initialize the facts keyword for */
   /* use with the watch command.      */
   /*==================================*/

#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,"facts",0,&FactData(theEnv)->WatchFacts,80,
                DeftemplateWatchAccess,DeftemplateWatchPrint);
#endif

   /*=========================================*/
   /* Initialize fact commands and functions. */
   /*=========================================*/

   FactCommandDefinitions(theEnv);
   FactFunctionDefinitions(theEnv);

   /*==============================*/
   /* Initialize fact set queries. */
   /*==============================*/

#if FACT_SET_QUERIES
   SetupFactQuery(theEnv);
#endif

   /*==================================*/
   /* Initialize fact patterns for use */
   /* with the bload/bsave commands.   */
   /*==================================*/

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   FactBinarySetup(theEnv);
#endif

   /*===================================*/
   /* Initialize fact patterns for use  */
   /* with the constructs-to-c command. */
   /*===================================*/

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   FactPatternsCompilerSetup(theEnv);
#endif
  }

/***********************************/
/* DeallocateFactData: Deallocates */
/*   environment data for facts.   */
/***********************************/
static void DeallocateFactData(
  Environment *theEnv)
  {
   struct factHashEntry *tmpFHEPtr, *nextFHEPtr;
   struct fact *tmpFactPtr, *nextFactPtr;
   unsigned long i;
   struct patternMatch *theMatch, *tmpMatch;

   for (i = 0; i < FactData(theEnv)->FactHashTableSize; i++)
     {
      tmpFHEPtr = FactData(theEnv)->FactHashTable[i];

      while (tmpFHEPtr != NULL)
        {
         nextFHEPtr = tmpFHEPtr->next;
         rtn_struct(theEnv,factHashEntry,tmpFHEPtr);
         tmpFHEPtr = nextFHEPtr;
        }
     }

   rm3(theEnv,FactData(theEnv)->FactHashTable,
       sizeof(struct factHashEntry *) * FactData(theEnv)->FactHashTableSize);

   tmpFactPtr = FactData(theEnv)->FactList;
   while (tmpFactPtr != NULL)
     {
      nextFactPtr = tmpFactPtr->nextFact;

      theMatch = (struct patternMatch *) tmpFactPtr->list;
      while (theMatch != NULL)
        {
         tmpMatch = theMatch->next;
         rtn_struct(theEnv,patternMatch,theMatch);
         theMatch = tmpMatch;
        }

      ReturnEntityDependencies(theEnv,(struct patternEntity *) tmpFactPtr);

      ReturnFact(theEnv,tmpFactPtr);
      tmpFactPtr = nextFactPtr;
     }

   tmpFactPtr = FactData(theEnv)->GarbageFacts;
   while (tmpFactPtr != NULL)
     {
      nextFactPtr = tmpFactPtr->nextFact;

      ReturnFact(theEnv,tmpFactPtr);
      tmpFactPtr = nextFactPtr;
     }

   DeallocateCallListWithArg(theEnv,FactData(theEnv)->ListOfAssertFunctions);
   DeallocateCallListWithArg(theEnv,FactData(theEnv)->ListOfRetractFunctions);
   DeallocateCallListWithArg(theEnv,FactData(theEnv)->ListOfModifyFunctions);
  }

/**********************************************/
/* PrintFactWithIdentifier: Displays a single */
/*   fact preceded by its fact identifier.    */
/**********************************************/
void PrintFactWithIdentifier(
  Environment *theEnv,
  const char *logicalName,
  Fact *factPtr,
  const char *changeMap)
  {
   char printSpace[20];

   gensprintf(printSpace,"f-%-5lld ",factPtr->factIndex);
   EnvPrintRouter(theEnv,logicalName,printSpace);
   PrintFact(theEnv,logicalName,factPtr,false,false,changeMap);
  }

/****************************************************/
/* PrintFactIdentifier: Displays a fact identifier. */
/****************************************************/
void PrintFactIdentifier(
  Environment *theEnv,
  const char *logicalName,
  Fact *factPtr)
  {
   char printSpace[20];

   gensprintf(printSpace,"f-%lld",factPtr->factIndex);
   EnvPrintRouter(theEnv,logicalName,printSpace);
  }

/********************************************/
/* PrintFactIdentifierInLongForm: Display a */
/*   fact identifier in a longer format.    */
/********************************************/
void PrintFactIdentifierInLongForm(
  Environment *theEnv,
  const char *logicalName,
  Fact *factPtr)
  {
   if (PrintUtilityData(theEnv)->AddressesToStrings) EnvPrintRouter(theEnv,logicalName,"\"");
   if (factPtr != &FactData(theEnv)->DummyFact)
     {
      EnvPrintRouter(theEnv,logicalName,"<Fact-");
      PrintLongInteger(theEnv,logicalName,factPtr->factIndex);
      EnvPrintRouter(theEnv,logicalName,">");
     }
   else
     { EnvPrintRouter(theEnv,logicalName,"<Dummy Fact>"); }

   if (PrintUtilityData(theEnv)->AddressesToStrings) EnvPrintRouter(theEnv,logicalName,"\"");
  }

/*******************************************/
/* DecrementFactBasisCount: Decrements the */
/*   partial match busy count of a fact    */
/*******************************************/
void DecrementFactBasisCount(
  Environment *theEnv,
  Fact *factPtr)
  {
   struct multifield *theSegment;
   int i;

   EnvDecrementFactCount(theEnv,factPtr);

   if (factPtr->basisSlots != NULL)
     {
      theSegment = factPtr->basisSlots;
      factPtr->basisSlots->busyCount--;
     }
   else
     { theSegment = &factPtr->theProposition; }

   for (i = 0 ; i < (int) theSegment->length ; i++)
     { AtomDeinstall(theEnv,theSegment->theFields[i].header->type,theSegment->theFields[i].value); }

   if ((factPtr->basisSlots != NULL) && (factPtr->basisSlots->busyCount == 0))
     {
      ReturnMultifield(theEnv,factPtr->basisSlots);
      factPtr->basisSlots = NULL;
     }
  }

/*******************************************/
/* IncrementFactBasisCount: Increments the */
/*   partial match busy count of a fact.   */
/*******************************************/
void IncrementFactBasisCount(
  Environment *theEnv,
  Fact *factPtr)
  {
   struct multifield *theSegment;
   int i;

   EnvIncrementFactCount(theEnv,factPtr);

   theSegment = &factPtr->theProposition;

   if (theSegment->length != 0)
     {
      if (factPtr->basisSlots != NULL)
        {
         factPtr->basisSlots->busyCount++;
        }
      else
        {
         factPtr->basisSlots = CopyMultifield(theEnv,theSegment);
         factPtr->basisSlots->busyCount = 1;
        }
      theSegment = factPtr->basisSlots;
     }

   for (i = 0 ; i < (int) theSegment->length ; i++)
     {
      AtomInstall(theEnv,theSegment->theFields[i].header->type,theSegment->theFields[i].value);
     }
  }

/******************/
/* FactIsDeleted: */
/******************/
bool FactIsDeleted(
  Environment *theEnv,
  Fact *theFact)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   return theFact->garbage;
  }

/**************************************************/
/* PrintFact: Displays the printed representation */
/*   of a fact containing the relation name and   */
/*   all of the fact's slots or fields.           */
/**************************************************/
void PrintFact(
  Environment *theEnv,
  const char *logicalName,
  Fact *factPtr,
  bool separateLines,
  bool ignoreDefaults,
  const char *changeMap)
  {
   Multifield *theMultifield;

   /*=========================================*/
   /* Print a deftemplate (non-ordered) fact. */
   /*=========================================*/

   if (factPtr->whichDeftemplate->implied == false)
     {
      PrintTemplateFact(theEnv,logicalName,factPtr,separateLines,ignoreDefaults,changeMap);
      return;
     }

   /*==============================*/
   /* Print an ordered fact (which */
   /* has an implied deftemplate). */
   /*==============================*/

   EnvPrintRouter(theEnv,logicalName,"(");

   EnvPrintRouter(theEnv,logicalName,factPtr->whichDeftemplate->header.name->contents);

   theMultifield = factPtr->theProposition.theFields[0].multifieldValue;
   if (theMultifield->length != 0)
     {
      EnvPrintRouter(theEnv,logicalName," ");
      PrintMultifield(theEnv,logicalName,theMultifield,0,
                      (long) (theMultifield->length - 1),
                      false);
     }

   EnvPrintRouter(theEnv,logicalName,")");
  }

/*********************************************/
/* MatchFactFunction: Filters a fact through */
/*   the appropriate fact pattern network.   */
/*********************************************/
void MatchFactFunction(
  Environment *theEnv,
  Fact *theFact)
  {
   FactPatternMatch(theEnv,theFact,theFact->whichDeftemplate->patternNetwork,0,NULL,NULL);
  }

/*************************************************/
/* RetractDriver: Driver routine for EnvRetract. */
/*************************************************/
bool RetractDriver(
  Environment *theEnv,
  Fact *theFact,
  bool modifyOperation,
  char *changeMap)
  {
   Deftemplate *theTemplate = theFact->whichDeftemplate;
   struct callFunctionItemWithArg *theRetractFunction;

   /*===========================================*/
   /* A fact can not be retracted while another */
   /* fact is being asserted or retracted.      */
   /*===========================================*/

   if (EngineData(theEnv)->JoinOperationInProgress)
     {
      PrintErrorID(theEnv,"FACTMNGR",1,true);
      EnvPrintRouter(theEnv,WERROR,"Facts may not be retracted during pattern-matching\n");
      return false;
     }

   /*====================================*/
   /* A NULL fact pointer indicates that */
   /* all facts should be retracted.     */
   /*====================================*/

   if (theFact == NULL)
     {
      RemoveAllFacts(theEnv);
      return true;
     }

   /*======================================================*/
   /* Check to see if the fact has already been retracted. */
   /*======================================================*/

   if (theFact->garbage) return false;

   /*===========================================*/
   /* Execute the list of functions that are    */
   /* to be called before each fact retraction. */
   /*===========================================*/

   for (theRetractFunction = FactData(theEnv)->ListOfRetractFunctions;
        theRetractFunction != NULL;
        theRetractFunction = theRetractFunction->next)
     {
      SetEnvironmentCallbackContext(theEnv,theRetractFunction->context);
      (*theRetractFunction->func)(theEnv,theFact);
     }

   /*============================*/
   /* Print retraction output if */
   /* facts are being watched.   */
   /*============================*/

#if DEBUGGING_FUNCTIONS
   if (theFact->whichDeftemplate->watch)
     {
      EnvPrintRouter(theEnv,WTRACE,"<== ");
      PrintFactWithIdentifier(theEnv,WTRACE,theFact,changeMap);
      EnvPrintRouter(theEnv,WTRACE,"\n");
     }
#endif

   /*==================================*/
   /* Set the change flag to indicate  */
   /* the fact-list has been modified. */
   /*==================================*/

   FactData(theEnv)->ChangeToFactList = true;

   /*===============================================*/
   /* Remove any links between the fact and partial */
   /* matches in the join network. These links are  */
   /* used to keep track of logical dependencies.   */
   /*===============================================*/

   RemoveEntityDependencies(theEnv,(struct patternEntity *) theFact);

   /*===========================================*/
   /* Remove the fact from the fact hash table. */
   /*===========================================*/

   RemoveHashedFact(theEnv,theFact);

   /*=========================================*/
   /* Remove the fact from its template list. */
   /*=========================================*/

   if (theFact == theTemplate->lastFact)
     { theTemplate->lastFact = theFact->previousTemplateFact; }

   if (theFact->previousTemplateFact == NULL)
     {
      theTemplate->factList = theTemplate->factList->nextTemplateFact;
      if (theTemplate->factList != NULL)
        { theTemplate->factList->previousTemplateFact = NULL; }
     }
   else
     {
      theFact->previousTemplateFact->nextTemplateFact = theFact->nextTemplateFact;
      if (theFact->nextTemplateFact != NULL)
        { theFact->nextTemplateFact->previousTemplateFact = theFact->previousTemplateFact; }
     }

   /*=====================================*/
   /* Remove the fact from the fact list. */
   /*=====================================*/

   if (theFact == FactData(theEnv)->LastFact)
     { FactData(theEnv)->LastFact = theFact->previousFact; }

   if (theFact->previousFact == NULL)
     {
      FactData(theEnv)->FactList = FactData(theEnv)->FactList->nextFact;
      if (FactData(theEnv)->FactList != NULL)
        { FactData(theEnv)->FactList->previousFact = NULL; }
     }
   else
     {
      theFact->previousFact->nextFact = theFact->nextFact;
      if (theFact->nextFact != NULL)
        { theFact->nextFact->previousFact = theFact->previousFact; }
     }

   /*===================================================*/
   /* Add the fact to the fact garbage list unless this */
   /* fact is being retract as part of a modify action. */
   /*===================================================*/

   if (! modifyOperation)
     {
      theFact->nextFact = FactData(theEnv)->GarbageFacts;
      FactData(theEnv)->GarbageFacts = theFact;
      UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
     }
   else
     {
      theFact->nextFact = NULL;
     }
   theFact->garbage = true;

   /*===================================================*/
   /* Reset the evaluation error flag since expressions */
   /* will be evaluated as part of the retract.         */
   /*===================================================*/

   EnvSetEvaluationError(theEnv,false);

   /*===========================================*/
   /* Loop through the list of all the patterns */
   /* that matched the fact and process the     */
   /* retract operation for each one.           */
   /*===========================================*/

   EngineData(theEnv)->JoinOperationInProgress = true;
   NetworkRetract(theEnv,(struct patternMatch *) theFact->list);
   theFact->list = NULL;
   EngineData(theEnv)->JoinOperationInProgress = false;

   /*=========================================*/
   /* Free partial matches that were released */
   /* by the retraction of the fact.          */
   /*=========================================*/

   if (EngineData(theEnv)->ExecutingRule == NULL)
     { FlushGarbagePartialMatches(theEnv); }

   /*=========================================*/
   /* Retract other facts that were logically */
   /* dependent on the fact just retracted.   */
   /*=========================================*/

   ForceLogicalRetractions(theEnv);

   /*===========================================*/
   /* Force periodic cleanup if the retract was */
   /* executed from an embedded application.    */
   /*===========================================*/

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     { CleanCurrentGarbageFrame(theEnv,NULL); }

   /*==================================*/
   /* Update busy counts and ephemeral */
   /* garbage information.             */
   /*==================================*/

   FactDeinstall(theEnv,theFact);

   /*==================================*/
   /* Return true to indicate the fact */
   /* was successfully retracted.      */
   /*==================================*/

   return true;
  }

/*********************************************************/
/* EnvRetract: C access routine for the retract command. */
/*********************************************************/
bool EnvRetract(
  Environment *theEnv,
  Fact *theFact)
  {
   return RetractDriver(theEnv,theFact,false,NULL);
  }

/*******************************************************************/
/* RemoveGarbageFacts: Returns facts that have been retracted to   */
/*   the pool of available memory. It is necessary to postpone     */
/*   returning the facts to memory because RHS actions retrieve    */
/*   their variable bindings directly from the fact data structure */
/*   and the facts may be in use in other data structures.         */
/*******************************************************************/
static void RemoveGarbageFacts(
  Environment *theEnv)
  {
   struct fact *factPtr, *nextPtr, *lastPtr = NULL;

   factPtr = FactData(theEnv)->GarbageFacts;

   while (factPtr != NULL)
     {
      nextPtr = factPtr->nextFact;
      if (factPtr->factHeader.busyCount == 0)
        {
         ReturnFact(theEnv,factPtr);
         if (lastPtr == NULL) FactData(theEnv)->GarbageFacts = nextPtr;
         else lastPtr->nextFact = nextPtr;
        }
      else
        { lastPtr = factPtr; }

      factPtr = nextPtr;
     }
  }

/********************************************************/
/* AssertDriver: Driver routine for the assert command. */
/********************************************************/
Fact *AssertDriver(
  Environment *theEnv,
  Fact *theFact,
  long long reuseIndex,
  Fact *factListPosition,
  Fact *templatePosition,
  char *changeMap)
  {
   unsigned long hashValue;
   unsigned long length, i;
   struct field *theField;
   bool duplicate;
   struct callFunctionItemWithArg *theAssertFunction;

   /*==========================================*/
   /* A fact can not be asserted while another */
   /* fact is being asserted or retracted.     */
   /*==========================================*/

   if (EngineData(theEnv)->JoinOperationInProgress)
     {
      ReturnFact(theEnv,theFact);
      PrintErrorID(theEnv,"FACTMNGR",2,true);
      EnvPrintRouter(theEnv,WERROR,"Facts may not be asserted during pattern-matching\n");
      return NULL;
     }

   /*=============================================================*/
   /* Replace invalid data types in the fact with the symbol nil. */
   /*=============================================================*/

   length = theFact->theProposition.length;
   theField = theFact->theProposition.theFields;

   for (i = 0; i < length; i++)
     {
      if (theField[i].value == VoidConstant(theEnv))
        { theField[i].value = EnvCreateSymbol(theEnv,"nil"); }
     }

   /*========================================================*/
   /* If fact assertions are being checked for duplications, */
   /* then search the fact list for a duplicate fact.        */
   /*========================================================*/

   hashValue = HandleFactDuplication(theEnv,theFact,&duplicate,reuseIndex);
   if (duplicate) return NULL;

   /*==========================================================*/
   /* If necessary, add logical dependency links between the   */
   /* fact and the partial match which is its logical support. */
   /*==========================================================*/

   if (AddLogicalDependencies(theEnv,(struct patternEntity *) theFact,false) == false)
     {
      if (reuseIndex == 0)
        { ReturnFact(theEnv,theFact); }
      else
        {
         theFact->nextFact = FactData(theEnv)->GarbageFacts;
         FactData(theEnv)->GarbageFacts = theFact;
         UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
         theFact->garbage = true;
        }
      return NULL;
     }

   /*======================================*/
   /* Add the fact to the fact hash table. */
   /*======================================*/

   AddHashedFact(theEnv,theFact,hashValue);

   /*================================*/
   /* Add the fact to the fact list. */
   /*================================*/

   if (reuseIndex == 0)
     { factListPosition = FactData(theEnv)->LastFact; }

   if (factListPosition == NULL)
     {
      theFact->nextFact = FactData(theEnv)->FactList;
      FactData(theEnv)->FactList = theFact;
      theFact->previousFact = NULL;
      if (theFact->nextFact != NULL)
        { theFact->nextFact->previousFact = theFact; }
     }
   else
     {
      theFact->nextFact = factListPosition->nextFact;
      theFact->previousFact = factListPosition;
      factListPosition->nextFact = theFact;
      if (theFact->nextFact != NULL)
        { theFact->nextFact->previousFact = theFact; }
     }

   if ((FactData(theEnv)->LastFact == NULL) || (theFact->nextFact == NULL))
     { FactData(theEnv)->LastFact = theFact; }

   /*====================================*/
   /* Add the fact to its template list. */
   /*====================================*/

   if (reuseIndex == 0)
     { templatePosition = theFact->whichDeftemplate->lastFact; }

   if (templatePosition == NULL)
     {
      theFact->nextTemplateFact = theFact->whichDeftemplate->factList;
      theFact->whichDeftemplate->factList = theFact;
      theFact->previousTemplateFact = NULL;
      if (theFact->nextTemplateFact != NULL)
        { theFact->nextTemplateFact->previousTemplateFact = theFact; }
     }
   else
     {
      theFact->nextTemplateFact = templatePosition->nextTemplateFact;
      theFact->previousTemplateFact = templatePosition;
      templatePosition->nextTemplateFact = theFact;
      if (theFact->nextTemplateFact != NULL)
        { theFact->nextTemplateFact->previousTemplateFact = theFact; }
     }

   if ((theFact->whichDeftemplate->lastFact == NULL) || (theFact->nextTemplateFact == NULL))
     { theFact->whichDeftemplate->lastFact = theFact; }

   /*==================================*/
   /* Set the fact index and time tag. */
   /*==================================*/

   if (reuseIndex > 0)
     { theFact->factIndex = reuseIndex; }
   else
     { theFact->factIndex = FactData(theEnv)->NextFactIndex++; }

   theFact->factHeader.timeTag = DefruleData(theEnv)->CurrentEntityTimeTag++;

   /*=====================*/
   /* Update busy counts. */
   /*=====================*/

   FactInstall(theEnv,theFact);

   /*==========================================*/
   /* Execute the list of functions that are   */
   /* to be called before each fact assertion. */
   /*==========================================*/

   for (theAssertFunction = FactData(theEnv)->ListOfAssertFunctions;
        theAssertFunction != NULL;
        theAssertFunction = theAssertFunction->next)
     {
      SetEnvironmentCallbackContext(theEnv,theAssertFunction->context);
      (*theAssertFunction->func)(theEnv,theFact);
     }

   /*==========================*/
   /* Print assert output if   */
   /* facts are being watched. */
   /*==========================*/

#if DEBUGGING_FUNCTIONS
   if (theFact->whichDeftemplate->watch)
     {
      EnvPrintRouter(theEnv,WTRACE,"==> ");
      PrintFactWithIdentifier(theEnv,WTRACE,theFact,changeMap);
      EnvPrintRouter(theEnv,WTRACE,"\n");
     }
#endif

   /*==================================*/
   /* Set the change flag to indicate  */
   /* the fact-list has been modified. */
   /*==================================*/

   FactData(theEnv)->ChangeToFactList = true;

   /*==========================================*/
   /* Check for constraint errors in the fact. */
   /*==========================================*/

   CheckTemplateFact(theEnv,theFact);

   /*===================================================*/
   /* Reset the evaluation error flag since expressions */
   /* will be evaluated as part of the assert .         */
   /*===================================================*/

   EnvSetEvaluationError(theEnv,false);

   /*=============================================*/
   /* Pattern match the fact using the associated */
   /* deftemplate's pattern network.              */
   /*=============================================*/

   EngineData(theEnv)->JoinOperationInProgress = true;
   FactPatternMatch(theEnv,theFact,theFact->whichDeftemplate->patternNetwork,0,NULL,NULL);
   EngineData(theEnv)->JoinOperationInProgress = false;

   /*===================================================*/
   /* Retract other facts that were logically dependent */
   /* on the non-existence of the fact just asserted.   */
   /*===================================================*/

   ForceLogicalRetractions(theEnv);

   /*=========================================*/
   /* Free partial matches that were released */
   /* by the assertion of the fact.           */
   /*=========================================*/

   if (EngineData(theEnv)->ExecutingRule == NULL) FlushGarbagePartialMatches(theEnv);

   /*==========================================*/
   /* Force periodic cleanup if the assert was */
   /* executed from an embedded application.   */
   /*==========================================*/

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     {
      CleanCurrentGarbageFrame(theEnv,NULL);
      CallPeriodicTasks(theEnv);
     }

   /*===============================*/
   /* Return a pointer to the fact. */
   /*===============================*/

   return theFact;
  }

/********************************************************/
/* EnvAssert: C access routine for the assert function. */
/********************************************************/
Fact *EnvAssert(
  Environment *theEnv,
  Fact *theFact)
  {
   return AssertDriver(theEnv,theFact,0,NULL,NULL,NULL);
  }

/**************************************/
/* RemoveAllFacts: Loops through the  */
/*   fact-list and removes each fact. */
/**************************************/
void RemoveAllFacts(
  Environment *theEnv)
  {
   while (FactData(theEnv)->FactList != NULL)
     { EnvRetract(theEnv,FactData(theEnv)->FactList); }
  }

/************************************************/
/* EnvCreateFact: Creates a fact data structure */
/*   of the specified deftemplate.              */
/************************************************/
Fact *EnvCreateFact(
  Environment *theEnv,
  Deftemplate *theDeftemplate)
  {
   Fact *newFact;
   int i;

   /*=================================*/
   /* A deftemplate must be specified */
   /* in order to create a fact.      */
   /*=================================*/

   if (theDeftemplate == NULL) return NULL;

   /*============================================*/
   /* Create a fact for an explicit deftemplate. */
   /*============================================*/

   if (theDeftemplate->implied == false)
     {
      newFact = CreateFactBySize(theEnv,theDeftemplate->numberOfSlots);
      for (i = 0;
           i < (int) theDeftemplate->numberOfSlots;
           i++)
        { newFact->theProposition.theFields[i].voidValue = VoidConstant(theEnv); }
     }

   /*===========================================*/
   /* Create a fact for an implied deftemplate. */
   /*===========================================*/

   else
     {
      newFact = CreateFactBySize(theEnv,1);
      newFact->theProposition.theFields[0].value = CreateUnmanagedMultifield(theEnv,0L);
     }

   /*===============================*/
   /* Return a pointer to the fact. */
   /*===============================*/

   newFact->whichDeftemplate = theDeftemplate;

   return newFact;
  }

/******************************************/
/* EnvGetFactSlot: Returns the slot value */
/*   from the specified slot of a fact.   */
/******************************************/
bool EnvGetFactSlot(
  Environment *theEnv,
  Fact *theFact,
  const char *slotName,
  CLIPSValue *theValue)
  {
   Deftemplate *theDeftemplate;
   short whichSlot;

   /*===============================================*/
   /* Get the deftemplate associated with the fact. */
   /*===============================================*/

   theDeftemplate = theFact->whichDeftemplate;

   /*==============================================*/
   /* Handle retrieving the slot value from a fact */
   /* having an implied deftemplate. An implied    */
   /* facts has a single multifield slot.          */
   /*==============================================*/

   if (theDeftemplate->implied)
     {
      if (slotName != NULL) return false;
      theValue->value = theFact->theProposition.theFields[0].value;
      return true;
     }

   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   if (FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&whichSlot) == NULL)
     { return false; }

   /*======================================================*/
   /* Return the slot value. If the slot value wasn't set, */
   /* then return false to indicate that an appropriate    */
   /* slot value wasn't available.                         */
   /*======================================================*/

   theValue->value = theFact->theProposition.theFields[whichSlot-1].value;

   if (theValue->header->type == VOID_TYPE) return false;

   return true;
  }

/***************************************/
/* EnvPutFactSlot: Sets the slot value */
/*   of the specified slot of a fact.  */
/***************************************/
bool EnvPutFactSlot(
  Environment *theEnv,
  Fact *theFact,
  const char *slotName,
  CLIPSValue *theValue)
  {
   Deftemplate *theDeftemplate;
   struct templateSlot *theSlot;
   short whichSlot;

   /*===============================================*/
   /* Get the deftemplate associated with the fact. */
   /*===============================================*/

   theDeftemplate = theFact->whichDeftemplate;

   /*============================================*/
   /* Handle setting the slot value of a fact    */
   /* having an implied deftemplate. An implied  */
   /* facts has a single multifield slot.        */
   /*============================================*/

   if (theDeftemplate->implied)
     {
      if ((slotName != NULL) || (theValue->header->type != MULTIFIELD_TYPE))
        { return false; }

      if (theFact->theProposition.theFields[0].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theFact->theProposition.theFields[0].multifieldValue); }

      theFact->theProposition.theFields[0].value = CopyMultifield(theEnv,theValue->multifieldValue);

      return true;
     }

   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&whichSlot)) == NULL)
     { return false; }

   /*=============================================*/
   /* Make sure a single field value is not being */
   /* stored in a multifield slot or vice versa.  */
   /*=============================================*/

   if (((theSlot->multislot == 0) && (theValue->header->type == MULTIFIELD_TYPE)) ||
       ((theSlot->multislot == 1) && (theValue->header->type != MULTIFIELD_TYPE)))
     { return false; }

   /*=====================*/
   /* Set the slot value. */
   /*=====================*/

   if (theFact->theProposition.theFields[whichSlot-1].header->type == MULTIFIELD_TYPE)
     { ReturnMultifield(theEnv,theFact->theProposition.theFields[whichSlot-1].multifieldValue); }

   if (theValue->header->type == MULTIFIELD_TYPE)
     { theFact->theProposition.theFields[whichSlot-1].multifieldValue = CopyMultifield(theEnv,theValue->multifieldValue); }
   else
     { theFact->theProposition.theFields[whichSlot-1].value = theValue->value; }

   return true;
  }

/********************************************************/
/* EnvAssignFactSlotDefaults: Sets a fact's slot values */
/*   to its default value if the value of the slot has  */
/*   not yet been set.                                  */
/********************************************************/
bool EnvAssignFactSlotDefaults(
  Environment *theEnv,
  Fact *theFact)
  {
   Deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;
   int i;
   UDFValue theResult;

   /*===============================================*/
   /* Get the deftemplate associated with the fact. */
   /*===============================================*/

   theDeftemplate = theFact->whichDeftemplate;

   /*================================================*/
   /* The value for the implied multifield slot of   */
   /* an implied deftemplate is set to a multifield  */
   /* of length zero when the fact is created.       */
   /*================================================*/

   if (theDeftemplate->implied) return true;

   /*============================================*/
   /* Loop through each slot of the deftemplate. */
   /*============================================*/

   for (i = 0, slotPtr = theDeftemplate->slotList;
        i < (int) theDeftemplate->numberOfSlots;
        i++, slotPtr = slotPtr->next)
     {
      /*===================================*/
      /* If the slot's value has been set, */
      /* then move on to the next slot.    */
      /*===================================*/

      if (theFact->theProposition.theFields[i].value != VoidConstant(theEnv)) continue;

      /*======================================================*/
      /* Assign the default value for the slot if one exists. */
      /*======================================================*/

      if (DeftemplateSlotDefault(theEnv,theDeftemplate,slotPtr,&theResult,false))
        {
         theFact->theProposition.theFields[i].value = theResult.value;
        }
     }

   /*==========================================*/
   /* Return true to indicate that the default */
   /* values have been successfully set.       */
   /*==========================================*/

   return true;
  }

/********************************************************/
/* DeftemplateSlotDefault: Determines the default value */
/*   for the specified slot of a deftemplate.           */
/********************************************************/
bool DeftemplateSlotDefault(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  struct templateSlot *slotPtr,
  UDFValue *theResult,
  bool garbageMultifield)
  {
   /*================================================*/
   /* The value for the implied multifield slot of an */
   /* implied deftemplate does not have a default.    */
   /*=================================================*/

   if (theDeftemplate->implied) return false;

   /*===============================================*/
   /* If the (default ?NONE) attribute was declared */
   /* for the slot, then return false to indicate   */
   /* the default values for the fact couldn't be   */
   /* supplied since this attribute requires that a */
   /* default value can't be used for the slot.     */
   /*===============================================*/

   if (slotPtr->noDefault) return false;

   /*==============================================*/
   /* Otherwise if a static default was specified, */
   /* use this as the default value.               */
   /*==============================================*/

   else if (slotPtr->defaultPresent)
     {
      if (slotPtr->multislot)
        {
         StoreInMultifield(theEnv,theResult,slotPtr->defaultList,garbageMultifield);
        }
      else
        {
         theResult->value = slotPtr->defaultList->value;
        }
     }

   /*================================================*/
   /* Otherwise if a dynamic-default was specified,  */
   /* evaluate it and use this as the default value. */
   /*================================================*/

   else if (slotPtr->defaultDynamic)
     {
      if (! EvaluateAndStoreInDataObject(theEnv,(int) slotPtr->multislot,
                                         (Expression *) slotPtr->defaultList,
                                         theResult,garbageMultifield))
        { return false; }
     }

   /*====================================*/
   /* Otherwise derive the default value */
   /* from the slot's constraints.       */
   /*====================================*/

   else
     {
      DeriveDefaultFromConstraints(theEnv,slotPtr->constraints,theResult,
                                  (int) slotPtr->multislot,garbageMultifield);
     }

   /*==========================================*/
   /* Return true to indicate that the default */
   /* values have been successfully set.       */
   /*==========================================*/

   return true;
  }

/***************************************************************/
/* CopyFactSlotValues: Copies the slot values from one fact to */
/*   another. Both facts must have the same relation name.     */
/***************************************************************/
bool CopyFactSlotValues(
  Environment *theEnv,
  Fact *theDestFact,
  Fact *theSourceFact)
  {
   Deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;
   int i;

   /*===================================*/
   /* Both facts must be the same type. */
   /*===================================*/

   theDeftemplate = theSourceFact->whichDeftemplate;
   if (theDestFact->whichDeftemplate != theDeftemplate)
     { return false; }

   /*===================================================*/
   /* Loop through each slot of the deftemplate copying */
   /* the source fact value to the destination fact.    */
   /*===================================================*/

   for (i = 0, slotPtr = theDeftemplate->slotList;
        i < (int) theDeftemplate->numberOfSlots;
        i++, slotPtr = slotPtr->next)
     {
      if (theSourceFact->theProposition.theFields[i].header->type != MULTIFIELD_TYPE)
        {
         theDestFact->theProposition.theFields[i].value =
           theSourceFact->theProposition.theFields[i].value;
        }
      else
        {
         theDestFact->theProposition.theFields[i].value = 
           CopyMultifield(theEnv,theSourceFact->theProposition.theFields[i].multifieldValue);
        }
     }

   /*========================================*/
   /* Return true to indicate that fact slot */
   /* values were successfully copied.       */
   /*========================================*/

   return true;
  }

/*********************************************/
/* CreateFactBySize: Allocates a fact data   */
/*   structure based on the number of slots. */
/*********************************************/
Fact *CreateFactBySize(
  Environment *theEnv,
  unsigned size)
  {
   struct fact *theFact;
   unsigned newSize;

   if (size <= 0) newSize = 1;
   else newSize = size;

   theFact = get_var_struct(theEnv,fact,sizeof(struct field) * (newSize - 1));

   theFact->factHeader.th.type = FACT_ADDRESS_TYPE;
   theFact->garbage = false;
   theFact->factIndex = 0LL;
   theFact->factHeader.busyCount = 0;
   theFact->factHeader.theInfo = &FactData(theEnv)->FactInfo;
   theFact->factHeader.dependents = NULL;
   theFact->whichDeftemplate = NULL;
   theFact->nextFact = NULL;
   theFact->previousFact = NULL;
   theFact->previousTemplateFact = NULL;
   theFact->nextTemplateFact = NULL;
   theFact->list = NULL;
   theFact->basisSlots = NULL;

   theFact->theProposition.length = size;
   theFact->theProposition.busyCount = 0;

   return(theFact);
  }

/*********************************************/
/* ReturnFact: Returns a fact data structure */
/*   to the pool of free memory.             */
/*********************************************/
void ReturnFact(
  Environment *theEnv,
  Fact *theFact)
  {
   Multifield *theSegment, *subSegment;
   long newSize, i;

   theSegment = &theFact->theProposition;

   for (i = 0; i < theSegment->length; i++)
     {
      if (theSegment->theFields[i].header->type == MULTIFIELD_TYPE)
        {
         subSegment = theSegment->theFields[i].multifieldValue;
         if (subSegment->busyCount == 0)
           { ReturnMultifield(theEnv,subSegment); }
         else
           { AddToMultifieldList(theEnv,subSegment); }
        }
     }

   if (theFact->theProposition.length == 0) newSize = 1;
   else newSize = theFact->theProposition.length;

   rtn_var_struct(theEnv,fact,sizeof(struct field) * (newSize - 1),theFact);
  }

/*************************************************************/
/* FactInstall: Increments the fact, deftemplate, and atomic */
/*   data value busy counts associated with the fact.        */
/*************************************************************/
void FactInstall(
  Environment *theEnv,
  Fact *newFact)
  {
   struct multifield *theSegment;
   int i;

   FactData(theEnv)->NumberOfFacts++;
   newFact->whichDeftemplate->busyCount++;
   theSegment = &newFact->theProposition;

   for (i = 0 ; i < (int) theSegment->length ; i++)
     {
      AtomInstall(theEnv,theSegment->theFields[i].header->type,theSegment->theFields[i].value);
     }

   newFact->factHeader.busyCount++;
  }

/***************************************************************/
/* FactDeinstall: Decrements the fact, deftemplate, and atomic */
/*   data value busy counts associated with the fact.          */
/***************************************************************/
void FactDeinstall(
  Environment *theEnv,
  Fact *newFact)
  {
   Multifield *theSegment;
   int i;

   FactData(theEnv)->NumberOfFacts--;
   theSegment = &newFact->theProposition;
   newFact->whichDeftemplate->busyCount--;

   for (i = 0 ; i < (int) theSegment->length ; i++)
     {
      AtomDeinstall(theEnv,theSegment->theFields[i].header->type,theSegment->theFields[i].value);
     }

   newFact->factHeader.busyCount--;
  }

/************************************************/
/* EnvIncrementFactCount: Increments the number */
/*   of references to a specified fact.         */
/************************************************/
void EnvIncrementFactCount(
  Environment *theEnv,
  Fact *factPtr)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   factPtr->factHeader.busyCount++;
  }

/************************************************/
/* EnvDecrementFactCount: Decrements the number */
/*   of references to a specified fact.         */
/************************************************/
void EnvDecrementFactCount(
  Environment *theEnv,
  Fact *factPtr)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   factPtr->factHeader.busyCount--;
  }

/*********************************************************/
/* EnvGetNextFact: If passed a NULL pointer, returns the */
/*   first fact in the fact-list. Otherwise returns the  */
/*   next fact following the fact passed as an argument. */
/*********************************************************/
Fact *EnvGetNextFact(
  Environment *theEnv,
  Fact *factPtr)
  {
   if (factPtr == NULL)
     { return FactData(theEnv)->FactList; }

   if (factPtr->garbage) return NULL;

   return factPtr->nextFact;
  }

/**************************************************/
/* GetNextFactInScope: Returns the next fact that */
/*   is in scope of the current module. Works in  */
/*   a similar fashion to GetNextFact, but skips  */
/*   facts that are out of scope.                 */
/**************************************************/
Fact *GetNextFactInScope(
  Environment *theEnv,
  Fact *theFact)
  {
   /*=======================================================*/
   /* If fact passed as an argument is a NULL pointer, then */
   /* we're just beginning a traversal of the fact list. If */
   /* the module index has changed since that last time the */
   /* fact list was traversed by this routine, then         */
   /* determine all of the deftemplates that are in scope   */
   /* of the current module.                                */
   /*=======================================================*/

   if (theFact == NULL)
     {
      theFact = FactData(theEnv)->FactList;
      if (FactData(theEnv)->LastModuleIndex != DefmoduleData(theEnv)->ModuleChangeIndex)
        {
         UpdateDeftemplateScope(theEnv);
         FactData(theEnv)->LastModuleIndex = DefmoduleData(theEnv)->ModuleChangeIndex;
        }
     }

   /*==================================================*/
   /* Otherwise, if the fact passed as an argument has */
   /* been retracted, then there's no way to determine */
   /* the next fact, so return a NULL pointer.         */
   /*==================================================*/

   else if (theFact->garbage)
     { return NULL; }

   /*==================================================*/
   /* Otherwise, start the search for the next fact in */
   /* scope with the fact immediately following the    */
   /* fact passed as an argument.                      */
   /*==================================================*/

   else
     { theFact = theFact->nextFact; }

   /*================================================*/
   /* Continue traversing the fact-list until a fact */
   /* is found that's associated with a deftemplate  */
   /* that's in scope.                               */
   /*================================================*/

   while (theFact != NULL)
     {
      if (theFact->whichDeftemplate->inScope) return theFact;

      theFact = theFact->nextFact;
     }

   return NULL;
  }

/*************************************/
/* FactPPForm: Returns the pretty    */
/*   print representation of a fact. */
/*************************************/
void FactPPForm(
  Fact *theFact,
  char *buffer,
  size_t bufferLength)
  {
   Environment *theEnv = theFact->whichDeftemplate->header.env;
   
   OpenStringDestination(theEnv,"FactPPForm",buffer,bufferLength);
   PrintFactWithIdentifier(theEnv,"FactPPForm",theFact,NULL);
   CloseStringDestination(theEnv,"FactPPForm");
  }

/**********************************/
/* EnvFactIndex: C access routine */
/*   for the fact-index function. */
/**********************************/
long long EnvFactIndex(
  Environment *theEnv,
  Fact *factPtr)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   return factPtr->factIndex;
  }

/*************************************/
/* EnvAssertString: C access routine */
/*   for the assert-string function. */
/*************************************/
Fact *EnvAssertString(
  Environment *theEnv,
  const char *theString)
  {
   struct fact *theFact;
   int danglingConstructs;
   danglingConstructs = ConstructData(theEnv)->DanglingConstructs;

   if ((theFact = StringToFact(theEnv,theString)) == NULL) return NULL;

   if ((! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL))
     { ConstructData(theEnv)->DanglingConstructs = danglingConstructs; }

   return EnvAssert(theEnv,theFact);
  }

/******************************************************/
/* EnvGetFactListChanged: Returns the flag indicating */
/*   whether a change to the fact-list has been made. */
/******************************************************/
bool EnvGetFactListChanged(
  Environment *theEnv)
  {
   return(FactData(theEnv)->ChangeToFactList);
  }

/***********************************************************/
/* EnvSetFactListChanged: Sets the flag indicating whether */
/*   a change to the fact-list has been made.              */
/***********************************************************/
void EnvSetFactListChanged(
  Environment *theEnv,
  bool value)
  {
   FactData(theEnv)->ChangeToFactList = value;
  }

/****************************************/
/* GetNumberOfFacts: Returns the number */
/* of facts in the fact-list.           */
/****************************************/
unsigned long GetNumberOfFacts(
  Environment *theEnv)
  {
   return(FactData(theEnv)->NumberOfFacts);
  }

/***********************************************************/
/* ResetFacts: Reset function for facts. Sets the starting */
/*   fact index to zero and removes all facts.             */
/***********************************************************/
static void ResetFacts(
  Environment *theEnv)
  {
   /*====================================*/
   /* Initialize the fact index to zero. */
   /*====================================*/

   FactData(theEnv)->NextFactIndex = 1L;

   /*======================================*/
   /* Remove all facts from the fact list. */
   /*======================================*/

   RemoveAllFacts(theEnv);
  }

/************************************************************/
/* ClearFactsReady: Clear ready function for facts. Returns */
/*   true if facts were successfully removed and the clear  */
/*   command can continue, otherwise false.                 */
/************************************************************/
static bool ClearFactsReady(
  Environment *theEnv)
  {
   /*======================================*/
   /* Facts can not be deleted when a join */
   /* operation is already in progress.    */
   /*======================================*/

   if (EngineData(theEnv)->JoinOperationInProgress) return false;

   /*====================================*/
   /* Initialize the fact index to zero. */
   /*====================================*/

   FactData(theEnv)->NextFactIndex = 1L;

   /*======================================*/
   /* Remove all facts from the fact list. */
   /*======================================*/

   RemoveAllFacts(theEnv);

   /*==============================================*/
   /* If for some reason there are any facts still */
   /* remaining, don't continue with the clear.    */
   /*==============================================*/

   if (EnvGetNextFact(theEnv,NULL) != NULL) return false;

   /*=============================*/
   /* Return true to indicate the */
   /* clear command can continue. */
   /*=============================*/

   return true;
  }

/***************************************************/
/* FindIndexedFact: Returns a pointer to a fact in */
/*   the fact list with the specified fact index.  */
/***************************************************/
Fact *FindIndexedFact(
  Environment *theEnv,
  long long factIndexSought)
  {
   Fact *theFact;

   for (theFact = EnvGetNextFact(theEnv,NULL);
        theFact != NULL;
        theFact = EnvGetNextFact(theEnv,theFact))
     {
      if (theFact->factIndex == factIndexSought)
        { return(theFact); }
     }

   return NULL;
  }

/*****************************************/
/* EnvAddAssertFunction: Adds a function */
/*   to the ListOfAssertFunctions.       */
/*****************************************/
bool EnvAddAssertFunction(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *, void *),
  int priority)
  {
   FactData(theEnv)->ListOfAssertFunctions =
      AddFunctionToCallListWithArg(theEnv,name,priority,
                                              functionPtr,
                                              FactData(theEnv)->ListOfAssertFunctions);
   return true;
  }

/********************************************/
/* EnvAddAssertFunctionWithContext: Adds a  */
/*   function to the ListOfAssertFunctions. */
/********************************************/
bool EnvAddAssertFunctionWithContext(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *, void *),
  int priority,
  void *context)
  {
   FactData(theEnv)->ListOfAssertFunctions =
      AddFunctionToCallListWithArgWithContext(theEnv,name,priority,functionPtr,
                                       FactData(theEnv)->ListOfAssertFunctions,
                                       context);
   return true;
  }

/***********************************************/
/* EnvRemoveAssertFunction: Removes a function */
/*   from the ListOfAssertFunctions.           */
/***********************************************/
bool EnvRemoveAssertFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   FactData(theEnv)->ListOfAssertFunctions =
      RemoveFunctionFromCallListWithArg(theEnv,name,FactData(theEnv)->ListOfAssertFunctions,&found);

   if (found) return true;

   return false;
  }

/******************************************/
/* EnvAddRetractFunction: Adds a function */
/*   to the ListOfRetractFunctions.       */
/******************************************/
bool EnvAddRetractFunction(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *, void *),
  int priority)
  {
   FactData(theEnv)->ListOfRetractFunctions =
      AddFunctionToCallListWithArg(theEnv,name,priority,
                                              functionPtr,
                                              FactData(theEnv)->ListOfRetractFunctions);
   return true;
  }

/*********************************************/
/* EnvAddRetractFunctionWithContext: Adds a  */
/*   function to the ListOfRetractFunctions. */
/*********************************************/
bool EnvAddRetractFunctionWithContext(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *, void *),
  int priority,
  void *context)
  {
   FactData(theEnv)->ListOfRetractFunctions =
      AddFunctionToCallListWithArgWithContext(theEnv,name,priority,functionPtr,
                                       FactData(theEnv)->ListOfRetractFunctions,
                                       context);
   return true;
  }

/************************************************/
/* EnvRemoveRetractFunction: Removes a function */
/*   from the ListOfRetractFunctions.           */
/************************************************/
bool EnvRemoveRetractFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   FactData(theEnv)->ListOfRetractFunctions =
      RemoveFunctionFromCallListWithArg(theEnv,name,FactData(theEnv)->ListOfRetractFunctions,&found);

   if (found) return true;

   return false;
  }

/*****************************************/
/* EnvAddModifyFunction: Adds a function */
/*   to the ListOfModifyFunctions.       */
/*****************************************/
bool EnvAddModifyFunction(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *, void *, void *),
  int priority)
  {
   FactData(theEnv)->ListOfModifyFunctions =
      AddFunctionToCallListWithArg(theEnv,name,priority,
                                              (void (*)(Environment *, void *)) functionPtr,
                                              FactData(theEnv)->ListOfModifyFunctions);
   return true;
  }

/********************************************/
/* EnvAddModifyFunctionWithContext: Adds a  */
/*   function to the ListOfModifyFunctions. */
/********************************************/
bool EnvAddModifyFunctionWithContext(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *, void *, void *),
  int priority,
  void *context)
  {
   FactData(theEnv)->ListOfModifyFunctions =
      AddFunctionToCallListWithArgWithContext(theEnv,name,priority,
                                       (void (*)(Environment *, void *)) functionPtr,
                                       FactData(theEnv)->ListOfModifyFunctions,
                                       context);
   return true;
  }

/***********************************************/
/* EnvRemoveModifyFunction: Removes a function */
/*   from the ListOfModifyFunctions.           */
/***********************************************/
bool EnvRemoveModifyFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   FactData(theEnv)->ListOfModifyFunctions =
      RemoveFunctionFromCallListWithArg(theEnv,name,FactData(theEnv)->ListOfModifyFunctions,&found);

   if (found) return true;

   return false;
  }

/*************************/
/* EnvCreateFactBuilder: */
/*************************/
FactBuilder *EnvCreateFactBuilder(
  Environment *theEnv,
  const char *deftemplateName)
  {
   FactBuilder *theFB;
   Deftemplate *theDeftemplate;
   int i;
   
   theDeftemplate = EnvFindDeftemplate(theEnv,deftemplateName);
   if (theDeftemplate == NULL) return NULL;
   
   if (theDeftemplate->implied) return NULL;
   
   theFB = get_struct(theEnv,factBuilder);
   if (theFB == NULL) return NULL;
   
   theFB->fbEnv = theEnv;
   theFB->fbDeftemplate = theDeftemplate;
      
   theFB->fbValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * theDeftemplate->numberOfSlots);

   for (i = 0; i < theDeftemplate->numberOfSlots; i++)
     { theFB->fbValueArray[i].voidValue = VoidConstant(theEnv); }

   return theFB;
  }

/********************/
/* FBPutSlotInteger */
/********************/
bool FBPutSlotInteger(
  FactBuilder *theFB,
  const char *slotName,
  CLIPSInteger *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.integerValue = slotValue;
   return FBPutSlot(theFB,slotName,&theValue);
  }

/*******************/
/* FMPutSlotLexeme */
/*******************/
bool FBPutSlotLexeme(
  FactBuilder *theFB,
  const char *slotName,
  CLIPSLexeme *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.lexemeValue = slotValue;
   return FBPutSlot(theFB,slotName,&theValue);
  }

/******************/
/* FMPutSlotFloat */
/******************/
bool FBPutSlotFloat(
  FactBuilder *theFB,
  const char *slotName,
  CLIPSFloat *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.floatValue = slotValue;
   return FBPutSlot(theFB,slotName,&theValue);
  }

/*****************/
/* FMPutSlotFact */
/*****************/
bool FBPutSlotFact(
  FactBuilder *theFB,
  const char *slotName,
  Fact *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.factValue = slotValue;
   return FBPutSlot(theFB,slotName,&theValue);
  }

/*********************/
/* FBPutSlotInstance */
/*********************/
bool FBPutSlotInstance(
  FactBuilder *theFB,
  const char *slotName,
  Instance *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.instanceValue = slotValue;
   return FBPutSlot(theFB,slotName,&theValue);
  }

/****************************/
/* FBPutSlotExternalAddress */
/****************************/
bool FBPutSlotExternalAddress(
  FactBuilder *theFB,
  const char *slotName,
  CLIPSExternalAddress *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.externalAddressValue = slotValue;
   return FBPutSlot(theFB,slotName,&theValue);
  }

/***********************/
/* FBPutSlotMultifield */
/***********************/
bool FBPutSlotMultifield(
  FactBuilder *theFB,
  const char *slotName,
  Multifield *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.multifieldValue = slotValue;
   return FBPutSlot(theFB,slotName,&theValue);
  }

/**************/
/* FBPutSlot: */
/**************/
bool FBPutSlot(
  FactBuilder *theFB,
  const char *slotName,
  CLIPSValue *slotValue)
  {
   Environment *theEnv = theFB->fbEnv;
   struct templateSlot *theSlot;
   short whichSlot;
   CLIPSValue oldValue;
   int i;
      
   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   if ((theSlot = FindSlot(theFB->fbDeftemplate,EnvCreateSymbol(theFB->fbEnv,slotName),&whichSlot)) == NULL)
     { return false; }
     
   /*=============================================*/
   /* Make sure a single field value is not being */
   /* stored in a multifield slot or vice versa.  */
   /*=============================================*/

   if (((theSlot->multislot == 0) && (slotValue->header->type == MULTIFIELD_TYPE)) ||
       ((theSlot->multislot == 1) && (slotValue->header->type != MULTIFIELD_TYPE)))
     { return false; }
     
   if (theFB->fbValueArray == NULL)
     {
      theFB->fbValueArray = (CLIPSValue *) gm3(theFB->fbEnv,sizeof(CLIPSValue) * theFB->fbDeftemplate->numberOfSlots);
      for (i = 0; i < theFB->fbDeftemplate->numberOfSlots; i++)
        { theFB->fbValueArray[i].voidValue = theFB->fbEnv->VoidConstant; }
     }

   /*=====================*/
   /* Set the slot value. */
   /*=====================*/
   
   oldValue.value = theFB->fbValueArray[whichSlot-1].value;
   
   if (oldValue.header->type == MULTIFIELD_TYPE)
     {
      if (MultifieldsEqual(oldValue.multifieldValue,slotValue->multifieldValue))
        { return true; }
     }
   else
     {
      if (oldValue.value == slotValue->value)
        { return true; }
     }
   
   CVAtomDeinstall(theEnv,oldValue.value);
   
   if (oldValue.header->type == MULTIFIELD_TYPE)
     { ReturnMultifield(theEnv,oldValue.multifieldValue); }

   if (slotValue->header->type == MULTIFIELD_TYPE)
     { theFB->fbValueArray[whichSlot-1].multifieldValue = CopyMultifield(theEnv,slotValue->multifieldValue); }
   else
     { theFB->fbValueArray[whichSlot-1].value = slotValue->value; }
      
   CVAtomInstall(theEnv,theFB->fbValueArray[whichSlot-1].value);
   
   return true;
  }

/*************/
/* FBAssert: */
/*************/
Fact *FBAssert(
  FactBuilder *theFB)
  {
   Environment *theEnv = theFB->fbEnv;
   int i;
   Fact *theFact;
   
   theFact = EnvCreateFact(theEnv,theFB->fbDeftemplate);
   
   for (i = 0; i < theFB->fbDeftemplate->numberOfSlots; i++)
     {
      if (theFB->fbValueArray[i].voidValue != VoidConstant(theEnv))
        {
         theFact->theProposition.theFields[i].value = theFB->fbValueArray[i].value;
         CVAtomDeinstall(theEnv,theFB->fbValueArray[i].value);
         theFB->fbValueArray[i].voidValue = VoidConstant(theEnv);
        }
     }

   EnvAssignFactSlotDefaults(theEnv,theFact);
   
   theFact = EnvAssert(theEnv,theFact);
      
   return theFact;
  }

/**************/
/* FBDispose: */
/**************/
void FBDispose(
  FactBuilder *theFB)
  {
   Environment *theEnv = theFB->fbEnv;

   FBAbort(theFB);
   
   if (theFB->fbValueArray != NULL)
     { rm3(theEnv,theFB->fbValueArray,sizeof(CLIPSValue) * theFB->fbDeftemplate->numberOfSlots); }
   
   rtn_struct(theEnv,factBuilder,theFB);
  }

/************/
/* FBAbort: */
/************/
void FBAbort(
  FactBuilder *theFB)
  {
   Environment *theEnv = theFB->fbEnv;
   int i;
   
   for (i = 0; i < theFB->fbDeftemplate->numberOfSlots; i++)
     {
      CVAtomDeinstall(theEnv,theFB->fbValueArray[i].value);
      
      if (theFB->fbValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theFB->fbValueArray[i].multifieldValue); }
        
      theFB->fbValueArray[i].voidValue = VoidConstant(theEnv);
     }
  }

/********************/
/* FBSetDeftemplate */
/********************/
bool FBSetDeftemplate(
  FactBuilder *theFB,
  const char *deftemplateName)
  {
   Deftemplate *theDeftemplate;
   Environment *theEnv = theFB->fbEnv;
   int i;
   
   FBAbort(theFB);
   
   theDeftemplate = EnvFindDeftemplate(theFB->fbEnv,deftemplateName);
   
   if (theDeftemplate == NULL) return false;
   if (theDeftemplate->implied) return false;

   if (theFB->fbValueArray != NULL)
     { rm3(theEnv,theFB->fbValueArray,sizeof(CLIPSValue) * theFB->fbDeftemplate->numberOfSlots); }

   theFB->fbDeftemplate = theDeftemplate;
   
   theFB->fbValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * theDeftemplate->numberOfSlots);

   for (i = 0; i < theDeftemplate->numberOfSlots; i++)
     { theFB->fbValueArray[i].voidValue = VoidConstant(theEnv); }

   return true;
  }

/**************************/
/* EnvCreateFactModifier: */
/**************************/
FactModifier *EnvCreateFactModifier(
  Environment *theEnv,
  Fact *oldFact)
  {
   FactModifier *theFM;
   int i;

   if (oldFact->garbage) return NULL;
   if (oldFact->whichDeftemplate->implied) return NULL;
   if (oldFact->whichDeftemplate->numberOfSlots == 0) return NULL;

   theFM = get_struct(theEnv,factModifier);
   if (theFM == NULL) return NULL;

   theFM->fmEnv = theEnv;
   theFM->fmOldFact = oldFact;
   EnvIncrementFactCount(theEnv,oldFact);

   theFM->fmValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * oldFact->whichDeftemplate->numberOfSlots);

   for (i = 0; i < oldFact->whichDeftemplate->numberOfSlots; i++)
     { theFM->fmValueArray[i].voidValue = VoidConstant(theEnv); }

   theFM->changeMap = (char *) gm2(theEnv,CountToBitMapSize(oldFact->whichDeftemplate->numberOfSlots));
   ClearBitString((void *) theFM->changeMap,CountToBitMapSize(oldFact->whichDeftemplate->numberOfSlots));

   return theFM;
  }

/********************/
/* FMPutSlotInteger */
/********************/
bool FMPutSlotInteger(
  FactModifier *theFM,
  const char *slotName,
  CLIPSInteger *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.integerValue = slotValue;
   return FMPutSlot(theFM,slotName,&theValue);
  }

/*******************/
/* FMPutSlotLexeme */
/*******************/
bool FMPutSlotLexeme(
  FactModifier *theFM,
  const char *slotName,
  CLIPSLexeme *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.lexemeValue = slotValue;
   return FMPutSlot(theFM,slotName,&theValue);
  }

/******************/
/* FMPutSlotFloat */
/******************/
bool FMPutSlotFloat(
  FactModifier *theFM,
  const char *slotName,
  CLIPSFloat *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.floatValue = slotValue;
   return FMPutSlot(theFM,slotName,&theValue);
  }

/*****************/
/* FMPutSlotFact */
/*****************/
bool FMPutSlotFact(
  FactModifier *theFM,
  const char *slotName,
  Fact *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.factValue = slotValue;
   return FMPutSlot(theFM,slotName,&theValue);
  }

/*********************/
/* FMPutSlotInstance */
/*********************/
bool FMPutSlotInstance(
  FactModifier *theFM,
  const char *slotName,
  Instance *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.instanceValue = slotValue;
   return FMPutSlot(theFM,slotName,&theValue);
  }

/****************************/
/* FMPutSlotExternalAddress */
/****************************/
bool FMPutSlotExternalAddress(
  FactModifier *theFM,
  const char *slotName,
  CLIPSExternalAddress *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.externalAddressValue = slotValue;
   return FMPutSlot(theFM,slotName,&theValue);
  }

/***********************/
/* FMPutSlotMultifield */
/***********************/
bool FMPutSlotMultifield(
  FactModifier *theFM,
  const char *slotName,
  Multifield *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.multifieldValue = slotValue;
   return FMPutSlot(theFM,slotName,&theValue);
  }

/**************/
/* FMPutSlot: */
/**************/
bool FMPutSlot(
  FactModifier *theFM,
  const char *slotName,
  CLIPSValue *slotValue)
  {
   Environment *theEnv = theFM->fmEnv;
   struct templateSlot *theSlot;
   short whichSlot;
   CLIPSValue oldValue;
   CLIPSValue oldFactValue;
   int i;

   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   if ((theSlot = FindSlot(theFM->fmOldFact->whichDeftemplate,EnvCreateSymbol(theEnv,slotName),&whichSlot)) == NULL)
     { return false; }

   /*=============================================*/
   /* Make sure a single field value is not being */
   /* stored in a multifield slot or vice versa.  */
   /*=============================================*/

   if (((theSlot->multislot == 0) && (slotValue->header->type == MULTIFIELD_TYPE)) ||
       ((theSlot->multislot == 1) && (slotValue->header->type != MULTIFIELD_TYPE)))
     { return false; }

   if (theFM->fmValueArray == NULL)
     {
      theFM->fmValueArray = (CLIPSValue *) gm3(theFM->fmEnv,sizeof(CLIPSValue) * theFM->fmOldFact->whichDeftemplate->numberOfSlots);
      for (i = 0; i < theFM->fmOldFact->whichDeftemplate->numberOfSlots; i++)
        { theFM->fmValueArray[i].voidValue = theFM->fmEnv->VoidConstant; }
     }

   if (theFM->changeMap == NULL)
     {
      theFM->changeMap = (char *) gm2(theFM->fmEnv,CountToBitMapSize(theFM->fmOldFact->whichDeftemplate->numberOfSlots));
      ClearBitString((void *) theFM->changeMap,CountToBitMapSize(theFM->fmOldFact->whichDeftemplate->numberOfSlots));
     }
     
   /*=====================*/
   /* Set the slot value. */
   /*=====================*/

   oldValue.value = theFM->fmValueArray[whichSlot-1].value;
   oldFactValue.value = theFM->fmOldFact->theProposition.theFields[whichSlot-1].value;

   if (oldFactValue.header->type == MULTIFIELD_TYPE)
     {
      if (MultifieldsEqual(oldFactValue.multifieldValue,slotValue->multifieldValue))
        {
         CVAtomDeinstall(theFM->fmEnv,oldValue.value);
         if (oldValue.header->type == MULTIFIELD_TYPE)
           { ReturnMultifield(theFM->fmEnv,oldValue.multifieldValue); }
         theFM->fmValueArray[whichSlot-1].voidValue = theFM->fmEnv->VoidConstant;
         ClearBitMap(theFM->changeMap,whichSlot-1);
         return true;
        }

      if (MultifieldsEqual(oldValue.multifieldValue,slotValue->multifieldValue))
        { return true; }
     }
   else
     {
      if (slotValue->value == oldFactValue.value)
        {
         CVAtomDeinstall(theFM->fmEnv,oldValue.value);
         theFM->fmValueArray[whichSlot-1].voidValue = theFM->fmEnv->VoidConstant;
         ClearBitMap(theFM->changeMap,whichSlot-1);
         return true;
        }
        
      if (oldValue.value == slotValue->value)
        { return true; }
     }

   SetBitMap(theFM->changeMap,whichSlot-1);

   CVAtomDeinstall(theFM->fmEnv,oldValue.value);

   if (oldValue.header->type == MULTIFIELD_TYPE)
     { ReturnMultifield(theFM->fmEnv,oldValue.multifieldValue); }
      
   if (slotValue->header->type == MULTIFIELD_TYPE)
     { theFM->fmValueArray[whichSlot-1].multifieldValue = CopyMultifield(theFM->fmEnv,slotValue->multifieldValue); }
   else
     { theFM->fmValueArray[whichSlot-1].value = slotValue->value; }

   CVAtomInstall(theFM->fmEnv,theFM->fmValueArray[whichSlot-1].value);

   return true;
  }

/*************/
/* FMModify: */
/*************/
Fact *FMModify(
  FactModifier *theFM)
  {
   Fact *rv;
   
   if (! BitStringHasBitsSet(theFM->changeMap,CountToBitMapSize(theFM->fmOldFact->whichDeftemplate->numberOfSlots)))
     { return theFM->fmOldFact; }
     
   rv = ReplaceFact(theFM->fmEnv,theFM->fmOldFact,theFM->fmValueArray,theFM->changeMap);
   
   FMAbort(theFM);
   
   return rv;
  }

/**************/
/* FMDispose: */
/**************/
void FMDispose(
  FactModifier *theFM)
  {
   Environment *theEnv = theFM->fmEnv;
   int i;

   /*========================*/
   /* Clear the value array. */
   /*========================*/
   
   for (i = 0; i < theFM->fmOldFact->whichDeftemplate->numberOfSlots; i++)
     {
      CVAtomDeinstall(theEnv,theFM->fmValueArray[i].value);

      if (theFM->fmValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theFM->fmValueArray[i].multifieldValue); }
     }
   
   /*=====================================*/
   /* Return the value and change arrays. */
   /*=====================================*/
   
   if (theFM->fmValueArray != NULL)
     { rm3(theEnv,theFM->fmValueArray,sizeof(CLIPSValue) * theFM->fmOldFact->whichDeftemplate->numberOfSlots); }
      
   if (theFM->changeMap != NULL)
     { rm(theEnv,(void *) theFM->changeMap,CountToBitMapSize(theFM->fmOldFact->whichDeftemplate->numberOfSlots)); }

   /*====================================*/
   /* Return the FactModifier structure. */
   /*====================================*/
   
   EnvDecrementFactCount(theEnv,theFM->fmOldFact);
   
   rtn_struct(theEnv,factModifier,theFM);
  }

/************/
/* FMAbort: */
/************/
void FMAbort(
  FactModifier *theFM)
  {
   Environment *theEnv = theFM->fmEnv;
   int i;
   
   for (i = 0; i < theFM->fmOldFact->whichDeftemplate->numberOfSlots; i++)
     {
      CVAtomDeinstall(theEnv,theFM->fmValueArray[i].value);

      if (theFM->fmValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theFM->fmValueArray[i].multifieldValue); }
        
      theFM->fmValueArray[i].voidValue = theFM->fmEnv->VoidConstant;
     }
     
   ClearBitString((void *) theFM->changeMap,CountToBitMapSize(theFM->fmOldFact->whichDeftemplate->numberOfSlots));
  }

/**************/
/* FMSetFact: */
/**************/
bool FMSetFact(
  FactModifier *theFM,
  Fact *oldFact)
  {
   Environment *theEnv = theFM->fmEnv;
   unsigned short currentSlotCount = theFM->fmOldFact->whichDeftemplate->numberOfSlots;
   int i;
   
   /*=================================================*/
   /* Modifiers can only be created for non-retracted */
   /* deftemplate facts with at least one slot.       */
   /*=================================================*/
   
   if (oldFact->garbage) return false;
   if (oldFact->whichDeftemplate->implied) return false;
   if (oldFact->whichDeftemplate->numberOfSlots == 0) return false;

   /*========================*/
   /* Clear the value array. */
   /*========================*/
   
   for (i = 0; i < theFM->fmOldFact->whichDeftemplate->numberOfSlots; i++)
     {
      CVAtomDeinstall(theEnv,theFM->fmValueArray[i].value);

      if (theFM->fmValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theFM->fmValueArray[i].multifieldValue); }
     }

   /*==================================================*/
   /* Resize the value and change arrays if necessary. */
   /*==================================================*/
   
   if (oldFact->whichDeftemplate->numberOfSlots != currentSlotCount)
     {
      if (theFM->fmValueArray != NULL)
        { rm3(theEnv,theFM->fmValueArray,sizeof(CLIPSValue) * currentSlotCount); }
      
      if (theFM->changeMap != NULL)
        { rm(theEnv,(void *) theFM->changeMap,currentSlotCount); }
        
      theFM->fmValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * oldFact->whichDeftemplate->numberOfSlots);
      theFM->changeMap = (char *) gm2(theEnv,CountToBitMapSize(oldFact->whichDeftemplate->numberOfSlots));
     }
   
   /*=================================*/
   /* Update the fact being modified. */
   /*=================================*/
   
   EnvDecrementFactCount(theEnv,theFM->fmOldFact);
   theFM->fmOldFact = oldFact;
   EnvIncrementFactCount(theEnv,theFM->fmOldFact);
   
   /*=========================================*/
   /* Initialize the value and change arrays. */
   /*=========================================*/
   
   for (i = 0; i < theFM->fmOldFact->whichDeftemplate->numberOfSlots; i++)
     { theFM->fmValueArray[i].voidValue = theFM->fmEnv->VoidConstant; }
   
   ClearBitString((void *) theFM->changeMap,CountToBitMapSize(theFM->fmOldFact->whichDeftemplate->numberOfSlots));

   /*================================================================*/
   /* Return true to indicate the modifier was successfully created. */
   /*================================================================*/
   
   return true;
  }


#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT */

