   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/01/16             */
   /*                                                     */
   /*             DEFTEMPLATE FUNCTIONS MODULE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the modify and duplicate functions.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Added deftemplate-slot-names,                  */
/*            deftemplate-slot-default-value,                */
/*            deftemplate-slot-cardinality,                  */
/*            deftemplate-slot-allowed-values,               */
/*            deftemplate-slot-range,                        */
/*            deftemplate-slot-types,                        */
/*            deftemplate-slot-multip,                       */
/*            deftemplate-slot-singlep,                      */
/*            deftemplate-slot-existp, and                   */
/*            deftemplate-slot-defaultp functions.           */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for deftemplate slot facets.           */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added deftemplate-slot-facet-existp and        */
/*            deftemplate-slot-facet-value functions.        */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Support for modify callback function.          */
/*                                                           */
/*            Added additional argument to function          */
/*            CheckDeftemplateAndSlotArguments to specify    */
/*            the expected number of arguments.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
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
/*************************************************************/

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>
#include <string.h>

#include "argacces.h"
#include "commline.h"
#include "constant.h"
#include "cstrnchk.h"
#include "default.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "factmngr.h"
#include "factrhs.h"
#include "memalloc.h"
#include "modulutl.h"
#include "prcdrpsr.h"
#include "reorder.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "sysdep.h"
#include "tmpltdef.h"
#include "tmpltlhs.h"
#include "tmpltrhs.h"
#include "tmpltutl.h"

#include "tmpltfun.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DuplicateModifyCommand(Environment *,int,UDFValue *);
   static CLIPSLexeme            *CheckDeftemplateAndSlotArguments(UDFContext *,Deftemplate **);

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static struct expr            *ModAndDupParse(Environment *,struct expr *,const char *,const char *);
#endif

/****************************************************************/
/* DeftemplateFunctions: Initializes the deftemplate functions. */
/****************************************************************/
void DeftemplateFunctions(
  Environment *theEnv)
  {
#if ! RUN_TIME
   EnvAddUDF(theEnv,"modify","bf",0,UNBOUNDED,NULL,ModifyCommand,"ModifyCommand",NULL);
   EnvAddUDF(theEnv,"duplicate","bf",0,UNBOUNDED,NULL,DuplicateCommand,"DuplicateCommand",NULL);

   EnvAddUDF(theEnv,"deftemplate-slot-names","bm",1,1,"y",DeftemplateSlotNamesFunction,"DeftemplateSlotNamesFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-default-value","*",2,2,"y",DeftemplateSlotDefaultValueFunction,"DeftemplateSlotDefaultValueFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-cardinality","*",2,2,"y",DeftemplateSlotCardinalityFunction,"DeftemplateSlotCardinalityFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-allowed-values","*",2,2,"y",DeftemplateSlotAllowedValuesFunction,"DeftemplateSlotAllowedValuesFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-range","*",2,2,"y",DeftemplateSlotRangeFunction,"DeftemplateSlotRangeFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-types","*",2,2,"y",DeftemplateSlotTypesFunction,"DeftemplateSlotTypesFunction",NULL);

   EnvAddUDF(theEnv,"deftemplate-slot-multip","b",2,2,"y",DeftemplateSlotMultiPFunction,"DeftemplateSlotMultiPFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-singlep","b",2,2,"y",DeftemplateSlotSinglePFunction,"DeftemplateSlotSinglePFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-existp","b",2,2,"y",DeftemplateSlotExistPFunction,"DeftemplateSlotExistPFunction",NULL);
   EnvAddUDF(theEnv,"deftemplate-slot-defaultp","y",2,2,"y",DeftemplateSlotDefaultPFunction,"DeftemplateSlotDefaultPFunction",NULL);

   EnvAddUDF(theEnv,"deftemplate-slot-facet-existp","b",3,3,"y",DeftemplateSlotFacetExistPFunction,"DeftemplateSlotFacetExistPFunction",NULL);

   EnvAddUDF(theEnv,"deftemplate-slot-facet-value","*",3,3,"y",DeftemplateSlotFacetValueFunction,"DeftemplateSlotFacetValueFunction",NULL);

#if (! BLOAD_ONLY)
   AddFunctionParser(theEnv,"modify",ModifyParse);
   AddFunctionParser(theEnv,"duplicate",DuplicateParse);
#endif
   FuncSeqOvlFlags(theEnv,"modify",false,false);
   FuncSeqOvlFlags(theEnv,"duplicate",false,false);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/*********************************************************************/
/* ModifyCommand: H/L access routine for the modify command. Calls   */
/*   the DuplicateModifyCommand function to perform the actual work. */
/*********************************************************************/
void ModifyCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   DuplicateModifyCommand(theEnv,true,returnValue);
  }

/***************************************************************************/
/* DuplicateCommand: H/L access routine for the duplicate command. Calls   */
/*   the DuplicateModifyCommand function to perform the actual work.       */
/***************************************************************************/
void DuplicateCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   DuplicateModifyCommand(theEnv,false,returnValue);
  }

/***************************************************************/
/* DuplicateModifyCommand: Implements the duplicate and modify */
/*   commands. The fact being duplicated or modified is first  */
/*   copied to a new fact. Replacements to the fields of the   */
/*   new fact are then made. If a modify command is being      */
/*   performed, the original fact is retracted. Lastly, the    */
/*   new fact is asserted.                                     */
/***************************************************************/
static void DuplicateModifyCommand(
  Environment *theEnv,
  int retractIt,
  UDFValue *returnValue)
  {
   long long factNum;
   Fact *oldFact, *newFact, *theFact;
   struct expr *testPtr;
   UDFValue computeResult;
   Deftemplate *templatePtr;
   struct templateSlot *slotPtr;
   int i, position;
   bool found;

   /*===================================================*/
   /* Set the default return value to the symbol FALSE. */
   /*===================================================*/

   returnValue->lexemeValue = theEnv->FalseSymbol;

   /*==================================================*/
   /* Evaluate the first argument which is used to get */
   /* a pointer to the fact to be modified/duplicated. */
   /*==================================================*/

   testPtr = GetFirstArgument();
   EnvIncrementClearReadyLocks(theEnv);
   EvaluateExpression(theEnv,testPtr,&computeResult);
   EnvDecrementClearReadyLocks(theEnv);

   /*==============================================================*/
   /* If an integer is supplied, then treat it as a fact-index and */
   /* search the fact-list for the fact with that fact-index.      */
   /*==============================================================*/

   if (computeResult.header->type == INTEGER)
     {
      factNum = computeResult.integerValue->contents;
      if (factNum < 0)
        {
         if (retractIt) ExpectedTypeError2(theEnv,"modify",1);
         else ExpectedTypeError2(theEnv,"duplicate",1);
         EnvSetEvaluationError(theEnv,true);
         return;
        }

      oldFact = EnvGetNextFact(theEnv,NULL);
      while (oldFact != NULL)
        {
         if (oldFact->factIndex == factNum)
           { break; }
         else
           { oldFact = oldFact->nextFact; }
        }

      if (oldFact == NULL)
        {
         char tempBuffer[20];
         gensprintf(tempBuffer,"f-%lld",factNum);
         CantFindItemErrorMessage(theEnv,"fact",tempBuffer);
         return;
        }
     }

   /*==========================================*/
   /* Otherwise, if a pointer is supplied then */
   /* no lookup is required.                   */
   /*==========================================*/

   else if (computeResult.header->type == FACT_ADDRESS)
     { oldFact = computeResult.factValue; }

   /*===========================================*/
   /* Otherwise, the first argument is invalid. */
   /*===========================================*/

   else
     {
      if (retractIt) ExpectedTypeError2(theEnv,"modify",1);
      else ExpectedTypeError2(theEnv,"duplicate",1);
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   /*==================================*/
   /* See if it is a deftemplate fact. */
   /*==================================*/

   templatePtr = oldFact->whichDeftemplate;

   if (templatePtr->implied) return;

   /*================================================================*/
   /* Duplicate the values from the old fact (skipping multifields). */
   /*================================================================*/

   newFact = CreateFactBySize(theEnv,oldFact->theProposition.multifieldLength);
   newFact->whichDeftemplate = templatePtr;
   for (i = 0; i < (int) oldFact->theProposition.multifieldLength; i++)
     {
      if (oldFact->theProposition.theFields[i].header->type != MULTIFIELD)
        { newFact->theProposition.theFields[i].value = oldFact->theProposition.theFields[i].value; }
      else
        { newFact->theProposition.theFields[i].value = NULL; }
     }

   /*========================*/
   /* Start replacing slots. */
   /*========================*/

   testPtr = testPtr->nextArg;
   while (testPtr != NULL)
     {
      /*============================================================*/
      /* If the slot identifier is an integer, then the slot was    */
      /* previously identified and its position within the template */
      /* was stored. Otherwise, the position of the slot within the */
      /* deftemplate has to be determined by comparing the name of  */
      /* the slot against the list of slots for the deftemplate.    */
      /*============================================================*/

      if (testPtr->type == INTEGER)
        { position = (int) testPtr->integerValue->contents; }
      else
        {
         found = false;
         position = 0;
         slotPtr = templatePtr->slotList;
         while (slotPtr != NULL)
           {
            if (slotPtr->slotName == testPtr->lexemeValue)
              {
               found = true;
               slotPtr = NULL;
              }
            else
              {
               slotPtr = slotPtr->next;
               position++;
              }
           }

         if (! found)
           {
            InvalidDeftemplateSlotMessage(theEnv,testPtr->lexemeValue->contents,
                                          templatePtr->header.name->contents,true);
            EnvSetEvaluationError(theEnv,true);
            ReturnFact(theEnv,newFact);
            return;
           }
        }

      /*===================================================*/
      /* If a single field slot is being replaced, then... */
      /*===================================================*/

      if (newFact->theProposition.theFields[position].value != NULL)
        {
         /*======================================================*/
         /* If the list of values to store in the slot is empty  */
         /* or contains more than one member than an error has   */
         /* occured because a single field slot can only contain */
         /* a single value.                                      */
         /*======================================================*/

         if ((testPtr->argList == NULL) ? true : (testPtr->argList->nextArg != NULL))
           {
            MultiIntoSingleFieldSlotError(theEnv,GetNthSlot(templatePtr,position),templatePtr);
            ReturnFact(theEnv,newFact);
            return;
           }

         /*===================================================*/
         /* Evaluate the expression to be stored in the slot. */
         /*===================================================*/

         EnvIncrementClearReadyLocks(theEnv);
         EvaluateExpression(theEnv,testPtr->argList,&computeResult);
         EnvSetEvaluationError(theEnv,false);
         EnvDecrementClearReadyLocks(theEnv);

         /*====================================================*/
         /* If the expression evaluated to a multifield value, */
         /* then an error occured since a multifield value can */
         /* not be stored in a single field slot.              */
         /*====================================================*/

         if (computeResult.header->type == MULTIFIELD)
           {
            ReturnFact(theEnv,newFact);
            MultiIntoSingleFieldSlotError(theEnv,GetNthSlot(templatePtr,position),templatePtr);
            return;
           }

         /*=============================*/
         /* Store the value in the slot */
         /*=============================*/

         newFact->theProposition.theFields[position].value = computeResult.value;
        }

      /*=================================*/
      /* Else replace a multifield slot. */
      /*=================================*/

      else
        {
         /*======================================*/
         /* Determine the new value of the slot. */
         /*======================================*/

         EnvIncrementClearReadyLocks(theEnv);
         StoreInMultifield(theEnv,&computeResult,testPtr->argList,false);
         EnvSetEvaluationError(theEnv,false);
         EnvDecrementClearReadyLocks(theEnv);

         /*=============================*/
         /* Store the value in the slot */
         /*=============================*/

         newFact->theProposition.theFields[position].value = computeResult.value;
        }

      testPtr = testPtr->nextArg;
     }

   /*=====================================*/
   /* Copy the multifield values from the */
   /* old fact that were not replaced.    */
   /*=====================================*/

   for (i = 0; i < (int) oldFact->theProposition.multifieldLength; i++)
     {
      if ((oldFact->theProposition.theFields[i].header->type == MULTIFIELD) &&
          (newFact->theProposition.theFields[i].value == NULL))

        {
         newFact->theProposition.theFields[i].value =
            CopyMultifield(theEnv,oldFact->theProposition.theFields[i].multifieldValue);
        }
     }

   /*================================================*/
   /* Call registered modify notification functions. */
   /*================================================*/

   if (retractIt &&
       (FactData(theEnv)->ListOfModifyFunctions != NULL))
     {
      struct callFunctionItemWithArg *theModifyFunction;
      struct fact *replacement = newFact;

      /*==================================================================*/
      /* If the fact already exists, determine if it's the fact we're     */
      /* modifying. If so it will be retracted and reasserted. If not,    */
      /* it will just be retracted, so pass NULL as the replacement fact. */
      /*==================================================================*/

      if (! FactWillBeAsserted(theEnv,newFact))
        {
         if (! MultifieldsEqual(&oldFact->theProposition,
                              &newFact->theProposition))
           { replacement = NULL; }
        }

      /*=========================================================*/
      /* Preassign the factIndex and timeTag so the notification */
      /* function will see the correct values.                   */
      /*=========================================================*/

      if (replacement != NULL)
        {
         replacement->factIndex = FactData(theEnv)->NextFactIndex;
         replacement->factHeader.timeTag = DefruleData(theEnv)->CurrentEntityTimeTag;
        }

      /*=========================================*/
      /* Call each modify notification function. */
      /*=========================================*/

      for (theModifyFunction = FactData(theEnv)->ListOfModifyFunctions;
           theModifyFunction != NULL;
           theModifyFunction = theModifyFunction->next)
        {
         SetEnvironmentCallbackContext(theEnv,theModifyFunction->context);
         ((void (*)(void *,void *,void *))(*theModifyFunction->func))(theEnv,oldFact,replacement);
        }
     }

   /*======================================*/
   /* Perform the duplicate/modify action. */
   /*======================================*/

   if (retractIt) EnvRetract(theEnv,oldFact);
   theFact = EnvAssert(theEnv,newFact);

   /*========================================*/
   /* The asserted fact is the return value. */
   /*========================================*/

   if (theFact != NULL)
     {
      returnValue->begin = 0;
      returnValue->range = theFact->theProposition.multifieldLength;
      returnValue->value = theFact;
     }

   return;
  }

/****************************************************/
/* DeftemplateSlotNamesFunction: H/L access routine */
/*   for the deftemplate-slot-names function.       */
/****************************************************/
void DeftemplateSlotNamesFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   const char *deftemplateName;
   Deftemplate *theDeftemplate;

   /*=============================================*/
   /* Set up the default return value for errors. */
   /*=============================================*/

   returnValue->value = theEnv->FalseSymbol;

   /*=======================================*/
   /* Get the reference to the deftemplate. */
   /*=======================================*/

   deftemplateName = GetConstructName(context,"deftemplate-slot-names","deftemplate name");
   if (deftemplateName == NULL) return;

   theDeftemplate = EnvFindDeftemplate(theEnv,deftemplateName);
   if (theDeftemplate == NULL)
     {
      CantFindItemErrorMessage(theEnv,"deftemplate",deftemplateName);
      return;
     }

   /*=====================*/
   /* Get the slot names. */
   /*=====================*/

   EnvDeftemplateSlotNames(theEnv,theDeftemplate,returnValue);
  }

/**********************************************/
/* EnvDeftemplateSlotNames: C access routine  */
/*   for the deftemplate-slot-names function. */
/**********************************************/
void EnvDeftemplateSlotNames(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  UDFValue *returnValue)
  {
   Multifield *theList;
   struct templateSlot *theSlot;
   unsigned long count;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      returnValue->begin = 0;
      returnValue->range = 1;
      theList = EnvCreateMultifield(theEnv,(int) 1);
      theList->theFields[0].lexemeValue = EnvCreateSymbol(theEnv,"implied");
      returnValue->value = theList;
      return;
     }

   /*=================================*/
   /* Count the number of slot names. */
   /*=================================*/

   for (count = 0, theSlot = theDeftemplate->slotList;
        theSlot != NULL;
        count++, theSlot = theSlot->next)
     { /* Do Nothing */ }

   /*=============================================================*/
   /* Create a multifield value in which to store the slot names. */
   /*=============================================================*/

   returnValue->begin = 0;
   returnValue->range = count;
   theList = EnvCreateMultifield(theEnv,count);
   returnValue->value = theList;

   /*===============================================*/
   /* Store the slot names in the multifield value. */
   /*===============================================*/

   for (count = 0, theSlot = theDeftemplate->slotList;
        theSlot != NULL;
        count++, theSlot = theSlot->next)
     {
      theList->theFields[count].lexemeValue = theSlot->slotName;
     }
  }

/*******************************************************/
/* DeftemplateSlotDefaultPFunction: H/L access routine */
/*   for the deftemplate-slot-defaultp function.       */
/*******************************************************/
void DeftemplateSlotDefaultPFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;
   int defaultType;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      returnValue->lexemeValue = theEnv->FalseSymbol;
      return;
     }

   /*===============================*/
   /* Does the slot have a default? */
   /*===============================*/

   defaultType = EnvDeftemplateSlotDefaultP(theEnv,theDeftemplate,slotName->contents);

   if (defaultType == STATIC_DEFAULT)
     { returnValue->lexemeValue = EnvCreateSymbol(theEnv,"static"); }
   else if (defaultType == DYNAMIC_DEFAULT)
     { returnValue->lexemeValue = EnvCreateSymbol(theEnv,"dynamic"); }
   else
     { returnValue->lexemeValue = theEnv->FalseSymbol; }
  }

/*************************************************/
/* EnvDeftemplateSlotDefaultP: C access routine  */
/*   for the deftemplate-slot-defaultp function. */
/*************************************************/
int EnvDeftemplateSlotDefaultP(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName)
  {
   short position;
   struct templateSlot *theSlot;

   /*==================================================*/
   /* Make sure the slot exists (the symbol implied is */
   /* used for the implied slot of an ordered fact).   */
   /*==================================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        {
         return(STATIC_DEFAULT);
        }
      else
        {
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return(NO_DEFAULT);
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return(NO_DEFAULT);
     }

   /*======================================*/
   /* Return the default type of the slot. */
   /*======================================*/

   if (theSlot->noDefault)
     { return(NO_DEFAULT); }
   else if (theSlot->defaultDynamic)
     { return(DYNAMIC_DEFAULT); }

   return(STATIC_DEFAULT);
  }

/*************************************************************/
/* DeftemplateSlotDefaultValueFunction: H/L access routine   */
/*   for the deftemplate-slot-default-value function.        */
/*************************************************************/
void DeftemplateSlotDefaultValueFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      returnValue->lexemeValue = theEnv->FalseSymbol;
      return;
     }

   /*=========================================*/
   /* Get the deftemplate slot default value. */
   /*=========================================*/

   EnvDeftemplateSlotDefaultValue(theEnv,theDeftemplate,slotName->contents,returnValue);
  }

/******************************************************/
/* EnvDeftemplateSlotDefaultValue: C access routine   */
/*   for the deftemplate-slot-default-value function. */
/******************************************************/
bool EnvDeftemplateSlotDefaultValue(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName,
  UDFValue *theValue)
  {
   short position;
   struct templateSlot *theSlot;
   UDFValue tempDO;

   /*=============================================*/
   /* Set up the default return value for errors. */
   /*=============================================*/

   theValue->value = theEnv->FalseSymbol;

   /*==================================================*/
   /* Make sure the slot exists (the symbol implied is */
   /* used for the implied slot of an ordered fact).   */
   /*==================================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        {
         theValue->value = EnvCreateMultifield(theEnv,0L);
         theValue->begin = 0;
         theValue->range = 0;
         return true;
        }
      else
        {
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return false;
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return false;
     }

   /*=======================================*/
   /* Return the default value of the slot. */
   /*=======================================*/

   if (theSlot->noDefault)
     {
      theValue->value = EnvCreateSymbol(theEnv,"?NONE");
     }
   else if (DeftemplateSlotDefault(theEnv,theDeftemplate,theSlot,&tempDO,true))
     {
      theValue->begin = tempDO.begin;
      theValue->range = tempDO.range;
      theValue->value = tempDO.value;
     }
   else
     { return false; }

   return true;
  }

/**********************************************************/
/* DeftemplateSlotCardinalityFunction: H/L access routine */
/*   for the deftemplate-slot-cardinality function.       */
/**********************************************************/
void DeftemplateSlotCardinalityFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }

   /*=======================================*/
   /* Get the deftemplate slot cardinality. */
   /*=======================================*/

   EnvDeftemplateSlotCardinality(theEnv,theDeftemplate,slotName->contents,returnValue);
  }

/****************************************************/
/* EnvDeftemplateSlotCardinality: C access routine  */
/*   for the deftemplate-slot-cardinality function. */
/****************************************************/
void EnvDeftemplateSlotCardinality(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName,
  UDFValue *returnValue)
  {
   short position;
   struct templateSlot *theSlot;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        {
         returnValue->begin = 0;
         returnValue->range = 2;
         returnValue->value = EnvCreateMultifield(theEnv,2L);
         returnValue->multifieldValue->theFields[0].integerValue = SymbolData(theEnv)->Zero;
         returnValue->multifieldValue->theFields[1].lexemeValue = SymbolData(theEnv)->PositiveInfinity;
         return;
        }
      else
        {
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return;
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return;
     }

   /*=====================================*/
   /* Return the cardinality of the slot. */
   /*=====================================*/

   if (theSlot->multislot == 0)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }

   returnValue->begin = 0;
   returnValue->range = 2;
   returnValue->value = EnvCreateMultifield(theEnv,2L);

   if (theSlot->constraints != NULL)
     {
      returnValue->multifieldValue->theFields[0].value = theSlot->constraints->minFields->value;
      returnValue->multifieldValue->theFields[1].value = theSlot->constraints->maxFields->value;
     }
   else
     {
      returnValue->multifieldValue->theFields[0].integerValue = SymbolData(theEnv)->Zero;
      returnValue->multifieldValue->theFields[1].lexemeValue = SymbolData(theEnv)->PositiveInfinity;
     }
  }

/************************************************************/
/* DeftemplateSlotAllowedValuesFunction: H/L access routine */
/*   for the deftemplate-slot-allowed-values function.      */
/************************************************************/
void DeftemplateSlotAllowedValuesFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }

   /*==========================================*/
   /* Get the deftemplate slot allowed values. */
   /*==========================================*/

   EnvDeftemplateSlotAllowedValues(theEnv,theDeftemplate,slotName->contents,returnValue);
  }

/*******************************************************/
/* EnvDeftemplateSlotAllowedValues: C access routine   */
/*   for the deftemplate-slot-allowed-values function. */
/*******************************************************/
void EnvDeftemplateSlotAllowedValues(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName,
  UDFValue *returnValue)
  {
   short position;
   struct templateSlot *theSlot;
   int i;
   EXPRESSION *theExp;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        {
         returnValue->value = theEnv->FalseSymbol;
         return;
        }
      else
        {
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return;
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return;
     }

   /*========================================*/
   /* Return the allowed values of the slot. */
   /*========================================*/

   if ((theSlot->constraints != NULL) ? (theSlot->constraints->restrictionList == NULL) : true)
     {
      returnValue->value = theEnv->FalseSymbol;
      return;
     }

   returnValue->begin = 0;
   returnValue->range = ExpressionSize(theSlot->constraints->restrictionList);
   returnValue->value = EnvCreateMultifield(theEnv,(unsigned long) returnValue->range);
   i = 0;

   theExp = theSlot->constraints->restrictionList;
   while (theExp != NULL)
     {
      returnValue->multifieldValue->theFields[i].value = theExp->value;
      theExp = theExp->nextArg;
      i++;
     }
  }

/****************************************************/
/* DeftemplateSlotRangeFunction: H/L access routine */
/*   for the deftemplate-slot-range function.       */
/****************************************************/
void DeftemplateSlotRangeFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }

   /*=================================*/
   /* Get the deftemplate slot range. */
   /*=================================*/

   EnvDeftemplateSlotRange(theEnv,theDeftemplate,slotName->contents,returnValue);
  }

/**********************************************/
/* EnvDeftemplateSlotRange: C access routine  */
/*   for the deftemplate-slot-range function. */
/**********************************************/
void EnvDeftemplateSlotRange(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName,
  UDFValue *returnValue)
  {
   short position;
   struct templateSlot *theSlot;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        {
         returnValue->begin = 0;
         returnValue->range = 2;
         returnValue->value = EnvCreateMultifield(theEnv,2L);
         returnValue->multifieldValue->theFields[0].lexemeValue =
            SymbolData(theEnv)->NegativeInfinity;
         returnValue->multifieldValue->theFields[1].lexemeValue =
            SymbolData(theEnv)->PositiveInfinity;
         return;
        }
      else
        {
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return;
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return;
     }

   /*===============================*/
   /* Return the range of the slot. */
   /*===============================*/

   if ((theSlot->constraints == NULL) ? false :
       (theSlot->constraints->anyAllowed || theSlot->constraints->floatsAllowed ||
        theSlot->constraints->integersAllowed))
     {
      returnValue->begin = 0;
      returnValue->range = 2;
      returnValue->value = EnvCreateMultifield(theEnv,2L);
      returnValue->multifieldValue->theFields[0].value = theSlot->constraints->minValue->value;
      returnValue->multifieldValue->theFields[1].value = theSlot->constraints->maxValue->value;
     }
   else
     {
      returnValue->value = theEnv->FalseSymbol;
      return;
     }
  }

/****************************************************/
/* DeftemplateSlotTypesFunction: H/L access routine */
/*   for the deftemplate-slot-types function.       */
/****************************************************/
void DeftemplateSlotTypesFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }

   /*=================================*/
   /* Get the deftemplate slot types. */
   /*=================================*/

   EnvDeftemplateSlotTypes(theEnv,theDeftemplate,slotName->contents,returnValue);
  }

/**********************************************/
/* EnvDeftemplateSlotTypes: C access routine  */
/*   for the deftemplate-slot-types function. */
/**********************************************/
void EnvDeftemplateSlotTypes(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName,
  UDFValue *returnValue)
  {
   short position;
   struct templateSlot *theSlot = NULL;
   int numTypes, i;
   bool allTypes = false;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot name is "implied."         */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") != 0)
        {
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return;
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return;
     }

   /*==============================================*/
   /* If the slot has no constraint information or */
   /* there is no type restriction, then all types */
   /* are allowed for the slot.                    */
   /*==============================================*/

   if ((theDeftemplate->implied) ||
       ((theSlot->constraints != NULL) ? theSlot->constraints->anyAllowed : true))
     {
#if OBJECT_SYSTEM
      numTypes = 8;
#else
      numTypes = 6;
#endif
      allTypes = true;
     }

   /*==============================================*/
   /* Otherwise count the number of types allowed. */
   /*==============================================*/

   else
     {
      numTypes = theSlot->constraints->symbolsAllowed +
                 theSlot->constraints->stringsAllowed +
                 theSlot->constraints->floatsAllowed +
                 theSlot->constraints->integersAllowed +
                 theSlot->constraints->instanceNamesAllowed +
                 theSlot->constraints->instanceAddressesAllowed +
                 theSlot->constraints->externalAddressesAllowed +
                 theSlot->constraints->factAddressesAllowed;
     }

   /*========================================*/
   /* Return the allowed types for the slot. */
   /*========================================*/

   returnValue->begin = 0;
   returnValue->range = numTypes;
   returnValue->value = EnvCreateMultifield(theEnv,(long) numTypes);

   i = 0;

   if (allTypes || theSlot->constraints->floatsAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"FLOAT");
     }

   if (allTypes || theSlot->constraints->integersAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"INTEGER");
     }

   if (allTypes || theSlot->constraints->symbolsAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"SYMBOL");
     }

   if (allTypes || theSlot->constraints->stringsAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"STRING");
     }

   if (allTypes || theSlot->constraints->externalAddressesAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"EXTERNAL-ADDRESS");
     }

   if (allTypes || theSlot->constraints->factAddressesAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"FACT-ADDRESS");
     }

#if OBJECT_SYSTEM
   if (allTypes || theSlot->constraints->instanceAddressesAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"INSTANCE-ADDRESS");
     }

   if (allTypes || theSlot->constraints->instanceNamesAllowed)
     {
      returnValue->multifieldValue->theFields[i++].lexemeValue = EnvCreateSymbol(theEnv,"INSTANCE-NAME");
     }
#endif
  }

/*****************************************************/
/* DeftemplateSlotMultiPFunction: H/L access routine */
/*   for the deftemplate-slot-multip function.       */
/*****************************************************/
void DeftemplateSlotMultiPFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      returnValue->lexemeValue = theEnv->FalseSymbol;
      return;
     }

   /*================================*/
   /* Is the slot a multifield slot? */
   /*================================*/

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvDeftemplateSlotMultiP(theEnv,theDeftemplate,slotName->contents));
  }

/***********************************************/
/* EnvDeftemplateSlotMultiP: C access routine  */
/*   for the deftemplate-slot-multip function. */
/***********************************************/
bool EnvDeftemplateSlotMultiP(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName)
  {
   short position;
   struct templateSlot *theSlot;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        { return true; }
      else
        {
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return false;
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return false;
     }

   /*================================*/
   /* Is the slot a multifield slot? */
   /*================================*/

   return(theSlot->multislot);
  }

/******************************************************/
/* DeftemplateSlotSinglePFunction: H/L access routine */
/*   for the deftemplate-slot-singlep function.       */
/******************************************************/
void DeftemplateSlotSinglePFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      returnValue->lexemeValue = theEnv->FalseSymbol;
      return;
     }

   /*==================================*/
   /* Is the slot a single field slot? */
   /*==================================*/

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvDeftemplateSlotSingleP(theEnv,theDeftemplate,slotName->contents));
  }

/************************************************/
/* EnvDeftemplateSlotSingleP: C access routine  */
/*   for the deftemplate-slot-singlep function. */
/************************************************/
bool EnvDeftemplateSlotSingleP(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName)
  {
   short position;
   struct templateSlot *theSlot;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        { return false; }
      else
        {
         EnvSetEvaluationError(theEnv,true);
         InvalidDeftemplateSlotMessage(theEnv,slotName,
                                       theDeftemplate->header.name->contents,false);
         return false;
        }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     {
      EnvSetEvaluationError(theEnv,true);
      InvalidDeftemplateSlotMessage(theEnv,slotName,
                                    theDeftemplate->header.name->contents,false);
      return false;
     }

   /*==================================*/
   /* Is the slot a single field slot? */
   /*==================================*/

   return(! theSlot->multislot);
  }

/*****************************************************/
/* DeftemplateSlotExistPFunction: H/L access routine */
/*   for the deftemplate-slot-existp function.       */
/*****************************************************/
void DeftemplateSlotExistPFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      returnValue->lexemeValue = theEnv->FalseSymbol;
      return;
     }

   /*======================*/
   /* Does the slot exist? */
   /*======================*/

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvDeftemplateSlotExistP(theEnv,theDeftemplate,slotName->contents));
  }

/************************************************/
/* EnvDeftemplateSlotExistP: C access routine  */
/*   for the deftemplate-slot-existp function. */
/************************************************/
bool EnvDeftemplateSlotExistP(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName)
  {
   short position;

   /*===============================================*/
   /* If we're dealing with an implied deftemplate, */
   /* then the only slot names is "implied."        */
   /*===============================================*/

   if (theDeftemplate->implied)
     {
      if (strcmp(slotName,"implied") == 0)
        { return true; }
      else
        { return false; }
     }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if (FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position) == NULL)
     { return false; }

   /*==================*/
   /* The slot exists. */
   /*==================*/

   return true;
  }

/**********************************************************/
/* DeftemplateSlotFacetExistPFunction: H/L access routine */
/*   for the deftemplate-slot-facet-existp function.      */
/**********************************************************/
void DeftemplateSlotFacetExistPFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;
   UDFValue facetName;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     {
      returnValue->lexemeValue = theEnv->FalseSymbol;
      return;
     }

   /*============================*/
   /* Get the name of the facet. */
   /*============================*/

   if (! UDFNextArgument(context,SYMBOL_TYPE,&facetName))
     {
      returnValue->lexemeValue = theEnv->FalseSymbol;
      return;
     }

   /*======================*/
   /* Does the slot exist? */
   /*======================*/

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvDeftemplateSlotFacetExistP(theEnv,theDeftemplate,slotName->contents,facetName.lexemeValue->contents));
  }

/*****************************************************/
/* EnvDeftemplateSlotFacetExistP: C access routine   */
/*   for the deftemplate-slot-facet-existp function. */
/*****************************************************/
bool EnvDeftemplateSlotFacetExistP(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName,
  const char *facetName)
  {
   short position;
   struct templateSlot *theSlot;
   CLIPSLexeme *facetHN;
   struct expr *tempFacet;

   /*=================================================*/
   /* An implied deftemplate doesn't have any facets. */
   /*=================================================*/

   if (theDeftemplate->implied)
     { return false; }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     { return false; }

   /*=======================*/
   /* Search for the facet. */
   /*=======================*/

   facetHN = FindSymbolHN(theEnv,facetName,SYMBOL_TYPE);
   for (tempFacet = theSlot->facetList;
        tempFacet != NULL;
        tempFacet = tempFacet->nextArg)
     {
      if (tempFacet->value == facetHN)
        { return true; }
     }

   /*===========================*/
   /* The facet does not exist. */
   /*===========================*/

   return false;
  }

/*********************************************************/
/* DeftemplateSlotFacetValueFunction: H/L access routine */
/*   for the deftemplate-slot-facet-value function.      */
/*********************************************************/
void DeftemplateSlotFacetValueFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   CLIPSLexeme *slotName;
   UDFValue facetName;

   /*=============================================*/
   /* Set up the default return value for errors. */
   /*=============================================*/

   returnValue->lexemeValue = theEnv->FalseSymbol;

   /*===================================================*/
   /* Retrieve the deftemplate and slot name arguments. */
   /*===================================================*/

   slotName = CheckDeftemplateAndSlotArguments(context,&theDeftemplate);
   if (slotName == NULL)
     { return; }

   /*============================*/
   /* Get the name of the facet. */
   /*============================*/

   if (! UDFNthArgument(context,3,SYMBOL_TYPE,&facetName))
     { return; }

   /*===========================*/
   /* Retrieve the facet value. */
   /*===========================*/

   EnvDeftemplateSlotFacetValue(theEnv,theDeftemplate,slotName->contents,facetName.lexemeValue->contents,returnValue);
  }

/****************************************************/
/* EnvDeftemplateSlotFacetValue: C access routine   */
/*   for the deftemplate-slot-facet-value function. */
/****************************************************/
bool EnvDeftemplateSlotFacetValue(
  Environment *theEnv,
  Deftemplate *theDeftemplate,
  const char *slotName,
  const char *facetName,
  UDFValue *rv)
  {
   short position;
   struct templateSlot *theSlot;
   CLIPSLexeme *facetHN;
   struct expr *tempFacet;

   /*=================================================*/
   /* An implied deftemplate doesn't have any facets. */
   /*=================================================*/

   if (theDeftemplate->implied)
     { return false; }

   /*============================================*/
   /* Otherwise search for the slot name in the  */
   /* list of slots defined for the deftemplate. */
   /*============================================*/

   else if ((theSlot = FindSlot(theDeftemplate,EnvCreateSymbol(theEnv,slotName),&position)) == NULL)
     { return false; }

   /*=======================*/
   /* Search for the facet. */
   /*=======================*/

   facetHN = FindSymbolHN(theEnv,facetName,SYMBOL_TYPE);
   for (tempFacet = theSlot->facetList;
        tempFacet != NULL;
        tempFacet = tempFacet->nextArg)
     {
      if (tempFacet->value == facetHN)
        {
         EvaluateExpression(theEnv,tempFacet->argList,rv);
         return true;
        }
     }

   /*===========================*/
   /* The facet does not exist. */
   /*===========================*/

   return false;
  }

/************************************************************/
/* CheckDeftemplateAndSlotArguments: Checks the deftemplate */
/*   and slot arguments for various functions.              */
/************************************************************/
static CLIPSLexeme *CheckDeftemplateAndSlotArguments(
  UDFContext *context,
  Deftemplate **theDeftemplate)
  {
   UDFValue theArg;
   const char *deftemplateName;
   Environment *theEnv = context->environment;

   /*=======================================*/
   /* Get the reference to the deftemplate. */
   /*=======================================*/

   if (! UDFFirstArgument(context,SYMBOL_TYPE,&theArg))
     { return NULL; }

   deftemplateName = theArg.lexemeValue->contents;

   *theDeftemplate = EnvFindDeftemplate(theEnv,deftemplateName);
   if (*theDeftemplate == NULL)
     {
      CantFindItemErrorMessage(theEnv,"deftemplate",deftemplateName);
      return NULL;
     }

   /*===========================*/
   /* Get the name of the slot. */
   /*===========================*/

   if (! UDFNextArgument(context,SYMBOL_TYPE,&theArg))
     { return NULL; }

   return theArg.lexemeValue;
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/***************************************************************/
/* UpdateModifyDuplicate: Changes the modify/duplicate command */
/*   found on the RHS of a rule such that the positions of the */
/*   slots for replacement are stored rather than the slot     */
/*   name which allows quicker replacement of slots. This      */
/*   substitution can only take place when the deftemplate     */
/*   type is known (i.e. if a fact-index is used you don't     */
/*   know which type of deftemplate is going to be replaced    */
/*   until you actually do the replacement of slots).          */
/***************************************************************/
bool UpdateModifyDuplicate(
  Environment *theEnv,
  struct expr *top,
  const char *name,
  void *vTheLHS)
  {
   struct expr *functionArgs, *tempArg;
   CLIPSLexeme *templateName;
   Deftemplate *theDeftemplate;
   struct templateSlot *slotPtr;
   short position;

   /*========================================*/
   /* Determine the fact-address or index to */
   /* be retracted by the modify command.    */
   /*========================================*/

   functionArgs = top->argList;
   if (functionArgs->type == SF_VARIABLE)
     {
      templateName = FindTemplateForFactAddress(functionArgs->lexemeValue,
                                                (struct lhsParseNode *) vTheLHS);
      if (templateName == NULL) return true;
     }
   else
     { return true; }

   /*========================================*/
   /* Make sure that the fact being modified */
   /* has a corresponding deftemplate.       */
   /*========================================*/

   theDeftemplate = (Deftemplate *)
                    LookupConstruct(theEnv,DeftemplateData(theEnv)->DeftemplateConstruct,
                                    templateName->contents,
                                    false);

   if (theDeftemplate == NULL) return true;

   if (theDeftemplate->implied) return true;

   /*=============================================================*/
   /* Make sure all the slot names are valid for the deftemplate. */
   /*=============================================================*/

   tempArg = functionArgs->nextArg;
   while (tempArg != NULL)
     {
      /*======================*/
      /* Does the slot exist? */
      /*======================*/

      if ((slotPtr = FindSlot(theDeftemplate,tempArg->lexemeValue,&position)) == NULL)
        {
         InvalidDeftemplateSlotMessage(theEnv,tempArg->lexemeValue->contents,
                                       theDeftemplate->header.name->contents,true);
         return false;
        }

      /*=========================================================*/
      /* Is a multifield value being put in a single field slot? */
      /*=========================================================*/

      if (slotPtr->multislot == false)
        {
         if (tempArg->argList == NULL)
           {
            SingleFieldSlotCardinalityError(theEnv,slotPtr->slotName->contents);
            return false;
           }
         else if (tempArg->argList->nextArg != NULL)
           {
            SingleFieldSlotCardinalityError(theEnv,slotPtr->slotName->contents);
            return false;
           }
         else if (tempArg->argList->type == FCALL)
           {
            if ((ExpressionUnknownFunctionType(tempArg->argList) & SINGLEFIELD_TYPES) == 0)
              {
               SingleFieldSlotCardinalityError(theEnv,slotPtr->slotName->contents);
               return false;
              }
           }
         else if (tempArg->argList->type == MF_VARIABLE)
           {
            SingleFieldSlotCardinalityError(theEnv,slotPtr->slotName->contents);
            return false;
           }
        }

      /*======================================*/
      /* Are the slot restrictions satisfied? */
      /*======================================*/

      if (CheckRHSSlotTypes(theEnv,tempArg->argList,slotPtr,name) == 0)
        return false;

      /*=============================================*/
      /* Replace the slot with the integer position. */
      /*=============================================*/

      tempArg->type = INTEGER;
      tempArg->value = EnvCreateInteger(theEnv,(long long) (FindSlotPosition(theDeftemplate,tempArg->lexemeValue) - 1));

      tempArg = tempArg->nextArg;
     }

   return true;
  }

/**************************************************/
/* FindTemplateForFactAddress: Searches for the   */
/*   deftemplate name associated with the pattern */
/*   to which a fact address has been bound.      */
/**************************************************/
CLIPSLexeme *FindTemplateForFactAddress(
  CLIPSLexeme *factAddress,
  struct lhsParseNode *theLHS)
  {
   struct lhsParseNode *thePattern = NULL;

   /*===============================================*/
   /* Look through the LHS patterns for the pattern */
   /* which is bound to the fact address used by    */
   /* the modify/duplicate function.                */
   /*===============================================*/

   while (theLHS != NULL)
     {
      if (theLHS->value == (void *) factAddress)
        {
         thePattern = theLHS;
         theLHS = NULL;
        }
      else
        { theLHS = theLHS->bottom; }
     }

   if (thePattern == NULL) return NULL;

   /*=====================================*/
   /* Verify that just a symbol is stored */
   /* as the first field of the pattern.  */
   /*=====================================*/

   thePattern = thePattern->right;
   if ((thePattern->pnType != SF_WILDCARD_NODE) || (thePattern->bottom == NULL))
     { return NULL; }

   thePattern = thePattern->bottom;
   if ((thePattern->pnType != SYMBOL_NODE) ||
            (thePattern->right != NULL) ||
            (thePattern->bottom != NULL))
    { return NULL; }

   /*==============================*/
   /* Return the deftemplate name. */
   /*==============================*/

   return thePattern->lexemeValue;
  }

/*******************************************/
/* ModifyParse: Parses the modify command. */
/*******************************************/
struct expr *ModifyParse(
  Environment *theEnv,
  struct expr *top,
  const char *logicalName)
  {
   return(ModAndDupParse(theEnv,top,logicalName,"modify"));
  }

/*************************************************/
/* DuplicateParse: Parses the duplicate command. */
/*************************************************/
struct expr *DuplicateParse(
  Environment *theEnv,
  struct expr *top,
  const char *logicalName)
  {
   return(ModAndDupParse(theEnv,top,logicalName,"duplicate"));
  }

/*************************************************************/
/* ModAndDupParse: Parses the modify and duplicate commands. */
/*************************************************************/
static struct expr *ModAndDupParse(
  Environment *theEnv,
  struct expr *top,
  const char *logicalName,
  const char *name)
  {
   bool error = false;
   struct token theToken;
   struct expr *nextOne, *tempSlot;
   struct expr *newField, *firstField, *lastField;
   bool printError;
   bool done;

   /*==================================================================*/
   /* Parse the fact-address or index to the modify/duplicate command. */
   /*==================================================================*/

   SavePPBuffer(theEnv," ");
   GetToken(theEnv,logicalName,&theToken);

   if ((theToken.tknType == SF_VARIABLE_TOKEN) || (theToken.tknType == GBL_VARIABLE_TOKEN))
     { nextOne = GenConstant(theEnv,TokenTypeToType(theToken.tknType),theToken.value); }
   else if (theToken.tknType == INTEGER_TOKEN)
     {
      if (! TopLevelCommand(theEnv))
        {
         PrintErrorID(theEnv,"TMPLTFUN",1,true);
         EnvPrintRouter(theEnv,WERROR,"Fact-indexes can only be used by ");
         EnvPrintRouter(theEnv,WERROR,name);
         EnvPrintRouter(theEnv,WERROR," as a top level command.\n");
         ReturnExpression(theEnv,top);
         return NULL;
        }

      nextOne = GenConstant(theEnv,INTEGER,theToken.value);
     }
   else
     {
      ExpectedTypeError2(theEnv,name,1);
      ReturnExpression(theEnv,top);
      return NULL;
     }

   nextOne->nextArg = NULL;
   nextOne->argList = NULL;
   top->argList = nextOne;
   nextOne = top->argList;

   /*=======================================================*/
   /* Parse the remaining modify/duplicate slot specifiers. */
   /*=======================================================*/

   GetToken(theEnv,logicalName,&theToken);
   while (theToken.tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      PPBackup(theEnv);
      SavePPBuffer(theEnv," ");
      SavePPBuffer(theEnv,theToken.printForm);

      /*=================================================*/
      /* Slot definition begins with a left parenthesis. */
      /*=================================================*/

      if (theToken.tknType != LEFT_PARENTHESIS_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"duplicate/modify function");
         ReturnExpression(theEnv,top);
         return NULL;
        }

      /*=================================*/
      /* The slot name must be a symbol. */
      /*=================================*/

      GetToken(theEnv,logicalName,&theToken);
      if (theToken.tknType != SYMBOL_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"duplicate/modify function");
         ReturnExpression(theEnv,top);
         return NULL;
        }

      /*=================================*/
      /* Check for duplicate slot names. */
      /*=================================*/

      for (tempSlot = top->argList->nextArg;
           tempSlot != NULL;
           tempSlot = tempSlot->nextArg)
        {
         if (tempSlot->value == theToken.value)
           {
            AlreadyParsedErrorMessage(theEnv,"slot ",theToken.lexemeValue->contents);
            ReturnExpression(theEnv,top);
            return NULL;
           }
        }

      /*=========================================*/
      /* Add the slot name to the list of slots. */
      /*=========================================*/

      nextOne->nextArg = GenConstant(theEnv,SYMBOL,theToken.value);
      nextOne = nextOne->nextArg;

      /*====================================================*/
      /* Get the values to be stored in the specified slot. */
      /*====================================================*/

      firstField = NULL;
      lastField = NULL;
      done = false;
      while (! done)
        {
         SavePPBuffer(theEnv," ");
         newField = GetAssertArgument(theEnv,logicalName,&theToken,&error,
                                      RIGHT_PARENTHESIS_TOKEN,false,&printError);

         if (error)
           {
            if (printError) SyntaxErrorMessage(theEnv,"deftemplate pattern");
            ReturnExpression(theEnv,top);
            return NULL;
           }

         if (newField == NULL)
           { done = true; }

         if (lastField == NULL)
           { firstField = newField; }
         else
           { lastField->nextArg = newField; }
         lastField = newField;
        }

      /*================================================*/
      /* Slot definition ends with a right parenthesis. */
      /*================================================*/

      if (theToken.tknType != RIGHT_PARENTHESIS_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"duplicate/modify function");
         ReturnExpression(theEnv,top);
         ReturnExpression(theEnv,firstField);
         return NULL;
        }
      else
        {
         PPBackup(theEnv);
         PPBackup(theEnv);
         SavePPBuffer(theEnv,")");
        }

      nextOne->argList = firstField;

      GetToken(theEnv,logicalName,&theToken);
     }

   /*================================================*/
   /* Return the parsed modify/duplicate expression. */
   /*================================================*/

   return(top);
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFTEMPLATE_CONSTRUCT */

