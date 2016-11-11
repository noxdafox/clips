   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.50  11/01/16             */
   /*                                                     */
   /*                FACT COMMANDS MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the facts, assert, retract, save-facts, */
/*   load-facts, set-fact-duplication, get-fact-duplication, */
/*   assert-string, and fact-index commands and functions.   */
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
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
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
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
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
/*            Eval support for run time and bload only.      */
/*                                                           */
/*      6.50: Watch facts for modify command only prints     */
/*            changed slots.                                 */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <string.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include "argacces.h"
#include "constant.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "extnfunc.h"
#include "facthsh.h"
#include "factmch.h"
#include "factmngr.h"
#include "factrhs.h"
#include "match.h"
#include "memalloc.h"
#include "modulutl.h"
#include "multifld.h"
#include "pprint.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "tmpltdef.h"
#include "tmpltfun.h"
#include "tmpltpsr.h"
#include "tmpltutl.h"

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
#include "bload.h"
#endif

#include "factcom.h"

#define INVALID     -2L
#define UNSPECIFIED -1L

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct expr            *AssertParse(Environment *,struct expr *,const char *);
#if DEBUGGING_FUNCTIONS
   static long long               GetFactsArgument(UDFContext *);
#endif
   static struct expr            *StandardLoadFact(Environment *,const char *,struct token *);
   static Deftemplate           **GetSaveFactsDeftemplateNames(Environment *,struct expr *,int,int *,bool *);

/***************************************/
/* FactCommandDefinitions: Initializes */
/*   fact commands and functions.      */
/***************************************/
void FactCommandDefinitions(
  Environment *theEnv)
  {
#if ! RUN_TIME
#if DEBUGGING_FUNCTIONS
   EnvAddUDF(theEnv,"facts","v",0,4,"l;*",FactsCommand,"FactsCommand",NULL);
#endif

   EnvAddUDF(theEnv,"assert","bf",0,UNBOUNDED,NULL,AssertCommand,"AssertCommand",NULL);
   EnvAddUDF(theEnv,"retract", "v",1,UNBOUNDED,"fly",RetractCommand,"RetractCommand",NULL);
   EnvAddUDF(theEnv,"assert-string","bf",1,1,"s",AssertStringFunction,"AssertStringFunction",NULL);
   EnvAddUDF(theEnv,"str-assert","bf",1,1,"s",AssertStringFunction,"AssertStringFunction",NULL);

   EnvAddUDF(theEnv,"get-fact-duplication","b",0,0,NULL,GetFactDuplicationCommand,"GetFactDuplicationCommand", NULL);
   EnvAddUDF(theEnv,"set-fact-duplication","b",1,1,NULL,SetFactDuplicationCommand,"SetFactDuplicationCommand", NULL);

   EnvAddUDF(theEnv,"save-facts","b",1,UNBOUNDED,"y;sy",SaveFactsCommand,"SaveFactsCommand",NULL);
   EnvAddUDF(theEnv,"load-facts","b",1,1,"sy",LoadFactsCommand,"LoadFactsCommand",NULL);
   EnvAddUDF(theEnv,"fact-index","l",1,1,"f",FactIndexFunction,"FactIndexFunction",NULL);

   FuncSeqOvlFlags(theEnv,"assert",false,false);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
   AddFunctionParser(theEnv,"assert",AssertParse);
  }

/***************************************/
/* AssertCommand: H/L access routine   */
/*   for the assert function.          */
/***************************************/
void AssertCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Deftemplate *theDeftemplate;
   struct field *theField;
   UDFValue theValue;
   struct expr *theExpression;
   struct templateSlot *slotPtr;
   Fact *newFact;
   bool error = false;
   int i;
   Fact *theFact;

   /*================================*/
   /* Get the deftemplate associated */
   /* with the fact being asserted.  */
   /*================================*/

   theExpression = GetFirstArgument();
   theDeftemplate = (Deftemplate *) theExpression->value;

   /*=======================================*/
   /* Create the fact and store the name of */
   /* the deftemplate as the 1st field.     */
   /*=======================================*/

   if (theDeftemplate->implied == false)
     {
      newFact = CreateFactBySize(theEnv,theDeftemplate->numberOfSlots);
      slotPtr = theDeftemplate->slotList;
     }
   else
     {
      newFact = CreateFactBySize(theEnv,1);
      if (theExpression->nextArg == NULL)
        {
         newFact->theProposition.theFields[0].value = CreateUnmanagedMultifield(theEnv,0L);
        }
      slotPtr = NULL;
     }

   newFact->whichDeftemplate = theDeftemplate;

   /*===================================================*/
   /* Evaluate the expression associated with each slot */
   /* and store the result in the appropriate slot of   */
   /* the newly created fact.                           */
   /*===================================================*/

   EnvIncrementClearReadyLocks(theEnv);

   theField = newFact->theProposition.theFields;

   for (theExpression = theExpression->nextArg, i = 0;
        theExpression != NULL;
        theExpression = theExpression->nextArg, i++)
     {
      /*===================================================*/
      /* Evaluate the expression to be stored in the slot. */
      /*===================================================*/

      EvaluateExpression(theEnv,theExpression,&theValue);

      /*============================================================*/
      /* A multifield value can't be stored in a single field slot. */
      /*============================================================*/

      if ((slotPtr != NULL) ?
          (slotPtr->multislot == false) && (theValue.header->type == MULTIFIELD_TYPE) :
          false)
        {
         MultiIntoSingleFieldSlotError(theEnv,slotPtr,theDeftemplate);
         theValue.value = FalseSymbol(theEnv);
         error = true;
        }

      /*==============================*/
      /* Store the value in the slot. */
      /*==============================*/

      theField[i].value = theValue.value;

      /*========================================*/
      /* Get the information for the next slot. */
      /*========================================*/

      if (slotPtr != NULL) slotPtr = slotPtr->next;
     }

   EnvDecrementClearReadyLocks(theEnv);

   /*============================================*/
   /* If an error occured while generating the   */
   /* fact's slot values, then abort the assert. */
   /*============================================*/

   if (error)
     {
      ReturnFact(theEnv,newFact);
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   /*================================*/
   /* Add the fact to the fact-list. */
   /*================================*/

   theFact = EnvAssert(theEnv,newFact);

   /*========================================*/
   /* The asserted fact is the return value. */
   /*========================================*/

   if (theFact != NULL)
     { returnValue->factValue = theFact; }
   else
     { returnValue->lexemeValue = FalseSymbol(theEnv); }

   return;
  }

/****************************************/
/* RetractCommand: H/L access routine   */
/*   for the retract command.           */
/****************************************/
void RetractCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   long long factIndex;
   Fact *ptr;
   UDFValue theArg;

   /*================================*/
   /* Iterate through each argument. */
   /*================================*/

   while (UDFHasNextArgument(context))
     {
      /*========================*/
      /* Evaluate the argument. */
      /*========================*/

      if (! UDFNextArgument(context,INTEGER_BIT | FACT_ADDRESS_BIT | SYMBOL_BIT,&theArg))
        { return; }

      /*======================================*/
      /* If the argument evaluates to a fact  */
      /* address, we can directly retract it. */
      /*======================================*/

      if (CVIsType(&theArg,FACT_ADDRESS_BIT))
        { EnvRetract(theEnv,(Fact *) theArg.value); }

      /*===============================================*/
      /* If the argument evaluates to an integer, then */
      /* it's assumed to be the fact index of the fact */
      /* to be retracted.                              */
      /*===============================================*/

      else if (CVIsType(&theArg,INTEGER_BIT))
        {
         /*==========================================*/
         /* A fact index must be a positive integer. */
         /*==========================================*/

         factIndex = theArg.integerValue->contents;
         if (factIndex < 0)
           {
            UDFInvalidArgumentMessage(context,"fact-address, fact-index, or the symbol *");
            return;
           }

         /*================================================*/
         /* See if a fact with the specified index exists. */
         /*================================================*/

         ptr = FindIndexedFact(theEnv,factIndex);

         /*=====================================*/
         /* If the fact exists then retract it, */
         /* otherwise print an error message.   */
         /*=====================================*/

         if (ptr != NULL)
           { EnvRetract(theEnv,ptr); }
         else
           {
            char tempBuffer[20];
            gensprintf(tempBuffer,"f-%lld",factIndex);
            CantFindItemErrorMessage(theEnv,"fact",tempBuffer);
           }
        }

      /*============================================*/
      /* Otherwise if the argument evaluates to the */
      /* symbol *, then all facts are retracted.    */
      /*============================================*/

      else if ((CVIsType(&theArg,SYMBOL_BIT)) ?
               (strcmp(theArg.lexemeValue->contents,"*") == 0) : false)
        {
         RemoveAllFacts(theEnv);
         return;
        }

      /*============================================*/
      /* Otherwise the argument has evaluated to an */
      /* illegal value for the retract command.     */
      /*============================================*/

      else
        {
         UDFInvalidArgumentMessage(context,"fact-address, fact-index, or the symbol *");
         EnvSetEvaluationError(theEnv,true);
        }
     }
  }

/***************************************************/
/* SetFactDuplicationCommand: H/L access routine   */
/*   for the set-fact-duplication command.         */
/***************************************************/
void SetFactDuplicationCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue theArg;

   /*=====================================================*/
   /* Get the old value of the fact duplication behavior. */
   /*=====================================================*/

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvGetFactDuplication(theEnv));

   /*========================*/
   /* Evaluate the argument. */
   /*========================*/

   if (! UDFFirstArgument(context,ANY_TYPE_BITS,&theArg))
     { return; }

   /*===============================================================*/
   /* If the argument evaluated to false, then the fact duplication */
   /* behavior is disabled, otherwise it is enabled.                */
   /*===============================================================*/

   EnvSetFactDuplication(theEnv,theArg.value != FalseSymbol(theEnv));
  }

/***************************************************/
/* GetFactDuplicationCommand: H/L access routine   */
/*   for the get-fact-duplication command.         */
/***************************************************/
void GetFactDuplicationCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvGetFactDuplication(theEnv));
  }

/*******************************************/
/* FactIndexFunction: H/L access routine   */
/*   for the fact-index function.          */
/*******************************************/
void FactIndexFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue theArg;

   /*======================================*/
   /* The argument must be a fact address. */
   /*======================================*/

   if (! UDFFirstArgument(context,FACT_ADDRESS_BIT,&theArg))
     { return; }

   /*================================================*/
   /* Return the fact index associated with the fact */
   /* address. If the fact has been retracted, then  */
   /* return -1 for the fact index.                  */
   /*================================================*/

   if (theArg.factValue->garbage)
     {
      returnValue->integerValue = EnvCreateInteger(theEnv,-1L);
      return;
     }

   returnValue->integerValue = EnvCreateInteger(theEnv,EnvFactIndex(theEnv,theArg.factValue));
  }

#if DEBUGGING_FUNCTIONS

/**************************************/
/* FactsCommand: H/L access routine   */
/*   for the facts command.           */
/**************************************/
void FactsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   long long start = UNSPECIFIED, end = UNSPECIFIED, max = UNSPECIFIED;
   Defmodule *theModule;
   UDFValue theArg;

   /*==================================*/
   /* The default module for the facts */
   /* command is the current module.   */
   /*==================================*/

   theModule = EnvGetCurrentModule(theEnv);

   /*==========================================*/
   /* If no arguments were specified, then use */
   /* the default values to list the facts.    */
   /*==========================================*/

   if (! UDFHasNextArgument(context))
     {
      EnvFacts(theEnv,WDISPLAY,theModule,start,end,max);
      return;
     }

   /*========================================================*/
   /* Since there are one or more arguments, see if a module */
   /* or start index was specified as the first argument.    */
   /*========================================================*/

   if (! UDFFirstArgument(context,SYMBOL_BIT | INTEGER_BIT,&theArg)) return;

   /*===============================================*/
   /* If the first argument is a symbol, then check */
   /* to see that a valid module was specified.     */
   /*===============================================*/

   if (CVIsType(&theArg,SYMBOL_BIT))
     {
      theModule = EnvFindDefmodule(theEnv,theArg.lexemeValue->contents);
      if ((theModule == NULL) && (strcmp(theArg.lexemeValue->contents,"*") != 0))
        {
         EnvSetEvaluationError(theEnv,true);
         CantFindItemErrorMessage(theEnv,"defmodule",theArg.lexemeValue->contents);
         return;
        }

      if ((start = GetFactsArgument(context)) == INVALID) return;
     }

   /*================================================*/
   /* Otherwise if the first argument is an integer, */
   /* check to see that a valid index was specified. */
   /*================================================*/

   else if (CVIsType(&theArg,INTEGER_BIT))
     {
      start = theArg.integerValue->contents;
      if (start < 0)
        {
         ExpectedTypeError1(theEnv,"facts",1,"symbol or positive number");
         UDFThrowError(context);
         return;
        }
     }

   /*==========================================*/
   /* Otherwise the first argument is invalid. */
   /*==========================================*/

   else
     {
      UDFInvalidArgumentMessage(context,"symbol or positive number");
      UDFThrowError(context);
      return;
     }

   /*==========================*/
   /* Get the other arguments. */
   /*==========================*/

   if ((end = GetFactsArgument(context)) == INVALID) return;
   if ((max = GetFactsArgument(context)) == INVALID) return;

   /*=================*/
   /* List the facts. */
   /*=================*/

   EnvFacts(theEnv,WDISPLAY,theModule,start,end,max);
  }

/*****************************************************/
/* EnvFacts: C access routine for the facts command. */
/*****************************************************/
void EnvFacts(
  Environment *theEnv,
  const char *logicalName,
  Defmodule *theModule,
  long long start,
  long long end,
  long long max)
  {
   Fact *factPtr;
   long count = 0;
   Defmodule *oldModule;
   bool allModules = false;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   oldModule = EnvGetCurrentModule(theEnv);

   /*=========================================================*/
   /* Determine if facts from all modules are to be displayed */
   /* or just facts from the current module.                  */
   /*=========================================================*/

   if (theModule == NULL) allModules = true;
   else EnvSetCurrentModule(theEnv,theModule);

   /*=====================================*/
   /* Get the first fact to be displayed. */
   /*=====================================*/

   if (allModules) factPtr = EnvGetNextFact(theEnv,NULL);
   else factPtr = GetNextFactInScope(theEnv,NULL);

   /*===============================*/
   /* Display facts until there are */
   /* no more facts to display.     */
   /*===============================*/

   while (factPtr != NULL)
     {
      /*==================================================*/
      /* Abort the display of facts if the Halt Execution */
      /* flag has been set (normally by user action).     */
      /*==================================================*/

      if (EnvGetHaltExecution(theEnv) == true)
        {
         EnvSetCurrentModule(theEnv,oldModule);
         return;
        }

      /*===============================================*/
      /* If the maximum fact index of facts to display */
      /* has been reached, then stop displaying facts. */
      /*===============================================*/

      if ((factPtr->factIndex > end) && (end != UNSPECIFIED))
        {
         PrintTally(theEnv,logicalName,count,"fact","facts");
         EnvSetCurrentModule(theEnv,oldModule);
         return;
        }

      /*================================================*/
      /* If the maximum number of facts to be displayed */
      /* has been reached, then stop displaying facts.  */
      /*================================================*/

      if (max == 0)
        {
         PrintTally(theEnv,logicalName,count,"fact","facts");
         EnvSetCurrentModule(theEnv,oldModule);
         return;
        }

      /*======================================================*/
      /* If the index of the fact is greater than the minimum */
      /* starting fact index, then display the fact.          */
      /*======================================================*/

      if (factPtr->factIndex >= start)
        {
         PrintFactWithIdentifier(theEnv,logicalName,factPtr,NULL);
         EnvPrintRouter(theEnv,logicalName,"\n");
         count++;
         if (max > 0) max--;
        }

      /*========================================*/
      /* Proceed to the next fact to be listed. */
      /*========================================*/

      if (allModules) factPtr = EnvGetNextFact(theEnv,factPtr);
      else factPtr = GetNextFactInScope(theEnv,factPtr);
     }

   /*===================================================*/
   /* Print the total of the number of facts displayed. */
   /*===================================================*/

   PrintTally(theEnv,logicalName,count,"fact","facts");

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   EnvSetCurrentModule(theEnv,oldModule);
  }

/****************************************************************/
/* GetFactsArgument: Returns an argument for the facts command. */
/*  A return value of -1 indicates that no value was specified. */
/*  A return value of -2 indicates that the value specified is  */
/*  invalid.                                                    */
/****************************************************************/
static long long GetFactsArgument(
  UDFContext *context)
  {
   long long factIndex;
   UDFValue theArg;

   if (! UDFHasNextArgument(context)) return(UNSPECIFIED);

   if (! UDFNextArgument(context,INTEGER_BIT,&theArg))
     { return(INVALID); }

   factIndex = theArg.integerValue->contents;

   if (factIndex < 0)
     {
      UDFInvalidArgumentMessage(context,"positive number");
      UDFThrowError(context);
      return(INVALID);
     }

   return(factIndex);
  }

#endif /* DEBUGGING_FUNCTIONS */

/**********************************************/
/* AssertStringFunction: H/L access routine   */
/*   for the assert-string function.          */
/**********************************************/
void AssertStringFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue theArg;
   Fact *theFact;

   /*=====================================================*/
   /* Check for the correct number and type of arguments. */
   /*=====================================================*/

   if (! UDFFirstArgument(context,STRING_BIT,&theArg))
     { return; }

   /*==========================================*/
   /* Call the driver routine for converting a */
   /* string to a fact and then assert it.     */
   /*==========================================*/

   theFact = EnvAssertString(theEnv,theArg.lexemeValue->contents);
   if (theFact != NULL)
     { returnValue->factValue = theFact; }
   else
     { returnValue->lexemeValue = FalseSymbol(theEnv); }
  }

/******************************************/
/* SaveFactsCommand: H/L access routine   */
/*   for the save-facts command.          */
/******************************************/
void SaveFactsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   const char *fileName;
   int numArgs, saveCode = LOCAL_SAVE;
   const char *argument;
   UDFValue theValue;
   struct expr *theList = NULL;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   numArgs = UDFArgumentCount(context);

   /*=================================================*/
   /* Get the file name to which facts will be saved. */
   /*=================================================*/

   if ((fileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   /*=============================================================*/
   /* If specified, the second argument to save-facts indicates   */
   /* whether just facts local to the current module or all facts */
   /* visible to the current module will be saved.                */
   /*=============================================================*/

   if (numArgs > 1)
     {
      if (! UDFNextArgument(context,SYMBOL_BIT,&theValue))
        {
         returnValue->lexemeValue = FalseSymbol(theEnv);
         return;
        }

      argument = theValue.lexemeValue->contents;

      if (strcmp(argument,"local") == 0)
        { saveCode = LOCAL_SAVE; }
      else if (strcmp(argument,"visible") == 0)
        { saveCode = VISIBLE_SAVE; }
      else
        {
         ExpectedTypeError1(theEnv,"save-facts",2,"symbol with value local or visible");
         returnValue->lexemeValue = FalseSymbol(theEnv);
         return;
        }
     }

   /*======================================================*/
   /* Subsequent arguments indicate that only those facts  */
   /* associated with the specified deftemplates should be */
   /* saved to the file.                                   */
   /*======================================================*/

   if (numArgs > 2) theList = GetFirstArgument()->nextArg->nextArg;

   /*====================================*/
   /* Call the SaveFacts driver routine. */
   /*====================================*/

   if (EnvSaveFactsDriver(theEnv,fileName,saveCode,theList) == false)
     { returnValue->lexemeValue = FalseSymbol(theEnv); }
   else
     { returnValue->lexemeValue = TrueSymbol(theEnv); }
  }

/******************************************/
/* LoadFactsCommand: H/L access routine   */
/*   for the load-facts command.          */
/******************************************/
void LoadFactsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   const char *fileName;

   /*====================================================*/
   /* Get the file name from which facts will be loaded. */
   /*====================================================*/

   if ((fileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   /*====================================*/
   /* Call the LoadFacts driver routine. */
   /*====================================*/

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvLoadFacts(theEnv,fileName));
  }

/**************************************************************/
/* EnvSaveFacts: C access routine for the save-facts command. */
/**************************************************************/
bool EnvSaveFacts(
  Environment *theEnv,
  const char *fileName,
  int saveCode)
  {
   return EnvSaveFactsDriver(theEnv,fileName,saveCode,NULL);
  }

/********************************************************************/
/* EnvSaveFactsDriver: C access routine for the save-facts command. */
/********************************************************************/
bool EnvSaveFactsDriver(
  Environment *theEnv,
  const char *fileName,
  int saveCode,
  struct expr *theList)
  {
   bool tempValue1, tempValue2, tempValue3;
   struct fact *theFact;
   FILE *filePtr;
   Defmodule *theModule;
   Deftemplate **deftemplateArray;
   int count, i;
   bool printFact, error;

   /*======================================================*/
   /* Open the file. Use either "fast save" or I/O Router. */
   /*======================================================*/

   if ((filePtr = GenOpen(theEnv,fileName,"w")) == NULL)
     {
      OpenErrorMessage(theEnv,"save-facts",fileName);
      return false;
     }

   SetFastSave(theEnv,filePtr);

   /*===========================================*/
   /* Set the print flags so that addresses and */
   /* strings are printed properly to the file. */
   /*===========================================*/

   tempValue1 = PrintUtilityData(theEnv)->PreserveEscapedCharacters;
   PrintUtilityData(theEnv)->PreserveEscapedCharacters = true;
   tempValue2 = PrintUtilityData(theEnv)->AddressesToStrings;
   PrintUtilityData(theEnv)->AddressesToStrings = true;
   tempValue3 = PrintUtilityData(theEnv)->InstanceAddressesToNames;
   PrintUtilityData(theEnv)->InstanceAddressesToNames = true;

   /*===================================================*/
   /* Determine the list of specific facts to be saved. */
   /*===================================================*/

   deftemplateArray = GetSaveFactsDeftemplateNames(theEnv,theList,saveCode,&count,&error);

   if (error)
     {
      PrintUtilityData(theEnv)->PreserveEscapedCharacters = tempValue1;
      PrintUtilityData(theEnv)->AddressesToStrings = tempValue2;
      PrintUtilityData(theEnv)->InstanceAddressesToNames = tempValue3;
      GenClose(theEnv,filePtr);
      SetFastSave(theEnv,NULL);
      return false;
     }

   /*=================*/
   /* Save the facts. */
   /*=================*/

   theModule = EnvGetCurrentModule(theEnv);

   for (theFact = GetNextFactInScope(theEnv,NULL);
        theFact != NULL;
        theFact = GetNextFactInScope(theEnv,theFact))
     {
      /*===========================================================*/
      /* If we're doing a local save and the facts's corresponding */
      /* deftemplate isn't in the current module, then don't save  */
      /* the fact.                                                 */
      /*===========================================================*/

      if ((saveCode == LOCAL_SAVE) &&
          (theFact->whichDeftemplate->header.whichModule->theModule != theModule))
        { printFact = false; }

      /*=====================================================*/
      /* Otherwise, if the list of facts to be printed isn't */
      /* restricted, then set the print flag to true.        */
      /*=====================================================*/

      else if (theList == NULL)
        { printFact = true; }

      /*=======================================================*/
      /* Otherwise see if the fact's corresponding deftemplate */
      /* is in the list of deftemplates whose facts are to be  */
      /* saved. If it's in the list, then set the print flag   */
      /* to true, otherwise set it to false.                   */
      /*=======================================================*/

      else
        {
         printFact = false;
         for (i = 0; i < count; i++)
           {
            if (deftemplateArray[i] == theFact->whichDeftemplate)
              {
               printFact = true;
               break;
              }
           }
        }

      /*===================================*/
      /* If the print flag is set to true, */
      /* then save the fact to the file.   */
      /*===================================*/

      if (printFact)
        {
         PrintFact(theEnv,(char *) filePtr,theFact,false,false,NULL);
         EnvPrintRouter(theEnv,(char *) filePtr,"\n");
        }
     }

   /*==========================*/
   /* Restore the print flags. */
   /*==========================*/

   PrintUtilityData(theEnv)->PreserveEscapedCharacters = tempValue1;
   PrintUtilityData(theEnv)->AddressesToStrings = tempValue2;
   PrintUtilityData(theEnv)->InstanceAddressesToNames = tempValue3;

   /*=================*/
   /* Close the file. */
   /*=================*/

   GenClose(theEnv,filePtr);
   SetFastSave(theEnv,NULL);

   /*==================================*/
   /* Free the deftemplate name array. */
   /*==================================*/

   if (theList != NULL)
     { rm3(theEnv,deftemplateArray,(long) sizeof(Deftemplate *) * count); }

   /*===================================*/
   /* Return true to indicate no errors */
   /* occurred while saving the facts.  */
   /*===================================*/

   return true;
  }

/*******************************************************************/
/* GetSaveFactsDeftemplateNames: Retrieves the list of deftemplate */
/*   names for saving specific facts with the save-facts command.  */
/*******************************************************************/
static Deftemplate **GetSaveFactsDeftemplateNames(
  Environment *theEnv,
  struct expr *theList,
  int saveCode,
  int *count,
  bool *error)
  {
   struct expr *tempList;
   Deftemplate **deftemplateArray;
   UDFValue tempArg;
   int i, tempCount;
   Deftemplate *theDeftemplate = NULL;

   /*=============================*/
   /* Initialize the error state. */
   /*=============================*/

   *error = false;

   /*=====================================================*/
   /* If no deftemplate names were specified as arguments */
   /* then the deftemplate name list is empty.            */
   /*=====================================================*/

   if (theList == NULL)
     {
      *count = 0;
      return NULL;
     }

   /*======================================*/
   /* Determine the number of deftemplate  */
   /* names to be stored in the name list. */
   /*======================================*/

   for (tempList = theList, *count = 0;
        tempList != NULL;
        tempList = tempList->nextArg, (*count)++)
     { /* Do Nothing */ }

   /*=========================================*/
   /* Allocate the storage for the name list. */
   /*=========================================*/

   deftemplateArray = (Deftemplate **) gm3(theEnv,(long) sizeof(Deftemplate *) * *count);

   /*=====================================*/
   /* Loop through each of the arguments. */
   /*=====================================*/

   for (tempList = theList, i = 0;
        i < *count;
        tempList = tempList->nextArg, i++)
     {
      /*========================*/
      /* Evaluate the argument. */
      /*========================*/

      EvaluateExpression(theEnv,tempList,&tempArg);

      if (EvaluationData(theEnv)->EvaluationError)
        {
         *error = true;
         rm3(theEnv,deftemplateArray,(long) sizeof(Deftemplate *) * *count);
         return NULL;
        }

      /*======================================*/
      /* A deftemplate name must be a symbol. */
      /*======================================*/

      if (tempArg.header->type != SYMBOL_TYPE)
        {
         *error = true;
         ExpectedTypeError1(theEnv,"save-facts",3+i,"symbol");
         rm3(theEnv,deftemplateArray,(long) sizeof(Deftemplate *) * *count);
         return NULL;
        }

      /*===================================================*/
      /* Find the deftemplate. For a local save, look only */
      /* in the current module. For a visible save, look   */
      /* in all visible modules.                           */
      /*===================================================*/

      if (saveCode == LOCAL_SAVE)
        {
         theDeftemplate = EnvFindDeftemplateInModule(theEnv,tempArg.lexemeValue->contents);
         if (theDeftemplate == NULL)
           {
            *error = true;
            ExpectedTypeError1(theEnv,"save-facts",3+i,"local deftemplate name");
            rm3(theEnv,deftemplateArray,(long) sizeof(Deftemplate *) * *count);
            return NULL;
           }
        }
      else if (saveCode == VISIBLE_SAVE)
        {
         theDeftemplate = (Deftemplate *)
           FindImportedConstruct(theEnv,"deftemplate",NULL,
                                 tempArg.lexemeValue->contents,
                                 &tempCount,true,NULL);
         if (theDeftemplate == NULL)
           {
            *error = true;
            ExpectedTypeError1(theEnv,"save-facts",3+i,"visible deftemplate name");
            rm3(theEnv,deftemplateArray,(long) sizeof(Deftemplate *) * *count);
            return NULL;
           }
        }

      /*==================================*/
      /* Add a pointer to the deftemplate */
      /* to the array being created.      */
      /*==================================*/

      deftemplateArray[i] = theDeftemplate;
     }

   /*===================================*/
   /* Return the array of deftemplates. */
   /*===================================*/

   return deftemplateArray;
  }

/**************************************************************/
/* EnvLoadFacts: C access routine for the load-facts command. */
/**************************************************************/
bool EnvLoadFacts(
  Environment *theEnv,
  const char *fileName)
  {
   FILE *filePtr;
   struct token theToken;
   struct expr *testPtr;
   UDFValue rv;

   /*======================================================*/
   /* Open the file. Use either "fast save" or I/O Router. */
   /*======================================================*/

   if ((filePtr = GenOpen(theEnv,fileName,"r")) == NULL)
     {
      OpenErrorMessage(theEnv,"load-facts",fileName);
      return false;
     }

   SetFastLoad(theEnv,filePtr);

   /*=================*/
   /* Load the facts. */
   /*=================*/

   theToken.tknType = LEFT_PARENTHESIS_TOKEN;
   while (theToken.tknType != STOP_TOKEN)
     {
      testPtr = StandardLoadFact(theEnv,(char *) filePtr,&theToken);
      if (testPtr == NULL) theToken.tknType = STOP_TOKEN;
      else EvaluateExpression(theEnv,testPtr,&rv);
      ReturnExpression(theEnv,testPtr);
     }

   /*=================*/
   /* Close the file. */
   /*=================*/

   SetFastLoad(theEnv,NULL);
   GenClose(theEnv,filePtr);

   /*================================================*/
   /* Return true if no error occurred while loading */
   /* the facts, otherwise return false.             */
   /*================================================*/

   if (EvaluationData(theEnv)->EvaluationError) return false;
   return true;
  }

/*********************************************/
/* EnvLoadFactsFromString: C access routine. */
/*********************************************/
bool EnvLoadFactsFromString(
  Environment *theEnv,
  const char *theString,
  long theMax)
  {
   const char *theStrRouter = "*** load-facts-from-string ***";
   struct token theToken;
   struct expr *testPtr;
   UDFValue rv;

   /*==========================*/
   /* Initialize string router */
   /*==========================*/

   if ((theMax == -1) ? (!OpenStringSource(theEnv,theStrRouter,theString,0)) :
                        (!OpenTextSource(theEnv,theStrRouter,theString,0,(size_t) theMax)))
     return false;

   /*=================*/
   /* Load the facts. */
   /*=================*/

   theToken.tknType = LEFT_PARENTHESIS_TOKEN;
   while (theToken.tknType != STOP_TOKEN)
     {
      testPtr = StandardLoadFact(theEnv,theStrRouter,&theToken);
      if (testPtr == NULL) theToken.tknType = STOP_TOKEN;
      else EvaluateExpression(theEnv,testPtr,&rv);
      ReturnExpression(theEnv,testPtr);
     }

   /*=================*/
   /* Close router.   */
   /*=================*/

   CloseStringSource(theEnv,theStrRouter);

   /*================================================*/
   /* Return true if no error occurred while loading */
   /* the facts, otherwise return false.             */
   /*================================================*/

   if (EvaluationData(theEnv)->EvaluationError) return false;
   return true;
  }

/**************************************************************************/
/* StandardLoadFact: Loads a single fact from the specified logical name. */
/**************************************************************************/
static struct expr *StandardLoadFact(
  Environment *theEnv,
  const char *logicalName,
  struct token *theToken)
  {
   bool error = false;
   struct expr *temp;

   GetToken(theEnv,logicalName,theToken);
   if (theToken->tknType != LEFT_PARENTHESIS_TOKEN) return NULL;

   temp = GenConstant(theEnv,FCALL,FindFunction(theEnv,"assert"));
   temp->argList = GetRHSPattern(theEnv,logicalName,theToken,&error,
                                  true,false,true,RIGHT_PARENTHESIS_TOKEN);

   if (error == true)
     {
      EnvPrintRouter(theEnv,WERROR,"Function load-facts encountered an error\n");
      EnvSetEvaluationError(theEnv,true);
      ReturnExpression(theEnv,temp);
      return NULL;
     }

   if (ExpressionContainsVariables(temp,true))
     {
      ReturnExpression(theEnv,temp);
      return NULL;
     }

   return(temp);
  }

/****************************************************************/
/* AssertParse: Driver routine for parsing the assert function. */
/****************************************************************/
static struct expr *AssertParse(
  Environment *theEnv,
  struct expr *top,
  const char *logicalName)
  {
   bool error;
   struct expr *rv;
   struct token theToken;

   ReturnExpression(theEnv,top);
   SavePPBuffer(theEnv," ");
   IncrementIndentDepth(theEnv,8);
   rv = BuildRHSAssert(theEnv,logicalName,&theToken,&error,true,true,"assert command");
   DecrementIndentDepth(theEnv,8);
   return(rv);
  }

#endif /* DEFTEMPLATE_CONSTRUCT */


