   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*               ARGUMENT ACCESS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides access routines for accessing arguments */
/*   passed to user or system functions defined using the    */
/*   DefineFunction protocol.                                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added IllegalLogicalNameMessage function.      */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Support for fact-address arguments.            */
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
/*************************************************************/

#include "setup.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "cstrnchk.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "factmngr.h"
#include "insfun.h"
#include "prntutil.h"
#include "router.h"
#include "sysdep.h"

#include "argacces.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    NonexistantError(Environment *,const char *,const char *,int);

/********************************************************************/
/* EnvRtnUnknown: Access function to retrieve the nth argument from */
/*   a user or system function defined using the DefineFunction     */
/*   protocol. The argument retrieved can be of any type. The value */
/*   and type of the argument are returned in a CLIPSValue          */
/*   structure provided by the calling function.                    */
/********************************************************************/
CLIPSValue *EnvRtnUnknown(
  Environment *theEnv,
  int argumentPosition,
  CLIPSValue *returnValue)
  {
   int count = 1;
   struct expr *argPtr;

   /*=====================================================*/
   /* Find the appropriate argument in the argument list. */
   /*=====================================================*/

   for (argPtr = EvaluationData(theEnv)->CurrentExpression->argList;
        (argPtr != NULL) && (count < argumentPosition);
        argPtr = argPtr->nextArg)
     { count++; }

   if (argPtr == NULL)
     {
      NonexistantError(theEnv,"RtnUnknown",
                       ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                       argumentPosition);
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return NULL;
     }

   /*=======================================*/
   /* Return the value of the nth argument. */
   /*=======================================*/

   EvaluateExpression(theEnv,argPtr,returnValue);
   return(returnValue);
  }

/***********************************************************/
/* EnvRtnArgCount: Returns the length of the argument list */
/*   for the function call currently being evaluated.      */
/***********************************************************/
int EnvRtnArgCount(
  Environment *theEnv)
  {
   int count = 0;
   struct expr *argPtr;

   for (argPtr = EvaluationData(theEnv)->CurrentExpression->argList;
        argPtr != NULL;
        argPtr = argPtr->nextArg)
     { count++; }

   return(count);
  }
  
/************************************************************************/
/* EnvArgCountCheck: Given the expected number of arguments, determines */
/*   if the function currently being evaluated has the correct number   */
/*   of arguments. Three types of argument checking are provided by     */
/*   this function: 1) The function has exactly the expected number of  */
/*   arguments; 2) The function has at least the expected number of     */
/*   arguments; 3) The function has at most the expected number of      */
/*   arguments. The number of arguments is returned if no error occurs, */
/*   otherwise -1 is returned.                                          */
/************************************************************************/
int EnvArgCountCheck(
  Environment *theEnv,
  const char *functionName,
  int countRelation,
  int expectedNumber)
  {
   int numberOfArguments;

   /*==============================================*/
   /* Get the number of arguments for the function */
   /* currently being evaluated.                   */
   /*==============================================*/

   numberOfArguments = EnvRtnArgCount(theEnv);

   /*=========================================================*/
   /* If the function satisfies expected number of arguments, */
   /* constraint, then return the number of arguments found.  */
   /*=========================================================*/

   if (countRelation == EXACTLY)
     { if (numberOfArguments == expectedNumber) return(numberOfArguments); }
   else if (countRelation == AT_LEAST)
     { if (numberOfArguments >= expectedNumber) return(numberOfArguments); }
   else if (countRelation == NO_MORE_THAN)
     { if (numberOfArguments <= expectedNumber) return(numberOfArguments); }

   /*================================================*/
   /* The correct number of arguments was not found. */
   /* Generate an error message and return -1.       */
   /*================================================*/

   ExpectedCountError(theEnv,functionName,countRelation,expectedNumber);

   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);

   return(-1);
  }

/****************************************************************/
/* EnvArgRangeCheck: Checks that the number of arguments passed */
/*   to a function falls within a specified minimum and maximum */
/*   range. The number of arguments passed to the function is   */
/*   returned if no error occurs, otherwise -1 is returned.     */
/****************************************************************/
int EnvArgRangeCheck(
  Environment *theEnv,
  const char *functionName,
  int min,
  int max)
  {
   int numberOfArguments;

   numberOfArguments = EnvRtnArgCount(theEnv);
   if ((numberOfArguments < min) || (numberOfArguments > max))
     {
      PrintErrorID(theEnv,"ARGACCES",1,false);
      EnvPrintRouter(theEnv,WERROR,"Function ");
      EnvPrintRouter(theEnv,WERROR,functionName);
      EnvPrintRouter(theEnv,WERROR," expected at least ");
      PrintLongInteger(theEnv,WERROR,(long) min);
      EnvPrintRouter(theEnv,WERROR," and no more than ");
      PrintLongInteger(theEnv,WERROR,(long) max);
      EnvPrintRouter(theEnv,WERROR," arguments.\n");
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      return(-1);
     }

   return(numberOfArguments);
  }

/*************************************************************/
/* EnvArgTypeCheck: Retrieves the nth argument passed to the */
/*   function call currently being evaluated and determines  */
/*   if it matches a specified type. Returns true if the     */
/*   argument was successfully retrieved and is of the       */
/*   appropriate type, otherwise returns false.              */
/*************************************************************/
bool EnvArgTypeCheck(
  Environment *theEnv,
  const char *functionName,
  int argumentPosition,
  int expectedType,
  CLIPSValue *returnValue)
  {
   /*========================*/
   /* Retrieve the argument. */
   /*========================*/

   EnvRtnUnknown(theEnv,argumentPosition,returnValue);
   if (EvaluationData(theEnv)->EvaluationError) return false;

   /*========================================*/
   /* If the argument's type exactly matches */
   /* the expected type, then return true.   */
   /*========================================*/

   if (returnValue->type == expectedType) return true;

   /*=============================================================*/
   /* Some expected types encompass more than one primitive type. */
   /* If the argument's type matches one of the primitive types   */
   /* encompassed by the expected type, then return true.         */
   /*=============================================================*/

   if ((expectedType == INTEGER_OR_FLOAT) &&
       ((returnValue->type == INTEGER) || (returnValue->type == FLOAT)))
     { return true; }

   if ((expectedType == SYMBOL_OR_STRING) &&
       ((returnValue->type == SYMBOL) || (returnValue->type == STRING)))
     { return true; }

#if OBJECT_SYSTEM
   if (((expectedType == SYMBOL_OR_STRING) || (expectedType == SYMBOL)) &&
       (returnValue->type == INSTANCE_NAME))
     { return true; }

   if ((expectedType == INSTANCE_NAME) &&
       ((returnValue->type == INSTANCE_NAME) || (returnValue->type == SYMBOL)))
     { return true; }

   if ((expectedType == INSTANCE_OR_INSTANCE_NAME) &&
       ((returnValue->type == INSTANCE_ADDRESS) ||
        (returnValue->type == INSTANCE_NAME) ||
        (returnValue->type == SYMBOL)))
     { return true; }
#endif

   /*===========================================================*/
   /* If the expected type is float and the argument's type is  */
   /* integer (or vice versa), then convert the argument's type */
   /* to match the expected type and then return true.          */
   /*===========================================================*/

   if ((returnValue->type == INTEGER) && (expectedType == FLOAT))
     {
      returnValue->type = FLOAT;
      returnValue->value = EnvAddDouble(theEnv,(double) ValueToLong(returnValue->value));
      return true;
     }

   if ((returnValue->type == FLOAT) && (expectedType == INTEGER))
     {
      returnValue->type = INTEGER;
      returnValue->value = EnvAddLong(theEnv,(long long) ValueToDouble(returnValue->value));
      return true;
     }

   /*=====================================================*/
   /* The argument's type didn't match the expected type. */
   /* Print an error message and return false.            */
   /*=====================================================*/

   if (expectedType == FLOAT) ExpectedTypeError1(theEnv,functionName,argumentPosition,"float");
   else if (expectedType == INTEGER) ExpectedTypeError1(theEnv,functionName,argumentPosition,"integer");
   else if (expectedType == SYMBOL) ExpectedTypeError1(theEnv,functionName,argumentPosition,"symbol");
   else if (expectedType == STRING) ExpectedTypeError1(theEnv,functionName,argumentPosition,"string");
   else if (expectedType == MULTIFIELD) ExpectedTypeError1(theEnv,functionName,argumentPosition,"multifield");
   else if (expectedType == INTEGER_OR_FLOAT)  ExpectedTypeError1(theEnv,functionName,argumentPosition,"integer or float");
   else if (expectedType == SYMBOL_OR_STRING) ExpectedTypeError1(theEnv,functionName,argumentPosition,"symbol or string");
   else if (expectedType == FACT_ADDRESS) ExpectedTypeError1(theEnv,functionName,argumentPosition,"fact address");
#if OBJECT_SYSTEM
   else if (expectedType == INSTANCE_NAME) ExpectedTypeError1(theEnv,functionName,argumentPosition,"instance name");
   else if (expectedType == INSTANCE_ADDRESS) ExpectedTypeError1(theEnv,functionName,argumentPosition,"instance address");
   else if (expectedType == INSTANCE_OR_INSTANCE_NAME) ExpectedTypeError1(theEnv,functionName,argumentPosition,"instance address or instance name");
#endif

   EnvSetHaltExecution(theEnv,true);
   EnvSetEvaluationError(theEnv,true);

   return false;
  }

/******************************************************************/
/* GetNumericArgument: Evaluates an expression to yield a numeric */
/*  argument. This provides quicker retrieval than using some of  */
/*  the other argument access routines. The numeric argument is   */
/*  returned in a CLIPSValue supplied by the calling function.    */
/*  true is returned if a numeric argument was successfully       */
/*  retrieved, otherwise false is returned.                       */
/******************************************************************/
bool GetNumericArgument(
  Environment *theEnv,
  struct expr *theArgument,
  const char *functionName,
  CLIPSValue *returnValue,
  bool convertToFloat,
  int whichArgument)
  {
   unsigned short theType;
   void *theValue;

   /*==================================================================*/
   /* Evaluate the expression (don't bother calling EvaluateExpression */
   /* if the type is float or integer).                                */
   /*==================================================================*/

   switch(theArgument->type)
     {
      case FLOAT:
      case INTEGER:
        theType = theArgument->type;
        theValue = theArgument->value;
        break;

      default:
        EvaluateExpression(theEnv,theArgument,returnValue);
        theType = returnValue->type;
        theValue = returnValue->value;
        break;
     }

   /*==========================================*/
   /* If the argument is not float or integer, */
   /* print an error message and return false. */
   /*==========================================*/

   if ((theType != FLOAT) && (theType != INTEGER))
     {
      ExpectedTypeError1(theEnv,functionName,whichArgument,"integer or float");
      EnvSetHaltExecution(theEnv,true);
      EnvSetEvaluationError(theEnv,true);
      returnValue->type = INTEGER;
      returnValue->value = EnvAddLong(theEnv,0LL);
      return false;
     }

   /*==========================================================*/
   /* If the argument is an integer and the "convert to float" */
   /* flag is true, then convert the integer to a float.       */
   /*==========================================================*/

   if ((convertToFloat) && (theType == INTEGER))
     {
      theType = FLOAT;
      theValue = EnvAddDouble(theEnv,(double) ValueToLong(theValue));
     }

   /*============================================================*/
   /* The numeric argument was successfully retrieved. Store the */
   /* argument in the user supplied CLIPSValue and return true.  */
   /*============================================================*/

   returnValue->type = theType;
   returnValue->value = theValue;

   return true;
  }

/*********************************************************************/
/* GetLogicalName: Retrieves the nth argument passed to the function */
/*   call currently being evaluated and determines if it is a valid  */
/*   logical name. If valid, the logical name is returned, otherwise */
/*   NULL is returned.                                               */
/*********************************************************************/
const char *GetLogicalName(
  Environment *theEnv,
  int whichArgument,
  const char *defaultLogicalName)
  {
   const char *logicalName;
   CLIPSValue result;

   EnvRtnUnknown(theEnv,whichArgument,&result);

   if ((GetType(result) == SYMBOL) ||
       (GetType(result) == STRING) ||
       (GetType(result) == INSTANCE_NAME))
     {
      logicalName = ValueToString(result.value);
      if ((strcmp(logicalName,"t") == 0) || (strcmp(logicalName,"T") == 0))
        { logicalName = defaultLogicalName; }
     }
   else if (GetType(result) == FLOAT)
     {
      logicalName = ValueToString(EnvAddSymbol(theEnv,FloatToString(theEnv,DOToDouble(result))));
     }
   else if (GetType(result) == INTEGER)
     {
      logicalName = ValueToString(EnvAddSymbol(theEnv,LongIntegerToString(theEnv,DOToLong(result))));
     }
   else
     { logicalName = NULL; }

   return(logicalName);
  }

/************************************************************/
/* GetFileName: Retrieves the nth argument passed to the    */
/*   function call currently being evaluated and determines */
/*   if it is a valid file name. If valid, the file name is */
/*   returned, otherwise NULL is returned.                  */
/************************************************************/
const char *GetFileName(
  Environment *theEnv,
  const char *functionName,
  int whichArgument)
  {
   CLIPSValue result;

   EnvRtnUnknown(theEnv,whichArgument,&result);
   if ((GetType(result) != STRING) && (GetType(result) != SYMBOL))
     {
      ExpectedTypeError1(theEnv,functionName,whichArgument,"file name");
      return NULL;
     }

   return(DOToString(result));
  }

/******************************************************************/
/* OpenErrorMessage: Generalized error message for opening files. */
/******************************************************************/
void OpenErrorMessage(
  Environment *theEnv,
  const char *functionName,
  const char *fileName)
  {
   PrintErrorID(theEnv,"ARGACCES",2,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," was unable to open file ");
   EnvPrintRouter(theEnv,WERROR,fileName);
   EnvPrintRouter(theEnv,WERROR,".\n");
  }

/************************************************************/
/* GetModuleName: Retrieves the nth argument passed to the  */
/*   function call currently being evaluated and determines */
/*   if it is a valid module name. If valid, the module     */
/*   name is returned or NULL is returned to indicate all   */
/*   modules.                                               */
/************************************************************/
Defmodule *GetModuleName(
  Environment *theEnv,
  const char *functionName,
  int whichArgument,
  bool *error)
  {
   CLIPSValue returnValue;
   Defmodule *theModule;

   *error = false;

   /*========================*/
   /* Retrieve the argument. */
   /*========================*/

   EnvRtnUnknown(theEnv,whichArgument,&returnValue);

   /*=================================*/
   /* A module name must be a symbol. */
   /*=================================*/

   if (GetType(returnValue) != SYMBOL)
     {
      ExpectedTypeError1(theEnv,functionName,whichArgument,"defmodule name");
      *error = true;
      return NULL;
     }

   /*=======================================*/
   /* Check to see that the symbol actually */
   /* corresponds to a defined module.      */
   /*=======================================*/

   if ((theModule = EnvFindDefmodule(theEnv,DOToString(returnValue))) == NULL)
     {
      if (strcmp("*",DOToString(returnValue)) != 0)
        {
         ExpectedTypeError1(theEnv,functionName,whichArgument,"defmodule name");
         *error = true;
        }
      return NULL;
     }

   /*=================================*/
   /* Return a pointer to the module. */
   /*=================================*/

   return(theModule);
  }

/****************************************************************/
/* GetConstructName: Retrieves the 1st argument passed to the   */
/*   function call currently being evaluated and determines if  */
/*   it is a valid name for a construct. Also checks that the   */
/*   function is only passed a single argument. This routine    */
/*   is used by functions such as ppdeftemplate, undefrule,     */
/*   etc... to retrieve the construct name on which to operate. */
/****************************************************************/
const char *GetConstructName(
  Environment *theEnv,
  const char *functionName,
  const char *constructType)
  {
   CLIPSValue returnValue;

   if (EnvRtnArgCount(theEnv) != 1)
     {
      ExpectedCountError(theEnv,functionName,EXACTLY,1);
      return NULL;
     }

   EnvRtnUnknown(theEnv,1,&returnValue);

   if (GetType(returnValue) != SYMBOL)
     {
      ExpectedTypeError1(theEnv,functionName,1,constructType);
      return NULL;
     }

   return(DOToString(returnValue));
  }

/**************************************************************************/
/* NonexistantError: Prints the error message for a nonexistant argument. */
/**************************************************************************/
static void NonexistantError(
  Environment *theEnv,
  const char *accessFunction,
  const char *functionName,
  int argumentPosition)
  {
   PrintErrorID(theEnv,"ARGACCES",3,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,accessFunction);
   EnvPrintRouter(theEnv,WERROR," received a request from function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," for argument #");
   PrintLongInteger(theEnv,WERROR,(long int) argumentPosition);
   EnvPrintRouter(theEnv,WERROR," which is non-existent\n");
  }

/*********************************************************/
/* ExpectedCountError: Prints the error message for an   */
/*   incorrect number of arguments passed to a function. */
/*********************************************************/
void ExpectedCountError(
  Environment *theEnv,
  const char *functionName,
  int countRelation,
  int expectedNumber)
  {
   PrintErrorID(theEnv,"ARGACCES",4,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,functionName);

   if (countRelation == EXACTLY)
     { EnvPrintRouter(theEnv,WERROR," expected exactly "); }
   else if (countRelation == AT_LEAST)
     { EnvPrintRouter(theEnv,WERROR," expected at least "); }
   else if (countRelation == NO_MORE_THAN)
     { EnvPrintRouter(theEnv,WERROR," expected no more than "); }
   else
     { EnvPrintRouter(theEnv,WERROR," generated an illegal argument check for "); }

   PrintLongInteger(theEnv,WERROR,(long int) expectedNumber);
   EnvPrintRouter(theEnv,WERROR," argument(s)\n");
  }

/*************************************************************/
/*  NAME         : CheckFunctionArgCount                     */
/*  DESCRIPTION  : Checks the number of arguments against    */
/*                 the system function restriction list      */
/*  INPUTS       : 1) Name of the calling function           */
/*                 2) The restriction list can be NULL       */
/*                 3) The number of arguments                */
/*  RETURNS      : True if OK, false otherwise               */
/*  SIDE EFFECTS : EvaluationError set on errrors            */
/*  NOTES        : Used to check generic function implicit   */
/*                 method (system function) calls and system */
/*                 function calls which have the sequence    */
/*                 expansion operator in their argument list */
/*************************************************************/
bool CheckFunctionArgCount(
  Environment *theEnv,
  struct FunctionDefinition *func,
  int argumentCount)
  {
   int minArguments, maxArguments;
   const char *functionName;
   const char *restrictions;
   char theChar[2];

   theChar[0] = '0';
   theChar[1] = EOS;

   functionName = func->callFunctionName->contents;
   if (func->restrictions == NULL) restrictions = NULL;
   else restrictions = func->restrictions->contents;
     
   /*===========================================*/
   /* Determine the minimum number of arguments */
   /* required by the function.                 */
   /*===========================================*/

   minArguments = func->minArgs;

   /*===========================================*/
   /* Determine the maximum number of arguments */
   /* required by the function.                 */
   /*===========================================*/

   maxArguments = func->maxArgs;

   /*=====================================*/
   /* If the function has no restrictions */
   /* on function arguments, return true. */
   /*=====================================*/

   if ((minArguments == UNBOUNDED) && (maxArguments == UNBOUNDED))
     { return true; }

   /*==============================================*/
   /* If the function expects exactly N arguments, */
   /* then check to see if there are N arguments.  */
   /*==============================================*/

   if (minArguments == maxArguments)
     {
      if (argumentCount != minArguments)
        {
         ExpectedCountError(theEnv,functionName,EXACTLY,minArguments);
         EnvSetEvaluationError(theEnv,true);
         return false;
        }
      return true;
     }

   /*==================================*/
   /* Check to see if there were fewer */
   /* arguments passed than expected.  */
   /*==================================*/

   if (argumentCount < minArguments)
     {
      ExpectedCountError(theEnv,functionName,AT_LEAST,minArguments);
      EnvSetEvaluationError(theEnv,true);
      return false;
     }

   /*=================================*/
   /* Check to see if there were more */
   /* arguments passed than expected. */
   /*=================================*/

   if ((maxArguments != UNBOUNDED) && (argumentCount > maxArguments))
     {
      ExpectedCountError(theEnv,functionName,NO_MORE_THAN,maxArguments);
      EnvSetEvaluationError(theEnv,true);
      return false;
     }

   /*===============================*/
   /* The number of arguments falls */
   /* within the expected range.    */
   /*===============================*/

   return true;
  }

/*******************************************************************/
/* ExpectedTypeError0: Prints the error message for the wrong type */
/*   of argument passed to a user or system defined function.      */
/*******************************************************************/
void ExpectedTypeError0(
  Environment *theEnv,
  const char *functionName,
  int whichArg)
  {
   PrintErrorID(theEnv,"ARGACCES",5,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," expected argument #");
   PrintLongInteger(theEnv,WERROR,(long int) whichArg);
   EnvPrintRouter(theEnv,WERROR," to be of type ");
  }

/*******************************************************************/
/* ExpectedTypeError1: Prints the error message for the wrong type */
/*   of argument passed to a user or system defined function. The  */
/*   expected type is passed as a string to this function.         */
/*******************************************************************/
void ExpectedTypeError1(
  Environment *theEnv,
  const char *functionName,
  int whichArg,
  const char *expectedType)
  {
   PrintErrorID(theEnv,"ARGACCES",5,false);
   EnvPrintRouter(theEnv,WERROR,"Function ");
   EnvPrintRouter(theEnv,WERROR,functionName);
   EnvPrintRouter(theEnv,WERROR," expected argument #");
   PrintLongInteger(theEnv,WERROR,(long int) whichArg);
   EnvPrintRouter(theEnv,WERROR," to be of type ");
   EnvPrintRouter(theEnv,WERROR,expectedType);
   EnvPrintRouter(theEnv,WERROR,"\n");
  }

/**************************************************************/
/* ExpectedTypeError2: Prints the error message for the wrong */
/*   type of argument passed to a user or system defined      */
/*   function. The expected type is derived by examining the  */
/*   function's argument restriction list.                    */
/**************************************************************/
void ExpectedTypeError2(
  Environment *theEnv,
  const char *functionName,
  int whichArg)
  {
   struct FunctionDefinition *theFunction;
   const char *theType;

   theFunction = FindFunction(theEnv,functionName);

   if (theFunction == NULL) return;

   theType = GetArgumentTypeName(GetNthRestriction(theFunction,whichArg));

   ExpectedTypeError1(theEnv,functionName,whichArg,theType);
  }

/***************************************************/
/* GetFactOrInstanceArgument: Utility routine for  */
/*   retrieving a fact or instance argument        */
/***************************************************/
void *GetFactOrInstanceArgument(
  Environment *theEnv,
  int thePosition,
  CLIPSValue *item,
  const char *functionName)
  {
#if DEFTEMPLATE_CONSTRUCT || OBJECT_SYSTEM
   void *ptr;
#endif

   /*==============================*/
   /* Retrieve the first argument. */
   /*==============================*/

   EnvRtnUnknown(theEnv,thePosition,item);

   /*==================================================*/
   /* Fact and instance addresses are valid arguments. */
   /*==================================================*/

   if ((GetpType(item) == FACT_ADDRESS) ||
       (GetpType(item) == INSTANCE_ADDRESS))
     { return(GetpValue(item)); }

   /*==================================================*/
   /* An integer is a valid argument if it corresponds */
   /* to the fact index of an existing fact.           */
   /*==================================================*/

#if DEFTEMPLATE_CONSTRUCT
   else if (GetpType(item) == INTEGER)
     {
      if ((ptr = (void *) FindIndexedFact(theEnv,DOPToLong(item))) == NULL)
        {
         char tempBuffer[20];
         gensprintf(tempBuffer,"f-%lld",DOPToLong(item));
         CantFindItemErrorMessage(theEnv,"fact",tempBuffer);
        }
      return(ptr);
     }
#endif

   /*================================================*/
   /* Instance names and symbols are valid arguments */
   /* if they correspond to an existing instance.    */
   /*================================================*/

#if OBJECT_SYSTEM
   else if ((GetpType(item) == INSTANCE_NAME) || (GetpType(item) == SYMBOL))
     {
      if ((ptr = (void *) FindInstanceBySymbol(theEnv,(SYMBOL_HN *) GetpValue(item))) == NULL)
        {
         CantFindItemErrorMessage(theEnv,"instance",ValueToString(GetpValue(item)));
        }
      return(ptr);
     }
#endif

   /*========================================*/
   /* Any other type is an invalid argument. */
   /*========================================*/

   ExpectedTypeError2(theEnv,functionName,thePosition);
   return NULL;
  }

/****************************************************/
/* IllegalLogicalNameMessage: Generic error message */
/*   for illegal logical names.                     */
/****************************************************/
void IllegalLogicalNameMessage(
  Environment *theEnv,
  const char *theFunction)
  {
   PrintErrorID(theEnv,"IOFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Illegal logical name used for ");
   EnvPrintRouter(theEnv,WERROR,theFunction);
   EnvPrintRouter(theEnv,WERROR," function.\n");
  }
