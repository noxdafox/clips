   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/04/16             */
   /*                                                     */
   /*             CONSTRAINT CHECKING MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for constraint checking of    */
/*   data types.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Dynamic constraint checking for the            */
/*            allowed-classes constraint now searches        */
/*            imported modules.                              */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "setup.h"

#include "cstrnutl.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "multifld.h"
#include "router.h"

#if OBJECT_SYSTEM
#include "classcom.h"
#include "classexm.h"
#include "inscom.h"
#include "insfun.h"
#endif

#include "cstrnchk.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static bool                    CheckRangeAgainstCardinalityConstraint(void *,int,int,CONSTRAINT_RECORD *);
   static bool                    CheckFunctionReturnType(int,CONSTRAINT_RECORD *);
   static bool                    CheckTypeConstraint(int,CONSTRAINT_RECORD *);
   static bool                    CheckRangeConstraint(void *,int,void *,CONSTRAINT_RECORD *);
   static void                    PrintRange(void *,const char *,CONSTRAINT_RECORD *);

/******************************************************/
/* CheckFunctionReturnType: Checks a functions return */
/*   type against a set of permissable return values. */
/*   Returns true if the return type is included      */
/*   among the permissible values, otherwise false.   */
/******************************************************/
static bool CheckFunctionReturnType(
  int functionReturnType,
  CONSTRAINT_RECORD *constraints)
  {
   if (constraints == NULL) return true;

   if (constraints->anyAllowed) return true;

   switch(functionReturnType)
     {
      case 'c':
      case 'w':
      case 'b':
        if (constraints->symbolsAllowed) return true;
        else return false;

      case 's':
        if (constraints->stringsAllowed) return true;
        else return false;

      case 'j':
        if ((constraints->symbolsAllowed) ||
            (constraints->stringsAllowed) ||
            (constraints->instanceNamesAllowed)) return true;
        else return false;

      case 'k':
        if ((constraints->symbolsAllowed) || (constraints->stringsAllowed)) return true;
        else return false;

      case 'd':
      case 'f':
        if (constraints->floatsAllowed) return true;
        else return false;

      case 'i':
      case 'l':
        if (constraints->integersAllowed) return true;
        else return false;

      case 'n':
        if ((constraints->integersAllowed) || (constraints->floatsAllowed)) return true;
        else return false;

      case 'm':
        if (constraints->multifieldsAllowed) return true;
        else return false;

      case 'a':
        if (constraints->externalAddressesAllowed) return true;
        else return false;

      case 'x':
        if (constraints->instanceAddressesAllowed) return true;
        else return false;

      case 'y':
        if (constraints->factAddressesAllowed) return true;
        else return false;

      case 'o':
        if (constraints->instanceNamesAllowed) return true;
        else return false;

      case 'u':
        return true;

      case 'v':
        if (constraints->voidAllowed) return true;
     }

   return true;
  }

/****************************************************/
/* CheckTypeConstraint: Determines if a primitive   */
/*   data type satisfies the type constraint fields */
/*   of aconstraint record.                         */
/****************************************************/
static bool CheckTypeConstraint(
  int type,
  CONSTRAINT_RECORD *constraints)
  {
   if (type == RVOID) return false;

   if (constraints == NULL) return true;

   if (constraints->anyAllowed == true) return true;

   if ((type == SYMBOL) && (constraints->symbolsAllowed != true))
     { return false; }

   if ((type == STRING) && (constraints->stringsAllowed != true))
     { return false; }

   if ((type == FLOAT) && (constraints->floatsAllowed != true))
     { return false; }

   if ((type == INTEGER) && (constraints->integersAllowed != true))
     { return false; }

#if OBJECT_SYSTEM
   if ((type == INSTANCE_NAME) && (constraints->instanceNamesAllowed != true))
     { return false; }

   if ((type == INSTANCE_ADDRESS) && (constraints->instanceAddressesAllowed != true))
     { return false; }
#endif

   if ((type == EXTERNAL_ADDRESS) && (constraints->externalAddressesAllowed != true))
     { return false; }

   if ((type == RVOID) && (constraints->voidAllowed != true))
     { return false; }

   if ((type == FACT_ADDRESS) && (constraints->factAddressesAllowed != true))
     { return false; }

   return true;
  }

/********************************************************/
/* CheckCardinalityConstraint: Determines if an integer */
/*   falls within the range of allowed cardinalities    */
/*   for a constraint record.                           */
/********************************************************/
bool CheckCardinalityConstraint(
  void *theEnv,
  long number,
  CONSTRAINT_RECORD *constraints)
  {
   /*=========================================*/
   /* If the constraint record is NULL, there */
   /* are no cardinality restrictions.        */
   /*=========================================*/

   if (constraints == NULL) return true;

   /*==================================*/
   /* Determine if the integer is less */
   /* than the minimum cardinality.    */
   /*==================================*/

   if (constraints->minFields != NULL)
     {
      if (constraints->minFields->value != SymbolData(theEnv)->NegativeInfinity)
        {
         if (number < ValueToLong(constraints->minFields->value))
           { return false; }
        }
     }

   /*=====================================*/
   /* Determine if the integer is greater */
   /* than the maximum cardinality.       */
   /*=====================================*/

   if (constraints->maxFields != NULL)
     {
      if (constraints->maxFields->value != SymbolData(theEnv)->PositiveInfinity)
        {
         if (number > ValueToLong(constraints->maxFields->value))
           { return false; }
        }
     }

   /*=========================================================*/
   /* The integer falls within the allowed cardinality range. */
   /*=========================================================*/

   return true;
  }

/*****************************************************************/
/* CheckRangeAgainstCardinalityConstraint: Determines if a range */
/*   of numbers could possibly fall within the range of allowed  */
/*   cardinalities for a constraint record. Returns true if at   */
/*   least one of the numbers in the range is within the allowed */
/*   cardinality, otherwise false is returned.                   */
/*****************************************************************/
static bool CheckRangeAgainstCardinalityConstraint(
  void *theEnv,
  int min,
  int max,
  CONSTRAINT_RECORD *constraints)
  {
   /*=========================================*/
   /* If the constraint record is NULL, there */
   /* are no cardinality restrictions.        */
   /*=========================================*/

   if (constraints == NULL) return true;

   /*===============================================================*/
   /* If the minimum value of the range is greater than the maximum */
   /* value of the cardinality, then there are no numbers in the    */
   /* range which could fall within the cardinality range, and so   */
   /* false is returned.                                            */
   /*===============================================================*/

   if (constraints->maxFields != NULL)
     {
      if (constraints->maxFields->value != SymbolData(theEnv)->PositiveInfinity)
        {
         if (min > ValueToLong(constraints->maxFields->value))
           { return false; }
        }
     }

   /*===============================================================*/
   /* If the maximum value of the range is less than the minimum    */
   /* value of the cardinality, then there are no numbers in the    */
   /* range which could fall within the cardinality range, and so   */
   /* false is returned. A maximum range value of -1 indicates that */
   /* the maximum possible value of the range is positive infinity. */
   /*===============================================================*/

   if ((constraints->minFields != NULL) && (max != -1))
     {
      if (constraints->minFields->value != SymbolData(theEnv)->NegativeInfinity)
        {
         if (max < ValueToLong(constraints->minFields->value))
           { return false; }
        }
     }

   /*=============================================*/
   /* At least one number in the specified range  */
   /* falls within the allowed cardinality range. */
   /*=============================================*/

   return true;
  }

/**********************************************************************/
/* CheckAllowedValuesConstraint: Determines if a primitive data type  */
/*   satisfies the allowed-... constraint fields of a constraint      */
/*   record. Returns true if the constraints are satisfied, otherwise */
/*   false is returned.                                               */
/**********************************************************************/
bool CheckAllowedValuesConstraint(
  int type,
  void *vPtr,
  CONSTRAINT_RECORD *constraints)
  {
   struct expr *tmpPtr;

   /*=========================================*/
   /* If the constraint record is NULL, there */
   /* are no allowed-... restrictions.        */
   /*=========================================*/

   if (constraints == NULL) return true;

   /*=====================================================*/
   /* Determine if there are any allowed-... restrictions */
   /* for the type of the value being checked.            */
   /*=====================================================*/

   switch (type)
     {
      case SYMBOL:
        if ((constraints->symbolRestriction == false) &&
            (constraints->anyRestriction == false))
          { return true; }
        break;

#if OBJECT_SYSTEM
      case INSTANCE_NAME:
        if ((constraints->instanceNameRestriction == false) &&
            (constraints->anyRestriction == false))
          { return true; }
        break;
#endif

      case STRING:
        if ((constraints->stringRestriction == false) &&
            (constraints->anyRestriction == false))
          { return true; }
        break;

      case INTEGER:
        if ((constraints->integerRestriction == false) &&
            (constraints->anyRestriction == false))
          { return true; }
        break;

      case FLOAT:
        if ((constraints->floatRestriction == false) &&
            (constraints->anyRestriction == false))
          { return true; }
        break;

      default:
        return true;
     }

   /*=========================================================*/
   /* Search through the restriction list to see if the value */
   /* matches one of the allowed values in the list.          */
   /*=========================================================*/

   for (tmpPtr = constraints->restrictionList;
        tmpPtr != NULL;
        tmpPtr = tmpPtr->nextArg)
     {
      if ((tmpPtr->type == type) && (tmpPtr->value == vPtr)) return true;
     }

   /*====================================================*/
   /* If the value wasn't found in the list, then return */
   /* false because the constraint has been violated.    */
   /*====================================================*/

   return false;
  }

/**********************************************************************/
/* CheckAllowedClassesConstraint: Determines if a primitive data type */
/*   satisfies the allowed-classes constraint fields of a constraint  */
/*   record. Returns true if the constraints are satisfied, otherwise */
/*   false is returned.                                               */
/**********************************************************************/
bool CheckAllowedClassesConstraint(
  void *theEnv,
  int type,
  void *vPtr,
  CONSTRAINT_RECORD *constraints)
  {
#if OBJECT_SYSTEM
   struct expr *tmpPtr;
   INSTANCE_TYPE *ins;
   DEFCLASS *insClass, *cmpClass;

   /*=========================================*/
   /* If the constraint record is NULL, there */
   /* is no allowed-classes restriction.      */
   /*=========================================*/

   if (constraints == NULL) return true;

   /*======================================*/
   /* The constraint is satisfied if there */
   /* aren't any class restrictions.       */
   /*======================================*/
   
   if (constraints->classList == NULL)
     { return true; }

   /*==================================*/
   /* Class restrictions only apply to */
   /* instances and instance names.    */
   /*==================================*/
    
   if ((type != INSTANCE_ADDRESS) && (type != INSTANCE_NAME))
     { return true; }

   /*=============================================*/
   /* If an instance name is specified, determine */
   /* whether the instance exists.                */
   /*=============================================*/
   
   if (type == INSTANCE_ADDRESS)
     { ins = (INSTANCE_TYPE *) vPtr; }
   else
     { ins = FindInstanceBySymbol(theEnv,(SYMBOL_HN *) vPtr); }
    
   if (ins == NULL)
     { return false; }
   
   /*======================================================*/
   /* Search through the class list to see if the instance */
   /* belongs to one of the allowed classes in the list.   */
   /*======================================================*/

   insClass = (DEFCLASS *) EnvGetInstanceClass(theEnv,ins);
   for (tmpPtr = constraints->classList;
        tmpPtr != NULL;
        tmpPtr = tmpPtr->nextArg)
     {
      //cmpClass = (DEFCLASS *) EnvFindDefclass(theEnv,ValueToString(tmpPtr->value));
      cmpClass = (DEFCLASS *) LookupDefclassByMdlOrScope(theEnv,ValueToString(tmpPtr->value));
      if (cmpClass == NULL) continue;
      if (cmpClass == insClass) return true;
      if (EnvSubclassP(theEnv,insClass,cmpClass)) return true;
     }

   /*=========================================================*/
   /* If a parent class wasn't found in the list, then return */
   /* false because the constraint has been violated.         */
   /*=========================================================*/

   return false;
#else

#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(type)
#pragma unused(vPtr)
#pragma unused(constraints)
#endif

   return true;
#endif     
  }

/*************************************************************/
/* CheckRangeConstraint: Determines if a primitive data type */
/*   satisfies the range constraint of a constraint record.  */
/*************************************************************/
static bool CheckRangeConstraint(
  void *theEnv,
  int type,
  void *vPtr,
  CONSTRAINT_RECORD *constraints)
  {
   struct expr *minList, *maxList;

   /*===================================*/
   /* If the constraint record is NULL, */
   /* there are no range restrictions.  */
   /*===================================*/

   if (constraints == NULL) return true;

   /*============================================*/
   /* If the value being checked isn't a number, */
   /* then the range restrictions don't apply.   */
   /*============================================*/

   if ((type != INTEGER) && (type != FLOAT)) return true;

   /*=====================================================*/
   /* Check each of the range restrictions to see if the  */
   /* number falls within at least one of the allowed     */
   /* ranges. If it falls within one of the ranges, then  */
   /* return true since the constraint is satisifed.      */
   /*=====================================================*/

   minList = constraints->minValue;
   maxList = constraints->maxValue;

   while (minList != NULL)
     {
      if (CompareNumbers(theEnv,type,vPtr,minList->type,minList->value) == LESS_THAN)
        {
         minList = minList->nextArg;
         maxList = maxList->nextArg;
        }
      else if (CompareNumbers(theEnv,type,vPtr,maxList->type,maxList->value) == GREATER_THAN)
        {
         minList = minList->nextArg;
         maxList = maxList->nextArg;
        }
      else
        { return true; }
     }

   /*===========================================*/
   /* Return false since the number didn't fall */
   /* within one of the allowed numeric ranges. */
   /*===========================================*/

   return false;
  }

/************************************************/
/* ConstraintViolationErrorMessage: Generalized */
/*   error message for constraint violations.   */
/************************************************/
void ConstraintViolationErrorMessage(
  void *theEnv,
  const char *theWhat,
  const char *thePlace,
  bool command,
  int thePattern,
  struct symbolHashNode *theSlot,
  int theField,
  int violationType,
  CONSTRAINT_RECORD *theConstraint,
  bool printPrelude)
  {
   /*======================================================*/
   /* Don't print anything other than the tail explanation */
   /* of the error unless asked to do so.                  */
   /*======================================================*/

   if (printPrelude)
     {
      /*===================================*/
      /* Print the name of the thing which */
      /* caused the constraint violation.  */
      /*===================================*/

      if (violationType == FUNCTION_RETURN_TYPE_VIOLATION)
        {
         PrintErrorID(theEnv,"CSTRNCHK",1,true);
         EnvPrintRouter(theEnv,WERROR,"The function return value ");
        }
      else if (theWhat != NULL)
        {
         PrintErrorID(theEnv,"CSTRNCHK",1,true);
         EnvPrintRouter(theEnv,WERROR,theWhat);
         EnvPrintRouter(theEnv,WERROR," ");
        }

      /*=======================================*/
      /* Print the location of the thing which */
      /* caused the constraint violation.      */
      /*=======================================*/

      if (thePlace != NULL)
        {
         EnvPrintRouter(theEnv,WERROR,"found in ");
         if (command) EnvPrintRouter(theEnv,WERROR,"the ");
         EnvPrintRouter(theEnv,WERROR,thePlace);
         if (command) EnvPrintRouter(theEnv,WERROR," command");
        }

      /*================================================*/
      /* If the violation occured in the LHS of a rule, */
      /* indicate which pattern was at fault.           */
      /*================================================*/

      if (thePattern > 0)
        {
         EnvPrintRouter(theEnv,WERROR,"found in CE #");
         PrintLongInteger(theEnv,WERROR,(long int) thePattern);
        }
     }

   /*===============================================================*/
   /* Indicate the type of constraint violation (type, range, etc). */
   /*===============================================================*/

   if ((violationType == TYPE_VIOLATION) ||
       (violationType == FUNCTION_RETURN_TYPE_VIOLATION))
     { EnvPrintRouter(theEnv,WERROR,"\ndoes not match the allowed types"); }
   else if (violationType == RANGE_VIOLATION)
     {
      EnvPrintRouter(theEnv,WERROR,"\ndoes not fall in the allowed range ");
      PrintRange(theEnv,WERROR,theConstraint);
     }
   else if (violationType == ALLOWED_VALUES_VIOLATION)
     { EnvPrintRouter(theEnv,WERROR,"\ndoes not match the allowed values"); }
   else if (violationType == CARDINALITY_VIOLATION)
     { EnvPrintRouter(theEnv,WERROR,"\ndoes not satisfy the cardinality restrictions"); }
   else if (violationType == ALLOWED_CLASSES_VIOLATION)
     { EnvPrintRouter(theEnv,WERROR,"\ndoes not match the allowed classes"); }

   /*==============================================*/
   /* Print either the slot name or field position */
   /* where the constraint violation occured.      */
   /*==============================================*/

   if (theSlot != NULL)
     {
      EnvPrintRouter(theEnv,WERROR," for slot ");
      EnvPrintRouter(theEnv,WERROR,ValueToString(theSlot));
     }
   else if (theField > 0)
     {
      EnvPrintRouter(theEnv,WERROR," for field #");
      PrintLongInteger(theEnv,WERROR,(long long) theField);
     }

   EnvPrintRouter(theEnv,WERROR,".\n");
  }

/********************************************************************/
/* PrintRange: Prints the range restriction of a constraint record. */
/*   For example, 8 to +00 (eight to positive infinity).            */
/********************************************************************/
static void PrintRange(
  void *theEnv,
  const char *logicalName,
  CONSTRAINT_RECORD *theConstraint)
  {
   if (theConstraint->minValue->value == SymbolData(theEnv)->NegativeInfinity)
     { EnvPrintRouter(theEnv,logicalName,ValueToString(SymbolData(theEnv)->NegativeInfinity)); }
   else PrintExpression(theEnv,logicalName,theConstraint->minValue);
   EnvPrintRouter(theEnv,logicalName," to ");
   if (theConstraint->maxValue->value == SymbolData(theEnv)->PositiveInfinity)
     { EnvPrintRouter(theEnv,logicalName,ValueToString(SymbolData(theEnv)->PositiveInfinity)); }
   else PrintExpression(theEnv,logicalName,theConstraint->maxValue);
  }

/*************************************************************/
/* ConstraintCheckDataObject: Given a value stored in a data */
/*   object structure and a constraint record, determines if */
/*   the data object satisfies the constraint record.        */
/*************************************************************/
int ConstraintCheckDataObject(
  void *theEnv,
  DATA_OBJECT *theData,
  CONSTRAINT_RECORD *theConstraints)
  {
   long i; /* 6.04 Bug Fix */
   int rv;
   struct field *theMultifield;

   if (theConstraints == NULL) return(NO_VIOLATION);

   if (theData->type == MULTIFIELD)
     {
      if (CheckCardinalityConstraint(theEnv,(theData->end - theData->begin) + 1,
                                     theConstraints) == false)
        { return(CARDINALITY_VIOLATION); }

      theMultifield = ((struct multifield *) theData->value)->theFields;
      for (i = theData->begin; i <= theData->end; i++)
        {
         if ((rv = ConstraintCheckValue(theEnv,theMultifield[i].type,
                                        theMultifield[i].value,
                                        theConstraints)) != NO_VIOLATION)
           { return(rv); }
        }

      return(NO_VIOLATION);
     }

   if (CheckCardinalityConstraint(theEnv,1L,theConstraints) == false)
    { return(CARDINALITY_VIOLATION); }

   return(ConstraintCheckValue(theEnv,theData->type,theData->value,theConstraints));
  }

/****************************************************************/
/* ConstraintCheckValue: Given a value and a constraint record, */
/*   determines if the value satisfies the constraint record.   */
/****************************************************************/
int ConstraintCheckValue(
  void *theEnv,
  int theType,
  void *theValue,
  CONSTRAINT_RECORD *theConstraints)
  {
   if (CheckTypeConstraint(theType,theConstraints) == false)
     { return(TYPE_VIOLATION); }

   else if (CheckAllowedValuesConstraint(theType,theValue,theConstraints) == false)
     { return(ALLOWED_VALUES_VIOLATION); }

   else if (CheckAllowedClassesConstraint(theEnv,theType,theValue,theConstraints) == false)
     { return(ALLOWED_CLASSES_VIOLATION); }

   else if (CheckRangeConstraint(theEnv,theType,theValue,theConstraints) == false)
     { return(RANGE_VIOLATION); }

   else if (theType == FCALL)
     {
      if (CheckFunctionReturnType((int) ValueFunctionType(theValue),theConstraints) == false)
        { return(FUNCTION_RETURN_TYPE_VIOLATION); }
     }

   return(NO_VIOLATION);
  }

/********************************************************************/
/* ConstraintCheckExpressionChain: Checks an expression and nextArg */
/* links for constraint conflicts (argList is not followed).        */
/********************************************************************/
int ConstraintCheckExpressionChain(
  void *theEnv,
  struct expr *theExpression,
  CONSTRAINT_RECORD *theConstraints)
  {
   struct expr *theExp;
   int min = 0, max = 0, vCode;

   /*===========================================================*/
   /* Determine the minimum and maximum number of value which   */
   /* can be derived from the expression chain (max of -1 means */
   /* positive infinity).                                       */
   /*===========================================================*/

   for (theExp = theExpression ; theExp != NULL ; theExp = theExp->nextArg)
     {
      if (ConstantType(theExp->type)) min++;
      else if (theExp->type == FCALL)
        {
         if ((ExpressionFunctionType(theExp) != 'm') &&
             (ExpressionFunctionType(theExp) != 'u')) min++;
         else max = -1;
        }
      else max = -1;
     }

   /*====================================*/
   /* Check for a cardinality violation. */
   /*====================================*/

   if (max == 0) max = min;
   if (CheckRangeAgainstCardinalityConstraint(theEnv,min,max,theConstraints) == false)
     { return(CARDINALITY_VIOLATION); }

   /*========================================*/
   /* Check for other constraint violations. */
   /*========================================*/

   for (theExp = theExpression ; theExp != NULL ; theExp = theExp->nextArg)
     {
      vCode = ConstraintCheckValue(theEnv,theExp->type,theExp->value,theConstraints);
      if (vCode != NO_VIOLATION)
        return(vCode);
     }

   return(NO_VIOLATION);
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/***************************************************/
/* ConstraintCheckExpression: Checks an expression */
/*   for constraint conflicts. Returns zero if     */
/*   conflicts are found, otherwise non-zero.      */
/***************************************************/
int ConstraintCheckExpression(
  void *theEnv,
  struct expr *theExpression,
  CONSTRAINT_RECORD *theConstraints)
  {
   int rv = NO_VIOLATION;

   if (theConstraints == NULL) return(rv);

   while (theExpression != NULL)
     {
      rv = ConstraintCheckValue(theEnv,theExpression->type,
                                theExpression->value,
                                theConstraints);
      if (rv != NO_VIOLATION) return(rv);
      rv = ConstraintCheckExpression(theEnv,theExpression->argList,theConstraints);
      if (rv != NO_VIOLATION) return(rv);
      theExpression = theExpression->nextArg;
     }

   return(rv);
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#if (! RUN_TIME)

/*****************************************************/
/* UnmatchableConstraint: Determines if a constraint */
/*  record can still be satisfied by some value.     */
/*****************************************************/
bool UnmatchableConstraint(
  CONSTRAINT_RECORD *theConstraint)
  {
   if (theConstraint == NULL) return false;

   if ((! theConstraint->anyAllowed) &&
       (! theConstraint->symbolsAllowed) &&
       (! theConstraint->stringsAllowed) &&
       (! theConstraint->floatsAllowed) &&
       (! theConstraint->integersAllowed) &&
       (! theConstraint->instanceNamesAllowed) &&
       (! theConstraint->instanceAddressesAllowed) &&
       (! theConstraint->multifieldsAllowed) &&
       (! theConstraint->externalAddressesAllowed) &&
       (! theConstraint->voidAllowed) &&
       (! theConstraint->factAddressesAllowed))
     { return true; }

   return false;
  }

#endif /* (! RUN_TIME) */

