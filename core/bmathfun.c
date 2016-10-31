   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/01/16             */
   /*                                                     */
   /*             BASIC MATH FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for numerous basic math        */
/*   functions including +, *, -, /, integer, float, div,    */
/*   abs,set-auto-float-dividend, get-auto-float-dividend,   */
/*   min, and max.                                           */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Converted API macros to function calls.        */
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
/*            Auto-float-dividend always enabled.            */
/*                                                           */
/*************************************************************/

#include <stdio.h>

#include "setup.h"

#include "argacces.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "prntutil.h"
#include "router.h"

#include "bmathfun.h"

/***************************************************************/
/* BasicMathFunctionDefinitions: Defines basic math functions. */
/***************************************************************/
void BasicMathFunctionDefinitions(
  Environment *theEnv)
  {
#if ! RUN_TIME
   EnvAddUDF(theEnv,"+","ld",2,UNBOUNDED,"ld",AdditionFunction,"AdditionFunction",NULL);
   EnvAddUDF(theEnv,"*","ld",2,UNBOUNDED,"ld",MultiplicationFunction,"MultiplicationFunction",NULL);
   EnvAddUDF(theEnv,"-","ld",2,UNBOUNDED,"ld",SubtractionFunction,"SubtractionFunction",NULL);
   EnvAddUDF(theEnv,"/","d",2,UNBOUNDED,"ld",DivisionFunction,"DivisionFunction",NULL);
   EnvAddUDF(theEnv,"div","l",2,UNBOUNDED,"ld",DivFunction,"DivFunction",NULL);
   EnvAddUDF(theEnv,"integer","l",1,1,"ld",IntegerFunction,"IntegerFunction",NULL);
   EnvAddUDF(theEnv,"float","d",1,1,"ld",FloatFunction,"FloatFunction",NULL);
   EnvAddUDF(theEnv,"abs","ld",1,1,"ld",AbsFunction,"AbsFunction",NULL);
   EnvAddUDF(theEnv,"min","ld",1,UNBOUNDED,"ld",MinFunction,"MinFunction",NULL);
   EnvAddUDF(theEnv,"max","ld",1,UNBOUNDED,"ld",MaxFunction,"MaxFunction",NULL);
#endif
  }

/**********************************/
/* AdditionFunction: H/L access   */
/*   routine for the + function.  */
/**********************************/
void AdditionFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double ftotal = 0.0;
   long long ltotal = 0LL;
   bool useFloatTotal = false;
   UDFValue theArg;

   /*=================================================*/
   /* Loop through each of the arguments adding it to */
   /* a running total. If a floating point number is  */
   /* encountered, then do all subsequent operations  */
   /* using floating point values.                    */
   /*=================================================*/

   while (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,NUMBER_BITS,&theArg))
        { return; }

      if (useFloatTotal)
        { ftotal += CVCoerceToFloat(&theArg); }
      else
        {
         if (CVIsType(&theArg,INTEGER_BIT))
           { ltotal += theArg.integerValue->contents; }
         else
           {
            ftotal = (double) ltotal + CVCoerceToFloat(&theArg);
            useFloatTotal = true;
           }
        }
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   if (useFloatTotal)
     { returnValue->floatValue = EnvCreateFloat(theEnv,ftotal); }
   else
     { returnValue->integerValue = EnvCreateInteger(theEnv,ltotal); }
  }

/****************************************/
/* MultiplicationFunction: CLIPS access */
/*   routine for the * function.        */
/****************************************/
void MultiplicationFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double ftotal = 1.0;
   long long ltotal = 1LL;
   bool useFloatTotal = false;
   UDFValue theArg;

   /*===================================================*/
   /* Loop through each of the arguments multiplying it */
   /* by a running product. If a floating point number  */
   /* is encountered, then do all subsequent operations */
   /* using floating point values.                      */
   /*===================================================*/

   while (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,NUMBER_BITS,&theArg))
        { return; }

      if (useFloatTotal)
        { ftotal *= CVCoerceToFloat(&theArg); }
      else
        {
         if (CVIsType(&theArg,INTEGER_BIT))
           { ltotal *= theArg.integerValue->contents; }
         else
           {
            ftotal = (double) ltotal * CVCoerceToFloat(&theArg);
            useFloatTotal = true;
           }
        }
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   if (useFloatTotal)
     { returnValue->floatValue = EnvCreateFloat(theEnv,ftotal); }
   else
     { returnValue->integerValue = EnvCreateInteger(theEnv,ltotal); }
  }

/*************************************/
/* SubtractionFunction: CLIPS access */
/*   routine for the - function.     */
/*************************************/
void SubtractionFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double ftotal = 0.0;
   long long ltotal = 0LL;
   bool useFloatTotal = false;
   UDFValue theArg;

   /*=================================================*/
   /* Get the first argument. This number which will  */
   /* be the starting total from which all subsequent */
   /* arguments will subtracted.                      */
   /*=================================================*/

   if (! UDFFirstArgument(context,NUMBER_BITS,&theArg))
     { return; }

   if (CVIsType(&theArg,INTEGER_BIT))
     { ltotal = theArg.integerValue->contents; }
   else
     {
      ftotal = CVCoerceToFloat(&theArg);
      useFloatTotal = true;
     }

   /*===================================================*/
   /* Loop through each of the arguments subtracting it */
   /* from a running total. If a floating point number  */
   /* is encountered, then do all subsequent operations */
   /* using floating point values.                      */
   /*===================================================*/

   while (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,NUMBER_BITS,&theArg))
        { return; }

      if (useFloatTotal)
        { ftotal -= CVCoerceToFloat(&theArg); }
      else
        {
         if (CVIsType(&theArg,INTEGER_BIT))
           { ltotal -= theArg.integerValue->contents; }
         else
           {
            ftotal = (double) ltotal - theArg.floatValue->contents;
            useFloatTotal = true;
           }
        }
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   if (useFloatTotal)
     { returnValue->floatValue = EnvCreateFloat(theEnv,ftotal); }
   else
     { returnValue->integerValue = EnvCreateInteger(theEnv,ltotal); }
  }

/***********************************/
/* DivisionFunction:  CLIPS access */
/*   routine for the / function.   */
/***********************************/
void DivisionFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   double ftotal = 1.0;
   double theNumber;
   UDFValue theArg;

   /*===================================================*/
   /* Get the first argument. This number which will be */
   /* the starting product from which all subsequent    */
   /* arguments will divide. If the auto float dividend */
   /* feature is enable, then this number is converted  */
   /* to a float if it is an integer.                   */
   /*===================================================*/

   if (! UDFFirstArgument(context,NUMBER_BITS,&theArg))
     { return; }

   ftotal = CVCoerceToFloat(&theArg);

   /*====================================================*/
   /* Loop through each of the arguments dividing it     */
   /* into a running product. If a floating point number */
   /* is encountered, then do all subsequent operations  */
   /* using floating point values. Each argument is      */
   /* checked to prevent a divide by zero error.         */
   /*====================================================*/

   while (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,NUMBER_BITS,&theArg))
        { return; }

      theNumber = CVCoerceToFloat(&theArg);

      if (theNumber == 0.0)
        {
         DivideByZeroErrorMessage(theEnv,"/");
         EnvSetEvaluationError(theEnv,true);
         returnValue->floatValue = EnvCreateFloat(theEnv,1.0);
         return;
        }

      ftotal /= theNumber;
     }

   /*======================================================*/
   /* If a floating point number was in the argument list, */
   /* then return a float, otherwise return an integer.    */
   /*======================================================*/

   returnValue->floatValue = EnvCreateFloat(theEnv,ftotal);
  }

/*************************************/
/* DivFunction: H/L access routine   */
/*   for the div function.           */
/*************************************/
void DivFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   long long total = 1LL;
   UDFValue theArg;
   long long theNumber;

   /*===================================================*/
   /* Get the first argument. This number which will be */
   /* the starting product from which all subsequent    */
   /* arguments will divide.                            */
   /*===================================================*/

   if (! UDFFirstArgument(context,NUMBER_BITS,&theArg))
     { return; }
   total = CVCoerceToInteger(&theArg);

   /*=====================================================*/
   /* Loop through each of the arguments dividing it into */
   /* a running product. Floats are converted to integers */
   /* and each argument is checked to prevent a divide by */
   /* zero error.                                         */
   /*=====================================================*/

   while (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,NUMBER_BITS,&theArg))
        { return; }

      theNumber = CVCoerceToInteger(&theArg);

      if (theNumber == 0LL)
        {
         DivideByZeroErrorMessage(theEnv,"div");
         EnvSetEvaluationError(theEnv,true);
         returnValue->integerValue = EnvCreateInteger(theEnv,1L);
         return;
        }

      total /= theNumber;
     }

   /*======================================================*/
   /* The result of the div function is always an integer. */
   /*======================================================*/

   returnValue->integerValue = EnvCreateInteger(theEnv,total);
  }

/*****************************************/
/* IntegerFunction: H/L access routine   */
/*   for the integer function.           */
/*****************************************/
void IntegerFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   /*======================================*/
   /* Check that the argument is a number. */
   /*======================================*/

   if (! UDFNthArgument(context,1,NUMBER_BITS,returnValue))
     { return; }

   /*============================================*/
   /* Convert a float type to integer, otherwise */
   /* return the argument unchanged.             */
   /*============================================*/

   if (CVIsType(returnValue,FLOAT_BIT))
     { returnValue->integerValue = EnvCreateInteger(theEnv,CVCoerceToInteger(returnValue)); }
  }

/***************************************/
/* FloatFunction: H/L access routine   */
/*   for the float function.           */
/***************************************/
void FloatFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   /*======================================*/
   /* Check that the argument is a number. */
   /*======================================*/

   if (! UDFNthArgument(context,1,NUMBER_BITS,returnValue))
     { return; }

   /*=============================================*/
   /* Convert an integer type to float, otherwise */
   /* return the argument unchanged.              */
   /*=============================================*/

   if (CVIsType(returnValue,INTEGER_BIT))
     { returnValue->floatValue = EnvCreateFloat(theEnv,CVCoerceToFloat(returnValue)); }
  }

/*************************************/
/* AbsFunction: H/L access routine   */
/*   for the abs function.           */
/*************************************/
void AbsFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   /*======================================*/
   /* Check that the argument is a number. */
   /*======================================*/

   if (! UDFNthArgument(context,1,NUMBER_BITS,returnValue))
     { return; }

   /*==========================================*/
   /* Return the absolute value of the number. */
   /*==========================================*/

   if (CVIsType(returnValue,INTEGER_BIT))
     {
      long long lv = returnValue->integerValue->contents;
      if (lv < 0L)
        { returnValue->integerValue = EnvCreateInteger(theEnv,-lv); }
     }
   else
     {
      double dv = returnValue->floatValue->contents;
      if (dv < 0.0)
        { returnValue->floatValue = EnvCreateFloat(theEnv,-dv); }
     }
  }

/*************************************/
/* MinFunction: H/L access routine   */
/*   for the min function.           */
/*************************************/
void MinFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue nextPossible;

   /*============================================*/
   /* Check that the first argument is a number. */
   /*============================================*/

   if (! UDFFirstArgument(context,NUMBER_BITS,returnValue))
     { return; }

   /*===========================================================*/
   /* Loop through the remaining arguments, first checking each */
   /* argument to see that it is a number, and then determining */
   /* if the argument is less than the previous arguments and   */
   /* is thus the minimum value.                                */
   /*===========================================================*/

   while (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,NUMBER_BITS,&nextPossible))
        { return; }

      /*=============================================*/
      /* If either argument is a float, convert both */
      /* to floats. Otherwise compare two integers.  */
      /*=============================================*/

      if (CVIsType(returnValue,FLOAT_BIT) || CVIsType(&nextPossible,FLOAT_BIT))
        {
         if (CVCoerceToFloat(returnValue) > CVCoerceToFloat(&nextPossible))
           { returnValue->value = nextPossible.value; }
        }
      else
        {
         if (returnValue->integerValue->contents > nextPossible.integerValue->contents)
           { returnValue->value = nextPossible.value; }
        }
     }
  }

/*************************************/
/* MaxFunction: H/L access routine   */
/*   for the max function.           */
/*************************************/
void MaxFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue nextPossible;

   /*============================================*/
   /* Check that the first argument is a number. */
   /*============================================*/

   if (! UDFFirstArgument(context,NUMBER_BITS,returnValue))
     { return; }

   /*===========================================================*/
   /* Loop through the remaining arguments, first checking each */
   /* argument to see that it is a number, and then determining */
   /* if the argument is greater than the previous arguments    */
   /* and is thus the maximum value.                            */
   /*===========================================================*/

   while (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,NUMBER_BITS,&nextPossible))
        { return; }

      /*=============================================*/
      /* If either argument is a float, convert both */
      /* to floats. Otherwise compare two integers.  */
      /*=============================================*/

      if (CVIsType(returnValue,FLOAT_BIT) || CVIsType(&nextPossible,FLOAT_BIT))
        {
         if (CVCoerceToFloat(returnValue) < CVCoerceToFloat(&nextPossible))
           { returnValue->value = nextPossible.value; }
        }
      else
        {
         if (returnValue->integerValue->contents < nextPossible.integerValue->contents)
           { returnValue->value = nextPossible.value; }
        }
     }
  }

