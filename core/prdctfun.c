   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*              PREDICATE FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several predicate          */
/*   functions including not, and, or, eq, neq, <=, >=, <,   */
/*   >, =, <>, symbolp, stringp, lexemep, numberp, integerp, */
/*   floatp, oddp, evenp, multifieldp, sequencep, and        */
/*   pointerp.                                               */
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
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include <stdio.h>

#include "setup.h"

#include "argacces.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "multifld.h"
#include "router.h"

#include "prdctfun.h"

/**************************************************/
/* PredicateFunctionDefinitions: Defines standard */
/*   math and predicate functions.                */
/**************************************************/
void PredicateFunctionDefinitions(
  Environment *theEnv)
  {
#if ! RUN_TIME
   EnvAddUDF(theEnv,"not","b",1,1,NULL,NotFunction,"NotFunction",NULL);
   EnvAddUDF(theEnv,"and","b",2,UNBOUNDED ,NULL,AndFunction,"AndFunction",NULL);
   EnvAddUDF(theEnv,"or","b",2,UNBOUNDED ,NULL,OrFunction,"OrFunction",NULL);

   EnvAddUDF(theEnv,"eq","b",2,UNBOUNDED,NULL,EqFunction,"EqFunction",NULL);
   EnvAddUDF(theEnv,"neq","b",2,UNBOUNDED,NULL,NeqFunction,"NeqFunction",NULL);

   EnvAddUDF(theEnv,"<=","b",2,UNBOUNDED ,"ld",LessThanOrEqualFunction,"LessThanOrEqualFunction",NULL);
   EnvAddUDF(theEnv,">=","b",2,UNBOUNDED ,"ld",GreaterThanOrEqualFunction,"GreaterThanOrEqualFunction",NULL);
   EnvAddUDF(theEnv,"<","b",2,UNBOUNDED ,"ld",LessThanFunction,"LessThanFunction",NULL);
   EnvAddUDF(theEnv,">","b",2,UNBOUNDED ,"ld",GreaterThanFunction,"GreaterThanFunction",NULL);
   EnvAddUDF(theEnv,"=","b",2,UNBOUNDED ,"ld",NumericEqualFunction,"NumericEqualFunction",NULL);
   EnvAddUDF(theEnv,"<>","b",2,UNBOUNDED ,"ld",NumericNotEqualFunction,"NumericNotEqualFunction",NULL);
   EnvAddUDF(theEnv,"!=","b",2,UNBOUNDED ,"ld",NumericNotEqualFunction,"NumericNotEqualFunction",NULL);

   EnvAddUDF(theEnv,"symbolp","b",1,1,NULL,SymbolpFunction,"SymbolpFunction",NULL);
   EnvAddUDF(theEnv,"wordp","b",1,1,NULL,SymbolpFunction,"SymbolpFunction",NULL);  // TBD Remove?
   EnvAddUDF(theEnv,"stringp","b",1,1,NULL,StringpFunction,"StringpFunction",NULL);
   EnvAddUDF(theEnv,"lexemep","b",1,1,NULL,LexemepFunction,"LexemepFunction",NULL);
   EnvAddUDF(theEnv,"numberp","b",1,1,NULL,NumberpFunction,"NumberpFunction",NULL);
   EnvAddUDF(theEnv,"integerp","b",1,1,NULL,IntegerpFunction,"IntegerpFunction",NULL);
   EnvAddUDF(theEnv,"floatp","b",1,1,NULL,FloatpFunction,"FloatpFunction",NULL);
   EnvAddUDF(theEnv,"oddp","b",1,1,"l",OddpFunction,"OddpFunction",NULL);
   EnvAddUDF(theEnv,"evenp","b",1,1,"l",EvenpFunction,"EvenpFunction",NULL);
   EnvAddUDF(theEnv,"multifieldp","b",1,1,NULL,MultifieldpFunction,"MultifieldpFunction",NULL);
   EnvAddUDF(theEnv,"sequencep","b",1,1,NULL,MultifieldpFunction,"MultifieldpFunction",NULL); // TBD Remove?
   EnvAddUDF(theEnv,"pointerp","b",1,1,NULL,PointerpFunction,"PointerpFunction",NULL);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/************************************/
/* EqFunction: H/L access routine   */
/*   for the eq function.           */
/************************************/
void EqFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item, nextItem;
   int numArgs, i;
   struct expr *theExpression;

   returnValue->type = SYMBOL;
   
   /*====================================*/
   /* Determine the number of arguments. */
   /*====================================*/

   numArgs = EnvRtnArgCount(theEnv);
   if (numArgs == 0)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*==============================================*/
   /* Get the value of the first argument against  */
   /* which subsequent arguments will be compared. */
   /*==============================================*/

   theExpression = GetFirstArgument();
   EvaluateExpression(theEnv,theExpression,&item);

   /*=====================================*/
   /* Compare all arguments to the first. */
   /* If any are the same, return FALSE.  */
   /*=====================================*/

   theExpression = GetNextArgument(theExpression);
   for (i = 2 ; i <= numArgs ; i++)
     {
      EvaluateExpression(theEnv,theExpression,&nextItem);

      if (GetType(nextItem) != GetType(item))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }

      if (GetType(nextItem) == MULTIFIELD)
        {
         if (MultifieldDOsEqual(&nextItem,&item) == false)
           {
            returnValue->value = EnvFalseSymbol(theEnv);
            return;
           }
        }
      else if (nextItem.value != item.value)
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }

      theExpression = GetNextArgument(theExpression);
     }

   /*=====================================*/
   /* All of the arguments were different */
   /* from the first. Return TRUE.        */
   /*=====================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/*************************************/
/* NeqFunction: H/L access routine   */
/*   for the neq function.           */
/*************************************/
void NeqFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item, nextItem;
   int numArgs, i;
   struct expr *theExpression;

   returnValue->type = SYMBOL;
   
   /*====================================*/
   /* Determine the number of arguments. */
   /*====================================*/

   numArgs = EnvRtnArgCount(theEnv);
   if (numArgs == 0)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   /*==============================================*/
   /* Get the value of the first argument against  */
   /* which subsequent arguments will be compared. */
   /*==============================================*/

   theExpression = GetFirstArgument();
   EvaluateExpression(theEnv,theExpression,&item);

   /*=====================================*/
   /* Compare all arguments to the first. */
   /* If any are different, return FALSE. */
   /*=====================================*/

   for (i = 2, theExpression = GetNextArgument(theExpression);
        i <= numArgs;
        i++, theExpression = GetNextArgument(theExpression))
     {
      EvaluateExpression(theEnv,theExpression,&nextItem);
      if (GetType(nextItem) != GetType(item))
        { continue; }
      else if (nextItem.type == MULTIFIELD)
        {
         if (MultifieldDOsEqual(&nextItem,&item) == true)
           {
            returnValue->value = EnvFalseSymbol(theEnv);
            return;
           }
        }
      else if (nextItem.value == item.value)
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
     }

   /*=====================================*/
   /* All of the arguments were identical */
   /* to the first. Return TRUE.          */
   /*=====================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/*****************************************/
/* StringpFunction: H/L access routine   */
/*   for the stringp function.           */
/*****************************************/
void StringpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) == STRING)
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/*****************************************/
/* SymbolpFunction: H/L access routine   */
/*   for the symbolp function.           */
/*****************************************/
void SymbolpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) == SYMBOL)
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/*****************************************/
/* LexemepFunction: H/L access routine   */
/*   for the lexemep function.           */
/*****************************************/
void LexemepFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if ((GetType(item) == SYMBOL) || (GetType(item) == STRING))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/*****************************************/
/* NumberpFunction: H/L access routine   */
/*   for the numberp function.           */
/*****************************************/
void NumberpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if ((GetType(item) == FLOAT) || (GetType(item) == INTEGER))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/****************************************/
/* FloatpFunction: H/L access routine   */
/*   for the floatp function.           */
/****************************************/
void FloatpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) == FLOAT)
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/******************************************/
/* IntegerpFunction: H/L access routine   */
/*   for the integerp function.           */
/******************************************/
void IntegerpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) != INTEGER)
     { returnValue->value = EnvFalseSymbol(theEnv); }
   else
     { returnValue->value = EnvTrueSymbol(theEnv); }
  }

/*********************************************/
/* MultifieldpFunction: H/L access routine   */
/*   for the multifieldp function.           */
/*********************************************/
void MultifieldpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) != MULTIFIELD)
     { returnValue->value = EnvFalseSymbol(theEnv); }
   else
     { returnValue->value = EnvTrueSymbol(theEnv); }
  }

/******************************************/
/* PointerpFunction: H/L access routine   */
/*   for the pointerp function.           */
/******************************************/
void PointerpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;

   returnValue->type = SYMBOL;
   
   EnvRtnUnknown(theEnv,1,&item);

   if (GetType(item) != EXTERNAL_ADDRESS)
     { returnValue->value = EnvFalseSymbol(theEnv); }
   else
     { returnValue->value = EnvTrueSymbol(theEnv); }
  }

/*************************************/
/* NotFunction: H/L access routine   */
/*   for the not function.           */
/*************************************/
void NotFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue result;

   returnValue->type = SYMBOL;
   
   theArgument = GetFirstArgument();
   if (theArgument == NULL)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   if (EvaluateExpression(theEnv,theArgument,&result))
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   if ((result.value == EnvFalseSymbol(theEnv)) && (result.type == SYMBOL))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

/*************************************/
/* AndFunction: H/L access routine   */
/*   for the and function.           */
/*************************************/
void AndFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue result;

   returnValue->type = SYMBOL;
   
   for (theArgument = GetFirstArgument();
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument))
     {
      if (EvaluateExpression(theEnv,theArgument,&result))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
      if ((result.value == EnvFalseSymbol(theEnv)) && (result.type == SYMBOL))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
     }

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/************************************/
/* OrFunction: H/L access routine   */
/*   for the or function.           */
/************************************/
void OrFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue result;

   returnValue->type = SYMBOL;
   
   for (theArgument = GetFirstArgument();
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument))
     {
      if (EvaluateExpression(theEnv,theArgument,&result))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }

      if ((result.value != EnvFalseSymbol(theEnv)) || (result.type != SYMBOL))
        {
         returnValue->value = EnvTrueSymbol(theEnv);
         return;
        }
     }

   returnValue->value = EnvFalseSymbol(theEnv);
  }

/*****************************************/
/* LessThanOrEqualFunction: H/L access   */
/*   routine for the <= function.        */
/*****************************************/
void LessThanOrEqualFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue rv1, rv2;
   int pos = 1;

   returnValue->type = SYMBOL;
   
   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL)
     {
      returnValue->value = EnvTrueSymbol(theEnv);
      return;
     }
   if (! GetNumericArgument(theEnv,theArgument,"<=",&rv1,false,pos))
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   pos++;

   /*====================================================*/
   /* Compare each of the subsequent arguments to its    */
   /* predecessor. If any is greater, then return FALSE. */
   /*====================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,"<=",&rv2,false,pos))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
        
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) > ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) > ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) > (double) ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if (ValueToDouble(rv1.value) > ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*======================================*/
   /* Each argument was less than or equal */
   /* to its predecessor. Return TRUE.     */
   /*======================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/********************************************/
/* GreaterThanOrEqualFunction: H/L access   */
/*   routine for the >= function.           */
/********************************************/
void GreaterThanOrEqualFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue rv1, rv2;
   int pos = 1;

   returnValue->type = SYMBOL;
   
   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL)
     {
      returnValue->value = EnvTrueSymbol(theEnv);
      return;
     }

   if (! GetNumericArgument(theEnv,theArgument,">=",&rv1,false,pos))
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   pos++;

   /*===================================================*/
   /* Compare each of the subsequent arguments to its   */
   /* predecessor. If any is lesser, then return FALSE. */
   /*===================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,">=",&rv2,false,pos))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
        
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) < ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) < ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) < (double) ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if (ValueToDouble(rv1.value) < ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*=========================================*/
   /* Each argument was greater than or equal */
   /* to its predecessor. Return TRUE.        */
   /*=========================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/**********************************/
/* LessThanFunction: H/L access   */
/*   routine for the < function.  */
/**********************************/
void LessThanFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue rv1, rv2;
   int pos = 1;

   returnValue->type = SYMBOL;
   
   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL)
     {
      returnValue->value = EnvTrueSymbol(theEnv);
      return;
     }

   if (! GetNumericArgument(theEnv,theArgument,"<",&rv1,false,pos))
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   pos++;

   /*==========================================*/
   /* Compare each of the subsequent arguments */
   /* to its predecessor. If any is greater or */
   /* equal, then return FALSE.                */
   /*==========================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,"<",&rv2,false,pos))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
        
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) >= ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) >= ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) >= (double) ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if (ValueToDouble(rv1.value) >= ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*=================================*/
   /* Each argument was less than its */
   /* predecessor. Return TRUE.       */
   /*=================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/*************************************/
/* GreaterThanFunction: H/L access   */
/*   routine for the > function.     */
/*************************************/
void GreaterThanFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue rv1, rv2;
   int pos = 1;

   returnValue->type = SYMBOL;
   
   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL)
     {
      returnValue->value = EnvTrueSymbol(theEnv);
      return;
     }

   if (! GetNumericArgument(theEnv,theArgument,">",&rv1,false,pos))
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   pos++;

   /*==========================================*/
   /* Compare each of the subsequent arguments */
   /* to its predecessor. If any is lesser or  */
   /* equal, then return FALSE.                */
   /*==========================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,">",&rv2,false,pos))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
        
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) <= ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) <= ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) <= (double) ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if (ValueToDouble(rv1.value) <= ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }

      rv1.type = rv2.type;
      rv1.value = rv2.value;
     }

   /*================================*/
   /* Each argument was greater than */
   /* its predecessor. Return TRUE.  */
   /*================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/**************************************/
/* NumericEqualFunction: H/L access   */
/*   routine for the = function.      */
/**************************************/
void NumericEqualFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue rv1, rv2;
   int pos = 1;

   returnValue->type = SYMBOL;
   
   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();

   if (theArgument == NULL)
     {
      returnValue->value = EnvTrueSymbol(theEnv);
      return;
     }

   if (! GetNumericArgument(theEnv,theArgument,"=",&rv1,false,pos))
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   pos++;

   /*=================================================*/
   /* Compare each of the subsequent arguments to the */
   /* first. If any is unequal, then return FALSE.    */
   /*=================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,"=",&rv2,false,pos))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
        
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) != ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) != ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) != (double) ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if (ValueToDouble(rv1.value) != ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
     }

   /*=================================*/
   /* All arguments were equal to the */
   /* first argument. Return TRUE.    */
   /*=================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/*****************************************/
/* NumericNotEqualFunction: H/L access   */
/*   routine for the <> function.        */
/*****************************************/
void NumericNotEqualFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   EXPRESSION *theArgument;
   CLIPSValue rv1, rv2;
   int pos = 1;

   returnValue->type = SYMBOL;
   
   /*=========================*/
   /* Get the first argument. */
   /*=========================*/

   theArgument = GetFirstArgument();
   if (theArgument == NULL)
     {
      returnValue->value = EnvTrueSymbol(theEnv);
      return;
     }

   if (! GetNumericArgument(theEnv,theArgument,"<>",&rv1,false,pos))
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   pos++;

   /*=================================================*/
   /* Compare each of the subsequent arguments to the */
   /* first. If any is equal, then return FALSE.      */
   /*=================================================*/

   for (theArgument = GetNextArgument(theArgument);
        theArgument != NULL;
        theArgument = GetNextArgument(theArgument), pos++)
     {
      if (! GetNumericArgument(theEnv,theArgument,"<>",&rv2,false,pos))
        {
         returnValue->value = EnvFalseSymbol(theEnv);
         return;
        }
        
      if (rv1.type == INTEGER)
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToLong(rv1.value) == ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if ((double) ValueToLong(rv1.value) == ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
      else
        {
         if (rv2.type == INTEGER)
           {
            if (ValueToDouble(rv1.value) == (double) ValueToLong(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
         else
           {
            if (ValueToDouble(rv1.value) == ValueToDouble(rv2.value))
              {
               returnValue->value = EnvFalseSymbol(theEnv);
               return;
              }
           }
        }
     }

   /*===================================*/
   /* All arguments were unequal to the */
   /* first argument. Return TRUE.      */
   /*===================================*/

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/**************************************/
/* OddpFunction: H/L access routine   */
/*   for the oddp function.           */
/**************************************/
void OddpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;
   long long num, halfnum;

   returnValue->type = SYMBOL;
      
   if (EnvArgTypeCheck(theEnv,"oddp",1,INTEGER,&item) == false)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   num = DOToLong(item);

   halfnum = (num / 2) * 2;
   if (num == halfnum)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   returnValue->value = EnvTrueSymbol(theEnv);
  }

/***************************************/
/* EvenpFunction: H/L access routine   */
/*   for the evenp function.           */
/***************************************/
void EvenpFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue item;
   long long num, halfnum;

   returnValue->type = SYMBOL;
        
   if (EnvArgTypeCheck(theEnv,"evenp",1,INTEGER,&item) == false)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   num = DOToLong(item);

   halfnum = (num / 2) * 2;
   if (num != halfnum)
     {
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   returnValue->value = EnvTrueSymbol(theEnv);
  }



