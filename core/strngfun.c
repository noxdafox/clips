   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*               STRING FUNCTIONS MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several string functions   */
/*   including str-cat, sym-cat, str-length, str-compare,    */
/*   upcase, lowcase, sub-string, str-index, eval, and       */
/*   build.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Barry Cameron                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added support for UTF-8 strings to str-length, */
/*            str-index, and sub-string functions.           */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Added code to keep track of pointers to        */
/*            constructs that are contained externally to    */
/*            to constructs, DanglingConstructs.             */
/*                                                           */
/*            Fixed str-cat bug that could be invoked by     */
/*            (funcall str-cat).                             */
/*                                                           */
/*      6.40: Prior error flags are cleared before EnvEval   */
/*            and EnvBuild are processed.                    */
/*                                                           */
/*            Added Env prefix to GetEvaluationError and     */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if STRING_FUNCTIONS

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include "argacces.h"
#include "commline.h"
#include "constrct.h"
#include "cstrcpsr.h"
#include "engine.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "extnfunc.h"
#include "memalloc.h"
#include "prcdrpsr.h"
#include "router.h"
#include "strngrtr.h"
#include "scanner.h"
#include "sysdep.h"

#if DEFRULE_CONSTRUCT
#include "drive.h"
#endif

#include "strngfun.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    StrOrSymCatFunction(UDFContext *,CLIPSValue *,unsigned short);

/******************************************/
/* StringFunctionDefinitions: Initializes */
/*   the string manipulation functions.   */
/******************************************/
void StringFunctionDefinitions(
  Environment *theEnv)
  {
#if ! RUN_TIME
   EnvAddUDF(theEnv,"str-cat","sy",1,UNBOUNDED,"synld" ,StrCatFunction,"StrCatFunction",NULL);
   EnvAddUDF(theEnv,"sym-cat","sy",1,UNBOUNDED,"synld" ,SymCatFunction,"SymCatFunction",NULL);
   EnvAddUDF(theEnv,"str-length","l",1,1,"syn",StrLengthFunction,"StrLengthFunction",NULL);
   EnvAddUDF(theEnv,"str-compare","l",2,3,"*;syn;syn;l" ,StrCompareFunction,"StrCompareFunction",NULL);
   EnvAddUDF(theEnv,"upcase","syn",1,1,"syn",UpcaseFunction,"UpcaseFunction",NULL);
   EnvAddUDF(theEnv,"lowcase","syn",1,1,"syn",LowcaseFunction,"LowcaseFunction",NULL);
   EnvAddUDF(theEnv,"sub-string","s",3,3,"*;l;l;syn",SubStringFunction,"SubStringFunction",NULL);
   EnvAddUDF(theEnv,"str-index","bl",2,2,"syn",StrIndexFunction,"StrIndexFunction",NULL);
   EnvAddUDF(theEnv,"eval","*",1,1,"sy",EvalFunction,"EvalFunction",NULL);
   EnvAddUDF(theEnv,"build","b",1,1,"sy",BuildFunction,"BuildFunction",NULL);
   EnvAddUDF(theEnv,"string-to-field","*",1,1,"syn",StringToFieldFunction,"StringToFieldFunction",NULL);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/****************************************/
/* StrCatFunction: H/L access routine   */
/*   for the str-cat function.          */
/****************************************/
void StrCatFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {   
   StrOrSymCatFunction(context,returnValue,STRING);
  }

/****************************************/
/* SymCatFunction: H/L access routine   */
/*   for the sym-cat function.          */
/****************************************/
void SymCatFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   StrOrSymCatFunction(context,returnValue,SYMBOL);
  }

/********************************************************/
/* StrOrSymCatFunction: Driver routine for implementing */
/*   the str-cat and sym-cat functions.                 */
/********************************************************/
static void StrOrSymCatFunction(
  UDFContext *context,
  CLIPSValue *returnValue,
  unsigned short returnType)
  {
   CLIPSValue theArg;
   int numArgs, i, total, j;
   char *theString;
   SYMBOL_HN **arrayOfStrings;
   SYMBOL_HN *hashPtr;
   Environment *theEnv = context->environment;

   /*===============================================*/
   /* Determine the number of arguments as create a */
   /* string array which is large enough to store   */
   /* the string representation of each argument.   */
   /*===============================================*/

   numArgs = UDFArgumentCount(context);
   if (numArgs == 0) return;
   
   arrayOfStrings = (SYMBOL_HN **) gm1(theEnv,(int) sizeof(SYMBOL_HN *) * numArgs);
   for (i = 0; i < numArgs; i++)   
     { arrayOfStrings[i] = NULL; }

   /*=============================================*/
   /* Evaluate each argument and store its string */
   /* representation in the string array.         */
   /*=============================================*/

   total = 1;
   for (i = 1 ; i <= numArgs ; i++)
     {
      UDFNthArgument(context,i,ANY_TYPE,&theArg);

      switch(GetType(theArg))
        {
         case STRING:
#if OBJECT_SYSTEM
         case INSTANCE_NAME:
#endif
         case SYMBOL:
           hashPtr = (SYMBOL_HN *) GetValue(theArg);
           arrayOfStrings[i-1] = hashPtr;
           IncrementSymbolCount(hashPtr); 
           break;

         case FLOAT:
           hashPtr = (SYMBOL_HN *) EnvAddSymbol(theEnv,FloatToString(theEnv,ValueToDouble(GetValue(theArg))));
           arrayOfStrings[i-1] = hashPtr;
           IncrementSymbolCount(hashPtr);
           break;

         case INTEGER:
           hashPtr = (SYMBOL_HN *) EnvAddSymbol(theEnv,LongIntegerToString(theEnv,ValueToLong(GetValue(theArg))));
           arrayOfStrings[i-1] = hashPtr;
           IncrementSymbolCount(hashPtr);
           break;

         default:
           UDFInvalidArgumentMessage(context,"string, instance name, symbol, float, or integer");
           EnvSetEvaluationError(theEnv,true);
           break;
        }

      if (EvaluationData(theEnv)->EvaluationError)
        {
         for (i = 0; i < numArgs; i++)
           {
            if (arrayOfStrings[i] != NULL)
              { DecrementSymbolCount(theEnv,arrayOfStrings[i]); }
           }

         rm(theEnv,arrayOfStrings,sizeof(SYMBOL_HN *) * numArgs);
         
         SetpType(returnValue,returnType);
         if (returnType == STRING)
           { SetpValue(returnValue,EnvAddSymbol(theEnv,"")); }
         else
           { SetpValue(returnValue,EnvAddSymbol(theEnv,"nil")); }
         return;
        }

      total += (int) strlen(ValueToString(arrayOfStrings[i - 1]));
     }

   /*=========================================================*/
   /* Allocate the memory to store the concatenated string or */
   /* symbol, then copy the values in the string array to the */
   /* memory just allocated.                                  */
   /*=========================================================*/

   theString = (char *) gm2(theEnv,(sizeof(char) * total));

   j = 0;
   for (i = 0 ; i < numArgs ; i++)
     {
      gensprintf(&theString[j],"%s",ValueToString(arrayOfStrings[i]));
      j += (int) strlen(ValueToString(arrayOfStrings[i]));
     }

   /*=========================================*/
   /* Return the concatenated value and clean */
   /* up the temporary memory used.           */
   /*=========================================*/

   SetpType(returnValue,returnType);
   SetpValue(returnValue,EnvAddSymbol(theEnv,theString));
   rm(theEnv,theString,sizeof(char) * total);

   for (i = 0; i < numArgs; i++)
     {
      if (arrayOfStrings[i] != NULL)
        { DecrementSymbolCount(theEnv,arrayOfStrings[i]); }
     }

   rm(theEnv,arrayOfStrings,sizeof(SYMBOL_HN *) * numArgs);
  }

/*******************************************/
/* StrLengthFunction: H/L access routine   */
/*   for the str-length function.          */
/*******************************************/
void StrLengthFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;

   /*==================================================================*/
   /* The argument should be of type symbol, string, or instance name. */
   /*==================================================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&theArg))
     { return; }

   /*============================================*/
   /* Return the length of the string or symbol. */
   /*============================================*/
   
   returnValue->type = INTEGER;
   returnValue->value = EnvAddLong(theEnv,UTF8Length(DOToString(theArg)));
  }

/****************************************/
/* UpcaseFunction: H/L access routine   */
/*   for the upcase function.           */
/****************************************/
void UpcaseFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;
   unsigned i;
   size_t slen;
   const char *osptr;
   char *nsptr;

   /*==================================================*/
   /* The argument should be of type symbol or string. */
   /*==================================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&theArg))
     { return; }

   /*======================================================*/
   /* Allocate temporary memory and then copy the original */
   /* string or symbol to that memory, while uppercasing   */
   /* lower case alphabetic characters.                    */
   /*======================================================*/

   osptr = DOToString(theArg);
   slen = strlen(osptr) + 1;
   nsptr = (char *) gm2(theEnv,slen);

   for (i = 0  ; i < slen ; i++)
     {
      if (islower(osptr[i]))
        { nsptr[i] = (char) toupper(osptr[i]); }
      else
        { nsptr[i] = osptr[i]; }
     }

   /*========================================*/
   /* Return the uppercased string and clean */
   /* up the temporary memory used.          */
   /*========================================*/

   SetpType(returnValue,GetType(theArg));
   SetpValue(returnValue,EnvAddSymbol(theEnv,nsptr));
   rm(theEnv,nsptr,slen);
  }

/*****************************************/
/* LowcaseFunction: H/L access routine   */
/*   for the lowcase function.           */
/*****************************************/
void LowcaseFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;
   unsigned i;
   size_t slen;
   const char *osptr;
   char *nsptr;

   /*==================================================*/
   /* The argument should be of type symbol or string. */
   /*==================================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&theArg))
     { return; }

   /*======================================================*/
   /* Allocate temporary memory and then copy the original */
   /* string or symbol to that memory, while lowercasing   */
   /* upper case alphabetic characters.                    */
   /*======================================================*/

   osptr = DOToString(theArg);
   slen = strlen(osptr) + 1;
   nsptr = (char *) gm2(theEnv,slen);

   for (i = 0  ; i < slen ; i++)
     {
      if (isupper(osptr[i]))
        { nsptr[i] = (char) tolower(osptr[i]); }
      else
        { nsptr[i] = osptr[i]; }
     }

   /*========================================*/
   /* Return the lowercased string and clean */
   /* up the temporary memory used.          */
   /*========================================*/

   SetpType(returnValue,GetType(theArg));
   SetpValue(returnValue,EnvAddSymbol(theEnv,nsptr));
   rm(theEnv,nsptr,slen);
  }

/********************************************/
/* StrCompareFunction: H/L access routine   */
/*   for the str-compare function.          */
/********************************************/
void StrCompareFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue arg1, arg2, arg3;
   int compareResult;

   /*=============================================================*/
   /* The first two arguments should be of type symbol or string. */
   /*=============================================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&arg1))
     { return; }

   if (! UDFNextArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&arg2))
     { return; }

   /*===================================================*/
   /* Compare the strings. Use the 3rd argument for the */
   /* maximum length of comparison, if it is provided.  */
   /*===================================================*/

   if (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,INTEGER_TYPE,&arg3))
        { return; }

      compareResult = strncmp(DOToString(arg1),DOToString(arg2),
                             (STD_SIZE) DOToInteger(arg3));
     }
   else
     { compareResult = strcmp(DOToString(arg1),DOToString(arg2)); }

   /*========================================================*/
   /* Return Values are as follows:                          */
   /* -1 is returned if <string-1> is less than <string-2>.  */
   /*  1 is return if <string-1> is greater than <string-2>. */
   /*  0 is returned if <string-1> is equal to <string-2>.   */
   /*========================================================*/

   returnValue->type = INTEGER;
   if (compareResult < 0)
     { returnValue->value = EnvAddLong(theEnv,-1); }
   else if (compareResult > 0)
     { returnValue->value = EnvAddLong(theEnv,1); }
   else
     { returnValue->value = EnvAddLong(theEnv,0); }
  }

/*******************************************/
/* SubStringFunction: H/L access routine   */
/*   for the sub-string function.          */
/*******************************************/
void SubStringFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;
   const char *tempString;
   char *returnString;
   size_t start, end, i, j, length;

   returnValue->type = STRING;
   
   /*===================================*/
   /* Check and retrieve the arguments. */
   /*===================================*/

   if (! UDFFirstArgument(context,INTEGER_TYPE,&theArg))
     { return; }

   if (CoerceToLongInteger(theArg.type,theArg.value) < 1)
     { start = 0; }
   else
     { start = (size_t) CoerceToLongInteger(theArg.type,theArg.value) - 1; }

   if (! UDFNextArgument(context,INTEGER_TYPE,&theArg))
     { return; }

   if (CoerceToLongInteger(theArg.type,theArg.value) < 1)
     {
      returnValue->value = EnvAddSymbol(theEnv,"");
      return;
     }
   else
     { end = (size_t) CoerceToLongInteger(theArg.type,theArg.value) - 1; }

   if (! UDFNextArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&theArg))
     { return; }
   
   tempString = DOToString(theArg);
   
   /*================================================*/
   /* If parameters are out of range return an error */
   /*================================================*/
   
   length = UTF8Length(tempString);
   
   if (end > length)
     { end = length; }

   /*==================================*/
   /* If the start is greater than the */
   /* end, return a null string.       */
   /*==================================*/

   if ((start > end) || (length == 0))
     {
      returnValue->value = EnvAddSymbol(theEnv,"");
      return;
     }

   /*=============================================*/
   /* Otherwise, allocate the string and copy the */
   /* designated portion of the old string to the */
   /* new string.                                 */
   /*=============================================*/

   else
     {
      start = UTF8Offset(tempString,start);
      end = UTF8Offset(tempString,end + 1) - 1;
      
      returnString = (char *) gm2(theEnv,(unsigned) (end - start + 2));  /* (end - start) inclusive + EOS */
      for(j=0, i=start;i <= end; i++, j++)
        { *(returnString+j) = *(tempString+i); }
      *(returnString+j) = '\0';
     }

   /*========================*/
   /* Return the new string. */
   /*========================*/

   returnValue->value = EnvAddSymbol(theEnv,returnString);
   rm(theEnv,returnString,(unsigned) (end - start + 2));
  }

/******************************************/
/* StrIndexFunction: H/L access routine   */
/*   for the sub-index function.          */
/******************************************/
void StrIndexFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg1, theArg2;
   const char *strg1, *strg2, *strg3;
   size_t i, j;

   returnValue->type = SYMBOL;
   returnValue->value = EnvFalseSymbol(theEnv);

   /*===================================*/
   /* Check and retrieve the arguments. */
   /*===================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&theArg1))
     { return; }

   if (! UDFNextArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&theArg2))
     { return; }

   strg1 = DOToString(theArg1);
   strg2 = DOToString(theArg2);

   /*=================================*/
   /* Find the position in string2 of */
   /* string1 (counting from 1).      */
   /*=================================*/

   if (strlen(strg1) == 0)
     {
      returnValue->type = INTEGER;
      returnValue->value = EnvAddLong(theEnv,(long long) UTF8Length(strg2) + 1LL);
      return;
     }
     
   strg3 = strg2;
   for (i=1; *strg2; i++, strg2++)
     {
      for (j=0; *(strg1+j) && *(strg1+j) == *(strg2+j); j++)
        { /* Do Nothing */ }

      if (*(strg1+j) == '\0')
        {
         returnValue->type = INTEGER;
         returnValue->value = EnvAddLong(theEnv,(long long) UTF8CharNum(strg3,i));
         return;
        }
     }

   return;
  }

/********************************************/
/* StringToFieldFunction: H/L access routine */
/*   for the string-to-field function.       */
/********************************************/
void StringToFieldFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;

   /*==================================================*/
   /* The argument should be of type symbol or string. */
   /*==================================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES | INSTANCE_NAME_TYPE,&theArg))
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** ERROR ***");
      return;
     }

   /*================================*/
   /* Convert the string to an atom. */
   /*================================*/

   StringToField(theEnv,DOToString(theArg),returnValue);
  }

/*************************************************************/
/* StringToField: Converts a string to an atomic data value. */
/*************************************************************/
void StringToField(
  Environment *theEnv,
  const char *theString,
  CLIPSValue *returnValue)
  {
   struct token theToken;

   /*====================================*/
   /* Open the string as an input source */
   /* and retrieve the first value.      */
   /*====================================*/

   OpenStringSource(theEnv,"string-to-field-str",theString,0);
   GetToken(theEnv,"string-to-field-str",&theToken);
   CloseStringSource(theEnv,"string-to-field-str");

   /*====================================================*/
   /* Copy the token to the return value data structure. */
   /*====================================================*/

   returnValue->type = theToken.type;
   if ((theToken.type == FLOAT) || (theToken.type == STRING) ||
#if OBJECT_SYSTEM
       (theToken.type == INSTANCE_NAME) ||
#endif
       (theToken.type == SYMBOL) || (theToken.type == INTEGER))
     { returnValue->value = theToken.value; }
   else if (theToken.type == STOP)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvAddSymbol(theEnv,"EOF");
     }
   else if (theToken.type == UNKNOWN_VALUE)
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,"*** ERROR ***");
     }
   else
     {
      returnValue->type = STRING;
      returnValue->value = EnvAddSymbol(theEnv,theToken.printForm);
     }
  }
  
#if (! RUN_TIME) && (! BLOAD_ONLY)

/**************************************/
/* EvalFunction: H/L access routine   */
/*   for the eval function.           */
/**************************************/
void EvalFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;

   /*==================================================*/
   /* The argument should be of type SYMBOL or STRING. */
   /*==================================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES,&theArg))
     { return; }

   /*======================*/
   /* Evaluate the string. */
   /*======================*/

   EnvEval(theEnv,DOToString(theArg),returnValue);
  }
  
/*****************************/
/* EnvEval: C access routine */
/*   for the eval function.  */
/*****************************/
bool EnvEval(
  Environment *theEnv,
  const char *theString,
  CLIPSValue *returnValue)
  {
   struct expr *top;
   bool ov;
   static int depth = 0;
   char logicalNameBuffer[20];
   struct BindInfo *oldBinds;
   int danglingConstructs;

   /*=====================================*/
   /* If embedded, clear the error flags. */
   /*=====================================*/
   
   if ((! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL))
     {
      EnvSetEvaluationError(theEnv,false);
      EnvSetHaltExecution(theEnv,false);
     }

   /*======================================================*/
   /* Evaluate the string. Create a different logical name */
   /* for use each time the eval function is called.       */
   /*======================================================*/

   depth++;
   gensprintf(logicalNameBuffer,"Eval-%d",depth);
   if (OpenStringSource(theEnv,logicalNameBuffer,theString,0) == 0)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      depth--;
      return false;
     }

   /*================================================*/
   /* Save the current parsing state before routines */
   /* are called to parse the eval string.           */
   /*================================================*/

   ov = GetPPBufferStatus(theEnv);
   SetPPBufferStatus(theEnv,false);
   oldBinds = GetParsedBindNames(theEnv);
   SetParsedBindNames(theEnv,NULL);
   danglingConstructs = ConstructData(theEnv)->DanglingConstructs;

   /*========================================================*/
   /* Parse the string argument passed to the eval function. */
   /*========================================================*/

   top = ParseAtomOrExpression(theEnv,logicalNameBuffer,NULL);

   /*============================*/
   /* Restore the parsing state. */
   /*============================*/

   SetPPBufferStatus(theEnv,ov);
   ClearParsedBindNames(theEnv);
   SetParsedBindNames(theEnv,oldBinds);

   /*===========================================*/
   /* Return if an error occured while parsing. */
   /*===========================================*/

   if (top == NULL)
     {
      EnvSetEvaluationError(theEnv,true);
      CloseStringSource(theEnv,logicalNameBuffer);
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      depth--;
      ConstructData(theEnv)->DanglingConstructs = danglingConstructs;
      return false;
     }

   /*==============================================*/
   /* The sequence expansion operator must be used */
   /* within the argument list of a function call. */
   /*==============================================*/

   if ((top->type == MF_GBL_VARIABLE) || (top->type == MF_VARIABLE))
     {
      PrintErrorID(theEnv,"MISCFUN",1,false);
      EnvPrintRouter(theEnv,WERROR,"expand$ must be used in the argument list of a function call.\n");
      EnvSetEvaluationError(theEnv,true);
      CloseStringSource(theEnv,logicalNameBuffer);
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      ReturnExpression(theEnv,top);
      depth--;
      ConstructData(theEnv)->DanglingConstructs = danglingConstructs;
      return false;
     }

   /*=======================================*/
   /* The expression to be evaluated cannot */
   /* contain any local variables.          */
   /*=======================================*/

   if (ExpressionContainsVariables(top,false))
     {
      PrintErrorID(theEnv,"STRNGFUN",2,false);
      EnvPrintRouter(theEnv,WERROR,"Some variables could not be accessed by the eval function.\n");
      EnvSetEvaluationError(theEnv,true);
      CloseStringSource(theEnv,logicalNameBuffer);
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      ReturnExpression(theEnv,top);
      depth--;
      ConstructData(theEnv)->DanglingConstructs = danglingConstructs;
      return false;
     }

   /*====================================*/
   /* Evaluate the expression and return */
   /* the memory used to parse it.       */
   /*====================================*/

   ExpressionInstall(theEnv,top);
   EvaluateExpression(theEnv,top,returnValue);
   ExpressionDeinstall(theEnv,top);

   depth--;
   ReturnExpression(theEnv,top);
   CloseStringSource(theEnv,logicalNameBuffer);

   /*==============================================*/
   /* If embedded, reset dangling construct count. */
   /*==============================================*/
   
   if ((! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL))
     { ConstructData(theEnv)->DanglingConstructs = danglingConstructs; }

   /*==========================================*/
   /* Perform periodic cleanup if the eval was */
   /* issued from an embedded controller.      */
   /*==========================================*/

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     { 
      CleanCurrentGarbageFrame(theEnv,returnValue);
      CallPeriodicTasks(theEnv);
     }

   if (EnvGetEvaluationError(theEnv)) return false;
   return true;
  }

#else

/*************************************************/
/* EvalFunction: This is the non-functional stub */
/*   provided for use with a run-time version.   */
/*************************************************/
void EvalFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   PrintErrorID(theEnv,"STRNGFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Function eval does not work in run time modules.\n");
   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,EnvFalseSymbol(theEnv));
  }

/*****************************************************/
/* EnvEval: This is the non-functional stub provided */
/*   for use with a run-time version.                */
/*****************************************************/
bool EnvEval(
  Environment *theEnv,
  const char *theString,
  CLIPSValue *returnValue)
  {
   PrintErrorID(theEnv,"STRNGFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Function eval does not work in run time modules.\n");
   SetpType(returnValue,SYMBOL);
   SetpValue(returnValue,EnvFalseSymbol(theEnv));
   return false;
  }

#endif

#if (! RUN_TIME) && (! BLOAD_ONLY)
/***************************************/
/* BuildFunction: H/L access routine   */
/*   for the build function.           */
/***************************************/
void BuildFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;

   returnValue->type = SYMBOL;
   
   /*==================================================*/
   /* The argument should be of type SYMBOL or STRING. */
   /*==================================================*/

   if (! UDFFirstArgument(context,LEXEME_TYPES,&theArg))
     { return; }

   /*======================*/
   /* Build the construct. */
   /*======================*/

   if (EnvBuild(theEnv,DOToString(theArg)))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }
  
/******************************/
/* EnvBuild: C access routine */
/*   for the build function.  */
/******************************/
bool EnvBuild(
  Environment *theEnv,
  const char *theString)
  {
   const char *constructType;
   struct token theToken;
   int errorFlag;
   
   /*=====================================*/
   /* If embedded, clear the error flags. */
   /*=====================================*/
   
   if ((! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL))
     {
      EnvSetEvaluationError(theEnv,false);
      EnvSetHaltExecution(theEnv,false);
     }

   /*====================================================*/
   /* No additions during defrule join network activity. */
   /*====================================================*/

#if DEFRULE_CONSTRUCT
   if (EngineData(theEnv)->JoinOperationInProgress) return false;
#endif

   /*===========================================*/
   /* Create a string source router so that the */
   /* string can be used as an input source.    */
   /*===========================================*/

   if (OpenStringSource(theEnv,"build",theString,0) == 0)
     { return false; }

   /*================================*/
   /* The first token of a construct */
   /* must be a left parenthesis.    */
   /*================================*/

   GetToken(theEnv,"build",&theToken);

   if (theToken.type != LPAREN)
     {
      CloseStringSource(theEnv,"build");
      return false;
     }

   /*==============================================*/
   /* The next token should be the construct type. */
   /*==============================================*/

   GetToken(theEnv,"build",&theToken);
   if (theToken.type != SYMBOL)
     {
      CloseStringSource(theEnv,"build");
      return false;
     }

   constructType = ValueToString(theToken.value);

   /*======================*/
   /* Parse the construct. */
   /*======================*/
   
   errorFlag = ParseConstruct(theEnv,constructType,"build");

   /*=================================*/
   /* Close the string source router. */
   /*=================================*/

   CloseStringSource(theEnv,"build");

   /*=========================================*/
   /* If an error occured while parsing the   */
   /* construct, then print an error message. */
   /*=========================================*/

   if (errorFlag == 1)
     {
      EnvPrintRouter(theEnv,WERROR,"\nERROR:\n");
      PrintInChunks(theEnv,WERROR,GetPPBuffer(theEnv));
      EnvPrintRouter(theEnv,WERROR,"\n");
     }

   DestroyPPBuffer(theEnv);

   /*===========================================*/
   /* Perform periodic cleanup if the build was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     {
      CleanCurrentGarbageFrame(theEnv,NULL);
      CallPeriodicTasks(theEnv);
     }

   /*===============================================*/
   /* Return true if the construct was successfully */
   /* parsed, otherwise return false.               */
   /*===============================================*/

   if (errorFlag == 0) return true;

   return false;
  }
#else
/**************************************************/
/* BuildFunction: This is the non-functional stub */
/*   provided for use with a run-time version.    */
/**************************************************/
void BuildFunction(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   PrintErrorID(theEnv,"STRNGFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Function build does not work in run time modules.\n");
   returnValue->type = SYMBOL;
   returnValue->value = EnvFalseSymbol(theEnv);
  }

/******************************************************/
/* EnvBuild: This is the non-functional stub provided */
/*   for use with a run-time version.                 */
/******************************************************/
bool EnvBuild(
  Environment *theEnv,
  const char *theString)
  { 
   PrintErrorID(theEnv,"STRNGFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Function build does not work in run time modules.\n");
   return false;
  }
#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* STRING_FUNCTIONS */
