   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*               EXTERNAL FUNCTION MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for adding new user or system defined   */
/*   functions.                                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Corrected code to remove run-time program      */
/*            compiler warning.                              */
/*                                                           */
/*      6.30: Added support for passing context information  */ 
/*            to user defined functions.                     */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Changed restrictions from char * to            */
/*            symbolHashNode * to support strings            */
/*            originating from sources that are not          */
/*            statically allocated.                          */
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

#include <ctype.h>
#include <stdlib.h>

#include "argacces.h"
#include "constant.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "router.h"

#include "extnfunc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    AddHashFunction(Environment *,struct FunctionDefinition *);
   static void                    InitializeFunctionHashTable(Environment *);
   static void                    DeallocateExternalFunctionData(Environment *);
#if (! RUN_TIME)
   static bool                    RemoveHashFunction(Environment *,struct FunctionDefinition *);
#endif
   static void                    PrintType(Environment *,const char *,int,int *,const char *);
   static bool                    DefineFunction(Environment *,const char *,unsigned,
                                                 void (*)(Environment *,UDFContext *,CLIPSValue *),
                                                 const char *,int,int,const char *,void *);

/*********************************************************/
/* InitializeExternalFunctionData: Allocates environment */
/*    data for external functions.                       */
/*********************************************************/
void InitializeExternalFunctionData(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,EXTERNAL_FUNCTION_DATA,sizeof(struct externalFunctionData),DeallocateExternalFunctionData);
  }

/***********************************************************/
/* DeallocateExternalFunctionData: Deallocates environment */
/*    data for external functions.                         */
/***********************************************************/
static void DeallocateExternalFunctionData(
  Environment *theEnv)
  {
   struct FunctionHash *fhPtr, *nextFHPtr;
   int i;

#if ! RUN_TIME
   struct FunctionDefinition *tmpPtr, *nextPtr;

   tmpPtr = ExternalFunctionData(theEnv)->ListOfFunctions;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rtn_struct(theEnv,FunctionDefinition,tmpPtr);
      tmpPtr = nextPtr;
     }
#endif

   if (ExternalFunctionData(theEnv)->FunctionHashtable == NULL)
     { return; }
     
   for (i = 0; i < SIZE_FUNCTION_HASH; i++)
     {
      fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[i];
      while (fhPtr != NULL)
        {
         nextFHPtr = fhPtr->next;
         rtn_struct(theEnv,FunctionHash,fhPtr);
         fhPtr = nextFHPtr;
        }
     }
   
   genfree(theEnv,ExternalFunctionData(theEnv)->FunctionHashtable,
           (int) sizeof (struct FunctionHash *) * SIZE_FUNCTION_HASH);
  }

#if (! RUN_TIME)

/*******************************************************/
/* EnvAddUDF: Used to define a system or user external */
/*   function so that the KB can access it.            */
/*******************************************************/
bool EnvAddUDF(
  Environment *theEnv,
  const char *clipsFunctionName,
  const char *returnTypes,
  int minArgs,
  int maxArgs,
  const char *argumentTypes,
  void (*cFunctionPointer)(Environment *,UDFContext *,CLIPSValue *),
  const char *cFunctionName,
  void *context)
  {
   unsigned returnTypeBits;
   
   if (returnTypes != NULL)
     { PopulateRestriction(theEnv,&returnTypeBits,ANY_TYPE,returnTypes,0); }
   else
     { returnTypeBits = ANY_TYPE; }

   return DefineFunction(theEnv,clipsFunctionName,returnTypeBits,cFunctionPointer,
                         cFunctionName,minArgs,maxArgs,argumentTypes,context);
  }

/*************************************************************/
/* DefineFunction: Used to define a system or user external  */
/*   function so that the KB can access it. Allows argument  */
/*   restrictions to be attached to the function.            */
/*************************************************************/
static bool DefineFunction(
  Environment *theEnv,
  const char *name,
  unsigned returnTypeBits,
  void (*pointer)(Environment *,UDFContext *,CLIPSValue *),
  const char *actualName,
  int minArgs,
  int maxArgs,
  const char *restrictions,
  void *context)
  {
   struct FunctionDefinition *newFunction;

   newFunction = FindFunction(theEnv,name);
   if (newFunction != NULL) return false;
   
   newFunction = get_struct(theEnv,FunctionDefinition);
   newFunction->callFunctionName = (SYMBOL_HN *) EnvAddSymbol(theEnv,name);
   IncrementSymbolCount(newFunction->callFunctionName);
   newFunction->next = GetFunctionList(theEnv);
   ExternalFunctionData(theEnv)->ListOfFunctions = newFunction;
   AddHashFunction(theEnv,newFunction);
     
   newFunction->unknownReturnValueType = returnTypeBits;
   newFunction->functionPointer = pointer;
   newFunction->actualFunctionName = actualName;
   
   newFunction->minArgs = minArgs;
   newFunction->maxArgs = maxArgs;
   
   if (restrictions == NULL)
     { newFunction->restrictions = NULL; }
   else
     {
      newFunction->restrictions = EnvAddSymbol(theEnv,restrictions);
      IncrementSymbolCount(newFunction->restrictions);
     }

   newFunction->parser = NULL;
   newFunction->overloadable = true;
   newFunction->sequenceuseok = true;
   newFunction->usrData = NULL;
   newFunction->context = context;

   return true;
  }
  
/********************************************/
/* EnvRemoveUDF: Used to remove a function  */
/*   definition from the list of functions. */
/********************************************/
bool EnvRemoveUDF(
  Environment *theEnv,
  const char *functionName)
  {
   SYMBOL_HN *findValue;
   struct FunctionDefinition *fPtr, *lastPtr = NULL;

   findValue = (SYMBOL_HN *) FindSymbolHN(theEnv,functionName);

   for (fPtr = ExternalFunctionData(theEnv)->ListOfFunctions;
        fPtr != NULL;
        fPtr = fPtr->next)
     {
      if (fPtr->callFunctionName == findValue)
        {
         DecrementSymbolCount(theEnv,fPtr->callFunctionName);
         RemoveHashFunction(theEnv,fPtr);

         if (lastPtr == NULL)
           { ExternalFunctionData(theEnv)->ListOfFunctions = fPtr->next; }
         else
           { lastPtr->next = fPtr->next; }
           
         if (fPtr->restrictions != NULL)
           { DecrementSymbolCount(theEnv,fPtr->restrictions); }
         ClearUserDataList(theEnv,fPtr->usrData);
         rtn_struct(theEnv,FunctionDefinition,fPtr);
         return true;
        }

      lastPtr = fPtr;
     }

   return false;
  }

/******************************************/
/* RemoveHashFunction: Removes a function */
/*   from the function hash table.        */
/******************************************/
static bool RemoveHashFunction(
  Environment *theEnv,
  struct FunctionDefinition *fdPtr)
  {
   struct FunctionHash *fhPtr, *lastPtr = NULL;
   unsigned hashValue;

   hashValue = HashSymbol(ValueToString(fdPtr->callFunctionName),SIZE_FUNCTION_HASH);

   for (fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[hashValue];
        fhPtr != NULL;
        fhPtr = fhPtr->next)
     {
      if (fhPtr->fdPtr == fdPtr)
        {
         if (lastPtr == NULL)
           { ExternalFunctionData(theEnv)->FunctionHashtable[hashValue] = fhPtr->next; }
         else
           { lastPtr->next = fhPtr->next; }

         rtn_struct(theEnv,FunctionHash,fhPtr);
         return true;
        }

      lastPtr = fhPtr;
     }

   return false;
  }

/***************************************************************************/
/* AddFunctionParser: Associates a specialized expression parsing function */
/*   with the function entry for a function which was defined using        */
/*   DefineFunction. When this function is parsed, the specialized parsing */
/*   function will be called to parse the arguments of the function. Only  */
/*   user and system defined functions can have specialized parsing        */
/*   routines. Generic functions and deffunctions can not have specialized */
/*   parsing routines.                                                     */
/***************************************************************************/
bool AddFunctionParser(
  Environment *theEnv,
  const char *functionName,
  struct expr *(*fpPtr)(Environment *,struct expr *,const char *))
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(theEnv,functionName);
   if (fdPtr == NULL)
     {
      EnvPrintRouter(theEnv,WERROR,"Function parsers can only be added for existing functions.\n");
      return false;
     }
   fdPtr->restrictions = NULL;
   fdPtr->parser = fpPtr;
   fdPtr->overloadable = false;

   return true;
  }

/*********************************************************************/
/* RemoveFunctionParser: Removes a specialized expression parsing    */
/*   function (if it exists) from the function entry for a function. */
/*********************************************************************/
bool RemoveFunctionParser(
  Environment *theEnv,
  const char *functionName)
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(theEnv,functionName);
   if (fdPtr == NULL)
     {
      EnvPrintRouter(theEnv,WERROR,"Function parsers can only be removed from existing functions.\n");
      return false;
     }

   fdPtr->parser = NULL;

   return true;
  }

/*****************************************************************/
/* FuncSeqOvlFlags: Makes a system function overloadable or not, */
/* i.e. can the function be a method for a generic function.     */
/*****************************************************************/
bool FuncSeqOvlFlags(
  Environment *theEnv,
  const char *functionName,
  bool seqp,
  bool ovlp)
  {
   struct FunctionDefinition *fdPtr;

   fdPtr = FindFunction(theEnv,functionName);
   if (fdPtr == NULL)
     {
      EnvPrintRouter(theEnv,WERROR,"Only existing functions can be marked as using sequence expansion arguments/overloadable or not.\n");
      return false;
     }
   fdPtr->sequenceuseok = (short) (seqp ? true : false);
   fdPtr->overloadable = (short) (ovlp ? true : false);
   return true;
  }

#endif

/*********************************************************/
/* GetArgumentTypeName: Returns a descriptive string for */
/*   a function argument type (used by DefineFunction2). */
/*********************************************************/
const char *GetArgumentTypeName(
  int theRestriction)
  {
   switch ((char) theRestriction)
     {
      case 'a':
        return("external address");

      case 'e':
        return("instance address, instance name, or symbol");

      case 'd':
      case 'f':
        return("float");

      case 'g':
        return("integer, float, or symbol");

      case 'h':
        return("instance address, instance name, fact address, integer, or symbol");

      case 'j':
        return("symbol, string, or instance name");

      case 'k':
        return("symbol or string");

      case 'i':
      case 'l':
        return("integer");

      case 'm':
        return("multifield");

      case 'n':
        return("integer or float");

      case 'o':
        return("instance name");

      case 'p':
        return("instance name or symbol");

      case 'q':
        return("multifield, symbol, or string");

      case 's':
        return("string");

      case 'w':
        return("symbol");

      case 'x':
        return("instance address");

      case 'y':
        return("fact-address");

      case 'z':
        return("fact-address, integer, or symbol");

      case 'u':
        return("non-void return value");
     }

   return("unknown argument type");
  }

/***************************************************/
/* GetNthRestriction: Returns the restriction type */
/*   for the nth parameter of a function.          */
/***************************************************/
int GetNthRestriction(
  struct FunctionDefinition *theFunction,
  int position)
  {
   int defaultRestriction = (int) 'u';
   size_t theLength;
   int i = 2;

   /*===========================================================*/
   /* If no restrictions at all are specified for the function, */
   /* then return 'u' to indicate that any value is suitable as */
   /* an argument to the function.                              */
   /*===========================================================*/

   if (theFunction == NULL) return(defaultRestriction);

   if (theFunction->restrictions == NULL) return(defaultRestriction);

   /*===========================================================*/
   /* If no type restrictions are specified for the function,   */
   /* then return 'u' to indicate that any value is suitable as */
   /* an argument to the function.                              */
   /*===========================================================*/

   theLength = strlen(theFunction->restrictions->contents);

   if (theLength < 3) return(defaultRestriction);

   /*==============================================*/
   /* Determine the functions default restriction. */
   /*==============================================*/

   defaultRestriction = (int) theFunction->restrictions->contents[i];

   if (defaultRestriction == '*') defaultRestriction = (int) 'u';

   /*=======================================================*/
   /* If the requested position does not have a restriction */
   /* specified, then return the default restriction.       */
   /*=======================================================*/

   if (theLength < (size_t) (position + 3)) return(defaultRestriction);

   /*=========================================================*/
   /* Return the restriction specified for the nth parameter. */
   /*=========================================================*/

   return((int) theFunction->restrictions->contents[position + 2]);
  }

/***************************************************/
/* GetNthRestriction2: Returns the restriction type */
/*   for the nth parameter of a function.          */
/***************************************************/
unsigned GetNthRestriction2(
  Environment *theEnv,
  struct FunctionDefinition *theFunction,
  int position)
  {
   unsigned rv, df;
   const char *restrictions;
   
   if (theFunction == NULL) return(ANY_TYPE);

   if (theFunction->restrictions == NULL) return(ANY_TYPE);
   restrictions = theFunction->restrictions->contents;
   
   PopulateRestriction(theEnv,&df,ANY_TYPE,restrictions,0);
   PopulateRestriction(theEnv,&rv,df,restrictions,position);

   return rv;
  }

/*************************************************/
/* GetFunctionList: Returns the ListOfFunctions. */
/*************************************************/
struct FunctionDefinition *GetFunctionList(
  Environment *theEnv)
  {
   return(ExternalFunctionData(theEnv)->ListOfFunctions);
  }

/**************************************************************/
/* InstallFunctionList: Sets the ListOfFunctions and adds all */
/*   the function entries to the FunctionHashTable.           */
/**************************************************************/
void InstallFunctionList(
  Environment *theEnv,
  struct FunctionDefinition *value)
  {
   int i;
   struct FunctionHash *fhPtr, *nextPtr;

   if (ExternalFunctionData(theEnv)->FunctionHashtable != NULL)
     {
      for (i = 0; i < SIZE_FUNCTION_HASH; i++)
        {
         fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[i];
         while (fhPtr != NULL)
           {
            nextPtr = fhPtr->next;
            rtn_struct(theEnv,FunctionHash,fhPtr);
            fhPtr = nextPtr;
           }
         ExternalFunctionData(theEnv)->FunctionHashtable[i] = NULL;
        }
     }

   ExternalFunctionData(theEnv)->ListOfFunctions = value;

   while (value != NULL)
     {
      AddHashFunction(theEnv,value);
      value = value->next;
     }
  }

/********************************************************/
/* FindFunction: Returns a pointer to the corresponding */
/*   FunctionDefinition structure if a function name is */
/*   in the function list, otherwise returns NULL.      */
/********************************************************/
struct FunctionDefinition *FindFunction(
  Environment *theEnv,
  const char *functionName)
  {
   struct FunctionHash *fhPtr;
   unsigned hashValue;
   SYMBOL_HN *findValue;

   if (ExternalFunctionData(theEnv)->FunctionHashtable == NULL) return NULL;
   
   hashValue = HashSymbol(functionName,SIZE_FUNCTION_HASH);

   findValue = (SYMBOL_HN *) FindSymbolHN(theEnv,functionName);

   for (fhPtr = ExternalFunctionData(theEnv)->FunctionHashtable[hashValue];
        fhPtr != NULL;
        fhPtr = fhPtr->next)
     {
      if (fhPtr->fdPtr->callFunctionName == findValue)
        { return(fhPtr->fdPtr); }
     }

   return NULL;
  }

/*********************************************************/
/* InitializeFunctionHashTable: Purpose is to initialize */
/*   the function hash table to NULL.                    */
/*********************************************************/
static void InitializeFunctionHashTable(
  Environment *theEnv)
  {
   int i;

   ExternalFunctionData(theEnv)->FunctionHashtable = (struct FunctionHash **)
                       gm2(theEnv,(int) sizeof (struct FunctionHash *) *
                           SIZE_FUNCTION_HASH);

   for (i = 0; i < SIZE_FUNCTION_HASH; i++) ExternalFunctionData(theEnv)->FunctionHashtable[i] = NULL;
  }

/****************************************************************/
/* AddHashFunction: Adds a function to the function hash table. */
/****************************************************************/
static void AddHashFunction(
  Environment *theEnv,
  struct FunctionDefinition *fdPtr)
  {
   struct FunctionHash *newhash, *temp;
   unsigned hashValue;

   if (ExternalFunctionData(theEnv)->FunctionHashtable == NULL) InitializeFunctionHashTable(theEnv);

   newhash = get_struct(theEnv,FunctionHash);
   newhash->fdPtr = fdPtr;

   hashValue = HashSymbol(fdPtr->callFunctionName->contents,SIZE_FUNCTION_HASH);

   temp = ExternalFunctionData(theEnv)->FunctionHashtable[hashValue];
   ExternalFunctionData(theEnv)->FunctionHashtable[hashValue] = newhash;
   newhash->next = temp;
  }

/*************************************************/
/* GetMinimumArgs: Returns the minimum number of */
/*   arguments expected by an external function. */
/*************************************************/
int GetMinimumArgs(
  struct FunctionDefinition *theFunction)
  {
   return theFunction->minArgs;
  }
  
/*************************************************/
/* GetMaximumArgs: Returns the maximum number of */
/*   arguments expected by an external function. */
/*************************************************/
int GetMaximumArgs(
  struct FunctionDefinition *theFunction)
  {
   return theFunction->maxArgs;
  }

/**************/
/* PrintType: */
/**************/
static void PrintType(
  Environment *theEnv,
  const char *logicalName,
  int typeCount,
  int *typesPrinted,
  const char *typeName)
  {
   if (*typesPrinted == 0)
     {
      EnvPrintRouter(theEnv,logicalName,typeName);
      (*typesPrinted)++;
      return;
     }

   if (typeCount == 2)
     { EnvPrintRouter(theEnv,logicalName," or "); }
   else if (((*typesPrinted) + 1) == typeCount)
     { EnvPrintRouter(theEnv,logicalName,", or "); }
   else
     { EnvPrintRouter(theEnv,logicalName,", "); }
     
   EnvPrintRouter(theEnv,logicalName,typeName);
   (*typesPrinted)++;
  }

/********************/
/* PrintTypesString */
/********************/
void PrintTypesString(
  Environment *theEnv,
  const char *logicalName,
  unsigned expectedType,
  bool printCRLF)
  {
   int typeCount, typesPrinted;

   typeCount = 0;
   if (expectedType & INTEGER_TYPE) typeCount++;
   if (expectedType & FLOAT_TYPE) typeCount++;
   if (expectedType & SYMBOL_TYPE) typeCount++;
   if (expectedType & STRING_TYPE) typeCount++;
   if (expectedType & INSTANCE_NAME_TYPE) typeCount++;
   if (expectedType & INSTANCE_ADDRESS_TYPE) typeCount++;
   if (expectedType & FACT_ADDRESS_TYPE) typeCount++;
   if (expectedType & EXTERNAL_ADDRESS_TYPE) typeCount++;
   if (expectedType & MULTIFIELD_TYPE) typeCount++;
   
   typesPrinted = 0;
   if (expectedType & INTEGER_TYPE)
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"integer"); }
  
    if (expectedType & FLOAT_TYPE)
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"float"); }

   if (expectedType & SYMBOL_TYPE)
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"symbol"); }

   if (expectedType & STRING_TYPE)
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"string"); }

   if (expectedType & INSTANCE_NAME_TYPE) 
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"instance name"); }

   if (expectedType & INSTANCE_ADDRESS_TYPE) 
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"instance address"); }

   if (expectedType & FACT_ADDRESS_TYPE) 
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"fact address"); }

   if (expectedType & EXTERNAL_ADDRESS_TYPE) 
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"external address"); }

   if (expectedType & MULTIFIELD_TYPE) 
     { PrintType(theEnv,logicalName,typeCount,&typesPrinted,"multifield"); }
   
   if (printCRLF)
     { EnvPrintRouter(theEnv,logicalName,"\n"); }
  }
