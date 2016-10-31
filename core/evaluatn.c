   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/01/16             */
   /*                                                     */
   /*                  EVALUATION MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for evaluating expressions.    */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EvaluateAndStoreInDataObject function.   */
/*                                                           */
/*      6.30: Added support for passing context information  */
/*            to user defined functions.                     */
/*                                                           */
/*            Added support for external address hash table  */
/*            and subtyping.                                 */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for DATA_OBJECT_ARRAY primitive.       */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "setup.h"

#include "argacces.h"
#include "commline.h"
#include "constant.h"
#include "envrnmnt.h"
#include "factmngr.h"
#include "memalloc.h"
#include "router.h"
#include "prcdrfun.h"
#include "multifld.h"
#include "prntutil.h"
#include "exprnpsr.h"
#include "utility.h"
#include "proflfun.h"
#include "sysdep.h"

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#endif

#if OBJECT_SYSTEM
#include "object.h"
#include "inscom.h"
#endif

#include "evaluatn.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DeallocateEvaluationData(Environment *);
   static void                    PrintCAddress(Environment *,const char *,void *);
   static void                    NewCAddress(UDFContext *,UDFValue *);
   /*
   static bool                    DiscardCAddress(void *,void *);
   */

/**************************************************/
/* InitializeEvaluationData: Allocates environment */
/*    data for expression evaluation.             */
/**************************************************/
void InitializeEvaluationData(
  Environment *theEnv)
  {
   struct externalAddressType cPointer = { "C", PrintCAddress, PrintCAddress, NULL, NewCAddress, NULL };

   AllocateEnvironmentData(theEnv,EVALUATION_DATA,sizeof(struct evaluationData),DeallocateEvaluationData);

   InstallExternalAddressType(theEnv,&cPointer);
  }

/*****************************************************/
/* DeallocateEvaluationData: Deallocates environment */
/*    data for evaluation data.                      */
/*****************************************************/
static void DeallocateEvaluationData(
  Environment *theEnv)
  {
   int i;

   for (i = 0; i < EvaluationData(theEnv)->numberOfAddressTypes; i++)
     { rtn_struct(theEnv,externalAddressType,EvaluationData(theEnv)->ExternalAddressTypes[i]); }
  }

/**************************************************************/
/* EvaluateExpression: Evaluates an expression. Returns false */
/*   if no errors occurred during evaluation, otherwise true. */
/**************************************************************/
bool EvaluateExpression(
  Environment *theEnv,
  struct expr *problem,
  UDFValue *returnValue)
  {
   struct expr *oldArgument;
   void *oldContext;
   struct functionDefinition *fptr;
   UDFContext theUDFContext;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   returnValue->voidValue = VoidConstant(theEnv);
   returnValue->range = -1;

   if (problem == NULL)
     {
      returnValue->value = FalseSymbol(theEnv);
      return(EvaluationData(theEnv)->EvaluationError);
     }

   switch (problem->type)
     {
      case STRING_TYPE:
      case SYMBOL_TYPE:
      case FLOAT_TYPE:
      case INTEGER_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
      case INSTANCE_ADDRESS_TYPE:
#endif
      case FACT_ADDRESS_TYPE:
      case EXTERNAL_ADDRESS_TYPE:
        returnValue->value = problem->value;
        break;

      case FCALL:
        {
         fptr = problem->functionValue;
         oldContext = SetEnvironmentFunctionContext(theEnv,fptr->context);

#if PROFILING_FUNCTIONS
         StartProfile(theEnv,&profileFrame,
                      &fptr->usrData,
                      ProfileFunctionData(theEnv)->ProfileUserFunctions);
#endif

         oldArgument = EvaluationData(theEnv)->CurrentExpression;
         EvaluationData(theEnv)->CurrentExpression = problem;

         theUDFContext.environment = theEnv;
         theUDFContext.theFunction = fptr;
         theUDFContext.lastArg = problem->argList;
         theUDFContext.lastPosition = 1;
         theUDFContext.returnValue = returnValue;
         fptr->functionPointer(theEnv,&theUDFContext,returnValue);

#if PROFILING_FUNCTIONS
        EndProfile(theEnv,&profileFrame);
#endif

        SetEnvironmentFunctionContext(theEnv,oldContext);
        EvaluationData(theEnv)->CurrentExpression = oldArgument;
        break;
        }

     case MULTIFIELD_TYPE:
        returnValue->value = ((UDFValue *) (problem->value))->value;
        returnValue->begin = ((UDFValue *) (problem->value))->begin;
        returnValue->range = ((UDFValue *) (problem->value))->range;
        break;

     case MF_VARIABLE:
     case SF_VARIABLE:
        if (GetBoundVariable(theEnv,returnValue,problem->lexemeValue) == false)
          {
           PrintErrorID(theEnv,"EVALUATN",1,false);
           EnvPrintRouter(theEnv,WERROR,"Variable ");
           EnvPrintRouter(theEnv,WERROR,problem->lexemeValue->contents);
           EnvPrintRouter(theEnv,WERROR," is unbound\n");
           returnValue->value = FalseSymbol(theEnv);
           EnvSetEvaluationError(theEnv,true);
          }
        break;

      default:
        if (EvaluationData(theEnv)->PrimitivesArray[problem->type] == NULL)
          {
           SystemError(theEnv,"EVALUATN",3);
           EnvExitRouter(theEnv,EXIT_FAILURE);
          }

        if (EvaluationData(theEnv)->PrimitivesArray[problem->type]->copyToEvaluate)
          {
           returnValue->value = problem->value;
           break;
          }

        if (EvaluationData(theEnv)->PrimitivesArray[problem->type]->evaluateFunction == NULL)
          {
           SystemError(theEnv,"EVALUATN",4);
           EnvExitRouter(theEnv,EXIT_FAILURE);
          }

        oldArgument = EvaluationData(theEnv)->CurrentExpression;
        EvaluationData(theEnv)->CurrentExpression = problem;

#if PROFILING_FUNCTIONS
        StartProfile(theEnv,&profileFrame,
                     &EvaluationData(theEnv)->PrimitivesArray[problem->type]->usrData,
                     ProfileFunctionData(theEnv)->ProfileUserFunctions);
#endif

        (*EvaluationData(theEnv)->PrimitivesArray[problem->type]->evaluateFunction)(theEnv,problem->value,returnValue);

#if PROFILING_FUNCTIONS
        EndProfile(theEnv,&profileFrame);
#endif

        EvaluationData(theEnv)->CurrentExpression = oldArgument;
        break;
     }
     
   return EvaluationData(theEnv)->EvaluationError;
  }

/******************************************/
/* InstallPrimitive: Installs a primitive */
/*   data type in the primitives array.   */
/******************************************/
void InstallPrimitive(
  Environment *theEnv,
  struct entityRecord *thePrimitive,
  int whichPosition)
  {
   if (EvaluationData(theEnv)->PrimitivesArray[whichPosition] != NULL)
     {
      SystemError(theEnv,"EVALUATN",5);
      EnvExitRouter(theEnv,EXIT_FAILURE);
     }

   EvaluationData(theEnv)->PrimitivesArray[whichPosition] = thePrimitive;
  }

/******************************************************/
/* InstallExternalAddressType: Installs an external   */
/*   address type in the external address type array. */
/******************************************************/
int InstallExternalAddressType(
  Environment *theEnv,
  struct externalAddressType *theAddressType)
  {
   struct externalAddressType *copyEAT;

   int rv = EvaluationData(theEnv)->numberOfAddressTypes;

   if (EvaluationData(theEnv)->numberOfAddressTypes == MAXIMUM_EXTERNAL_ADDRESS_TYPES)
     {
      SystemError(theEnv,"EVALUATN",6);
      EnvExitRouter(theEnv,EXIT_FAILURE);
     }

   copyEAT = (struct externalAddressType *) genalloc(theEnv,sizeof(struct externalAddressType));
   memcpy(copyEAT,theAddressType,sizeof(struct externalAddressType));
   EvaluationData(theEnv)->ExternalAddressTypes[EvaluationData(theEnv)->numberOfAddressTypes++] = copyEAT;

   return rv;
  }

/*********************************************************/
/* EnvSetEvaluationError: Sets the EvaluationError flag. */
/*********************************************************/
void EnvSetEvaluationError(
  Environment *theEnv,
  bool value)
  {
   EvaluationData(theEnv)->EvaluationError = value;
   if (value == true)
     { EvaluationData(theEnv)->HaltExecution = true; }
  }

/************************************************************/
/* EnvGetEvaluationError: Returns the EvaluationError flag. */
/************************************************************/
bool EnvGetEvaluationError(
  Environment *theEnv)
  {
   return(EvaluationData(theEnv)->EvaluationError);
  }

/*****************************************************/
/* EnvSetHaltExecution: Sets the HaltExecution flag. */
/*****************************************************/
void EnvSetHaltExecution(
  Environment *theEnv,
  bool value)
  {
   EvaluationData(theEnv)->HaltExecution = value;
  }

/********************************************************/
/* EnvGetHaltExecution: Returns the HaltExecution flag. */
/********************************************************/
bool EnvGetHaltExecution(
  Environment *theEnv)
  {
   return(EvaluationData(theEnv)->HaltExecution);
  }

/*****************************************************/
/* ReturnValues: Returns a linked list of UDFValue */
/*   structures to the pool of free memory.          */
/*****************************************************/
void ReturnValues(
  Environment *theEnv,
  UDFValue *garbagePtr,
  bool decrementSupplementalInfo)
  {
   UDFValue *nextPtr;

   while (garbagePtr != NULL)
     {
      nextPtr = garbagePtr->next;
      ValueDeinstall(theEnv,garbagePtr);
      if ((garbagePtr->supplementalInfo != NULL) && decrementSupplementalInfo)
        { DecrementSymbolCount(theEnv,(struct symbolHashNode *) garbagePtr->supplementalInfo); }
      rtn_struct(theEnv,udfValue,garbagePtr);
      garbagePtr = nextPtr;
     }
  }

/**************************************************/
/* PrintDataObject: Prints a UDFValue structure */
/*   to the specified logical name.               */
/**************************************************/
void PrintDataObject(
  Environment *theEnv,
  const char *fileid,
  UDFValue *argPtr)
  {
   switch(argPtr->header->type)
     {
      case VOID_TYPE:
      case SYMBOL_TYPE:
      case STRING_TYPE:
      case INTEGER_TYPE:
      case FLOAT_TYPE:
      case EXTERNAL_ADDRESS_TYPE:
      case FACT_ADDRESS_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
      case INSTANCE_ADDRESS_TYPE:
#endif
        PrintAtom(theEnv,fileid,argPtr->header->type,argPtr->value);
        break;

      case MULTIFIELD_TYPE:
        PrintMultifield(theEnv,fileid,argPtr->multifieldValue,
                        argPtr->begin,(argPtr->begin + argPtr->range) - 1,true);
        break;

      default:
        EnvPrintRouter(theEnv,fileid,"<UnknownPrintType");
        PrintLongInteger(theEnv,fileid,(long int) argPtr->header->type);
        EnvPrintRouter(theEnv,fileid,">");
        EnvSetHaltExecution(theEnv,true);
        EnvSetEvaluationError(theEnv,true);
        break;
     }
  }

/****************************************************/
/* EnvSetMultifieldErrorValue: Creates a multifield */
/*   value of length zero for error returns.        */
/****************************************************/
void EnvSetMultifieldErrorValue(
  Environment *theEnv,
  UDFValue *returnValue)
  {
   returnValue->value = EnvCreateMultifield(theEnv,0L);
   returnValue->begin = 0;
   returnValue->range = 0;
  }

/**************************************************/
/* ValueInstall: Increments the appropriate count */
/*   (in use) values for a UDFValue structure.  */
/**************************************************/
void ValueInstall(
  Environment *theEnv,
  UDFValue *vPtr)
  {
   if (vPtr->header->type == MULTIFIELD_TYPE)
     { CVMultifieldInstall(theEnv,vPtr->multifieldValue); }
   else
     { CVAtomInstall(theEnv,vPtr->value); }
  }

/****************************************************/
/* ValueDeinstall: Decrements the appropriate count */
/*   (in use) values for a UDFValue structure.    */
/****************************************************/
void ValueDeinstall(
  Environment *theEnv,
  UDFValue *vPtr)
  {
   if (vPtr->header->type == MULTIFIELD_TYPE)
     { CVMultifieldDeinstall(theEnv,vPtr->multifieldValue); }
   else
     { CVAtomDeinstall(theEnv,vPtr->value); }
  }

/*******************************************/
/* CVAtomInstall: Increments the reference */
/*   count of an atomic data type.         */
/*******************************************/
void CVAtomInstall(
  Environment *theEnv,
  void *vPtr)
  {
   switch (((TypeHeader *) vPtr)->type)
     {
      case SYMBOL_TYPE:
      case STRING_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
        IncrementSymbolCount(vPtr);
        break;

      case FLOAT_TYPE:
        IncrementFloatCount(vPtr);
        break;

      case INTEGER_TYPE:
        IncrementIntegerCount(vPtr);
        break;

      case EXTERNAL_ADDRESS_TYPE:
        IncrementExternalAddressCount(vPtr);
        break;

      case MULTIFIELD_TYPE:
        MultifieldInstall(theEnv,(Multifield *) vPtr);
        break;
        
      case INSTANCE_ADDRESS_TYPE:
        EnvIncrementInstanceCount(theEnv,(Instance *) vPtr);
        break;
     
      case FACT_ADDRESS_TYPE:
        EnvIncrementFactCount(theEnv,(Fact *) vPtr);
        break;
     
      case VOID_TYPE:
        break;

      default:
        SystemError(theEnv,"EVALUATN",7);
        EnvExitRouter(theEnv,EXIT_FAILURE);
        break;
     }
  }

/*********************************************/
/* CVAtomDeinstall: Decrements the reference */
/*   count of an atomic data type.           */
/*********************************************/
void CVAtomDeinstall(
  Environment *theEnv,
  void *vPtr)
  {
   switch (((TypeHeader *) vPtr)->type)
     {
      case SYMBOL_TYPE:
      case STRING_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
        DecrementSymbolCount(theEnv,(CLIPSLexeme *) vPtr);
        break;

      case FLOAT_TYPE:
        DecrementFloatCount(theEnv,(CLIPSFloat *) vPtr);
        break;

      case INTEGER_TYPE:
        DecrementIntegerCount(theEnv,(CLIPSInteger *) vPtr);
        break;

      case EXTERNAL_ADDRESS_TYPE:
        DecrementExternalAddressCount(theEnv,(CLIPSExternalAddress *) vPtr);
        break;

      case MULTIFIELD_TYPE:
        MultifieldDeinstall(theEnv,(Multifield *) vPtr);
        break;
        
      case INSTANCE_ADDRESS_TYPE:
        EnvDecrementInstanceCount(theEnv,(Instance *) vPtr);
        break;
     
      case FACT_ADDRESS_TYPE:
        EnvDecrementFactCount(theEnv,(Fact *) vPtr);
        break;

      case VOID_TYPE:
        break;

      default:
        SystemError(theEnv,"EVALUATN",8);
        EnvExitRouter(theEnv,EXIT_FAILURE);
        break;
     }
  }

/*****************************************/
/* AtomInstall: Increments the reference */
/*   count of an atomic data type.       */
/*****************************************/
void AtomInstall(
  Environment *theEnv,
  int type,
  void *vPtr)
  {
   switch (type)
     {
      case SYMBOL_TYPE:
      case STRING_TYPE:
#if DEFGLOBAL_CONSTRUCT
      case GBL_VARIABLE:
#endif
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
        IncrementSymbolCount(vPtr);
        break;

      case FLOAT_TYPE:
        IncrementFloatCount(vPtr);
        break;

      case INTEGER_TYPE:
        IncrementIntegerCount(vPtr);
        break;

      case EXTERNAL_ADDRESS_TYPE:
        IncrementExternalAddressCount(vPtr);
        break;

      case MULTIFIELD_TYPE:
        MultifieldInstall(theEnv,(Multifield *) vPtr);
        break;

      case VOID_TYPE:
        break;

      default:
        if (EvaluationData(theEnv)->PrimitivesArray[type] == NULL) break;
        if (EvaluationData(theEnv)->PrimitivesArray[type]->bitMap) IncrementBitMapCount(vPtr);
        else if (EvaluationData(theEnv)->PrimitivesArray[type]->incrementBusyCount)
          { (*EvaluationData(theEnv)->PrimitivesArray[type]->incrementBusyCount)(theEnv,vPtr); }
        break;
     }
  }

/*******************************************/
/* AtomDeinstall: Decrements the reference */
/*   count of an atomic data type.         */
/*******************************************/
void AtomDeinstall(
  Environment *theEnv,
  int type,
  void *vPtr)
  {
   switch (type)
     {
      case SYMBOL_TYPE:
      case STRING_TYPE:
#if DEFGLOBAL_CONSTRUCT
      case GBL_VARIABLE:
#endif
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
        DecrementSymbolCount(theEnv,(CLIPSLexeme *) vPtr);
        break;

      case FLOAT_TYPE:
        DecrementFloatCount(theEnv,(CLIPSFloat *) vPtr);
        break;

      case INTEGER_TYPE:
        DecrementIntegerCount(theEnv,(CLIPSInteger *) vPtr);
        break;

      case EXTERNAL_ADDRESS_TYPE:
        DecrementExternalAddressCount(theEnv,(CLIPSExternalAddress *) vPtr);
        break;

      case MULTIFIELD_TYPE:
        MultifieldDeinstall(theEnv,(Multifield *) vPtr);
        break;

      case VOID_TYPE:
        break;

      default:
        if (EvaluationData(theEnv)->PrimitivesArray[type] == NULL) break;
        if (EvaluationData(theEnv)->PrimitivesArray[type]->bitMap) DecrementBitMapCount(theEnv,(CLIPSBitMap *) vPtr);
        else if (EvaluationData(theEnv)->PrimitivesArray[type]->decrementBusyCount)
          { (*EvaluationData(theEnv)->PrimitivesArray[type]->decrementBusyCount)(theEnv,vPtr); }
     }
  }

#if DEFFUNCTION_CONSTRUCT || DEFGENERIC_CONSTRUCT

/********************************************/
/* EnvFunctionCall: Allows Deffunctions and */
/*   Generic Functions to be called from C. */
/*   Allows only constants as arguments.    */
/********************************************/
bool EnvFunctionCall(
  Environment *theEnv,
  const char *name,
  const char *args,
  CLIPSValue *returnValue)
  {
   Expression theReference;
   UDFValue evalResult;
   bool rv;

   /*=======================================*/
   /* Call the function if it can be found. */
   /*=======================================*/

   if (GetFunctionReference(theEnv,name,&theReference))
     {
      rv = FunctionCall2(theEnv,&theReference,args,&evalResult);
      NormalizeMultifield(theEnv,&evalResult);
      returnValue->value = evalResult.value;
      return rv;
     }

   /*=========================================================*/
   /* Otherwise signal an error if a deffunction, defgeneric, */
   /* or user defined function doesn't exist that matches     */
   /* the specified function name.                            */
   /*=========================================================*/

   PrintErrorID(theEnv,"EVALUATN",2,false);
   EnvPrintRouter(theEnv,WERROR,"No function, generic function or deffunction of name ");
   EnvPrintRouter(theEnv,WERROR,name);
   EnvPrintRouter(theEnv,WERROR," exists for external call.\n");
   return true;
  }

/********************************************/
/* FunctionCall2: Allows Deffunctions and    */
/*   Generic Functions to be called from C. */
/*   Allows only constants as arguments.    */
/********************************************/
bool FunctionCall2(
  Environment *theEnv,
  Expression *theReference,
  const char *args,
  UDFValue *returnValue)
  {
   Expression *argexps;
   bool error = false;

   /*=============================================*/
   /* Force periodic cleanup if the function call */
   /* was executed from an embedded application.  */
   /*=============================================*/

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     {
      CleanCurrentGarbageFrame(theEnv,NULL);
      CallPeriodicTasks(theEnv);
     }

   /*========================*/
   /* Reset the error state. */
   /*========================*/

   if (UtilityData(theEnv)->CurrentGarbageFrame->topLevel) EnvSetHaltExecution(theEnv,false);
   EvaluationData(theEnv)->EvaluationError = false;

   /*======================================*/
   /* Initialize the default return value. */
   /*======================================*/

   returnValue->value = FalseSymbol(theEnv);

   /*============================*/
   /* Parse the argument string. */
   /*============================*/

   argexps = ParseConstantArguments(theEnv,args,&error);
   if (error == true) return true;

   /*====================*/
   /* Call the function. */
   /*====================*/

   theReference->argList = argexps;
   error = EvaluateExpression(theEnv,theReference,returnValue);

   /*========================*/
   /* Return the expression. */
   /*========================*/

   ReturnExpression(theEnv,argexps);
   theReference->argList = NULL;

   /*==========================*/
   /* Return the error status. */
   /*==========================*/

   return(error);
  }

#endif

/***************************************************/
/* CopyDataObject: Copies the values from a source */
/*   UDFValue to a destination UDFValue.       */
/***************************************************/
void CopyDataObject(
  Environment *theEnv,
  UDFValue *dst,
  UDFValue *src,
  int garbageMultifield)
  {
   if (src->header->type != MULTIFIELD_TYPE)
     {
      dst->value = src->value;
     }
   else
     {
      DuplicateMultifield(theEnv,dst,src);
      if (garbageMultifield)
        { AddToMultifieldList(theEnv,dst->multifieldValue); }
     }
  }

/***********************************************/
/* TransferDataObjectValues: Copies the values */
/*   directly from a source UDFValue to a    */
/*   destination UDFValue.                   */
/***********************************************/
void TransferDataObjectValues(
  UDFValue *dst,
  UDFValue *src)
  {
   dst->value = src->value;
   dst->begin = src->begin;
   dst->range = src->range;
   dst->supplementalInfo = src->supplementalInfo;
   dst->next = src->next;
  }

/************************************************************************/
/* ConvertValueToExpression: Converts the value stored in a data object */
/*   into an expression. For multifield values, a chain of expressions  */
/*   is generated and the chain is linked by the nextArg field. For a   */
/*   single field value, a single expression is created.                */
/************************************************************************/
struct expr *ConvertValueToExpression(
  Environment *theEnv,
  UDFValue *theValue)
  {
   long i;
   struct expr *head = NULL, *last = NULL, *newItem;

   if (theValue->header->type != MULTIFIELD_TYPE)
     { return(GenConstant(theEnv,theValue->header->type,theValue->value)); }

   for (i = theValue->begin; i < (theValue->begin + theValue->range); i++)
     {
      newItem = GenConstant(theEnv,theValue->multifieldValue->theFields[i].header->type,
                                   theValue->multifieldValue->theFields[i].value);
      if (last == NULL) head = newItem;
      else last->nextArg = newItem;
      last = newItem;
     }

   if (head == NULL)
     return(GenConstant(theEnv,FCALL,FindFunction(theEnv,"create$")));

   return(head);
  }

/****************************************/
/* GetAtomicHashValue: Returns the hash */
/*   value for an atomic data type.     */
/****************************************/
unsigned long GetAtomicHashValue(
  unsigned short type,
  void *value,
  int position)
  {
   unsigned long tvalue;
   union
     {
      double fv;
      void *vv;
      unsigned long liv;
     } fis;

   switch (type)
     {
      case FLOAT_TYPE:
        fis.liv = 0;
        fis.fv = ((CLIPSFloat *) value)->contents;
        tvalue = fis.liv;
        break;

      case INTEGER_TYPE:
        tvalue = (unsigned long) ((CLIPSInteger *) value)->contents;
        break;

      case EXTERNAL_ADDRESS_TYPE:
         fis.liv = 0;
         fis.vv = ((CLIPSExternalAddress *) value)->contents;
         tvalue = (unsigned long) fis.liv;
         break;

      case FACT_ADDRESS_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS_TYPE:
#endif
         fis.liv = 0;
         fis.vv = value;
         tvalue = (unsigned long) fis.liv;
         break;

      case STRING_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
      case SYMBOL_TYPE:
        tvalue = ((CLIPSLexeme *) value)->bucket;
        break;

      default:
        tvalue = type;
     }

   if (position < 0) return(tvalue);

   return((unsigned long) (tvalue * (((unsigned long) position) + 29)));
  }

/***********************************************************/
/* FunctionReferenceExpression: Returns an expression with */
/*   an appropriate expression reference to the specified  */
/*   name if it is the name of a deffunction, defgeneric,  */
/*   or user/system defined function.                      */
/***********************************************************/
struct expr *FunctionReferenceExpression(
  Environment *theEnv,
  const char *name)
  {
#if DEFGENERIC_CONSTRUCT
   Defgeneric *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
   Deffunction *dptr;
#endif
   struct functionDefinition *fptr;

   /*=====================================================*/
   /* Check to see if the function call is a deffunction. */
   /*=====================================================*/

#if DEFFUNCTION_CONSTRUCT
   if ((dptr = LookupDeffunctionInScope(theEnv,name)) != NULL)
     { return(GenConstant(theEnv,PCALL,dptr)); }
#endif

   /*====================================================*/
   /* Check to see if the function call is a defgeneric. */
   /*====================================================*/

#if DEFGENERIC_CONSTRUCT
   if ((gfunc = LookupDefgenericInScope(theEnv,name)) != NULL)
     { return(GenConstant(theEnv,GCALL,gfunc)); }
#endif

   /*======================================*/
   /* Check to see if the function call is */
   /* a system or user defined function.   */
   /*======================================*/

   if ((fptr = FindFunction(theEnv,name)) != NULL)
     { return(GenConstant(theEnv,FCALL,fptr)); }

   /*===================================================*/
   /* The specified function name is not a deffunction, */
   /* defgeneric, or user/system defined function.      */
   /*===================================================*/

   return NULL;
  }

/******************************************************************/
/* GetFunctionReference: Fills an expression with an appropriate  */
/*   expression reference to the specified name if it is the      */
/*   name of a deffunction, defgeneric, or user/system defined    */
/*   function.                                                    */
/******************************************************************/
bool GetFunctionReference(
  Environment *theEnv,
  const char *name,
  Expression *theReference)
  {
#if DEFGENERIC_CONSTRUCT
   Defgeneric *gfunc;
#endif
#if DEFFUNCTION_CONSTRUCT
   Deffunction *dptr;
#endif
   struct functionDefinition *fptr;

   theReference->nextArg = NULL;
   theReference->argList = NULL;
   theReference->type = VOID_TYPE;
   theReference->value = NULL;

   /*=====================================================*/
   /* Check to see if the function call is a deffunction. */
   /*=====================================================*/

#if DEFFUNCTION_CONSTRUCT
   if ((dptr = LookupDeffunctionInScope(theEnv,name)) != NULL)
     {
      theReference->type = PCALL;
      theReference->value = dptr;
      return true;
     }
#endif

   /*====================================================*/
   /* Check to see if the function call is a defgeneric. */
   /*====================================================*/

#if DEFGENERIC_CONSTRUCT
   if ((gfunc = LookupDefgenericInScope(theEnv,name)) != NULL)
     {
      theReference->type = GCALL;
      theReference->value = gfunc;
      return true;
     }
#endif

   /*======================================*/
   /* Check to see if the function call is */
   /* a system or user defined function.   */
   /*======================================*/

   if ((fptr = FindFunction(theEnv,name)) != NULL)
     {
      theReference->type = FCALL;
      theReference->value = fptr;
      return true;
     }

   /*===================================================*/
   /* The specified function name is not a deffunction, */
   /* defgeneric, or user/system defined function.      */
   /*===================================================*/

   return false;
  }

/*******************************************************/
/* DOsEqual: Determines if two DATA_OBJECTS are equal. */
/*******************************************************/
bool DOsEqual(
  UDFValue *dobj1,
  UDFValue *dobj2)
  {
   if (dobj1->header->type != dobj2->header->type)
     { return false; }

   if (dobj1->header->type == MULTIFIELD_TYPE)
     {
      if (MultifieldDOsEqual(dobj1,dobj2) == false)
        { return false; }
     }
   else if (dobj1->value != dobj2->value)
     { return false; }

   return true;
  }

/***********************************************************
  NAME         : EvaluateAndStoreInDataObject
  DESCRIPTION  : Evaluates slot-value expressions
                   and stores the result in a
                   Kernel data object
  INPUTS       : 1) Flag indicating if multifields are OK
                 2) The value-expression
                 3) The data object structure
                 4) Flag indicating if a multifield value
                    should be placed on the garbage list.
  RETURNS      : False on errors, true otherwise
  SIDE EFFECTS : Segment allocated for storing
                 multifield values
  NOTES        : None
 ***********************************************************/
bool EvaluateAndStoreInDataObject(
  Environment *theEnv,
  bool mfp,
  Expression *theExp,
  UDFValue *val,
  bool garbageSegment)
  {
   val->begin = 0;
   val->range = 0;

   if (theExp == NULL)
     {
      if (garbageSegment) val->value = EnvCreateMultifield(theEnv,0L);
      else val->value = CreateUnmanagedMultifield(theEnv,0L);

      return true;
     }

   if ((mfp == false) && (theExp->nextArg == NULL))
     EvaluateExpression(theEnv,theExp,val);
   else
     StoreInMultifield(theEnv,val,theExp,garbageSegment);

   return(EvaluationData(theEnv)->EvaluationError ? false : true);
  }

/******************/
/* PrintCAddress: */
/******************/
static void PrintCAddress(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
   char buffer[20];

   EnvPrintRouter(theEnv,logicalName,"<Pointer-C-");

   gensprintf(buffer,"%p",((CLIPSExternalAddress *) theValue)->contents);
   EnvPrintRouter(theEnv,logicalName,buffer);
   EnvPrintRouter(theEnv,logicalName,">");
  }

/****************/
/* NewCAddress: */
/****************/
static void NewCAddress(
  UDFContext *context,
  UDFValue *rv)
  {
   int numberOfArguments;
   Environment *theEnv = context->environment;

   numberOfArguments = UDFArgumentCount(context);

   if (numberOfArguments != 1)
     {
      PrintErrorID(theEnv,"NEW",1,false);
      EnvPrintRouter(theEnv,WERROR,"Function new expected no additional arguments for the C external language type.\n");
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   rv->value = EnvAddExternalAddress(theEnv,NULL,0);
  }

/*******************************/
/* DiscardCAddress: TBD Remove */
/*******************************/
/*
static bool DiscardCAddress(
  Environment *theEnv,
  void *theValue)
  {
   EnvPrintRouter(theEnv,WDISPLAY,"Discarding C Address\n");

   return true;
  }
*/

