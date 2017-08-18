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
#include <stdint.h>
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
   struct functionDefinition *fptr;
   UDFContext theUDFContext;
#if PROFILING_FUNCTIONS
   struct profileFrameInfo profileFrame;
#endif

   returnValue->voidValue = VoidConstant(theEnv);
   returnValue->begin = 0;
   returnValue->range = SIZE_MAX;

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

#if PROFILING_FUNCTIONS
         StartProfile(theEnv,&profileFrame,
                      &fptr->usrData,
                      ProfileFunctionData(theEnv)->ProfileUserFunctions);
#endif

         oldArgument = EvaluationData(theEnv)->CurrentExpression;
         EvaluationData(theEnv)->CurrentExpression = problem;

         theUDFContext.environment = theEnv;
         theUDFContext.context = fptr->context;
         theUDFContext.theFunction = fptr;
         theUDFContext.lastArg = problem->argList;
         theUDFContext.lastPosition = 1;
         theUDFContext.returnValue = returnValue;
         fptr->functionPointer(theEnv,&theUDFContext,returnValue);
         if ((returnValue->header->type == MULTIFIELD_TYPE) &&
             (returnValue->range == SIZE_MAX))
           { returnValue->range = returnValue->multifieldValue->length; }

#if PROFILING_FUNCTIONS
        EndProfile(theEnv,&profileFrame);
#endif

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
           WriteString(theEnv,STDERR,"Variable ");
           WriteString(theEnv,STDERR,problem->lexemeValue->contents);
           WriteString(theEnv,STDERR," is unbound\n");
           returnValue->value = FalseSymbol(theEnv);
           SetEvaluationError(theEnv,true);
          }
        break;

      default:
        if (EvaluationData(theEnv)->PrimitivesArray[problem->type] == NULL)
          {
           SystemError(theEnv,"EVALUATN",3);
           ExitRouter(theEnv,EXIT_FAILURE);
          }

        if (EvaluationData(theEnv)->PrimitivesArray[problem->type]->copyToEvaluate)
          {
           returnValue->value = problem->value;
           break;
          }

        if (EvaluationData(theEnv)->PrimitivesArray[problem->type]->evaluateFunction == NULL)
          {
           SystemError(theEnv,"EVALUATN",4);
           ExitRouter(theEnv,EXIT_FAILURE);
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
      ExitRouter(theEnv,EXIT_FAILURE);
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
      ExitRouter(theEnv,EXIT_FAILURE);
     }

   copyEAT = (struct externalAddressType *) genalloc(theEnv,sizeof(struct externalAddressType));
   memcpy(copyEAT,theAddressType,sizeof(struct externalAddressType));
   EvaluationData(theEnv)->ExternalAddressTypes[EvaluationData(theEnv)->numberOfAddressTypes++] = copyEAT;

   return rv;
  }

/******************************************************/
/* SetEvaluationError: Sets the EvaluationError flag. */
/******************************************************/
void SetEvaluationError(
  Environment *theEnv,
  bool value)
  {
   EvaluationData(theEnv)->EvaluationError = value;
   if (value == true)
     { EvaluationData(theEnv)->HaltExecution = true; }
  }

/*********************************************************/
/* GetEvaluationError: Returns the EvaluationError flag. */
/*********************************************************/
bool GetEvaluationError(
  Environment *theEnv)
  {
   return(EvaluationData(theEnv)->EvaluationError);
  }

/**************************************************/
/* SetHaltExecution: Sets the HaltExecution flag. */
/**************************************************/
void SetHaltExecution(
  Environment *theEnv,
  bool value)
  {
   EvaluationData(theEnv)->HaltExecution = value;
  }

/*****************************************************/
/* GetHaltExecution: Returns the HaltExecution flag. */
/*****************************************************/
bool GetHaltExecution(
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
      UDFRelease(theEnv,garbagePtr);
      if ((garbagePtr->supplementalInfo != NULL) && decrementSupplementalInfo)
        { DecrementLexemeReferenceCount(theEnv,(CLIPSLexeme *) garbagePtr->supplementalInfo); }
      rtn_struct(theEnv,udfValue,garbagePtr);
      garbagePtr = nextPtr;
     }
  }

/**************************************************/
/* WriteCLIPSValue: Prints a CLIPSValue structure */
/*   to the specified logical name.               */
/**************************************************/
void WriteCLIPSValue(
  Environment *theEnv,
  const char *fileid,
  CLIPSValue *argPtr)
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
        PrintMultifieldDriver(theEnv,fileid,argPtr->multifieldValue,
                              0,argPtr->multifieldValue->length,true);
        break;

      default:
        WriteString(theEnv,fileid,"<UnknownPrintType");
        WriteInteger(theEnv,fileid,argPtr->header->type);
        WriteString(theEnv,fileid,">");
        SetHaltExecution(theEnv,true);
        SetEvaluationError(theEnv,true);
        break;
     }
  }

/**********************************************/
/* WriteUDFValue: Prints a UDFValue structure */
/*   to the specified logical name.           */
/**********************************************/
void WriteUDFValue(
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
        PrintMultifieldDriver(theEnv,fileid,argPtr->multifieldValue,
                              argPtr->begin,argPtr->range,true);
        break;

      default:
        WriteString(theEnv,fileid,"<UnknownPrintType");
        WriteInteger(theEnv,fileid,argPtr->header->type);
        WriteString(theEnv,fileid,">");
        SetHaltExecution(theEnv,true);
        SetEvaluationError(theEnv,true);
        break;
     }
  }

/*************************************************/
/* SetMultifieldErrorValue: Creates a multifield */
/*   value of length zero for error returns.     */
/*************************************************/
void SetMultifieldErrorValue(
  Environment *theEnv,
  UDFValue *returnValue)
  {
   returnValue->value = CreateMultifield(theEnv,0L);
   returnValue->begin = 0;
   returnValue->range = 0;
  }

/***********************************************/
/* UDFRetain: Increments the appropriate count */
/*   (in use) values for a UDFValue structure. */
/***********************************************/
void UDFRetain(
  Environment *theEnv,
  UDFValue *vPtr)
  {
   if (vPtr->header->type == MULTIFIELD_TYPE)
     { IncrementCLIPSValueMultifieldReferenceCount(theEnv,vPtr->multifieldValue); }
   else
     { Retain(theEnv,vPtr->header); }
  }

/***********************************************/
/* UDFRetain: Decrements the appropriate count */
/*   (in use) values for a UDFValue structure. */
/***********************************************/
void UDFRelease(
  Environment *theEnv,
  UDFValue *vPtr)
  {
   if (vPtr->header->type == MULTIFIELD_TYPE)
     { DecrementCLIPSValueMultifieldReferenceCount(theEnv,vPtr->multifieldValue); }
   else
     { Release(theEnv,vPtr->header); }
  }

/*************************************************/
/* CVRetain: Increments the appropriate count    */
/*   (in use) values for a CLIPSValue structure. */
/*************************************************/
void CVRetain(
  Environment *theEnv,
  CLIPSValue *vPtr)
  {
   if (vPtr->header->type == MULTIFIELD_TYPE)
     { IncrementCLIPSValueMultifieldReferenceCount(theEnv,vPtr->multifieldValue); }
   else
     { Retain(theEnv,vPtr->header); }
  }

/*************************************************/
/* CVRelease: Decrements the appropriate count   */
/*   (in use) values for a CLIPSValue structure. */
/*************************************************/
void CVRelease(
  Environment *theEnv,
  CLIPSValue *vPtr)
  {
   if (vPtr->header->type == MULTIFIELD_TYPE)
     { DecrementCLIPSValueMultifieldReferenceCount(theEnv,vPtr->multifieldValue); }
   else
     { Release(theEnv,vPtr->header); }
  }

/******************************************/
/* Retain: Increments the reference count */
/*   of an atomic data type.              */
/******************************************/
void Retain(
  Environment *theEnv,
  TypeHeader *th)
  {
   switch (th->type)
     {
      case SYMBOL_TYPE:
      case STRING_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
        IncrementLexemeCount(th);
        break;

      case FLOAT_TYPE:
        IncrementFloatCount(th);
        break;

      case INTEGER_TYPE:
        IncrementIntegerCount(th);
        break;

      case EXTERNAL_ADDRESS_TYPE:
        IncrementExternalAddressCount(th);
        break;

      case MULTIFIELD_TYPE:
        IncrementMultifieldReferenceCount(theEnv,(Multifield *) th);
        break;
        
#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS_TYPE:
        IncrementInstanceReferenceCount((Instance *) th);
        break;
#endif

#if DEFTEMPLATE_CONSTRUCT
      case FACT_ADDRESS_TYPE:
        IncrementFactReferenceCount((Fact *) th);
        break;
#endif
     
      case VOID_TYPE:
        break;

      default:
        SystemError(theEnv,"EVALUATN",7);
        ExitRouter(theEnv,EXIT_FAILURE);
        break;
     }
  }

/*************************************/
/* Release: Decrements the reference */
/*   count of an atomic data type.   */
/*************************************/
void Release(
  Environment *theEnv,
  TypeHeader *th)
  {
   switch (th->type)
     {
      case SYMBOL_TYPE:
      case STRING_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
        DecrementLexemeReferenceCount(theEnv,(CLIPSLexeme *) th);
        break;

      case FLOAT_TYPE:
        DecrementFloatReferenceCount(theEnv,(CLIPSFloat *) th);
        break;

      case INTEGER_TYPE:
        DecrementIntegerReferenceCount(theEnv,(CLIPSInteger *) th);
        break;

      case EXTERNAL_ADDRESS_TYPE:
        DecrementExternalAddressReferenceCount(theEnv,(CLIPSExternalAddress *) th);
        break;

      case MULTIFIELD_TYPE:
        DecrementMultifieldReferenceCount(theEnv,(Multifield *) th);
        break;
        
#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS_TYPE:
        DecrementInstanceReferenceCount((Instance *) th);
        break;
#endif
     
#if DEFTEMPLATE_CONSTRUCT
      case FACT_ADDRESS_TYPE:
        DecrementFactReferenceCount((Fact *) th);
        break;
#endif

      case VOID_TYPE:
        break;

      default:
        SystemError(theEnv,"EVALUATN",8);
        ExitRouter(theEnv,EXIT_FAILURE);
        break;
     }
  }

/*****************************************/
/* AtomInstall: Increments the reference */
/*   count of an atomic data type.       */
/*****************************************/
void AtomInstall(
  Environment *theEnv,
  unsigned short type,
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
        IncrementLexemeCount(vPtr);
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
        IncrementMultifieldReferenceCount(theEnv,(Multifield *) vPtr);
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
  unsigned short type,
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
        DecrementLexemeReferenceCount(theEnv,(CLIPSLexeme *) vPtr);
        break;

      case FLOAT_TYPE:
        DecrementFloatReferenceCount(theEnv,(CLIPSFloat *) vPtr);
        break;

      case INTEGER_TYPE:
        DecrementIntegerReferenceCount(theEnv,(CLIPSInteger *) vPtr);
        break;

      case EXTERNAL_ADDRESS_TYPE:
        DecrementExternalAddressReferenceCount(theEnv,(CLIPSExternalAddress *) vPtr);
        break;

      case MULTIFIELD_TYPE:
        DecrementMultifieldReferenceCount(theEnv,(Multifield *) vPtr);
        break;

      case VOID_TYPE:
        break;

      default:
        if (EvaluationData(theEnv)->PrimitivesArray[type] == NULL) break;
        if (EvaluationData(theEnv)->PrimitivesArray[type]->bitMap) DecrementBitMapReferenceCount(theEnv,(CLIPSBitMap *) vPtr);
        else if (EvaluationData(theEnv)->PrimitivesArray[type]->decrementBusyCount)
          { (*EvaluationData(theEnv)->PrimitivesArray[type]->decrementBusyCount)(theEnv,vPtr); }
     }
  }

/***************************************************/
/* CopyDataObject: Copies the values from a source */
/*   UDFValue to a destination UDFValue.           */
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
   size_t i;
   struct expr *head = NULL, *last = NULL, *newItem;

   if (theValue->header->type != MULTIFIELD_TYPE)
     { return(GenConstant(theEnv,theValue->header->type,theValue->value)); }

   for (i = theValue->begin; i < (theValue->begin + theValue->range); i++)
     {
      newItem = GenConstant(theEnv,theValue->multifieldValue->contents[i].header->type,
                                   theValue->multifieldValue->contents[i].value);
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
  unsigned short position)
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
         tvalue = fis.liv;
         break;

      case FACT_ADDRESS_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS_TYPE:
#endif
         fis.liv = 0;
         fis.vv = value;
         tvalue = fis.liv;
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

   if (position < 0) return tvalue;

   return tvalue * (position + 29);
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
      if (garbageSegment) val->value = CreateMultifield(theEnv,0L);
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

   WriteString(theEnv,logicalName,"<Pointer-C-");

   gensprintf(buffer,"%p",((CLIPSExternalAddress *) theValue)->contents);
   WriteString(theEnv,logicalName,buffer);
   WriteString(theEnv,logicalName,">");
  }

/****************/
/* NewCAddress: */
/****************/
static void NewCAddress(
  UDFContext *context,
  UDFValue *rv)
  {
   unsigned int numberOfArguments;
   Environment *theEnv = context->environment;

   numberOfArguments = UDFArgumentCount(context);

   if (numberOfArguments != 1)
     {
      PrintErrorID(theEnv,"NEW",1,false);
      WriteString(theEnv,STDERR,"Function new expected no additional arguments for the C external language type.\n");
      SetEvaluationError(theEnv,true);
      return;
     }

   rv->value = CreateExternalAddress(theEnv,NULL,0);
  }

/*******************************/
/* DiscardCAddress: TBD Remove */
/*******************************/
/*
static bool DiscardCAddress(
  Environment *theEnv,
  void *theValue)
  {
   WriteString(theEnv,STDOUT,"Discarding C Address\n");

   return true;
  }
*/

