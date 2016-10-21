   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/01/16            */
   /*                                                     */
   /*               EVALUATION HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for evaluating expressions.    */
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
/*            Removed LOCALE definition.                     */
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
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*************************************************************/

#ifndef _H_evaluatn

#pragma once

#define _H_evaluatn

struct entityRecord;
struct udfValue;

typedef struct clipsValue CLIPSValue;
typedef struct udfValue UDFValue;
typedef struct expr FUNCTION_REFERENCE;
typedef struct typeHeader TypeHeader;

struct typeHeader
  {
   unsigned short type;
  };

typedef void EntityPrintFunction(Environment *,const char *,void *);
typedef bool EntityEvaluationFunction(Environment *,void *,UDFValue *);
typedef void EntityBusyCountFunction(Environment *,void *);

#include "constant.h"
#include "symbol.h"
#include "expressn.h"

#define C_POINTER_EXTERNAL_ADDRESS 0

#include "userdata.h"

struct entityRecord
  {
   const char *name;
   unsigned int type : 13;
   unsigned int copyToEvaluate : 1;
   unsigned int bitMap : 1;
   unsigned int addsToRuleComplexity : 1;
   EntityPrintFunction *shortPrintFunction;
   EntityPrintFunction *longPrintFunction;
   bool (*deleteFunction)(void *,void *);
   EntityEvaluationFunction *evaluateFunction;
   void *(*getNextFunction)(void *,void *);
   EntityBusyCountFunction *decrementBusyCount;
   EntityBusyCountFunction *incrementBusyCount;
   void (*propagateDepth)(void *,void *);
   void (*markNeeded)(void *,void *);
   void (*install)(void *,void *);
   void (*deinstall)(void *,void *);
   struct userData *usrData;
  };

#include "factmngr.h"
#include "object.h"

struct clipsValue
  {
   union
     {
      void *value;
      TypeHeader const *header;
      Fact *factValue;
      Instance *instanceValue;
      CLIPSLexeme *lexemeValue;
      CLIPSFloat *floatValue;
      CLIPSInteger *integerValue;
      CLIPSVoid *voidValue;
      Multifield *multifieldValue;
      CLIPSExternalAddress *externalAddressValue;
     };
  };

struct udfValue
  {
   void *supplementalInfo;
   union
     {
      void *value;
      TypeHeader const *header;
      Fact *factValue;
      Instance *instanceValue;
      CLIPSLexeme *lexemeValue;
      CLIPSFloat *floatValue;
      CLIPSInteger *integerValue;
      CLIPSVoid *voidValue;
      Multifield *multifieldValue;
      CLIPSExternalAddress *externalAddressValue;
     };
   long begin;
   long range;
   struct udfValue *next;
  };

struct externalAddressType
  {
   const  char *name;
   void (*shortPrintFunction)(Environment *,const char *,void *);
   void (*longPrintFunction)(Environment *,const char *,void *);
   bool (*discardFunction)(Environment *,void *);
   void (*newFunction)(UDFContext *,UDFValue *);
   bool (*callFunction)(UDFContext *,UDFValue *,UDFValue *);
  };

typedef struct entityRecord ENTITY_RECORD;
typedef struct entityRecord * ENTITY_RECORD_PTR;

#define CoerceToLongInteger(t,v) ((t == INTEGER_TYPE) ? ValueToLong(v) : (long int) ValueToDouble(v))
#define CoerceToInteger(t,v) ((t == INTEGER_TYPE) ? (int) ValueToLong(v) : (int) ValueToDouble(v))
#define CoerceToDouble(t,v) ((t == INTEGER_TYPE) ? (double) ValueToLong(v) : ValueToDouble(v))

#define GetFirstArgument()           (EvaluationData(theEnv)->CurrentExpression->argList)
#define GetNextArgument(ep)          (ep->nextArg)

#define MAXIMUM_PRIMITIVES 150
#define MAXIMUM_EXTERNAL_ADDRESS_TYPES 10

#define BITS_PER_BYTE    8

#define BitwiseTest(n,b)   ((n) & (char) (1 << (b)))
#define BitwiseSet(n,b)    (n |= (char) (1 << (b)))
#define BitwiseClear(n,b)  (n &= (char) ~(1 << (b)))

#define CountToBitMapSize(c) (((c) + (BITS_PER_BYTE - 1)) / BITS_PER_BYTE) 
#define TestBitMap(map,id)  BitwiseTest(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define SetBitMap(map,id)   BitwiseSet(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)
#define ClearBitMap(map,id) BitwiseClear(map[(id) / BITS_PER_BYTE],(id) % BITS_PER_BYTE)

#define EVALUATION_DATA 44

struct evaluationData
  {
   struct expr *CurrentExpression;
   bool EvaluationError;
   bool HaltExecution;
   int CurrentEvaluationDepth;
   int numberOfAddressTypes;
   struct entityRecord *PrimitivesArray[MAXIMUM_PRIMITIVES];
   struct externalAddressType *ExternalAddressTypes[MAXIMUM_EXTERNAL_ADDRESS_TYPES];
  };

#define EvaluationData(theEnv) ((struct evaluationData *) GetEnvironmentData(theEnv,EVALUATION_DATA))

   void                           InitializeEvaluationData(Environment *);
   bool                           EvaluateExpression(Environment *,struct expr *,UDFValue *);
   void                           EnvSetEvaluationError(Environment *,bool);
   bool                           EnvGetEvaluationError(Environment *);
   void                           EnvSetHaltExecution(Environment *,bool);
   bool                           EnvGetHaltExecution(Environment *);
   void                           ReturnValues(Environment *,UDFValue *,bool);
   void                           PrintDataObject(Environment *,const char *,UDFValue *);
   void                           EnvSetMultifieldErrorValue(Environment *,UDFValue *);
   void                           ValueInstall(Environment *,UDFValue *);
   void                           ValueDeinstall(Environment *,UDFValue *);
#if DEFFUNCTION_CONSTRUCT || DEFGENERIC_CONSTRUCT
   bool                           EnvFunctionCall(Environment *,const char *,const char *,CLIPSValue *);
   bool                           FunctionCall2(Environment *,FUNCTION_REFERENCE *,const char *,UDFValue *);
#endif
   void                           CopyDataObject(Environment *,UDFValue *,UDFValue *,int);
   void                           AtomInstall(Environment *,int,void *);
   void                           AtomDeinstall(Environment *,int,void *);
   void                           CVAtomInstall(Environment *,void *);
   void                           CVAtomDeinstall(Environment *,void *);
   struct expr                   *ConvertValueToExpression(Environment *,UDFValue *);
   unsigned long                  GetAtomicHashValue(unsigned short,void *,int);
   void                           InstallPrimitive(Environment *,struct entityRecord *,int);
   int                            InstallExternalAddressType(Environment *,struct externalAddressType *);
   void                           TransferDataObjectValues(UDFValue *,UDFValue *);
   struct expr                   *FunctionReferenceExpression(Environment *,const char *);
   bool                           GetFunctionReference(Environment *,const char *,FUNCTION_REFERENCE *);
   bool                           DOsEqual(UDFValue *,UDFValue *);
   bool                           EvaluateAndStoreInDataObject(Environment *,bool,EXPRESSION *,UDFValue *,bool);

#define CVIsType(cv,cvType) ((1 << (((TypeHeader *) (cv)->value)->type)) & (cvType))

#define ValueIsType(value,vType) ((1 << (((TypeHeader *) value)->type)) & (vType))

#define CVCoerceToFloat(cv) (((cv)->header->type == FLOAT_TYPE) ? \
                             ((cv)->floatValue->contents) : \
                             ((double) (cv)->integerValue->contents))

#define CVCoerceToInteger(cv) (((cv)->header->type == INTEGER_TYPE) ? \
                               ((cv)->integerValue->contents) : \
                               ((long long) (cv)->floatValue->contents))

#endif /* _H_evaluatn */
