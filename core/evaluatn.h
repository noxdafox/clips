   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*************************************************************/

#ifndef _H_evaluatn

#pragma once

#define _H_evaluatn

struct entityRecord;
struct dataObject;

typedef struct dataObject DATA_OBJECT;
typedef struct dataObject * DATA_OBJECT_PTR;
typedef struct expr FUNCTION_REFERENCE;

typedef void EntityPrintFunction(Environment *,const char *,void *);
typedef bool EntityEvaluationFunction(Environment *,void *,DATA_OBJECT *);
typedef void EntityBusyCountFunction(Environment *,void *);

#include "constant.h"
#include "symbol.h"
#include "expressn.h"

struct dataObject
  {
   void *supplementalInfo;
   unsigned short type;
   void *value;
   long begin;
   long end;
   struct dataObject *next;
  };

#define DATA_OBJECT_PTR_ARG DATA_OBJECT_PTR

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

struct externalAddressType
  {
   const  char *name;
   void (*shortPrintFunction)(Environment *,const char *,void *);
   void (*longPrintFunction)(Environment *,const char *,void *);
   bool (*discardFunction)(Environment *,void *);
   void (*newFunction)(Environment *,DATA_OBJECT *);
   bool (*callFunction)(Environment *,DATA_OBJECT *,DATA_OBJECT *);
  };

typedef struct entityRecord ENTITY_RECORD;
typedef struct entityRecord * ENTITY_RECORD_PTR;

#define GetDOLength(target)       (((target).end - (target).begin) + 1)
#define GetpDOLength(target)      (((target)->end - (target)->begin) + 1)
#define GetDOBegin(target)        ((target).begin + 1)
#define GetDOEnd(target)          ((target).end + 1)
#define GetpDOBegin(target)       ((target)->begin + 1)
#define GetpDOEnd(target)         ((target)->end + 1)
#define SetDOBegin(target,val)   ((target).begin = (long) ((val) - 1))
#define SetDOEnd(target,val)     ((target).end = (long) ((val) - 1))
#define SetpDOBegin(target,val)   ((target)->begin = (long) ((val) - 1))
#define SetpDOEnd(target,val)     ((target)->end = (long) ((val) - 1))

#define EnvGetDOLength(theEnv,target)       (((target).end - (target).begin) + 1)
#define EnvGetpDOLength(theEnv,target)      (((target)->end - (target)->begin) + 1)
#define EnvGetDOBegin(theEnv,target)        ((target).begin + 1)
#define EnvGetDOEnd(theEnv,target)          ((target).end + 1)
#define EnvGetpDOBegin(theEnv,target)       ((target)->begin + 1)
#define EnvGetpDOEnd(theEnv,target)         ((target)->end + 1)
#define EnvSetDOBegin(theEnv,target,val)   ((target).begin = (long) ((val) - 1))
#define EnvSetDOEnd(theEnv,target,val)     ((target).end = (long) ((val) - 1))
#define EnvSetpDOBegin(theEnv,target,val)   ((target)->begin = (long) ((val) - 1))
#define EnvSetpDOEnd(theEnv,target,val)     ((target)->end = (long) ((val) - 1))

#define DOPToString(target) (((struct symbolHashNode *) ((target)->value))->contents)
#define DOPToDouble(target) (((struct floatHashNode *) ((target)->value))->contents)
#define DOPToFloat(target) ((float) (((struct floatHashNode *) ((target)->value))->contents))
#define DOPToLong(target) (((struct integerHashNode *) ((target)->value))->contents)
#define DOPToInteger(target) ((int) (((struct integerHashNode *) ((target)->value))->contents))
#define DOPToPointer(target)       ((target)->value)
#define DOPToExternalAddress(target) (((struct externalAddressHashNode *) ((target)->value))->externalAddress)

#define EnvDOPToString(theEnv,target) (((struct symbolHashNode *) ((target)->value))->contents)
#define EnvDOPToDouble(theEnv,target) (((struct floatHashNode *) ((target)->value))->contents)
#define EnvDOPToFloat(theEnv,target) ((float) (((struct floatHashNode *) ((target)->value))->contents))
#define EnvDOPToLong(theEnv,target) (((struct integerHashNode *) ((target)->value))->contents)
#define EnvDOPToInteger(theEnv,target) ((int) (((struct integerHashNode *) ((target)->value))->contents))
#define EnvDOPToPointer(theEnv,target)       ((target)->value)
#define EnvDOPToExternalAddress(target) (((struct externalAddressHashNode *) ((target)->value))->externalAddress)

#define DOToString(target) (((struct symbolHashNode *) ((target).value))->contents)
#define DOToDouble(target) (((struct floatHashNode *) ((target).value))->contents)
#define DOToFloat(target) ((float) (((struct floatHashNode *) ((target).value))->contents))
#define DOToLong(target) (((struct integerHashNode *) ((target).value))->contents)
#define DOToInteger(target) ((int) (((struct integerHashNode *) ((target).value))->contents))
#define DOToPointer(target)        ((target).value)
#define DOToExternalAddress(target) (((struct externalAddressHashNode *) ((target).value))->externalAddress)

#define EnvDOToString(theEnv,target) (((struct symbolHashNode *) ((target).value))->contents)
#define EnvDOToDouble(theEnv,target) (((struct floatHashNode *) ((target).value))->contents)
#define EnvDOToFloat(theEnv,target) ((float) (((struct floatHashNode *) ((target).value))->contents))
#define EnvDOToLong(theEnv,target) (((struct integerHashNode *) ((target).value))->contents)
#define EnvDOToInteger(theEnv,target) ((int) (((struct integerHashNode *) ((target).value))->contents))
#define EnvDOToPointer(theEnv,target)        ((target).value)
#define EnvDOToExternalAddress(target) (((struct externalAddressHashNode *) ((target).value))->externalAddress)

#define CoerceToLongInteger(t,v) ((t == INTEGER) ? ValueToLong(v) : (long int) ValueToDouble(v))
#define CoerceToInteger(t,v) ((t == INTEGER) ? (int) ValueToLong(v) : (int) ValueToDouble(v))
#define CoerceToDouble(t,v) ((t == INTEGER) ? (double) ValueToLong(v) : ValueToDouble(v))

#define GetFirstArgument()           (EvaluationData(theEnv)->CurrentExpression->argList)
#define GetNextArgument(ep)          (ep->nextArg)

#define MAXIMUM_PRIMITIVES 150
#define MAXIMUM_EXTERNAL_ADDRESS_TYPES 10

#define BITS_PER_BYTE    8

#define BitwiseTest(n,b)   ((n) & (char) (1 << (b)))
#define BitwiseSet(n,b)    (n |= (char) (1 << (b)))
#define BitwiseClear(n,b)  (n &= (char) ~(1 << (b)))

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

#include "factmngr.h"
#include "object.h"

   void                           InitializeEvaluationData(Environment *);
   bool                           EvaluateExpression(Environment *,struct expr *,struct dataObject *);
   void                           EnvSetEvaluationError(Environment *,bool);
   bool                           EnvGetEvaluationError(Environment *);
   void                           EnvSetHaltExecution(Environment *,bool);
   bool                           EnvGetHaltExecution(Environment *);
   void                           ReturnValues(Environment *,struct dataObject *,bool);
   void                           PrintDataObject(Environment *,const char *,struct dataObject *);
   void                           EnvSetMultifieldErrorValue(Environment *,struct dataObject *);
   void                           ValueInstall(Environment *,struct dataObject *);
   void                           ValueDeinstall(Environment *,struct dataObject *);
#if DEFFUNCTION_CONSTRUCT || DEFGENERIC_CONSTRUCT
   bool                           EnvFunctionCall(Environment *,const char *,const char *,DATA_OBJECT *);
   bool                           FunctionCall2(Environment *,FUNCTION_REFERENCE *,const char *,DATA_OBJECT *);
#endif
   void                           CopyDataObject(Environment *,DATA_OBJECT *,DATA_OBJECT *,int);
   void                           AtomInstall(Environment *,int,void *);
   void                           AtomDeinstall(Environment *,int,void *);
   struct expr                   *ConvertValueToExpression(Environment *,DATA_OBJECT *);
   unsigned long                  GetAtomicHashValue(unsigned short,void *,int);
   void                           InstallPrimitive(Environment *,struct entityRecord *,int);
   int                            InstallExternalAddressType(Environment *,struct externalAddressType *);
   void                           TransferDataObjectValues(DATA_OBJECT *,DATA_OBJECT *);
   struct expr                   *FunctionReferenceExpression(Environment *,const char *);
   bool                           GetFunctionReference(Environment *,const char *,FUNCTION_REFERENCE *);
   bool                           DOsEqual(DATA_OBJECT_PTR,DATA_OBJECT_PTR);
   bool                           EvaluateAndStoreInDataObject(Environment *,bool,EXPRESSION *,DATA_OBJECT *,bool);

#if ALLOW_ENVIRONMENT_GLOBALS

   void                           SetMultifieldErrorValue(DATA_OBJECT_PTR);
   bool                           FunctionCall(const char *,const char *,DATA_OBJECT *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_evaluatn */
