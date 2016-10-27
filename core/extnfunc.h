   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/01/16            */
   /*                                                     */
   /*            EXTERNAL FUNCTIONS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for adding new user or system defined   */
/*   functions.                                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added support for passing context information  */
/*            to user defined functions.                     */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Replaced ALLOW_ENVIRONMENT_GLOBALS macros      */
/*            with functions.                                */
/*                                                           */
/*      6.40: Changed restrictions from char * to            */
/*            symbolHashNode * to support strings            */
/*            originating from sources that are not          */
/*            statically allocated.                          */
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
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_extnfunc

#pragma once

#define _H_extnfunc

struct FunctionDefinition;
struct UDFContext_t;
typedef struct UDFContext_t UDFContext;

#include "evaluatn.h"
#include "expressn.h"
#include "symbol.h"
#include "userdata.h"

struct FunctionDefinition
  {
   CLIPSLexeme *callFunctionName;
   const char *actualFunctionName;
   unsigned unknownReturnValueType;
   void (*functionPointer)(Environment *,UDFContext *,UDFValue *);
   struct expr *(*parser)(Environment *,struct expr *,const char *);
   CLIPSLexeme *restrictions;
   int minArgs;
   int maxArgs;
   bool overloadable;
   bool sequenceuseok;
   bool neededFunction;
   short int bsaveIndex;
   struct FunctionDefinition *next;
   struct userData *usrData;
   void *context;
  };

#define UnknownFunctionType(target) (((struct FunctionDefinition *) target)->unknownReturnValueType)
#define ExpressionFunctionPointer(target) ((target)->functionValue->functionPointer)
#define ExpressionFunctionCallName(target) ((target)->functionValue->callFunctionName)
#define ExpressionFunctionRealName(target) ((target)->functionValue->actualFunctionName)
#define ExpressionUnknownFunctionType(target) ((target)->functionValue->unknownReturnValueType)

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

#define EXTERNAL_FUNCTION_DATA 50

struct externalFunctionData
  {
   struct FunctionDefinition *ListOfFunctions;
   struct FunctionHash **FunctionHashtable;
  };

struct UDFContext_t
  {
   Environment *environment;
   struct FunctionDefinition *theFunction;
   int lastPosition;
   struct expr *lastArg;
   UDFValue *returnValue;
  };

#define ExternalFunctionData(theEnv) ((struct externalFunctionData *) GetEnvironmentData(theEnv,EXTERNAL_FUNCTION_DATA))

struct FunctionHash
  {
   struct FunctionDefinition *fdPtr;
   struct FunctionHash *next;
  };

#define SIZE_FUNCTION_HASH 517

   void                           InitializeExternalFunctionData(Environment *);
   bool                           EnvAddUDF(Environment *,const char *,const char *,
                                            int,int,const char *,
                                            void (*)(Environment *,UDFContext *,UDFValue *),
                                            const char *,void *);
   bool                           AddFunctionParser(Environment *,const char *,
                                                           struct expr *(*)( Environment *,struct expr *,const char *));
   bool                           RemoveFunctionParser(Environment *,const char *);
   bool                           FuncSeqOvlFlags(Environment *,const char *,bool,bool);
   struct FunctionDefinition     *GetFunctionList(Environment *);
   void                           InstallFunctionList(Environment *,struct FunctionDefinition *);
   struct FunctionDefinition     *FindFunction(Environment *,const char *);
   int                            GetNthRestriction(struct FunctionDefinition *,int);
   unsigned                       GetNthRestriction2(Environment *,struct FunctionDefinition *,int);
   const char                    *GetArgumentTypeName(int);
   bool                           EnvRemoveUDF(Environment *,const char *);
   int                            GetMinimumArgs(struct FunctionDefinition *);
   int                            GetMaximumArgs(struct FunctionDefinition *);
   int                            UDFArgumentCount(UDFContext *);
   bool                           UDFNthArgument(UDFContext *,int,unsigned,UDFValue *);
   void                           UDFInvalidArgumentMessage(UDFContext *,const char *);
   const char                    *UDFContextFunctionName(UDFContext *);
   void                           PrintTypesString(Environment *,const char *,unsigned,bool);
   bool                           UDFFirstArgument(UDFContext *,unsigned,UDFValue *);
   bool                           UDFNextArgument(UDFContext *,unsigned,UDFValue *);

#define UDFHasNextArgument(context) (context->lastArg != NULL)

#endif /* _H_extnfunc */



