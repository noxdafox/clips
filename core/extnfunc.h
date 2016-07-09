   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
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
/*************************************************************/

#ifndef _H_extnfunc

#pragma once

#define _H_extnfunc

struct FunctionDefinition;

#include "evaluatn.h"
#include "expressn.h"
#include "symbol.h"
#include "userdata.h"

struct FunctionDefinition
  {
   struct symbolHashNode *callFunctionName;
   const char *actualFunctionName;
   char returnValueType;
   int (*functionPointer)(void);
   struct expr *(*parser)(void *,struct expr *,const char *);
   struct symbolHashNode *restrictions;
   bool overloadable;
   bool sequenceuseok;
   bool environmentAware;
   short int bsaveIndex;
   struct FunctionDefinition *next;
   struct userData *usrData;
   void *context;
  };

#define ValueFunctionType(target) (((struct FunctionDefinition *) target)->returnValueType)
#define ExpressionFunctionType(target) (((struct FunctionDefinition *) ((target)->value))->returnValueType)
#define ExpressionFunctionPointer(target) (((struct FunctionDefinition *) ((target)->value))->functionPointer)
#define ExpressionFunctionCallName(target) (((struct FunctionDefinition *) ((target)->value))->callFunctionName)
#define ExpressionFunctionRealName(target) (((struct FunctionDefinition *) ((target)->value))->actualFunctionName)

#define PTIF (int (*)(void))
#define PTIEF (int (*)(void *))

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

#define EXTERNAL_FUNCTION_DATA 50

struct externalFunctionData
  {
   struct FunctionDefinition *ListOfFunctions;
   struct FunctionHash **FunctionHashtable;
  };

#define ExternalFunctionData(theEnv) ((struct externalFunctionData *) GetEnvironmentData(theEnv,EXTERNAL_FUNCTION_DATA))

struct FunctionHash
  {
   struct FunctionDefinition *fdPtr;
   struct FunctionHash *next;
  };

#define SIZE_FUNCTION_HASH 517

   void                           InitializeExternalFunctionData(void *);
   bool                           EnvDefineFunction(void *,const char *,int,
                                                           int (*)(void *),const char *);
   bool                           EnvDefineFunction2(void *,const char *,int,
                                                            int (*)(void *),const char *,const char *);
   bool                           EnvDefineFunctionWithContext(void *,const char *,int,
                                                           int (*)(void *),const char *,void *);
   bool                           EnvDefineFunction2WithContext(void *,const char *,int,
                                                            int (*)(void *),const char *,const char *,void *);
   bool                           DefineFunction3(void *,const char *,int,
                                                         int (*)(void *),const char *,const char *,bool,void *);
   bool                           AddFunctionParser(void *,const char *,
                                                           struct expr *(*)( void *,struct expr *,const char *));
   bool                           RemoveFunctionParser(void *,const char *);
   bool                           FuncSeqOvlFlags(void *,const char *,bool,bool);
   struct FunctionDefinition     *GetFunctionList(void *);
   void                           InstallFunctionList(void *,struct FunctionDefinition *);
   struct FunctionDefinition     *FindFunction(void *,const char *);
   int                            GetNthRestriction(struct FunctionDefinition *,int);
   const char                    *GetArgumentTypeName(int);
   bool                           UndefineFunction(void *,const char *);
   int                            GetMinimumArgs(struct FunctionDefinition *);
   int                            GetMaximumArgs(struct FunctionDefinition *);

#if ALLOW_ENVIRONMENT_GLOBALS

#if (! RUN_TIME)
   bool                           DefineFunction(const char *,int,int (*)(void),const char *);
   bool                           DefineFunction2(const char *,int,int (*)(void),const char *,const char *);
#endif /* (! RUN_TIME) */

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_extnfunc */



