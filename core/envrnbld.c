   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/18/16             */
   /*                                                     */
   /*             ENVIRONMENT BUILD MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for supporting multiple environments.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Added to separate environment creation and     */
/*            deletion code.                                 */
/*                                                           */
/*************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "setup.h"

#include "bmathfun.h"
#include "commline.h"
#include "emathfun.h"
#include "envrnmnt.h"
#include "engine.h"
#include "filecom.h"
#include "iofun.h"
#include "memalloc.h"
#include "miscfun.h"
#include "multifun.h"
#include "parsefun.h"
#include "pprint.h"
#include "prccode.h"
#include "prcdrfun.h"
#include "prdctfun.h"
#include "prntutil.h"
#include "proflfun.h"
#include "router.h"
#include "sortfun.h"
#include "strngfun.h"
#include "sysdep.h"
#include "textpro.h"
#include "utility.h"
#include "watch.h"

#if DEFFACTS_CONSTRUCT
#include "dffctdef.h"
#endif

#if DEFRULE_CONSTRUCT
#include "ruledef.h"
#endif

#if DEFGENERIC_CONSTRUCT
#include "genrccom.h"
#endif

#if DEFFUNCTION_CONSTRUCT
#include "dffnxfun.h"
#endif

#if DEFGLOBAL_CONSTRUCT
#include "globldef.h"
#endif

#if DEFTEMPLATE_CONSTRUCT
#include "tmpltdef.h"
#endif

#if OBJECT_SYSTEM
#include "classini.h"
#endif

#if DEVELOPER
#include "developr.h"
#endif

#include "envrnbld.h"

/****************************************/
/* GLOBAL EXTERNAL FUNCTION DEFINITIONS */
/****************************************/

   extern void                    EnvUserFunctions(Environment *);

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    RemoveEnvironmentCleanupFunctions(struct environmentData *);
   static Environment            *CreateEnvironmentDriver(CLIPSLexeme **,CLIPSFloat **,
                                                          CLIPSInteger **,struct bitMapHashNode **,
                                                          struct externalAddressHashNode **,
                                                          struct functionDefinition *);
   static void                    SystemFunctionDefinitions(Environment *);
   static void                    InitializeKeywords(Environment *);
   static void                    InitializeEnvironment(Environment *,CLIPSLexeme **,CLIPSFloat **,
					   								       CLIPSInteger **,struct bitMapHashNode **,
														   struct externalAddressHashNode **,
                                                           struct functionDefinition *);

/************************************************************/
/* CreateEnvironment: Creates an environment data structure */
/*   and initializes its content to zero/null.              */
/************************************************************/
Environment *CreateEnvironment()
  {
   return CreateEnvironmentDriver(NULL,NULL,NULL,NULL,NULL,NULL);
  }

/**********************************************************/
/* CreateRuntimeEnvironment: Creates an environment data  */
/*   structure and initializes its content to zero/null.  */
/**********************************************************/
Environment *CreateRuntimeEnvironment(
  CLIPSLexeme **symbolTable,
  CLIPSFloat **floatTable,
  CLIPSInteger **integerTable,
  struct bitMapHashNode **bitmapTable,
  struct functionDefinition *functions)
  {
   return CreateEnvironmentDriver(symbolTable,floatTable,integerTable,bitmapTable,NULL,functions);
  }

/*********************************************************/
/* CreateEnvironmentDriver: Creates an environment data  */
/*   structure and initializes its content to zero/null. */
/*********************************************************/
Environment *CreateEnvironmentDriver(
  CLIPSLexeme **symbolTable,
  CLIPSFloat **floatTable,
  CLIPSInteger **integerTable,
  struct bitMapHashNode **bitmapTable,
  struct externalAddressHashNode **externalAddressTable,
  struct functionDefinition *functions)
  {
   struct environmentData *theEnvironment;
   void *theData;

   theEnvironment = (struct environmentData *) malloc(sizeof(struct environmentData));

   if (theEnvironment == NULL)
     {
      printf("\n[ENVRNMNT5] Unable to create new environment.\n");
      return NULL;
     }

   theData = malloc(sizeof(void *) * MAXIMUM_ENVIRONMENT_POSITIONS);

   if (theData == NULL)
     {
      free(theEnvironment);
      printf("\n[ENVRNMNT6] Unable to create environment data.\n");
      return NULL;
     }

   memset(theData,0,sizeof(void *) * MAXIMUM_ENVIRONMENT_POSITIONS);

   theEnvironment->initialized = false;
   theEnvironment->theData = (void **) theData;
   theEnvironment->next = NULL;
   theEnvironment->listOfCleanupEnvironmentFunctions = NULL;
   theEnvironment->context = NULL;
   theEnvironment->functionContext = NULL;

   /*=============================================*/
   /* Allocate storage for the cleanup functions. */
   /*=============================================*/

   theData = malloc(sizeof(void (*)(struct environmentData *)) * MAXIMUM_ENVIRONMENT_POSITIONS);

   if (theData == NULL)
     {
      free(theEnvironment->theData);
      free(theEnvironment);
      printf("\n[ENVRNMNT7] Unable to create environment data.\n");
      return NULL;
     }

   memset(theData,0,sizeof(void (*)(struct environmentData *)) * MAXIMUM_ENVIRONMENT_POSITIONS);
   theEnvironment->cleanupFunctions = (void (**)(Environment *))theData;

   InitializeEnvironment(theEnvironment,symbolTable,floatTable,integerTable,
                         bitmapTable,externalAddressTable,functions);

   return theEnvironment;
  }

/**********************************************/
/* DestroyEnvironment: Destroys the specified */
/*   environment returning all of its memory. */
/**********************************************/
bool DestroyEnvironment(
  Environment *theEnvironment)
  {
   struct environmentCleanupFunction *cleanupPtr;
   int i;
   struct memoryData *theMemData;
   bool rv = true;
/*
   if (EvaluationData(theEnvironment)->CurrentExpression != NULL)
     { return false; }

#if DEFRULE_CONSTRUCT
   if (EngineData(theEnvironment)->ExecutingRule != NULL)
     { return false; }
#endif
*/
   theMemData = MemoryData(theEnvironment);

   ReleaseMem(theEnvironment,-1);

   for (i = 0; i < MAXIMUM_ENVIRONMENT_POSITIONS; i++)
     {
      if (theEnvironment->cleanupFunctions[i] != NULL)
        { (*theEnvironment->cleanupFunctions[i])(theEnvironment); }
     }

   free(theEnvironment->cleanupFunctions);

   for (cleanupPtr = theEnvironment->listOfCleanupEnvironmentFunctions;
        cleanupPtr != NULL;
        cleanupPtr = cleanupPtr->next)
     { (*cleanupPtr->func)(theEnvironment); }

   RemoveEnvironmentCleanupFunctions(theEnvironment);

   ReleaseMem(theEnvironment,-1);

   if ((theMemData->MemoryAmount != 0) || (theMemData->MemoryCalls != 0))
     {
      printf("\n[ENVRNMNT8] Environment data not fully deallocated.\n");
      printf("\n[ENVRNMNT8] MemoryAmount = %ld.\n",(long) theMemData->MemoryAmount);
      printf("\n[ENVRNMNT8] MemoryCalls = %ld.\n",(long) theMemData->MemoryCalls);
      rv = false;
     }

#if (MEM_TABLE_SIZE > 0)
   free(theMemData->MemoryTable);
#endif

   for (i = 0; i < MAXIMUM_ENVIRONMENT_POSITIONS; i++)
     {
      if (theEnvironment->theData[i] != NULL)
        {
         free(theEnvironment->theData[i]);
         theEnvironment->theData[i] = NULL;
        }
     }

   free(theEnvironment->theData);

   free(theEnvironment);

   return rv;
  }

/**************************************************/
/* RemoveEnvironmentCleanupFunctions: Removes the */
/*   list of environment cleanup functions.       */
/**************************************************/
static void RemoveEnvironmentCleanupFunctions(
  struct environmentData *theEnv)
  {
   struct environmentCleanupFunction *nextPtr;

   while (theEnv->listOfCleanupEnvironmentFunctions != NULL)
     {
      nextPtr = theEnv->listOfCleanupEnvironmentFunctions->next;
      free(theEnv->listOfCleanupEnvironmentFunctions);
      theEnv->listOfCleanupEnvironmentFunctions = nextPtr;
     }
  }

/**************************************************/
/* InitializeEnvironment: Performs initialization */
/*   of the KB environment.                       */
/**************************************************/
static void InitializeEnvironment(
  Environment *theEnvironment,
  CLIPSLexeme **symbolTable,
  CLIPSFloat **floatTable,
  CLIPSInteger **integerTable,
  struct bitMapHashNode **bitmapTable,
  struct externalAddressHashNode **externalAddressTable,
  struct functionDefinition *functions)
  {
   /*================================================*/
   /* Don't allow the initialization to occur twice. */
   /*================================================*/

   if (theEnvironment->initialized) return;

   /*================================*/
   /* Initialize the memory manager. */
   /*================================*/

   InitializeMemory(theEnvironment);

   /*===================================================*/
   /* Initialize environment data for various features. */
   /*===================================================*/

   InitializeCommandLineData(theEnvironment);
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   InitializeConstructCompilerData(theEnvironment);
#endif
   InitializeConstructData(theEnvironment);
   InitializeEvaluationData(theEnvironment);
   InitializeExternalFunctionData(theEnvironment);
   InitializePrettyPrintData(theEnvironment);
   InitializePrintUtilityData(theEnvironment);
   InitializeScannerData(theEnvironment);
   InitializeSystemDependentData(theEnvironment);
   InitializeUserDataData(theEnvironment);
   InitializeUtilityData(theEnvironment);
#if DEBUGGING_FUNCTIONS
   InitializeWatchData(theEnvironment);
#endif

   /*===============================================*/
   /* Initialize the hash tables for atomic values. */
   /*===============================================*/

   InitializeAtomTables(theEnvironment,symbolTable,floatTable,integerTable,bitmapTable,externalAddressTable);

   /*=========================================*/
   /* Initialize file and string I/O routers. */
   /*=========================================*/

   InitializeDefaultRouters(theEnvironment);

   /*=========================================================*/
   /* Initialize some system dependent features such as time. */
   /*=========================================================*/

   InitializeNonportableFeatures(theEnvironment);

   /*=============================================*/
   /* Register system and user defined functions. */
   /*=============================================*/

   if (functions != NULL)
     { InstallFunctionList(theEnvironment,functions); }

   SystemFunctionDefinitions(theEnvironment);
   EnvUserFunctions(theEnvironment);

   /*====================================*/
   /* Initialize the constraint manager. */
   /*====================================*/

   InitializeConstraints(theEnvironment);

   /*==========================================*/
   /* Initialize the expression hash table and */
   /* pointers to specific functions.          */
   /*==========================================*/

   InitExpressionData(theEnvironment);

   /*===================================*/
   /* Initialize the construct manager. */
   /*===================================*/

#if ! RUN_TIME
   InitializeConstructs(theEnvironment);
#endif

   /*=====================================*/
   /* Initialize the defmodule construct. */
   /*=====================================*/

   AllocateDefmoduleGlobals(theEnvironment);

   /*===================================*/
   /* Initialize the defrule construct. */
   /*===================================*/

#if DEFRULE_CONSTRUCT
   InitializeDefrules(theEnvironment);
#endif

   /*====================================*/
   /* Initialize the deffacts construct. */
   /*====================================*/

#if DEFFACTS_CONSTRUCT
   InitializeDeffacts(theEnvironment);
#endif

   /*=====================================================*/
   /* Initialize the defgeneric and defmethod constructs. */
   /*=====================================================*/

#if DEFGENERIC_CONSTRUCT
   SetupGenericFunctions(theEnvironment);
#endif

   /*=======================================*/
   /* Initialize the deffunction construct. */
   /*=======================================*/

#if DEFFUNCTION_CONSTRUCT
   SetupDeffunctions(theEnvironment);
#endif

   /*=====================================*/
   /* Initialize the defglobal construct. */
   /*=====================================*/

#if DEFGLOBAL_CONSTRUCT
   InitializeDefglobals(theEnvironment);
#endif

   /*=======================================*/
   /* Initialize the deftemplate construct. */
   /*=======================================*/

#if DEFTEMPLATE_CONSTRUCT
   InitializeDeftemplates(theEnvironment);
#endif

   /*=============================*/
   /* Initialize COOL constructs. */
   /*=============================*/

#if OBJECT_SYSTEM
   SetupObjectSystem(theEnvironment);
#endif

   /*=====================================*/
   /* Initialize the defmodule construct. */
   /*=====================================*/

   InitializeDefmodules(theEnvironment);

   /*======================================================*/
   /* Register commands and functions for development use. */
   /*======================================================*/

#if DEVELOPER
   DeveloperCommands(theEnvironment);
#endif

   /*=========================================*/
   /* Install the special function primitives */
   /* used by procedural code in constructs.  */
   /*=========================================*/

   InstallProcedurePrimitives(theEnvironment);

   /*==============================================*/
   /* Install keywords in the symbol table so that */
   /* they are available for command completion.   */
   /*==============================================*/

   InitializeKeywords(theEnvironment);

   /*========================*/
   /* Issue a clear command. */
   /*========================*/

   Clear(theEnvironment);

   /*=============================*/
   /* Initialization is complete. */
   /*=============================*/

   theEnvironment->initialized = true;
  }

/**************************************************/
/* SystemFunctionDefinitions: Sets up definitions */
/*   of system defined functions.                 */
/**************************************************/
static void SystemFunctionDefinitions(
  Environment *theEnv)
  {
   ProceduralFunctionDefinitions(theEnv);
   MiscFunctionDefinitions(theEnv);

#if IO_FUNCTIONS
   IOFunctionDefinitions(theEnv);
#endif

   PredicateFunctionDefinitions(theEnv);
   BasicMathFunctionDefinitions(theEnv);
   FileCommandDefinitions(theEnv);
   SortFunctionDefinitions(theEnv);

#if DEBUGGING_FUNCTIONS
   WatchFunctionDefinitions(theEnv);
#endif

#if MULTIFIELD_FUNCTIONS
   MultifieldFunctionDefinitions(theEnv);
#endif

#if STRING_FUNCTIONS
   StringFunctionDefinitions(theEnv);
#endif

#if EXTENDED_MATH_FUNCTIONS
   ExtendedMathFunctionDefinitions(theEnv);
#endif

#if TEXTPRO_FUNCTIONS
   HelpFunctionDefinitions(theEnv);
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   ConstructsToCCommandDefinition(theEnv);
#endif

#if PROFILING_FUNCTIONS
   ConstructProfilingFunctionDefinitions(theEnv);
#endif

   ParseFunctionDefinitions(theEnv);
  }

/*********************************************/
/* InitializeKeywords: Adds key words to the */
/*   symbol table so that they are available */
/*   for command completion.                 */
/*********************************************/
static void InitializeKeywords(
  Environment *theEnv)
  {
#if (! RUN_TIME) && WINDOW_INTERFACE
   void *ts;

   /*====================*/
   /* construct keywords */
   /*====================*/

   ts = CreateSymbol(theEnv,"defrule");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"defglobal");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"deftemplate");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"deffacts");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"deffunction");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"defmethod");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"defgeneric");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"defclass");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"defmessage-handler");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"definstances");
   IncrementSymbolCount(ts);

   /*=======================*/
   /* set-strategy keywords */
   /*=======================*/

   ts = CreateSymbol(theEnv,"depth");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"breadth");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"lex");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"mea");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"simplicity");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"complexity");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"random");
   IncrementSymbolCount(ts);

   /*==================================*/
   /* set-salience-evaluation keywords */
   /*==================================*/

   ts = CreateSymbol(theEnv,"when-defined");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"when-activated");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"every-cycle");
   IncrementSymbolCount(ts);

   /*======================*/
   /* deftemplate keywords */
   /*======================*/

   ts = CreateSymbol(theEnv,"field");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"multifield");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"default");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"type");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-symbols");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-strings");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-numbers");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-integers");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-floats");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-values");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"min-number-of-elements");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"max-number-of-elements");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"NONE");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"VARIABLE");
   IncrementSymbolCount(ts);

   /*==================*/
   /* defrule keywords */
   /*==================*/

   ts = CreateSymbol(theEnv,"declare");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"salience");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"test");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"or");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"and");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"not");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"logical");
   IncrementSymbolCount(ts);

   /*===============*/
   /* COOL keywords */
   /*===============*/

   ts = CreateSymbol(theEnv,"is-a");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"role");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"abstract");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"concrete");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"pattern-match");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"reactive");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"non-reactive");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"slot");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"field");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"multiple");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"single");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"storage");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"shared");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"local");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"access");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"read");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"write");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"read-only");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"read-write");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"initialize-only");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"propagation");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"inherit");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"no-inherit");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"source");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"composite");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"exclusive");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-lexemes");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"allowed-instances");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"around");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"before");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"primary");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"after");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"of");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"self");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"visibility");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"override-message");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"private");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"public");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"create-accessor");
   IncrementSymbolCount(ts);

   /*================*/
   /* watch keywords */
   /*================*/

   ts = CreateSymbol(theEnv,"compilations");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"deffunctions");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"globals");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"rules");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"activations");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"statistics");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"facts");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"generic-functions");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"methods");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"instances");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"slots");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"messages");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"message-handlers");
   IncrementSymbolCount(ts);
   ts = CreateSymbol(theEnv,"focus");
   IncrementSymbolCount(ts);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

