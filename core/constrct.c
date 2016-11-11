   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.50  08/25/16             */
   /*                                                     */
   /*                  CONSTRUCT MODULE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides basic functionality for creating new    */
/*   types of constructs, saving constructs to a file, and   */
/*   adding new functionality to the clear and reset         */
/*   commands.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed garbage collection algorithm.          */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added code for capturing errors/warnings       */
/*            (EnvSetParserErrorCallback).                   */
/*                                                           */
/*            Fixed issue with save function when multiple   */
/*            defmodules exist.                              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
/*                                                           */
/*            Added code to keep track of pointers to        */
/*            constructs that are contained externally to    */
/*            to constructs, DanglingConstructs.             */
/*                                                           */
/*      6.40: Added Env prefix to GetHaltExecution and       */
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
/*      6.50: Modified EnvClear to return completion status. */
/*                                                           */
/*            Compilation watch flag defaults to off.        */
/*                                                           */
/*            File name/line count displayed for errors      */
/*            and warnings during load command.              */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <string.h>

#include "setup.h"

#include "argacces.h"
#include "commline.h"
#include "constant.h"
#include "cstrcpsr.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "moduldef.h"
#include "modulutl.h"
#include "multifld.h"
#include "prcdrfun.h"
#include "prcdrpsr.h"
#include "prntutil.h"
#include "router.h"
#include "ruledef.h"
#include "scanner.h"
#include "sysdep.h"
#include "utility.h"
#include "watch.h"

#include "constrct.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                        DeallocateConstructData(Environment *);

/**************************************************/
/* InitializeConstructData: Allocates environment */
/*    data for constructs.                        */
/**************************************************/
void InitializeConstructData(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,CONSTRUCT_DATA,sizeof(struct constructData),DeallocateConstructData);

#if (! RUN_TIME) && (! BLOAD_ONLY)
   ConstructData(theEnv)->WatchCompilations = true;
#endif
  }

/****************************************************/
/* DeallocateConstructData: Deallocates environment */
/*    data for constructs.                          */
/****************************************************/
static void DeallocateConstructData(
  Environment *theEnv)
  {
   Construct *tmpPtr, *nextPtr;

#if (! RUN_TIME) && (! BLOAD_ONLY)
   DeallocateSaveCallList(theEnv,ConstructData(theEnv)->ListOfSaveFunctions);
#endif
   DeallocateVoidCallList(theEnv,ConstructData(theEnv)->ListOfResetFunctions);
   DeallocateVoidCallList(theEnv,ConstructData(theEnv)->ListOfClearFunctions);
   DeallocateBoolCallList(theEnv,ConstructData(theEnv)->ListOfClearReadyFunctions);

#if (! RUN_TIME) && (! BLOAD_ONLY)
   if (ConstructData(theEnv)->ErrorString != NULL)
     { genfree(theEnv,ConstructData(theEnv)->ErrorString,sizeof(ConstructData(theEnv)->ErrorString) + 1); }

   if (ConstructData(theEnv)->WarningString != NULL)
     { genfree(theEnv,ConstructData(theEnv)->WarningString,sizeof(ConstructData(theEnv)->WarningString) + 1); }

   ConstructData(theEnv)->ErrorString = NULL;
   ConstructData(theEnv)->WarningString = NULL;

   EnvSetParsingFileName(theEnv,NULL);
   EnvSetWarningFileName(theEnv,NULL);
   EnvSetErrorFileName(theEnv,NULL);
#endif

   tmpPtr = ConstructData(theEnv)->ListOfConstructs;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rtn_struct(theEnv,construct,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/**************************************************/
/* EnvSetParserErrorCallback: Allows the function */
/*   which is called when a construct parsing     */
/*    error occurs to be changed.                 */
/**************************************************/
ParserErrorFunction *EnvSetParserErrorCallback(
   Environment *theEnv,
   ParserErrorFunction *functionPtr)
  {
   ParserErrorFunction *tmpPtr;

   tmpPtr = ConstructData(theEnv)->ParserErrorCallback;
   ConstructData(theEnv)->ParserErrorCallback = functionPtr;
   return(tmpPtr);
  }

/*************************************************/
/* FindConstruct: Determines whether a construct */
/*   type is in the ListOfConstructs.            */
/*************************************************/
Construct *FindConstruct(
  Environment *theEnv,
  const char *name)
  {
   Construct *currentPtr;

   for (currentPtr = ConstructData(theEnv)->ListOfConstructs;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->constructName) == 0)
        { return currentPtr; }
     }

   return NULL;
  }

/***********************************************************/
/* RemoveConstruct: Removes a construct and its associated */
/*   parsing function from the ListOfConstructs. Returns   */
/*   true if the construct type was removed, otherwise     */
/*   false.                                                */
/***********************************************************/
bool RemoveConstruct(
  Environment *theEnv,
  const char *name)
  {
   Construct *currentPtr, *lastPtr = NULL;

   for (currentPtr = ConstructData(theEnv)->ListOfConstructs;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->constructName) == 0)
        {
         if (lastPtr == NULL)
           { ConstructData(theEnv)->ListOfConstructs = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }
         rtn_struct(theEnv,construct,currentPtr);
         return true;
        }

      lastPtr = currentPtr;
     }

   return false;
  }

/************************************************/
/* Save: C access routine for the save command. */
/************************************************/
bool EnvSave(
  Environment *theEnv,
  const char *fileName)
  {
   struct saveCallFunctionItem *saveFunction;
   FILE *filePtr;
   Defmodule *defmodulePtr;
   bool updated = false;
   bool unvisited = true;

   /*=====================*/
   /* Open the save file. */
   /*=====================*/

   if ((filePtr = GenOpen(theEnv,fileName,"w")) == NULL)
     { return false; }

   /*===========================*/
   /* Bypass the router system. */
   /*===========================*/

   SetFastSave(theEnv,filePtr);

   /*================================*/
   /* Mark all modules as unvisited. */
   /*================================*/

   MarkModulesAsUnvisited(theEnv);

   /*===============================================*/
   /* Save the constructs. Repeatedly loop over the */
   /* modules until each module has been save.      */
   /*===============================================*/

   while (unvisited)
     {
      unvisited = false;
      updated = false;

      for (defmodulePtr = EnvGetNextDefmodule(theEnv,NULL);
           defmodulePtr != NULL;
           defmodulePtr = EnvGetNextDefmodule(theEnv,defmodulePtr))
        {
         /*=================================================================*/
         /* We only want to save a module if all of the modules it imports  */
         /* from have already been saved. Since there can't be circular     */
         /* dependencies in imported modules, this should save the modules  */
         /* that don't import anything first and then work back from those. */
         /*=================================================================*/

         if (defmodulePtr->visitedFlag)
           { /* Module has already been saved. */ }
         else if (AllImportedModulesVisited(theEnv,defmodulePtr))
           {
            for (saveFunction = ConstructData(theEnv)->ListOfSaveFunctions;
                 saveFunction != NULL;
                 saveFunction = saveFunction->next)
              { (*saveFunction->func)(theEnv,defmodulePtr,(char *) filePtr); }

            updated = true;
            defmodulePtr->visitedFlag = true;
           }
         else
           { unvisited = true; }
        }

      /*=====================================================================*/
      /* At least one module should be saved in every pass. If all have been */
      /* visited/saved, then both flags will be false. If all remaining      */
      /* unvisited/unsaved modules were visited/saved, then unvisited will   */
      /* be false and updated will be true. If some, but not all, remaining  */
      /* unvisited/unsaved modules are visited/saved, then  unvisited will   */
      /* be true and updated will be true. This leaves the case where there  */
      /* are remaining unvisited/unsaved modules, but none were              */
      /* visited/saved: unvisited is true and updated is false.              */
      /*=====================================================================*/

      if (unvisited && (! updated))
        {
         SystemError(theEnv,"CONSTRCT",2);
         break;
        }
     }

   /*======================*/
   /* Close the save file. */
   /*======================*/

   GenClose(theEnv,filePtr);

   /*===========================*/
   /* Remove the router bypass. */
   /*===========================*/

   SetFastSave(theEnv,NULL);

   /*=========================*/
   /* Return true to indicate */
   /* successful completion.  */
   /*=========================*/

   return true;
  }

/*******************************************************/
/* RemoveSaveFunction: Removes a function from the     */
/*   ListOfSaveFunctions. Returns true if the function */
/*   was successfully removed, otherwise false.        */
/*******************************************************/
bool RemoveSaveFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   ConstructData(theEnv)->ListOfSaveFunctions =
     RemoveSaveFunctionFromCallList(theEnv,name,ConstructData(theEnv)->ListOfSaveFunctions,&found);

   if (found) return true;

   return false;
  }

/**********************************/
/* SetCompilationsWatch: Sets the */
/*   value of WatchCompilations.  */
/**********************************/
void SetCompilationsWatch(
  Environment *theEnv,
  bool value)
  {
   ConstructData(theEnv)->WatchCompilations = value;
  }

/*************************************/
/* GetCompilationsWatch: Returns the */
/*   value of WatchCompilations.     */
/*************************************/
bool GetCompilationsWatch(
  Environment *theEnv)
  {
   return ConstructData(theEnv)->WatchCompilations;
  }

/**********************************/
/* SetPrintWhileLoading: Sets the */
/*   value of PrintWhileLoading.  */
/**********************************/
void SetPrintWhileLoading(
  Environment *theEnv,
  bool value)
  {
   ConstructData(theEnv)->PrintWhileLoading = value;
  }

/*************************************/
/* GetPrintWhileLoading: Returns the */
/*   value of PrintWhileLoading.     */
/*************************************/
bool GetPrintWhileLoading(
  Environment *theEnv)
  {
   return(ConstructData(theEnv)->PrintWhileLoading);
  }

/*******************************/
/* SetLoadInProgress: Sets the */
/*   value of LoadInProgress.  */
/*******************************/
void SetLoadInProgress(
  Environment *theEnv,
  bool value)
  {
   ConstructData(theEnv)->LoadInProgress = value;
  }

/**********************************/
/* GetLoadInProgress: Returns the */
/*   value of LoadInProgress.     */
/**********************************/
bool GetLoadInProgress(
  Environment *theEnv)
  {
   return(ConstructData(theEnv)->LoadInProgress);
  }

#endif

/*************************************/
/* InitializeConstructs: Initializes */
/*   the Construct Manager.          */
/*************************************/
void InitializeConstructs(
  Environment *theEnv)
  {
#if (! RUN_TIME)
   EnvAddUDF(theEnv,"clear","v",0,0,NULL,ClearCommand,"ClearCommand",NULL);
   EnvAddUDF(theEnv,"reset","v",0,0,NULL,ResetCommand,"ResetCommand",NULL);

#if DEBUGGING_FUNCTIONS && (! BLOAD_ONLY)
   AddWatchItem(theEnv,"compilations",0,&ConstructData(theEnv)->WatchCompilations,30,NULL,NULL);
#endif
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/**************************************/
/* ClearCommand: H/L access routine   */
/*   for the clear command.           */
/**************************************/
void ClearCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   EnvClear(theEnv);
  }

/**************************************/
/* ResetCommand: H/L access routine   */
/*   for the reset command.           */
/**************************************/
void ResetCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   EnvReset(theEnv);
  }

/******************************/
/* EnvReset: C access routine */
/*   for the reset command.   */
/******************************/
void EnvReset(
  Environment *theEnv)
  {
   struct voidCallFunctionItem *resetPtr;

   /*=====================================*/
   /* The reset command can't be executed */
   /* while a reset is in progress.       */
   /*=====================================*/

   if (ConstructData(theEnv)->ResetInProgress) return;

   ConstructData(theEnv)->ResetInProgress = true;
   ConstructData(theEnv)->ResetReadyInProgress = true;

   /*================================================*/
   /* If the reset is performed from the top level   */
   /* command prompt, reset the halt execution flag. */
   /*================================================*/

   if (UtilityData(theEnv)->CurrentGarbageFrame->topLevel) EnvSetHaltExecution(theEnv,false);

   /*=======================================================*/
   /* Call the before reset function to determine if the    */
   /* reset should continue. [Used by the some of the       */
   /* windowed interfaces to query the user whether a       */
   /* reset should proceed with activations on the agenda.] */
   /*=======================================================*/

   if ((ConstructData(theEnv)->BeforeResetCallback != NULL) ?
       ((*ConstructData(theEnv)->BeforeResetCallback)(theEnv) == false) : false)
     {
      ConstructData(theEnv)->ResetReadyInProgress = false;
      ConstructData(theEnv)->ResetInProgress = false;
      return;
     }
   ConstructData(theEnv)->ResetReadyInProgress = false;

   /*===========================*/
   /* Call each reset function. */
   /*===========================*/

   for (resetPtr = ConstructData(theEnv)->ListOfResetFunctions;
        (resetPtr != NULL) && (EnvGetHaltExecution(theEnv) == false);
        resetPtr = resetPtr->next)
     { (*resetPtr->func)(theEnv); }

   /*============================================*/
   /* Set the current module to the MAIN module. */
   /*============================================*/

   EnvSetCurrentModule(theEnv,EnvFindDefmodule(theEnv,"MAIN"));

   /*===========================================*/
   /* Perform periodic cleanup if the reset was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     {
      CleanCurrentGarbageFrame(theEnv,NULL);
      CallPeriodicTasks(theEnv);
     }

   /*===================================*/
   /* A reset is no longer in progress. */
   /*===================================*/

   ConstructData(theEnv)->ResetInProgress = false;
  }

/************************************/
/* SetBeforeResetFunction: Sets the */
/*  value of BeforeResetFunction.   */
/************************************/
BeforeResetFunction *SetBeforeResetFunction(
  Environment *theEnv,
  BeforeResetFunction *theFunction)
  {
   BeforeResetFunction *tempFunction;

   tempFunction = ConstructData(theEnv)->BeforeResetCallback;
   ConstructData(theEnv)->BeforeResetCallback = theFunction;
   return tempFunction;
  }

/****************************************/
/* EnvAddResetFunction: Adds a function */
/*   to ListOfResetFunctions.           */
/****************************************/
bool EnvAddResetFunction(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *),
  int priority)
  {
   ConstructData(theEnv)->ListOfResetFunctions = AddVoidFunctionToCallList(theEnv,name,priority,
                                                functionPtr,
                                                ConstructData(theEnv)->ListOfResetFunctions);
   return true;
  }

/**********************************************/
/* EnvRemoveResetFunction: Removes a function */
/*   from the ListOfResetFunctions.           */
/**********************************************/
bool EnvRemoveResetFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   ConstructData(theEnv)->ListOfResetFunctions =
      RemoveVoidFunctionFromCallList(theEnv,name,ConstructData(theEnv)->ListOfResetFunctions,&found);

   return found;
  }

/*******************************************/
/* EnvIncrementClearReadyLocks: Increments */
/*   the number of clear ready locks.      */
/*******************************************/
void EnvIncrementClearReadyLocks(
  Environment *theEnv)
  {
   ConstructData(theEnv)->ClearReadyLocks++;
  }

/*******************************************/
/* EnvDecrementClearReadyLocks: Decrements */
/*   the number of clear locks.            */
/*******************************************/
void EnvDecrementClearReadyLocks(
  Environment *theEnv)
  {
   if (ConstructData(theEnv)->ClearReadyLocks > 0)
     { ConstructData(theEnv)->ClearReadyLocks--; }
  }

/*****************************************************/
/* EnvClear: C access routine for the clear command. */
/*****************************************************/
bool EnvClear(
  Environment *theEnv)
  {
   struct voidCallFunctionItem *theFunction;

   /*==========================================*/
   /* Activate the watch router which captures */
   /* trace output so that it is not displayed */
   /* during a clear.                          */
   /*==========================================*/

#if DEBUGGING_FUNCTIONS
   EnvActivateRouter(theEnv,WTRACE);
#endif

   /*===================================*/
   /* Determine if a clear is possible. */
   /*===================================*/

   ConstructData(theEnv)->ClearReadyInProgress = true;
   if ((ConstructData(theEnv)->ClearReadyLocks > 0) ||
       (ConstructData(theEnv)->DanglingConstructs > 0) ||
       (ClearReady(theEnv) == false))
     {
      PrintErrorID(theEnv,"CONSTRCT",1,false);
      EnvPrintRouter(theEnv,WERROR,"Some constructs are still in use. Clear cannot continue.\n");
#if DEBUGGING_FUNCTIONS
      EnvDeactivateRouter(theEnv,WTRACE);
#endif
      ConstructData(theEnv)->ClearReadyInProgress = false;
      return false;
     }
   ConstructData(theEnv)->ClearReadyInProgress = false;

   /*===========================*/
   /* Call all clear functions. */
   /*===========================*/

   ConstructData(theEnv)->ClearInProgress = true;

   for (theFunction = ConstructData(theEnv)->ListOfClearFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     { (*theFunction->func)(theEnv); }

   /*=============================*/
   /* Deactivate the watch router */
   /* for capturing output.       */
   /*=============================*/

#if DEBUGGING_FUNCTIONS
   EnvDeactivateRouter(theEnv,WTRACE);
#endif

   /*===========================================*/
   /* Perform periodic cleanup if the clear was */
   /* issued from an embedded controller.       */
   /*===========================================*/

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     {
      CleanCurrentGarbageFrame(theEnv,NULL);
      CallPeriodicTasks(theEnv);
     }

   /*===========================*/
   /* Clear has been completed. */
   /*===========================*/

   ConstructData(theEnv)->ClearInProgress = false;

#if DEFRULE_CONSTRUCT
   if ((DefruleData(theEnv)->RightPrimeJoins != NULL) ||
       (DefruleData(theEnv)->LeftPrimeJoins != NULL))
     { SystemError(theEnv,"CONSTRCT",1); }
#endif

   /*============================*/
   /* Perform reset after clear. */
   /*============================*/

   EnvReset(theEnv);

   return true;
  }

/*********************************************************/
/* ClearReady: Returns true if a clear can be performed, */
/*   otherwise false. Note that this is destructively    */
/*   determined (e.g. facts will be deleted as part of   */
/*   the determination).                                 */
/*********************************************************/
bool ClearReady(
  Environment *theEnv)
  {
   struct boolCallFunctionItem *theFunction;

   for (theFunction = ConstructData(theEnv)->ListOfClearReadyFunctions;
        theFunction != NULL;
        theFunction = theFunction->next)
     {
      if ((*theFunction->func)(theEnv) == false)
        { return false; }
     }

   return true;
  }

/******************************************/
/* AddClearReadyFunction: Adds a function */
/*   to ListOfClearReadyFunctions.        */
/******************************************/
bool AddClearReadyFunction(
  Environment *theEnv,
  const char *name,
  bool (*functionPtr)(Environment *),
  int priority)
  {
   ConstructData(theEnv)->ListOfClearReadyFunctions =
     AddBoolFunctionToCallList(theEnv,name,priority,functionPtr,
                               ConstructData(theEnv)->ListOfClearReadyFunctions);
   return true;
  }

/************************************************/
/* RemoveClearReadyFunction: Removes a function */
/*   from the ListOfClearReadyFunctions.        */
/************************************************/
bool RemoveClearReadyFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   ConstructData(theEnv)->ListOfClearReadyFunctions =
      RemoveBoolFunctionFromCallList(theEnv,name,ConstructData(theEnv)->ListOfClearReadyFunctions,&found);

   if (found) return true;

   return false;
  }

/****************************************/
/* EnvAddClearFunction: Adds a function */
/*   to ListOfClearFunctions.           */
/****************************************/
bool EnvAddClearFunction(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *),
  int priority)
  {
   ConstructData(theEnv)->ListOfClearFunctions =
      AddVoidFunctionToCallList(theEnv,name,priority,
                            functionPtr,
                            ConstructData(theEnv)->ListOfClearFunctions);
   return true;
  }

/**********************************************/
/* EnvRemoveClearFunction: Removes a function */
/*    from the ListOfClearFunctions.          */
/**********************************************/
bool EnvRemoveClearFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   ConstructData(theEnv)->ListOfClearFunctions =
     RemoveVoidFunctionFromCallList(theEnv,name,ConstructData(theEnv)->ListOfClearFunctions,&found);

   if (found) return true;

   return false;
  }

/********************************************/
/* ExecutingConstruct: Returns true if a    */
/*   construct is currently being executed, */
/*   otherwise false.                       */
/********************************************/
bool ExecutingConstruct(
  Environment *theEnv)
  {
   return ConstructData(theEnv)->Executing;
  }

/********************************************/
/* SetExecutingConstruct: Sets the value of */
/*   the executing variable indicating that */
/*   actions such as reset, clear, etc      */
/*   should not be performed.               */
/********************************************/
void SetExecutingConstruct(
  Environment *theEnv,
  bool value)
  {
   ConstructData(theEnv)->Executing = value;
  }

/*******************************************************/
/* DeinstallConstructHeader: Decrements the busy count */
/*   of a construct name and frees its pretty print    */
/*   representation string (both of which are stored   */
/*   in the generic construct header).                 */
/*******************************************************/
void DeinstallConstructHeader(
  Environment *theEnv,
  ConstructHeader *theHeader)
  {
   DecrementSymbolCount(theEnv,theHeader->name);
   if (theHeader->ppForm != NULL)
     {
      rm(theEnv,(void *) theHeader->ppForm,
         sizeof(char) * (strlen(theHeader->ppForm) + 1));
      theHeader->ppForm = NULL;
     }

   if (theHeader->usrData != NULL)
     {
      ClearUserDataList(theEnv,theHeader->usrData);
      theHeader->usrData = NULL;
     }
  }

/**************************************************/
/* DestroyConstructHeader: Frees the pretty print */
/*   representation string and user data (both of */
/*   which are stored in the generic construct    */
/*   header).                                     */
/**************************************************/
void DestroyConstructHeader(
  Environment *theEnv,
  ConstructHeader *theHeader)
  {
   if (theHeader->ppForm != NULL)
     {
      rm(theEnv,(void *) theHeader->ppForm,
         sizeof(char) * (strlen(theHeader->ppForm) + 1));
      theHeader->ppForm = NULL;
     }

   if (theHeader->usrData != NULL)
     {
      ClearUserDataList(theEnv,theHeader->usrData);
      theHeader->usrData = NULL;
     }
  }

/*****************************************************/
/* AddConstruct: Adds a construct and its associated */
/*   parsing function to the ListOfConstructs.       */
/*****************************************************/
Construct *AddConstruct(
  Environment *theEnv,
  const char *name,
  const char *pluralName,
  bool (*parseFunction)(Environment *,const char *),
  FindConstructFunction *findFunction,
  CLIPSLexeme *(*getConstructNameFunction)(ConstructHeader *),
  const char *(*getPPFormFunction)(ConstructHeader *),
  struct defmoduleItemHeader *(*getModuleItemFunction)(ConstructHeader *),
  GetNextConstructFunction *getNextItemFunction,
  void (*setNextItemFunction)(ConstructHeader *,ConstructHeader *),
  IsConstructDeletableFunction *isConstructDeletableFunction,
  DeleteConstructFunction *deleteFunction,
  FreeConstructFunction *freeFunction)
  {
   Construct *newPtr;

   /*=============================*/
   /* Allocate and initialize the */
   /* construct data structure.   */
   /*=============================*/

   newPtr = get_struct(theEnv,construct);

   newPtr->constructName = name;
   newPtr->pluralName = pluralName;
   newPtr->parseFunction = parseFunction;
   newPtr->findFunction = findFunction;
   newPtr->getConstructNameFunction = getConstructNameFunction;
   newPtr->getPPFormFunction = getPPFormFunction;
   newPtr->getModuleItemFunction = getModuleItemFunction;
   newPtr->getNextItemFunction = getNextItemFunction;
   newPtr->setNextItemFunction = setNextItemFunction;
   newPtr->isConstructDeletableFunction = isConstructDeletableFunction;
   newPtr->deleteFunction = deleteFunction;
   newPtr->freeFunction = freeFunction;

   /*===============================*/
   /* Add the construct to the list */
   /* of constructs and return it.  */
   /*===============================*/

   newPtr->next = ConstructData(theEnv)->ListOfConstructs;
   ConstructData(theEnv)->ListOfConstructs = newPtr;
   return(newPtr);
  }

/************************************/
/* AddSaveFunction: Adds a function */
/*   to the ListOfSaveFunctions.    */
/************************************/
bool AddSaveFunction(
  Environment *theEnv,
  const char *name,
  void (*functionPtr)(Environment *,Defmodule *,const char *),
  int priority)
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   ConstructData(theEnv)->ListOfSaveFunctions =
     AddSaveFunctionToCallList(theEnv,name,priority,
                           functionPtr,
                           ConstructData(theEnv)->ListOfSaveFunctions);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif

   return true;
  }

