   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
   /*                                                     */
   /*              DEFGLOBAL COMMANDS MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides the show-defglobals, set-reset-globals, */
/*   and get-reset-globals commands.                         */
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
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if DEFGLOBAL_CONSTRUCT

#include "argacces.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "globldef.h"
#include "prntutil.h"
#include "router.h"

#include "globlcom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
   static void                       PrintDefglobalValueForm(Environment *,const char *,Defglobal *);
#endif

/************************************************************/
/* DefglobalCommandDefinitions: Defines defglobal commands. */
/************************************************************/
void DefglobalCommandDefinitions(
  Environment *theEnv)
  {
#if ! RUN_TIME
   EnvDefineFunction2(theEnv,"set-reset-globals",'b',
                  PTIEF SetResetGlobalsCommand,"SetResetGlobalsCommand", "11");
   EnvDefineFunction2(theEnv,"get-reset-globals",'b',
                   PTIEF GetResetGlobalsCommand,"GetResetGlobalsCommand", "00");

#if DEBUGGING_FUNCTIONS
   EnvDefineFunction2(theEnv,"show-defglobals",'v',
                   PTIEF ShowDefglobalsCommand,"ShowDefglobalsCommand", "01w");
#endif

#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

/************************************************/
/* SetResetGlobalsCommand: H/L access routine   */
/*   for the get-reset-globals command.         */
/************************************************/
bool SetResetGlobalsCommand(
  Environment *theEnv)
  {
   bool oldValue;
   DATA_OBJECT arg_ptr;

   /*===========================================*/
   /* Remember the old value of this attribute. */
   /*===========================================*/

   oldValue = EnvGetResetGlobals(theEnv);

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   if (EnvArgCountCheck(theEnv,"set-reset-globals",EXACTLY,1) == -1)
     { return(oldValue); }

   /*===========================================*/
   /* Determine the new value of the attribute. */
   /*===========================================*/

   EnvRtnUnknown(theEnv,1,&arg_ptr);

   if ((arg_ptr.value == EnvFalseSymbol(theEnv)) && (arg_ptr.type == SYMBOL))
     { EnvSetResetGlobals(theEnv,false); }
   else
     { EnvSetResetGlobals(theEnv,true); }

   /*========================================*/
   /* Return the old value of the attribute. */
   /*========================================*/

   return(oldValue);
  }

/****************************************/
/* EnvSetResetGlobals: C access routine */
/*   for the set-reset-globals command. */
/****************************************/
bool EnvSetResetGlobals(
  Environment *theEnv,
  bool value)
  {
   bool ov;

   ov = DefglobalData(theEnv)->ResetGlobals;
   DefglobalData(theEnv)->ResetGlobals = value;
   return(ov);
  }

/************************************************/
/* GetResetGlobalsCommand: H/L access routine   */
/*   for the get-reset-globals command.         */
/************************************************/
bool GetResetGlobalsCommand(
  Environment *theEnv)
  {
   bool oldValue;

   oldValue = EnvGetResetGlobals(theEnv);

   if (EnvArgCountCheck(theEnv,"get-reset-globals",EXACTLY,0) == -1)
     { return(oldValue); }

   return(oldValue);
  }

/****************************************/
/* EnvGetResetGlobals: C access routine */
/*   for the get-reset-globals command. */
/****************************************/
bool EnvGetResetGlobals(
  Environment *theEnv)
  {   
   return(DefglobalData(theEnv)->ResetGlobals); 
  }

#if DEBUGGING_FUNCTIONS

/***********************************************/
/* ShowDefglobalsCommand: H/L access routine   */
/*   for the show-defglobals command.          */
/***********************************************/
void ShowDefglobalsCommand(
  Environment *theEnv)
  {
   Defmodule *theModule;
   int numArgs;
   bool error;

   if ((numArgs = EnvArgCountCheck(theEnv,"show-defglobals",NO_MORE_THAN,1)) == -1) return;

   if (numArgs == 1)
     {
      theModule = GetModuleName(theEnv,"show-defglobals",1,&error);
      if (error) return;
     }
   else
     { theModule = EnvGetCurrentModule(theEnv); }

   EnvShowDefglobals(theEnv,WDISPLAY,theModule);
  }

/***************************************/
/* EnvShowDefglobals: C access routine */
/*   for the show-defglobals command.  */
/***************************************/
void EnvShowDefglobals(
  Environment *theEnv,
  const char *logicalName,
  Defmodule *theModule)
  {
   struct constructHeader *constructPtr;
   bool allModules = false;
   struct defmoduleItemHeader *theModuleItem;

   /*=======================================*/
   /* If the module specified is NULL, then */
   /* list all constructs in all modules.   */
   /*=======================================*/

   if (theModule == NULL)
     {
      theModule = EnvGetNextDefmodule(theEnv,NULL);
      allModules = true;
     }

   /*======================================================*/
   /* Print out the constructs in the specified module(s). */
   /*======================================================*/

   for (;
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
     {
      /*===========================================*/
      /* Print the module name before every group  */
      /* of defglobals listed if we're listing the */
      /* defglobals from every module.             */
      /*===========================================*/

      if (allModules)
        {
         EnvPrintRouter(theEnv,logicalName,EnvGetDefmoduleName(theEnv,theModule));
         EnvPrintRouter(theEnv,logicalName,":\n");
        }

      /*=====================================*/
      /* Print every defglobal in the module */
      /* currently being examined.           */
      /*=====================================*/

      theModuleItem = (struct defmoduleItemHeader *) GetModuleItem(theEnv,theModule,DefglobalData(theEnv)->DefglobalModuleIndex);

      for (constructPtr = theModuleItem->firstItem;
           constructPtr != NULL;
           constructPtr = constructPtr->next)
        {
         if (EvaluationData(theEnv)->HaltExecution == true) return;

         if (allModules) EnvPrintRouter(theEnv,logicalName,"   ");
         PrintDefglobalValueForm(theEnv,logicalName,(Defglobal *) constructPtr);
         EnvPrintRouter(theEnv,logicalName,"\n");
        }

      /*===================================*/
      /* If we're only listing the globals */
      /* for one module, then return.      */
      /*===================================*/

      if (! allModules) return;
     }
  }

/*****************************************************/
/* PrintDefglobalValueForm: Prints the value form of */
/*   a defglobal (the current value). For example,   */
/*   ?*x* = 3                                        */
/*****************************************************/
static void PrintDefglobalValueForm(
  Environment *theEnv,
  const char *logicalName,
  Defglobal *theGlobal)
  {
   EnvPrintRouter(theEnv,logicalName,"?*");
   EnvPrintRouter(theEnv,logicalName,ValueToString(theGlobal->header.name));
   EnvPrintRouter(theEnv,logicalName,"* = ");
   PrintDataObject(theEnv,logicalName,&theGlobal->current);
  }

#endif /* DEBUGGING_FUNCTIONS */

/*#####################################*/
/* ALLOW_ENVIRONMENT_GLOBALS Functions */
/*#####################################*/

#if ALLOW_ENVIRONMENT_GLOBALS

bool GetResetGlobals()
  {   
   return EnvGetResetGlobals(GetCurrentEnvironment());
  }

bool SetResetGlobals(
  bool value)
  {
   return EnvSetResetGlobals(GetCurrentEnvironment(),value);
  }

#if DEBUGGING_FUNCTIONS

void ShowDefglobals(
  const char *logicalName,
  Defmodule *theModule)
  {
   EnvShowDefglobals(GetCurrentEnvironment(),logicalName,theModule);
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* DEFGLOBAL_CONSTRUCT */

