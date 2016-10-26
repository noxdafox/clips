   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*         DEFMODULE BASIC COMMANDS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the defmodule       */
/*   construct such as clear, reset, save, ppdefmodule       */
/*   list-defmodules, and get-defmodule-list.                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include "setup.h"

#include <stdio.h>
#include <string.h>

#include "argacces.h"
#include "bload.h"
#include "constrct.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "modulbin.h"
#include "modulcmp.h"
#include "multifld.h"
#include "prntutil.h"
#include "router.h"

#include "modulbsc.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    ClearDefmodules(Environment *);
#if DEFMODULE_CONSTRUCT
   static void                    SaveDefmodules(Environment *,Defmodule *,const char *);
#endif

/*****************************************************************/
/* DefmoduleBasicCommands: Initializes basic defmodule commands. */
/*****************************************************************/
void DefmoduleBasicCommands(
  Environment *theEnv)
  {
   EnvAddClearFunction(theEnv,"defmodule",ClearDefmodules,2000);

#if DEFMODULE_CONSTRUCT
   AddSaveFunction(theEnv,"defmodule",SaveDefmodules,1100);

#if ! RUN_TIME
   EnvAddUDF(theEnv,"get-defmodule-list","m",0,0,NULL,GetDefmoduleListFunction,"GetDefmoduleListFunction",NULL);

#if DEBUGGING_FUNCTIONS
   EnvAddUDF(theEnv,"list-defmodules","v",0,0,NULL,ListDefmodulesCommand,"ListDefmodulesCommand",NULL);
   EnvAddUDF(theEnv,"ppdefmodule","v",1,1,"y",PPDefmoduleCommand,"PPDefmoduleCommand",NULL);
#endif
#endif
#endif

#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)
   DefmoduleBinarySetup(theEnv);
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   DefmoduleCompilerSetup(theEnv);
#endif
  }

/*********************************************************/
/* ClearDefmodules: Defmodule clear routine for use with */
/*   the clear command. Creates the MAIN module.         */
/*********************************************************/
static void ClearDefmodules(
  Environment *theEnv)
  {
#if (BLOAD || BLOAD_AND_BSAVE || BLOAD_ONLY) && (! RUN_TIME)
   if (Bloaded(theEnv) == true) return;
#endif
#if (! RUN_TIME)
   RemoveAllDefmodules(theEnv);

   CreateMainModule(theEnv);
   DefmoduleData(theEnv)->MainModuleRedefinable = true;
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }

#if DEFMODULE_CONSTRUCT

/******************************************/
/* SaveDefmodules: Defmodule save routine */
/*   for use with the save command.       */
/******************************************/
static void SaveDefmodules(
  Environment *theEnv,
  Defmodule *theModule,
  const char *logicalName)
  {
   const char *ppform;

   ppform = DefmodulePPForm(theModule);
   if (ppform != NULL)
     {
      PrintInChunks(theEnv,logicalName,ppform);
      EnvPrintRouter(theEnv,logicalName,"\n");
     }
  }

/************************************************/
/* GetDefmoduleListFunction: H/L access routine */
/*   for the get-defmodule-list function.       */
/************************************************/
void GetDefmoduleListFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   CLIPSValue result;
   
   EnvGetDefmoduleList(theEnv,&result);
   CLIPSToUDFValue(&result,returnValue);
  }

/******************************************/
/* EnvGetDefmoduleList: C access routine  */
/*   for the get-defmodule-list function. */
/******************************************/
void EnvGetDefmoduleList(
  Environment *theEnv,
  CLIPSValue *returnValue)
  {
   Defmodule *theConstruct;
   unsigned long count = 0;
   Multifield *theList;

   /*====================================*/
   /* Determine the number of constructs */
   /* of the specified type.             */
   /*====================================*/

   for (theConstruct = EnvGetNextDefmodule(theEnv,NULL);
        theConstruct != NULL;
        theConstruct = EnvGetNextDefmodule(theEnv,theConstruct))
     { count++; }

   /*===========================*/
   /* Create a multifield large */
   /* enough to store the list. */
   /*===========================*/

   theList = EnvCreateMultifield(theEnv,count);
   returnValue->value = theList;

   /*====================================*/
   /* Store the names in the multifield. */
   /*====================================*/

   for (theConstruct = EnvGetNextDefmodule(theEnv,NULL), count = 0;
        theConstruct != NULL;
        theConstruct = EnvGetNextDefmodule(theEnv,theConstruct), count++)
     {
      if (EvaluationData(theEnv)->HaltExecution == true)
        {
         returnValue->multifieldValue = EnvCreateMultifield(theEnv,0L);
         return;
        }
      theList->theFields[count].lexemeValue = EnvCreateSymbol(theEnv,DefmoduleName(theConstruct));
     }
  }

#if DEBUGGING_FUNCTIONS

/********************************************/
/* PPDefmoduleCommand: H/L access routine   */
/*   for the ppdefmodule command.           */
/********************************************/
void PPDefmoduleCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   const char *defmoduleName;

   defmoduleName = GetConstructName(context,"ppdefmodule","defmodule name");
   if (defmoduleName == NULL) return;

   PPDefmodule(theEnv,defmoduleName,WDISPLAY);

   return;
  }

/*************************************/
/* PPDefmodule: C access routine for */
/*   the ppdefmodule command.        */
/*************************************/
bool PPDefmodule(
  Environment *theEnv,
  const char *defmoduleName,
  const char *logicalName)
  {
   Defmodule *defmodulePtr;

   defmodulePtr = EnvFindDefmodule(theEnv,defmoduleName);
   if (defmodulePtr == NULL)
     {
      CantFindItemErrorMessage(theEnv,"defmodule",defmoduleName);
      return false;
     }

   if (DefmodulePPForm(defmodulePtr) == NULL) return true;
   PrintInChunks(theEnv,logicalName,DefmodulePPForm(defmodulePtr));

   return true;
  }

/***********************************************/
/* ListDefmodulesCommand: H/L access routine   */
/*   for the list-defmodules command.          */
/***********************************************/
void ListDefmodulesCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   EnvListDefmodules(theEnv,WDISPLAY);
  }

/***************************************/
/* EnvListDefmodules: C access routine */
/*   for the list-defmodules command.  */
/***************************************/
void EnvListDefmodules(
  Environment *theEnv,
  const char *logicalName)
  {
   Defmodule *theModule;
   int count = 0;

   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
    {
     EnvPrintRouter(theEnv,logicalName,DefmoduleName(theModule));
     EnvPrintRouter(theEnv,logicalName,"\n");
     count++;
    }

   PrintTally(theEnv,logicalName,count,"defmodule","defmodules");
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* DEFMODULE_CONSTRUCT */


