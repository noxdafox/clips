   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                 FILE COMMANDS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for file commands including    */
/*   batch, dribble-on, dribble-off, save, load, bsave, and  */
/*   bload.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Bebe Ly                                              */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added code for capturing errors/warnings.      */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*            Added STDOUT and STDIN logical name            */
/*            definitions.                                   */
/*                                                           */
/*      6.40: Split inputSource to fileSource and            */
/*            logicalSource.                                 */
/*                                                           */
/*            Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Changed return values for router functions.    */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <string.h>

#include "setup.h"

#include "argacces.h"
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
#include "bload.h"
#include "bsave.h"
#endif
#include "cstrcpsr.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "fileutil.h"
#include "memalloc.h"
#include "router.h"
#include "sysdep.h"
#include "utility.h"

#include "filecom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DeallocateFileCommandData(Environment *);

/***************************************/
/* FileCommandDefinitions: Initializes */
/*   file commands.                    */
/***************************************/
void FileCommandDefinitions(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,FILECOM_DATA,sizeof(struct fileCommandData),DeallocateFileCommandData);

#if ! RUN_TIME
#if DEBUGGING_FUNCTIONS
   EnvAddUDF(theEnv,"batch","b",1,1,"sy",BatchCommand,"BatchCommand",NULL);
   EnvAddUDF(theEnv,"batch*","b",1,1,"sy",BatchStarCommand,"BatchStarCommand",NULL);
   EnvAddUDF(theEnv,"dribble-on","b",1,1,"sy",DribbleOnCommand,"DribbleOnCommand",NULL);
   EnvAddUDF(theEnv,"dribble-off","b",0,0,NULL,DribbleOffCommand,"DribbleOffCommand",NULL);
   EnvAddUDF(theEnv,"save","b",1,1,"sy",SaveCommand,"SaveCommand",NULL);
#endif
   EnvAddUDF(theEnv,"load","b",1,1,"sy",LoadCommand,"LoadCommand",NULL);
   EnvAddUDF(theEnv,"load*","b",1,1,"sy",LoadStarCommand,"LoadStarCommand",NULL);
#if BLOAD_AND_BSAVE
   EnvAddUDF(theEnv,"bsave","b",1,1,"sy",BsaveCommand,"BsaveCommand",NULL);
#endif
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE
   InitializeBsaveData(theEnv);
   InitializeBloadData(theEnv);
   EnvAddUDF(theEnv,"bload","b",1,1,"sy",BloadCommand,"BloadCommand",NULL);
#endif
#endif
  }

/******************************************************/
/* DeallocateFileCommandData: Deallocates environment */
/*    data for file commands.                         */
/******************************************************/
static void DeallocateFileCommandData(
  Environment *theEnv)
  {
   struct batchEntry *theEntry, *nextEntry;

   theEntry = FileCommandData(theEnv)->TopOfBatchList;
   while (theEntry != NULL)
     {
      nextEntry = theEntry->next;

      if (theEntry->batchType == FILE_BATCH)
        { GenClose(theEnv,FileCommandData(theEnv)->TopOfBatchList->fileSource); }
      else
        { rm(theEnv,(void *) theEntry->theString,strlen(theEntry->theString) + 1); }

      DeleteString(theEnv,(char *) theEntry->logicalSource);
      rtn_struct(theEnv,batchEntry,theEntry);

      theEntry = nextEntry;
     }

   if (FileCommandData(theEnv)->BatchBuffer != NULL)
     { rm(theEnv,FileCommandData(theEnv)->BatchBuffer,FileCommandData(theEnv)->BatchMaximumPosition); }

   DeleteString(theEnv,FileCommandData(theEnv)->batchPriorParsingFile);
   FileCommandData(theEnv)->batchPriorParsingFile = NULL;

#if DEBUGGING_FUNCTIONS
   if (FileCommandData(theEnv)->DribbleBuffer != NULL)
     { rm(theEnv,FileCommandData(theEnv)->DribbleBuffer,FileCommandData(theEnv)->DribbleMaximumPosition); }

   if (FileCommandData(theEnv)->DribbleFP != NULL)
     { GenClose(theEnv,FileCommandData(theEnv)->DribbleFP); }
#endif
  }

#if DEBUGGING_FUNCTIONS

/******************************************/
/* DribbleOnCommand: H/L access routine   */
/*   for the dribble-on command.          */
/******************************************/
void DribbleOnCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   const char *fileName;

   if ((fileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvDribbleOn(theEnv,fileName));
  }

/*******************************************/
/* DribbleOffCommand: H/L access  routine  */
/*   for the dribble-off command.          */
/*******************************************/
void DribbleOffCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvDribbleOff(theEnv));
  }

#endif /* DEBUGGING_FUNCTIONS */

/**************************************/
/* BatchCommand: H/L access routine   */
/*   for the batch command.           */
/**************************************/
void BatchCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   const char *fileName;

   if ((fileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,OpenBatch(theEnv,fileName,false));
  }

/******************************************/
/* BatchStarCommand: H/L access routine   */
/*   for the batch* command.              */
/******************************************/
void BatchStarCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   const char *fileName;

   if ((fileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,EnvBatchStar(theEnv,fileName));
  }

/***********************************************************/
/* LoadCommand: H/L access routine for the load command.   */
/***********************************************************/
void LoadCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
#if (! BLOAD_ONLY) && (! RUN_TIME)
   const char *theFileName;
   int rv;

   if ((theFileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   SetPrintWhileLoading(theEnv,true);

   if ((rv = EnvLoad(theEnv,theFileName)) == 0)
     {
      SetPrintWhileLoading(theEnv,false);
      OpenErrorMessage(theEnv,"load",theFileName);
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   SetPrintWhileLoading(theEnv,false);

   if (rv == -1) returnValue->lexemeValue = FalseSymbol(theEnv);
   else returnValue->lexemeValue = TrueSymbol(theEnv);
#else
   EnvPrintRouter(theEnv,WDIALOG,"Load is not available in this environment\n");
   returnValue->lexemeValue = FalseSymbol(theEnv);
#endif
  }

/****************************************************************/
/* LoadStarCommand: H/L access routine for the load* command.   */
/****************************************************************/
void LoadStarCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
#if (! BLOAD_ONLY) && (! RUN_TIME)
   const char *theFileName;
   int rv;

   if ((theFileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   if ((rv = EnvLoad(theEnv,theFileName)) == 0)
     {
      OpenErrorMessage(theEnv,"load*",theFileName);
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   if (rv == -1) returnValue->lexemeValue = FalseSymbol(theEnv);
   else returnValue->lexemeValue = TrueSymbol(theEnv);
#else
   EnvPrintRouter(theEnv,WDIALOG,"Load* is not available in this environment\n");
   returnValue->lexemeValue = FalseSymbol(theEnv);
#endif
  }

#if DEBUGGING_FUNCTIONS
/*********************************************************/
/* SaveCommand: H/L access routine for the save command. */
/*********************************************************/
void SaveCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
#if (! BLOAD_ONLY) && (! RUN_TIME)
   const char *theFileName;

   if ((theFileName = GetFileName(context)) == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   if (EnvSave(theEnv,theFileName) == false)
     {
      OpenErrorMessage(theEnv,"save",theFileName);
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   returnValue->lexemeValue = TrueSymbol(theEnv);
#else
   EnvPrintRouter(theEnv,WDIALOG,"Save is not available in this environment\n");
   returnValue->lexemeValue = FalseSymbol(theEnv);
#endif
  }
#endif
