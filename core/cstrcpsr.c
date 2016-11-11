   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.50  10/01/16             */
   /*                                                     */
   /*              CONSTRUCT PARSER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parsing routines and utilities for parsing       */
/*   constructs.                                             */
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
/*            Made the construct redefinition message more   */
/*            prominent.                                     */
/*                                                           */
/*            Added pragmas to remove compilation warnings.  */
/*                                                           */
/*      6.30: Added code for capturing errors/warnings.      */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW, MAC_MCW, */
/*            and IBM_TBC).                                  */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            GetConstructNameAndComment API change.         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
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
/*            Changed return values for router functions.    */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Added CLIPSBlockStart and CLIPSBlockEnd        */
/*            functions for garbage collection blocks.       */
/*                                                           */
/*      6.50: File name/line count displayed for errors      */
/*            and warnings during load command.              */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "envrnmnt.h"
#include "router.h"
#include "watch.h"
#include "constrct.h"
#include "prcdrpsr.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "modulutl.h"
#include "modulpsr.h"
#include "pprint.h"
#include "prntutil.h"
#include "sysdep.h"
#include "utility.h"

#include "cstrcpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static bool                    FindConstructBeginning(Environment *,const char *,struct token *,bool,bool *);

/************************************************************/
/* EnvLoad: C access routine for the load command. Returns  */
/*   0 if the file couldn't be opened, -1 if the file was   */
/*   opened but an error occurred while loading constructs, */
/*   and 1 if the file was opened and no errors occured     */
/*   while loading.                                         */
/************************************************************/
int EnvLoad(
  Environment *theEnv,
  const char *fileName)
  {
   FILE *theFile;
   char *oldParsingFileName;
   int noErrorsDetected;

   /*=======================================*/
   /* Open the file specified by file name. */
   /*=======================================*/

   if ((theFile = GenOpen(theEnv,fileName,"r")) == NULL)
     { return 0; }

   /*===================================================*/
   /* Read in the constructs. Enabling fast load allows */
   /* the router system to be bypassed for quicker load */
   /* times.                                            */
   /*===================================================*/

   SetFastLoad(theEnv,theFile);

   oldParsingFileName = CopyString(theEnv,EnvGetParsingFileName(theEnv));
   EnvSetParsingFileName(theEnv,fileName);

   SetLoadInProgress(theEnv,true);
   noErrorsDetected = LoadConstructsFromLogicalName(theEnv,(char *) theFile);
   SetLoadInProgress(theEnv,false);

   EnvSetParsingFileName(theEnv,oldParsingFileName);
   DeleteString(theEnv,oldParsingFileName);

   EnvSetWarningFileName(theEnv,NULL);
   EnvSetErrorFileName(theEnv,NULL);

   SetFastLoad(theEnv,NULL);

   /*=================*/
   /* Close the file. */
   /*=================*/

   GenClose(theEnv,theFile);

   /*========================================*/
   /* If no errors occurred during the load, */
   /* return 1, otherwise return -1.         */
   /*========================================*/

   if (noErrorsDetected)
     { return 1; }

   return -1;
  }

/*******************************************************/
/* EnvSetParsingFileName: Sets the file name currently */
/*   being parsed by the load/batch command.           */
/*******************************************************/
void EnvSetParsingFileName(
  Environment *theEnv,
  const char *fileName)
  {
   char *fileNameCopy = NULL;

   if (fileName != NULL)
     {
      fileNameCopy = (char *) genalloc(theEnv,strlen(fileName) + 1);
      genstrcpy(fileNameCopy,fileName);
     }

   if (ConstructData(theEnv)->ParsingFileName != NULL)
     { genfree(theEnv,ConstructData(theEnv)->ParsingFileName,strlen(ConstructData(theEnv)->ParsingFileName) + 1); }

   ConstructData(theEnv)->ParsingFileName = fileNameCopy;
  }

/**********************************************************/
/* EnvGetParsingFileName: Returns the file name currently */
/*   being parsed by the load/batch command.              */
/**********************************************************/
char *EnvGetParsingFileName(
  Environment *theEnv)
  {
   return ConstructData(theEnv)->ParsingFileName;
  }

/**********************************************/
/* EnvSetErrorFileName: Sets the file name    */
/*   associated with the last error detected. */
/**********************************************/
void EnvSetErrorFileName(
  Environment *theEnv,
  const char *fileName)
  {
   char *fileNameCopy = NULL;

   if (fileName != NULL)
     {
      fileNameCopy = (char *) genalloc(theEnv,strlen(fileName) + 1);
      genstrcpy(fileNameCopy,fileName);
     }

   if (ConstructData(theEnv)->ErrorFileName != NULL)
     { genfree(theEnv,ConstructData(theEnv)->ErrorFileName,strlen(ConstructData(theEnv)->ErrorFileName) + 1); }

   ConstructData(theEnv)->ErrorFileName = fileNameCopy;
  }

/**********************************************/
/* EnvGetErrorFileName: Returns the file name */
/*   associated with the last error detected. */
/**********************************************/
char *EnvGetErrorFileName(
  Environment *theEnv)
  {
   return ConstructData(theEnv)->ErrorFileName;
  }

/************************************************/
/* EnvSetWarningFileName: Sets the file name    */
/*   associated with the last warning detected. */
/************************************************/
void EnvSetWarningFileName(
  Environment *theEnv,
  const char *fileName)
  {
   char *fileNameCopy = NULL;

   if (fileName != NULL)
     {
      fileNameCopy = (char *) genalloc(theEnv,strlen(fileName) + 1);
      genstrcpy(fileNameCopy,fileName);
     }

   if (ConstructData(theEnv)->WarningFileName != NULL)
     { genfree(theEnv,ConstructData(theEnv)->WarningFileName,strlen(ConstructData(theEnv)->WarningFileName) + 1); }

   ConstructData(theEnv)->WarningFileName = fileNameCopy;
  }

/************************************************/
/* EnvGetWarningFileName: Returns the file name */
/*   associated with the last warning detected. */
/************************************************/
char *EnvGetWarningFileName(
  Environment *theEnv)
  {
   return ConstructData(theEnv)->WarningFileName;
  }

/*****************************************************************/
/* LoadConstructsFromLogicalName: Loads a set of constructs into */
/*   the current environment from a specified logical name.      */
/*****************************************************************/
int LoadConstructsFromLogicalName(
  Environment *theEnv,
  const char *readSource)
  {
   int constructFlag;
   struct token theToken;
   bool noErrors = true;
   bool foundConstruct;
   CLIPSBlock gcBlock;
   long oldLineCountValue;
   const char *oldLineCountRouter;

   /*===================================================*/
   /* Create a router to capture the error information. */
   /*===================================================*/

   CreateErrorCaptureRouter(theEnv);

   /*==============================*/
   /* Initialize the line counter. */
   /*==============================*/

   oldLineCountValue = SetLineCount(theEnv,1);
   oldLineCountRouter = RouterData(theEnv)->LineCountRouter;
   RouterData(theEnv)->LineCountRouter = readSource;

   /*=========================================*/
   /* Reset the halt execution and evaluation */
   /* error flags in preparation for parsing. */
   /*=========================================*/

   if (UtilityData(theEnv)->CurrentGarbageFrame->topLevel) EnvSetHaltExecution(theEnv,false);
   EnvSetEvaluationError(theEnv,false);

   /*==========================================*/
   /* Set up the frame for garbage collection. */
   /*==========================================*/

   CLIPSBlockStart(theEnv,&gcBlock);

   /*========================================================*/
   /* Find the beginning of the first construct in the file. */
   /*========================================================*/

   GetToken(theEnv,readSource,&theToken);
   foundConstruct = FindConstructBeginning(theEnv,readSource,&theToken,false,&noErrors);

   /*==================================================*/
   /* Parse the file until the end of file is reached. */
   /*==================================================*/

   while ((foundConstruct == true) && (EnvGetHaltExecution(theEnv) == false))
     {
      /*===========================================================*/
      /* Clear the pretty print buffer in preparation for parsing. */
      /*===========================================================*/

      FlushPPBuffer(theEnv);

      /*======================*/
      /* Parse the construct. */
      /*======================*/

      constructFlag = ParseConstruct(theEnv,theToken.lexemeValue->contents,readSource);

      /*==============================================================*/
      /* If an error occurred while parsing, then find the beginning  */
      /* of the next construct (but don't generate any more error     */
      /* messages--in effect, skip everything until another construct */
      /* is found).                                                   */
      /*==============================================================*/

      if (constructFlag == 1)
        {
         EnvPrintRouter(theEnv,WERROR,"\nERROR:\n");
         PrintInChunks(theEnv,WERROR,GetPPBuffer(theEnv));
         EnvPrintRouter(theEnv,WERROR,"\n");

         FlushParsingMessages(theEnv);

         noErrors = false;
         GetToken(theEnv,readSource,&theToken);
         foundConstruct = FindConstructBeginning(theEnv,readSource,&theToken,true,&noErrors);
        }

      /*======================================================*/
      /* Otherwise, find the beginning of the next construct. */
      /*======================================================*/

      else
        {
         FlushParsingMessages(theEnv);
         GetToken(theEnv,readSource,&theToken);
         foundConstruct = FindConstructBeginning(theEnv,readSource,&theToken,false,&noErrors);
        }

      /*=====================================================*/
      /* Yield time if necessary to foreground applications. */
      /*=====================================================*/

      if (foundConstruct)
         { IncrementSymbolCount(theToken.value); }

      CleanCurrentGarbageFrame(theEnv,NULL);
      CallPeriodicTasks(theEnv);

      YieldTime(theEnv);

      if (foundConstruct)
         { DecrementSymbolCount(theEnv,theToken.lexemeValue); }
     }

   /*========================================================*/
   /* Print a carriage return if a single character is being */
   /* printed to indicate constructs are being processed.    */
   /*========================================================*/

#if DEBUGGING_FUNCTIONS
   if ((EnvGetWatchItem(theEnv,"compilations") != 1) && GetPrintWhileLoading(theEnv))
#else
   if (GetPrintWhileLoading(theEnv))
#endif
     { EnvPrintRouter(theEnv,WDIALOG,"\n"); }

   /*=============================================================*/
   /* Once the load is complete, destroy the pretty print buffer. */
   /* This frees up any memory that was used to create the pretty */
   /* print forms for constructs during parsing. Thus calls to    */
   /* the mem-used function will accurately reflect the amount of */
   /* memory being used after a load command.                     */
   /*=============================================================*/

   DestroyPPBuffer(theEnv);

   /*======================================*/
   /* Remove the garbage collection frame. */
   /*======================================*/

   CLIPSBlockEnd(theEnv,&gcBlock,NULL);
   CallPeriodicTasks(theEnv);

   /*==============================*/
   /* Deactivate the line counter. */
   /*==============================*/

   SetLineCount(theEnv,oldLineCountValue);
   RouterData(theEnv)->LineCountRouter = oldLineCountRouter;

   /*===========================================*/
   /* Invoke the parser error callback function */
   /* and delete the error capture router.      */
   /*===========================================*/

   FlushParsingMessages(theEnv);
   DeleteErrorCaptureRouter(theEnv);

   /*==========================================================*/
   /* Return a boolean flag which indicates whether any errors */
   /* were encountered while loading the constructs.           */
   /*==========================================================*/

   return(noErrors);
  }

/********************************************************************/
/* FindConstructBeginning: Searches for a left parenthesis followed */
/*   by the name of a valid construct. Used by the load command to  */
/*   find the next construct to be parsed. Returns true is the      */
/*   beginning of a construct was found, otherwise false.           */
/********************************************************************/
static bool FindConstructBeginning(
  Environment *theEnv,
  const char *readSource,
  struct token *theToken,
  bool errorCorrection,
  bool *noErrors)
  {
   bool leftParenthesisFound = false;
   bool firstAttempt = true;

   /*===================================================*/
   /* Process tokens until the beginning of a construct */
   /* is found or there are no more tokens.             */
   /*===================================================*/

   while (theToken->tknType != STOP_TOKEN)
     {
      /*=====================================================*/
      /* Constructs begin with a left parenthesis. Make note */
      /* that the opening parenthesis has been found.        */
      /*=====================================================*/

      if (theToken->tknType == LEFT_PARENTHESIS_TOKEN)
        { leftParenthesisFound = true; }

      /*=================================================================*/
      /* The name of the construct follows the opening left parenthesis. */
      /* If it is the name of a valid construct, then return true.       */
      /* Otherwise, reset the flags to look for the beginning of a       */
      /* construct. If error correction is being performed (i.e. the     */
      /* last construct parsed had an error in it), then don't bother to */
      /* print an error message, otherwise, print an error message.      */
      /*=================================================================*/

      else if ((theToken->tknType == SYMBOL_TOKEN) && (leftParenthesisFound == true))
        {
         /*===========================================================*/
         /* Is this a valid construct name (e.g., defrule, deffacts). */
         /*===========================================================*/

         if (FindConstruct(theEnv,theToken->lexemeValue->contents) != NULL) return true;

         /*===============================================*/
         /* The construct name is invalid. Print an error */
         /* message if one hasn't already been printed.   */
         /*===============================================*/

         if (firstAttempt && (! errorCorrection))
           {
            errorCorrection = true;
            *noErrors = false;
            PrintErrorID(theEnv,"CSTRCPSR",1,true);
            EnvPrintRouter(theEnv,WERROR,"Expected the beginning of a construct.\n");
           }

         /*======================================================*/
         /* Indicate that an error has been found and that we're */
         /* looking for a left parenthesis again.                */
         /*======================================================*/

         firstAttempt = false;
         leftParenthesisFound = false;
        }

      /*====================================================================*/
      /* Any token encountered other than a left parenthesis or a construct */
      /* name following a left parenthesis is illegal. Again, if error      */
      /* correction is in progress, no error message is printed, otherwise, */
      /*  an error message is printed.                                      */
      /*====================================================================*/

      else
        {
         if (firstAttempt && (! errorCorrection))
           {
            errorCorrection = true;
            *noErrors = false;
            PrintErrorID(theEnv,"CSTRCPSR",1,true);
            EnvPrintRouter(theEnv,WERROR,"Expected the beginning of a construct.\n");
           }

         firstAttempt = false;
         leftParenthesisFound = false;
        }

      /*============================================*/
      /* Move on to the next token to be processed. */
      /*============================================*/

      GetToken(theEnv,readSource,theToken);
     }

   /*===================================================================*/
   /* Couldn't find the beginning of a construct, so false is returned. */
   /*===================================================================*/

   return false;
  }

/*************************************************/
/* FindError: Find routine for the error router. */
/*************************************************/
static bool FindError(
  Environment *theEnv,
  const char *logicalName)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   if ((strcmp(logicalName,WERROR) == 0) ||
       (strcmp(logicalName,WWARNING) == 0))
     { return true; }

    return false;
  }

/***************************************************/
/* PrintError: Print routine for the error router. */
/***************************************************/
static void PrintError(
  Environment *theEnv,
  const char *logicalName,
  const char *str)
  {
   if (strcmp(logicalName,WERROR) == 0)
     {
      ConstructData(theEnv)->ErrorString =
         AppendToString(theEnv,str,ConstructData(theEnv)->ErrorString,
                                   &ConstructData(theEnv)->CurErrPos,
                                   &ConstructData(theEnv)->MaxErrChars);
     }
   else if (strcmp(logicalName,WWARNING) == 0)
     {
      ConstructData(theEnv)->WarningString =
         AppendToString(theEnv,str,ConstructData(theEnv)->WarningString,
                                   &ConstructData(theEnv)->CurWrnPos,
                                   &ConstructData(theEnv)->MaxWrnChars);
     }

   EnvDeactivateRouter(theEnv,"error-capture");
   EnvPrintRouter(theEnv,logicalName,str);
   EnvActivateRouter(theEnv,"error-capture");
  }

/***********************************************/
/* CreateErrorCaptureRouter: Creates the error */
/*   capture router if it doesn't exists.      */
/***********************************************/
void CreateErrorCaptureRouter(
  Environment *theEnv)
  {
   /*===========================================================*/
   /* Don't bother creating the error capture router if there's */
   /* no parser callback. The implication of this is that the   */
   /* parser callback should be created before any routines     */
   /* which could generate errors are called.                   */
   /*===========================================================*/

   if (ConstructData(theEnv)->ParserErrorCallback == NULL) return;

   /*=======================================================*/
   /* If the router hasn't already been created, create it. */
   /*=======================================================*/

   if (ConstructData(theEnv)->errorCaptureRouterCount == 0)
     {
      EnvAddRouter(theEnv,"error-capture", 40,
                      FindError, PrintError,
                      NULL, NULL,NULL);
     }

   /*==================================================*/
   /* Increment the count for the number of references */
   /* that want the error capture router functioning.  */
   /*==================================================*/

   ConstructData(theEnv)->errorCaptureRouterCount++;
  }

/***********************************************/
/* DeleteErrorCaptureRouter: Deletes the error */
/*   capture router if it exists.              */
/***********************************************/
void DeleteErrorCaptureRouter(
   Environment *theEnv)
   {
   /*===========================================================*/
   /* Don't bother deleting the error capture router if there's */
   /* no parser callback. The implication of this is that the   */
   /* parser callback should be created before any routines     */
   /* which could generate errors are called.                   */
   /*===========================================================*/

   if (ConstructData(theEnv)->ParserErrorCallback == NULL) return;

    ConstructData(theEnv)->errorCaptureRouterCount--;

    if (ConstructData(theEnv)->errorCaptureRouterCount == 0)
      { EnvDeleteRouter(theEnv,"error-capture"); }
   }

/*******************************************************/
/* FlushParsingMessages: Invokes the callback routines */
/*   for any existing warning/error messages.          */
/*******************************************************/
void FlushParsingMessages(
  Environment *theEnv)
  {
   /*===========================================================*/
   /* Don't bother flushing the error capture router if there's */
   /* no parser callback. The implication of this is that the   */
   /* parser callback should be created before any routines     */
   /* which could generate errors are called.                   */
   /*===========================================================*/

   if (ConstructData(theEnv)->ParserErrorCallback == NULL) return;

   /*=================================*/
   /* If an error occurred invoke the */
   /* parser error callback function. */
   /*=================================*/

   if (ConstructData(theEnv)->ErrorString != NULL)
     {
      (*ConstructData(theEnv)->ParserErrorCallback)(theEnv,EnvGetErrorFileName(theEnv),
                                                           NULL,ConstructData(theEnv)->ErrorString,
                                                           ConstructData(theEnv)->ErrLineNumber);
     }

   if (ConstructData(theEnv)->WarningString != NULL)
     {
      (*ConstructData(theEnv)->ParserErrorCallback)(theEnv,EnvGetWarningFileName(theEnv),
                                                           ConstructData(theEnv)->WarningString,NULL,
                                                           ConstructData(theEnv)->WrnLineNumber);
     }

   /*===================================*/
   /* Delete the error capture strings. */
   /*===================================*/

   EnvSetErrorFileName(theEnv,NULL);
   if (ConstructData(theEnv)->ErrorString != NULL)
     { genfree(theEnv,ConstructData(theEnv)->ErrorString,strlen(ConstructData(theEnv)->ErrorString) + 1); }
   ConstructData(theEnv)->ErrorString = NULL;
   ConstructData(theEnv)->CurErrPos = 0;
   ConstructData(theEnv)->MaxErrChars = 0;

   EnvSetWarningFileName(theEnv,NULL);
   if (ConstructData(theEnv)->WarningString != NULL)
     { genfree(theEnv,ConstructData(theEnv)->WarningString,strlen(ConstructData(theEnv)->WarningString) + 1); }
   ConstructData(theEnv)->WarningString = NULL;
   ConstructData(theEnv)->CurWrnPos = 0;
   ConstructData(theEnv)->MaxWrnChars = 0;
  }

/***********************************************************/
/* ParseConstruct: Parses a construct. Returns an integer. */
/*   -1 if the construct name has no parsing function, 0   */
/*   if the construct was parsed successfully, and 1 if    */
/*   the construct was parsed unsuccessfully.              */
/***********************************************************/
int ParseConstruct(
  Environment *theEnv,
  const char *name,
  const char *logicalName)
  {
   Construct *currentPtr;
   int rv;
   bool ov;
   CLIPSBlock gcBlock;

   /*=================================*/
   /* Look for a valid construct name */
   /* (e.g. defrule, deffacts).       */
   /*=================================*/

   currentPtr = FindConstruct(theEnv,name);
   if (currentPtr == NULL) return(-1);

   /*==========================================*/
   /* Set up the frame for garbage collection. */
   /*==========================================*/

   CLIPSBlockStart(theEnv,&gcBlock);

   /*==================================*/
   /* Prepare the parsing environment. */
   /*==================================*/

   ov = EnvGetHaltExecution(theEnv);
   EnvSetEvaluationError(theEnv,false);
   EnvSetHaltExecution(theEnv,false);
   ClearParsedBindNames(theEnv);
   PushRtnBrkContexts(theEnv);
   ExpressionData(theEnv)->ReturnContext = false;
   ExpressionData(theEnv)->BreakContext = false;

   /*=======================================*/
   /* Call the construct's parsing routine. */
   /*=======================================*/

   ConstructData(theEnv)->ParsingConstruct = true;
   
   if ((*currentPtr->parseFunction)(theEnv,logicalName))
     { rv = 1; }
   else
     { rv = 0; }
     
   ConstructData(theEnv)->ParsingConstruct = false;

   /*===============================*/
   /* Restore environment settings. */
   /*===============================*/

   PopRtnBrkContexts(theEnv);

   ClearParsedBindNames(theEnv);
   SetPPBufferStatus(theEnv,false);
   EnvSetHaltExecution(theEnv,ov);

   /*======================================*/
   /* Remove the garbage collection frame. */
   /*======================================*/

   CLIPSBlockEnd(theEnv,&gcBlock,NULL);
   CallPeriodicTasks(theEnv);

   /*==============================*/
   /* Return the status of parsing */
   /* the construct.               */
   /*==============================*/

   return rv;
  }

/******************************************************/
/* ImportExportConflictMessage: Generic error message */
/*   for an import/export module conflict detected    */
/*   when a construct is being defined.               */
/******************************************************/
void ImportExportConflictMessage(
  Environment *theEnv,
  const char *constructName,
  const char *itemName,
  const char *causedByConstruct,
  const char *causedByName)
  {
   PrintErrorID(theEnv,"CSTRCPSR",3,true);
   EnvPrintRouter(theEnv,WERROR,"Cannot define ");
   EnvPrintRouter(theEnv,WERROR,constructName);
   EnvPrintRouter(theEnv,WERROR," ");
   EnvPrintRouter(theEnv,WERROR,itemName);
   EnvPrintRouter(theEnv,WERROR," because of an import/export conflict");

   if (causedByConstruct == NULL) EnvPrintRouter(theEnv,WERROR,".\n");
   else
     {
      EnvPrintRouter(theEnv,WERROR," caused by the ");
      EnvPrintRouter(theEnv,WERROR,causedByConstruct);
      EnvPrintRouter(theEnv,WERROR," ");
      EnvPrintRouter(theEnv,WERROR,causedByName);
      EnvPrintRouter(theEnv,WERROR,".\n");
     }
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */


