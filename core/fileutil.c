   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  11/01/16            */
   /*                                                     */
   /*                 FILE UTILITY MODULE                 */
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
/*      6.40: Split from filecom.c                           */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <string.h>

#include "setup.h"

#include "argacces.h"
#include "commline.h"
#include "cstrcpsr.h"
#include "memalloc.h"
#include "prcdrfun.h"
#include "pprint.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "strngrtr.h"
#include "sysdep.h"
#include "filecom.h"
#include "utility.h"

#include "fileutil.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if DEBUGGING_FUNCTIONS
   static bool                    FindDribble(Environment *,const char *);
   static int                     GetcDribble(Environment *,const char *);
   static int                     UngetcDribble(Environment *,int,const char *);
   static void                    ExitDribble(Environment *,int);
   static void                    PrintDribble(Environment *,const char *,const char *);
   static void                    PutcDribbleBuffer(Environment *,int);
#endif
   static bool                    FindBatch(Environment *,const char *);
   static int                     GetcBatch(Environment *,const char *);
   static int                     UngetcBatch(Environment *,int,const char *);
   static void                    ExitBatch(Environment *,int);
   static void                    AddBatch(Environment *,bool,FILE *,const char *,int,const char *,const char *);

#if DEBUGGING_FUNCTIONS
/*****************************************************/
/* FindDribble: Find routine for the dribble router. */
/*****************************************************/
static bool FindDribble(
  Environment *theEnv,
  const char *logicalName)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   if ( (strcmp(logicalName,STDOUT) == 0) ||
        (strcmp(logicalName,STDIN) == 0) ||
        (strcmp(logicalName,WPROMPT) == 0) ||
        (strcmp(logicalName,WTRACE) == 0) ||
        (strcmp(logicalName,WERROR) == 0) ||
        (strcmp(logicalName,WWARNING) == 0) ||
        (strcmp(logicalName,WDISPLAY) == 0) ||
        (strcmp(logicalName,WDIALOG) == 0) )
     { return true; }

    return false;
  }

/*******************************************************/
/* PrintDribble: Print routine for the dribble router. */
/*******************************************************/
static void PrintDribble(
  Environment *theEnv,
  const char *logicalName,
  const char *str)
  {
   int i;

   /*======================================*/
   /* Send the output to the dribble file. */
   /*======================================*/

   for (i = 0 ; str[i] != EOS ; i++)
     { PutcDribbleBuffer(theEnv,str[i]); }

   /*===========================================================*/
   /* Send the output to any routers interested in printing it. */
   /*===========================================================*/

   EnvDeactivateRouter(theEnv,"dribble");
   EnvPrintRouter(theEnv,logicalName,str);
   EnvActivateRouter(theEnv,"dribble");
  }

/*****************************************************/
/* GetcDribble: Getc routine for the dribble router. */
/*****************************************************/
static int GetcDribble(
  Environment *theEnv,
  const char *logicalName)
  {
   int rv;

   /*===========================================*/
   /* Deactivate the dribble router and get the */
   /* character from another active router.     */
   /*===========================================*/

   EnvDeactivateRouter(theEnv,"dribble");
   rv = EnvGetcRouter(theEnv,logicalName);
   EnvActivateRouter(theEnv,"dribble");

   /*==========================================*/
   /* Put the character retrieved from another */
   /* router into the dribble buffer.          */
   /*==========================================*/

   PutcDribbleBuffer(theEnv,rv);

   /*=======================*/
   /* Return the character. */
   /*=======================*/

   return(rv);
  }

/***********************************************************/
/* PutcDribbleBuffer: Putc routine for the dribble router. */
/***********************************************************/
static void PutcDribbleBuffer(
  Environment *theEnv,
  int rv)
  {
   /*===================================================*/
   /* Receiving an end-of-file character will cause the */
   /* contents of the dribble buffer to be flushed.     */
   /*===================================================*/

   if (rv == EOF)
     {
      if (FileCommandData(theEnv)->DribbleCurrentPosition > 0)
        {
         fprintf(FileCommandData(theEnv)->DribbleFP,"%s",FileCommandData(theEnv)->DribbleBuffer);
         FileCommandData(theEnv)->DribbleCurrentPosition = 0;
         FileCommandData(theEnv)->DribbleBuffer[0] = EOS;
        }
     }

   /*===========================================================*/
   /* If we aren't receiving command input, then the character  */
   /* just received doesn't need to be placed in the dribble    */
   /* buffer--It can be written directly to the file. This will */
   /* occur for example when the command prompt is being        */
   /* printed (the AwaitingInput variable will be false because */
   /* command input has not been receivied yet). Before writing */
   /* the character to the file, the dribble buffer is flushed. */
   /*===========================================================*/

   else if (RouterData(theEnv)->AwaitingInput == false)
     {
      if (FileCommandData(theEnv)->DribbleCurrentPosition > 0)
        {
         fprintf(FileCommandData(theEnv)->DribbleFP,"%s",FileCommandData(theEnv)->DribbleBuffer);
         FileCommandData(theEnv)->DribbleCurrentPosition = 0;
         FileCommandData(theEnv)->DribbleBuffer[0] = EOS;
        }

      fputc(rv,FileCommandData(theEnv)->DribbleFP);
     }

   /*=====================================================*/
   /* Otherwise, add the character to the dribble buffer. */
   /*=====================================================*/

   else
     {
      FileCommandData(theEnv)->DribbleBuffer = ExpandStringWithChar(theEnv,rv,FileCommandData(theEnv)->DribbleBuffer,
                                           &FileCommandData(theEnv)->DribbleCurrentPosition,
                                           &FileCommandData(theEnv)->DribbleMaximumPosition,
                                           FileCommandData(theEnv)->DribbleMaximumPosition+BUFFER_SIZE);
     }
  }

/*********************************************************/
/* UngetcDribble: Ungetc routine for the dribble router. */
/*********************************************************/
static int UngetcDribble(
  Environment *theEnv,
  int ch,
  const char *logicalName)
  {
   int rv;

   /*===============================================*/
   /* Remove the character from the dribble buffer. */
   /*===============================================*/

   if (FileCommandData(theEnv)->DribbleCurrentPosition > 0) FileCommandData(theEnv)->DribbleCurrentPosition--;
   FileCommandData(theEnv)->DribbleBuffer[FileCommandData(theEnv)->DribbleCurrentPosition] = EOS;

   /*=============================================*/
   /* Deactivate the dribble router and pass the  */
   /* ungetc request to the other active routers. */
   /*=============================================*/

   EnvDeactivateRouter(theEnv,"dribble");
   rv = EnvUngetcRouter(theEnv,ch,logicalName);
   EnvActivateRouter(theEnv,"dribble");

   /*==========================================*/
   /* Return the result of the ungetc request. */
   /*==========================================*/

   return(rv);
  }

/*****************************************************/
/* ExitDribble: Exit routine for the dribble router. */
/*****************************************************/
static void ExitDribble(
  Environment *theEnv,
  int num)
  {
#if MAC_XCD
#pragma unused(num)
#endif

   if (FileCommandData(theEnv)->DribbleCurrentPosition > 0)
     { fprintf(FileCommandData(theEnv)->DribbleFP,"%s",FileCommandData(theEnv)->DribbleBuffer); }

   if (FileCommandData(theEnv)->DribbleFP != NULL) GenClose(theEnv,FileCommandData(theEnv)->DribbleFP);
  }

/**********************************/
/* EnvDribbleOn: C access routine */
/*   for the dribble-on command.  */
/**********************************/
bool EnvDribbleOn(
  Environment *theEnv,
  const char *fileName)
  {
   /*==============================*/
   /* If a dribble file is already */
   /* open, then close it.         */
   /*==============================*/

   if (FileCommandData(theEnv)->DribbleFP != NULL)
     { EnvDribbleOff(theEnv); }

   /*========================*/
   /* Open the dribble file. */
   /*========================*/

   FileCommandData(theEnv)->DribbleFP = GenOpen(theEnv,fileName,"w");
   if (FileCommandData(theEnv)->DribbleFP == NULL)
     {
      OpenErrorMessage(theEnv,"dribble-on",fileName);
      return false;
     }

   /*============================*/
   /* Create the dribble router. */
   /*============================*/

   EnvAddRouter(theEnv,"dribble", 40,
             FindDribble, PrintDribble,
             GetcDribble, UngetcDribble,
             ExitDribble);

   FileCommandData(theEnv)->DribbleCurrentPosition = 0;

   /*================================================*/
   /* Call the dribble status function. This is used */
   /* by some of the machine specific interfaces to  */
   /* do things such as changing the wording of menu */
   /* items from "Turn Dribble On..." to             */
   /* "Turn Dribble Off..."                          */
   /*================================================*/

   if (FileCommandData(theEnv)->DribbleStatusFunction != NULL)
     { (*FileCommandData(theEnv)->DribbleStatusFunction)(theEnv,true); }

   /*=====================================*/
   /* Return true to indicate the dribble */
   /* file was successfully opened.       */
   /*=====================================*/

   return true;
  }

/*************************************************/
/* EnvDribbleActive: Returns true if the dribble */
/*   router is active, otherwise false.          */
/*************************************************/
bool EnvDribbleActive(
  Environment *theEnv)
  {
   if (FileCommandData(theEnv)->DribbleFP != NULL) return true;

   return false;
  }

/***********************************/
/* EnvDribbleOff: C access routine */
/*   for the dribble-off command.  */
/***********************************/
bool EnvDribbleOff(
  Environment *theEnv)
  {
   bool rv = false;

   /*================================================*/
   /* Call the dribble status function. This is used */
   /* by some of the machine specific interfaces to  */
   /* do things such as changing the wording of menu */
   /* items from "Turn Dribble On..." to             */
   /* "Turn Dribble Off..."                          */
   /*================================================*/

   if (FileCommandData(theEnv)->DribbleStatusFunction != NULL)
     { (*FileCommandData(theEnv)->DribbleStatusFunction)(theEnv,false); }

   /*=======================================*/
   /* Close the dribble file and deactivate */
   /* the dribble router.                   */
   /*=======================================*/

   if (FileCommandData(theEnv)->DribbleFP != NULL)
     {
      if (FileCommandData(theEnv)->DribbleCurrentPosition > 0)
        { fprintf(FileCommandData(theEnv)->DribbleFP,"%s",FileCommandData(theEnv)->DribbleBuffer); }
      EnvDeleteRouter(theEnv,"dribble");
      if (GenClose(theEnv,FileCommandData(theEnv)->DribbleFP) == 0) rv = true;
     }
   else
     { rv = true; }

   FileCommandData(theEnv)->DribbleFP = NULL;

   /*============================================*/
   /* Free the space used by the dribble buffer. */
   /*============================================*/

   if (FileCommandData(theEnv)->DribbleBuffer != NULL)
     {
      rm(theEnv,FileCommandData(theEnv)->DribbleBuffer,FileCommandData(theEnv)->DribbleMaximumPosition);
      FileCommandData(theEnv)->DribbleBuffer = NULL;
     }

   FileCommandData(theEnv)->DribbleCurrentPosition = 0;
   FileCommandData(theEnv)->DribbleMaximumPosition = 0;

   /*============================================*/
   /* Return true if the dribble file was closed */
   /* without error, otherwise return false.     */
   /*============================================*/

   return(rv);
  }
  
#endif /* DEBUGGING_FUNCTIONS */

/*************************************************/
/* FindBatch: Find routine for the batch router. */
/*************************************************/
static bool FindBatch(
  Environment *theEnv,
  const char *logicalName)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   if (strcmp(logicalName,STDIN) == 0)
     { return true; }

   return false;
  }

/*************************************************/
/* GetcBatch: Getc routine for the batch router. */
/*************************************************/
static int GetcBatch(
  Environment *theEnv,
  const char *logicalName)
  {
   return(LLGetcBatch(theEnv,logicalName,false));
  }

/***************************************************/
/* LLGetcBatch: Lower level routine for retrieving */
/*   a character when a batch file is active.      */
/***************************************************/
int LLGetcBatch(
  Environment *theEnv,
  const char *logicalName,
  bool returnOnEOF)
  {
   int rv = EOF, flag = 1;

   /*=================================================*/
   /* Get a character until a valid character appears */
   /* or no more batch files are left.                */
   /*=================================================*/

   while ((rv == EOF) && (flag == 1))
     {
      if (FileCommandData(theEnv)->BatchType == FILE_BATCH)
        { rv = getc(FileCommandData(theEnv)->BatchFileSource); }
      else
        { rv = EnvGetcRouter(theEnv,FileCommandData(theEnv)->BatchLogicalSource); }

      if (rv == EOF)
        {
         if (FileCommandData(theEnv)->BatchCurrentPosition > 0) EnvPrintRouter(theEnv,STDOUT,(char *) FileCommandData(theEnv)->BatchBuffer);
         flag = RemoveBatch(theEnv);
        }
     }

   /*=========================================================*/
   /* If the character retrieved is an end-of-file character, */
   /* then there are no batch files with character input      */
   /* remaining. Remove the batch router.                     */
   /*=========================================================*/

   if (rv == EOF)
     {
      if (FileCommandData(theEnv)->BatchCurrentPosition > 0) EnvPrintRouter(theEnv,STDOUT,(char *) FileCommandData(theEnv)->BatchBuffer);
      EnvDeleteRouter(theEnv,"batch");
      RemoveBatch(theEnv);
      if (returnOnEOF == true)
        { return (EOF); }
      else
        { return(EnvGetcRouter(theEnv,logicalName)); }
     }

   /*========================================*/
   /* Add the character to the batch buffer. */
   /*========================================*/

   FileCommandData(theEnv)->BatchBuffer = ExpandStringWithChar(theEnv,(char) rv,FileCommandData(theEnv)->BatchBuffer,&FileCommandData(theEnv)->BatchCurrentPosition,
                                      &FileCommandData(theEnv)->BatchMaximumPosition,FileCommandData(theEnv)->BatchMaximumPosition+BUFFER_SIZE);

   /*======================================*/
   /* If a carriage return is encountered, */
   /* then flush the batch buffer.         */
   /*======================================*/

   if ((char) rv == '\n')
     {
      EnvPrintRouter(theEnv,STDOUT,(char *) FileCommandData(theEnv)->BatchBuffer);
      FileCommandData(theEnv)->BatchCurrentPosition = 0;
      if ((FileCommandData(theEnv)->BatchBuffer != NULL) && (FileCommandData(theEnv)->BatchMaximumPosition > BUFFER_SIZE))
        {
         rm(theEnv,FileCommandData(theEnv)->BatchBuffer,FileCommandData(theEnv)->BatchMaximumPosition);
         FileCommandData(theEnv)->BatchMaximumPosition = 0;
         FileCommandData(theEnv)->BatchBuffer = NULL;
        }
     }

   /*=============================*/
   /* Increment the line counter. */
   /*=============================*/

   if (((char) rv == '\r') || ((char) rv == '\n'))
     { IncrementLineCount(theEnv); }

   /*=====================================================*/
   /* Return the character retrieved from the batch file. */
   /*=====================================================*/

   return(rv);
  }

/*****************************************************/
/* UngetcBatch: Ungetc routine for the batch router. */
/*****************************************************/
static int UngetcBatch(
  Environment *theEnv,
  int ch,
  const char *logicalName)
  {
#if MAC_XCD
#pragma unused(logicalName)
#endif

   if (FileCommandData(theEnv)->BatchCurrentPosition > 0) FileCommandData(theEnv)->BatchCurrentPosition--;
   if (FileCommandData(theEnv)->BatchBuffer != NULL) FileCommandData(theEnv)->BatchBuffer[FileCommandData(theEnv)->BatchCurrentPosition] = EOS;
   if (FileCommandData(theEnv)->BatchType == FILE_BATCH)
     { return(ungetc(ch,FileCommandData(theEnv)->BatchFileSource)); }

   return(EnvUngetcRouter(theEnv,ch,FileCommandData(theEnv)->BatchLogicalSource));
  }

/*************************************************/
/* ExitBatch: Exit routine for the batch router. */
/*************************************************/
static void ExitBatch(
  Environment *theEnv,
  int num)
  {
#if MAC_XCD
#pragma unused(num)
#endif
   CloseAllBatchSources(theEnv);
  }

/**************************************************/
/* Batch: C access routine for the batch command. */
/**************************************************/
bool Batch(
  Environment *theEnv,
  const char *fileName)
  { return(OpenBatch(theEnv,fileName,false)); }

/***********************************************/
/* OpenBatch: Adds a file to the list of files */
/*   opened with the batch command.            */
/***********************************************/
bool OpenBatch(
  Environment *theEnv,
  const char *fileName,
  bool placeAtEnd)
  {
   FILE *theFile;

   /*======================*/
   /* Open the batch file. */
   /*======================*/

   theFile = GenOpen(theEnv,fileName,"r");

   if (theFile == NULL)
     {
      OpenErrorMessage(theEnv,"batch",fileName);
      return false;
     }

   /*============================*/
   /* Create the batch router if */
   /* it doesn't already exist.  */
   /*============================*/

   if (FileCommandData(theEnv)->TopOfBatchList == NULL)
     {
      EnvAddRouter(theEnv,"batch", 20,
                 FindBatch, NULL,
                 GetcBatch, UngetcBatch,
                 ExitBatch);
     }

   /*===============================================================*/
   /* If a batch file is already open, save its current line count. */
   /*===============================================================*/

   if (FileCommandData(theEnv)->TopOfBatchList != NULL)
     { FileCommandData(theEnv)->TopOfBatchList->lineNumber = GetLineCount(theEnv); }

#if (! RUN_TIME) && (! BLOAD_ONLY)

   /*========================================================================*/
   /* If this is the first batch file, remember the prior parsing file name. */
   /*========================================================================*/

   if (FileCommandData(theEnv)->TopOfBatchList == NULL)
     { FileCommandData(theEnv)->batchPriorParsingFile = CopyString(theEnv,EnvGetParsingFileName(theEnv)); }

   /*=======================================================*/
   /* Create the error capture router if it does not exist. */
   /*=======================================================*/

   EnvSetParsingFileName(theEnv,fileName);
   SetLineCount(theEnv,0);

   CreateErrorCaptureRouter(theEnv);
#endif

   /*====================================*/
   /* Add the newly opened batch file to */
   /* the list of batch files opened.    */
   /*====================================*/

   AddBatch(theEnv,placeAtEnd,theFile,NULL,FILE_BATCH,NULL,fileName);

   /*===================================*/
   /* Return true to indicate the batch */
   /* file was successfully opened.     */
   /*===================================*/

   return true;
  }

/*****************************************************************/
/* OpenStringBatch: Opens a string source for batch processing.  */
/*   The memory allocated for the argument stringName must be    */
/*   deallocated by the user. The memory allocated for theString */
/*   will be deallocated by the batch routines when batch        */
/*   processing for the  string is completed.                    */
/*****************************************************************/
bool OpenStringBatch(
  Environment *theEnv,
  const char *stringName,
  const char *theString,
  bool placeAtEnd)
  {
   if (OpenStringSource(theEnv,stringName,theString,0) == false)
     { return false; }

   if (FileCommandData(theEnv)->TopOfBatchList == NULL)
     {
      EnvAddRouter(theEnv,"batch", 20,
                 FindBatch, NULL,
                 GetcBatch, UngetcBatch,
                 ExitBatch);
     }

   AddBatch(theEnv,placeAtEnd,NULL,stringName,STRING_BATCH,theString,NULL);

   return true;
  }

/*******************************************************/
/* AddBatch: Creates the batch file data structure and */
/*   adds it to the list of opened batch files.        */
/*******************************************************/
static void AddBatch(
  Environment *theEnv,
  bool placeAtEnd,
  FILE *theFileSource,
  const char *theLogicalSource,
  int type,
  const char *theString,
  const char *theFileName)
  {
   struct batchEntry *bptr;

   /*=========================*/
   /* Create the batch entry. */
   /*=========================*/

   bptr = get_struct(theEnv,batchEntry);
   bptr->batchType = type;
   bptr->fileSource = theFileSource;
   bptr->logicalSource = CopyString(theEnv,theLogicalSource);
   bptr->theString = theString;
   bptr->fileName = CopyString(theEnv,theFileName);
   bptr->lineNumber = 0;
   bptr->next = NULL;

   /*============================*/
   /* Add the entry to the list. */
   /*============================*/

   if (FileCommandData(theEnv)->TopOfBatchList == NULL)
     {
      FileCommandData(theEnv)->TopOfBatchList = bptr;
      FileCommandData(theEnv)->BottomOfBatchList = bptr;
      FileCommandData(theEnv)->BatchType = type;
      FileCommandData(theEnv)->BatchFileSource = theFileSource;
      FileCommandData(theEnv)->BatchLogicalSource = bptr->logicalSource;
      FileCommandData(theEnv)->BatchCurrentPosition = 0;
     }
   else if (placeAtEnd == false)
     {
      bptr->next = FileCommandData(theEnv)->TopOfBatchList;
      FileCommandData(theEnv)->TopOfBatchList = bptr;
      FileCommandData(theEnv)->BatchType = type;
      FileCommandData(theEnv)->BatchFileSource = theFileSource;
      FileCommandData(theEnv)->BatchLogicalSource = bptr->logicalSource;
      FileCommandData(theEnv)->BatchCurrentPosition = 0;
     }
   else
     {
      FileCommandData(theEnv)->BottomOfBatchList->next = bptr;
      FileCommandData(theEnv)->BottomOfBatchList = bptr;
     }
  }

/******************************************************************/
/* RemoveBatch: Removes the top entry on the list of batch files. */
/******************************************************************/
bool RemoveBatch(
  Environment *theEnv)
  {
   struct batchEntry *bptr;
   bool rv, fileBatch = false;

   if (FileCommandData(theEnv)->TopOfBatchList == NULL) return false;

   /*==================================================*/
   /* Close the source from which batch input is read. */
   /*==================================================*/

   if (FileCommandData(theEnv)->TopOfBatchList->batchType == FILE_BATCH)
     {
      fileBatch = true;
      GenClose(theEnv,FileCommandData(theEnv)->TopOfBatchList->fileSource);
#if (! RUN_TIME) && (! BLOAD_ONLY)
      FlushParsingMessages(theEnv);
      DeleteErrorCaptureRouter(theEnv);
#endif
     }
   else
     {
      CloseStringSource(theEnv,FileCommandData(theEnv)->TopOfBatchList->logicalSource);
      rm(theEnv,(void *) FileCommandData(theEnv)->TopOfBatchList->theString,
         strlen(FileCommandData(theEnv)->TopOfBatchList->theString) + 1);
     }

   /*=================================*/
   /* Remove the entry from the list. */
   /*=================================*/

   DeleteString(theEnv,(char *) FileCommandData(theEnv)->TopOfBatchList->fileName);
   bptr = FileCommandData(theEnv)->TopOfBatchList;
   FileCommandData(theEnv)->TopOfBatchList = FileCommandData(theEnv)->TopOfBatchList->next;

   DeleteString(theEnv,(char *) bptr->logicalSource);
   rtn_struct(theEnv,batchEntry,bptr);

   /*========================================================*/
   /* If there are no batch files remaining to be processed, */
   /* then free the space used by the batch buffer.          */
   /*========================================================*/

   if (FileCommandData(theEnv)->TopOfBatchList == NULL)
     {
      FileCommandData(theEnv)->BottomOfBatchList = NULL;
      FileCommandData(theEnv)->BatchFileSource = NULL;
      FileCommandData(theEnv)->BatchLogicalSource = NULL;
      if (FileCommandData(theEnv)->BatchBuffer != NULL)
        {
         rm(theEnv,FileCommandData(theEnv)->BatchBuffer,FileCommandData(theEnv)->BatchMaximumPosition);
         FileCommandData(theEnv)->BatchBuffer = NULL;
        }
      FileCommandData(theEnv)->BatchCurrentPosition = 0;
      FileCommandData(theEnv)->BatchMaximumPosition = 0;
      rv = false;

#if (! RUN_TIME) && (! BLOAD_ONLY)
      if (fileBatch)
        {
         EnvSetParsingFileName(theEnv,FileCommandData(theEnv)->batchPriorParsingFile);
         DeleteString(theEnv,FileCommandData(theEnv)->batchPriorParsingFile);
         FileCommandData(theEnv)->batchPriorParsingFile = NULL;
        }
#endif
     }

   /*===========================================*/
   /* Otherwise move on to the next batch file. */
   /*===========================================*/

   else
     {
      FileCommandData(theEnv)->BatchType = FileCommandData(theEnv)->TopOfBatchList->batchType;
      FileCommandData(theEnv)->BatchFileSource = FileCommandData(theEnv)->TopOfBatchList->fileSource;
      FileCommandData(theEnv)->BatchLogicalSource = FileCommandData(theEnv)->TopOfBatchList->logicalSource;
      FileCommandData(theEnv)->BatchCurrentPosition = 0;
      rv = true;
#if (! RUN_TIME) && (! BLOAD_ONLY)
      if (FileCommandData(theEnv)->TopOfBatchList->batchType == FILE_BATCH)
        { EnvSetParsingFileName(theEnv,FileCommandData(theEnv)->TopOfBatchList->fileName); }

      SetLineCount(theEnv,FileCommandData(theEnv)->TopOfBatchList->lineNumber);
#endif
     }

   /*====================================================*/
   /* Return true if a batch file if there are remaining */
   /* batch files to be processed, otherwise false.      */
   /*====================================================*/

   return(rv);
  }

/****************************************/
/* BatchActive: Returns true if a batch */
/*   file is open, otherwise false.     */
/****************************************/
bool BatchActive(
  Environment *theEnv)
  {
   if (FileCommandData(theEnv)->TopOfBatchList != NULL) return true;

   return false;
  }

/******************************************************/
/* CloseAllBatchSources: Closes all open batch files. */
/******************************************************/
void CloseAllBatchSources(
  Environment *theEnv)
  {
   /*================================================*/
   /* Free the batch buffer if it contains anything. */
   /*================================================*/

   if (FileCommandData(theEnv)->BatchBuffer != NULL)
     {
      if (FileCommandData(theEnv)->BatchCurrentPosition > 0) EnvPrintRouter(theEnv,STDOUT,(char *) FileCommandData(theEnv)->BatchBuffer);
      rm(theEnv,FileCommandData(theEnv)->BatchBuffer,FileCommandData(theEnv)->BatchMaximumPosition);
      FileCommandData(theEnv)->BatchBuffer = NULL;
      FileCommandData(theEnv)->BatchCurrentPosition = 0;
      FileCommandData(theEnv)->BatchMaximumPosition = 0;
     }

   /*==========================*/
   /* Delete the batch router. */
   /*==========================*/

   EnvDeleteRouter(theEnv,"batch");

   /*=====================================*/
   /* Close each of the open batch files. */
   /*=====================================*/

   while (RemoveBatch(theEnv))
     { /* Do Nothing */ }
  }

#if ! RUN_TIME

/**********************************************************/
/* EnvBatchStar: C access routine for the batch* command. */
/**********************************************************/
bool EnvBatchStar(
  Environment *theEnv,
  const char *fileName)
  {
   int inchar;
   FILE *theFile;
   char *theString = NULL;
   size_t position = 0;
   size_t maxChars = 0;
#if (! RUN_TIME) && (! BLOAD_ONLY)
   char *oldParsingFileName;
   long oldLineCountValue;
#endif
   /*======================*/
   /* Open the batch file. */
   /*======================*/

   theFile = GenOpen(theEnv,fileName,"r");

   if (theFile == NULL)
     {
      OpenErrorMessage(theEnv,"batch",fileName);
      return false;
     }

   /*======================================*/
   /* Setup for capturing errors/warnings. */
   /*======================================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   oldParsingFileName = CopyString(theEnv,EnvGetParsingFileName(theEnv));
   EnvSetParsingFileName(theEnv,fileName);

   CreateErrorCaptureRouter(theEnv);

   oldLineCountValue = SetLineCount(theEnv,1);
#endif

   /*========================*/
   /* Reset the error state. */
   /*========================*/

   EnvSetHaltExecution(theEnv,false);
   EnvSetEvaluationError(theEnv,false);

   /*=============================================*/
   /* Evaluate commands from the file one by one. */
   /*=============================================*/

   while ((inchar = getc(theFile)) != EOF)
     {
      theString = ExpandStringWithChar(theEnv,inchar,theString,&position,
                                       &maxChars,maxChars+80);

      if (CompleteCommand(theString) != 0)
        {
         FlushPPBuffer(theEnv);
         SetPPBufferStatus(theEnv,false);
         RouteCommand(theEnv,theString,false);
         FlushPPBuffer(theEnv);
         EnvSetHaltExecution(theEnv,false);
         EnvSetEvaluationError(theEnv,false);
         FlushBindList(theEnv);
         genfree(theEnv,theString,(unsigned) maxChars);
         theString = NULL;
         maxChars = 0;
         position = 0;
#if (! RUN_TIME) && (! BLOAD_ONLY)
         FlushParsingMessages(theEnv);
#endif
        }

      if ((inchar == '\r') || (inchar == '\n'))
        { IncrementLineCount(theEnv); }
     }

   if (theString != NULL)
     { genfree(theEnv,theString,(unsigned) maxChars); }

   /*=======================*/
   /* Close the batch file. */
   /*=======================*/

   GenClose(theEnv,theFile);

   /*========================================*/
   /* Cleanup for capturing errors/warnings. */
   /*========================================*/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   FlushParsingMessages(theEnv);
   DeleteErrorCaptureRouter(theEnv);

   SetLineCount(theEnv,oldLineCountValue);

   EnvSetParsingFileName(theEnv,oldParsingFileName);
   DeleteString(theEnv,oldParsingFileName);
#endif

   return true;
  }

#else

/**************************************************/
/* EnvBatchStar: This is the non-functional stub  */
/*   provided for use with a run-time version.    */
/**************************************************/
bool EnvBatchStar(
  Environment *theEnv,
  const char *fileName)
  {
   PrintErrorID(theEnv,"FILECOM",1,false);
   EnvPrintRouter(theEnv,WERROR,"Function batch* does not work in run time modules.\n");
   return false;
  }

#endif
