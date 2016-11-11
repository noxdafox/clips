   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
   /*                                                     */
   /*                 PRETTY PRINT MODULE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for processing the pretty print         */
/*   representation of constructs.                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Chris Culbert                                        */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Corrected code generating compilation          */
/*            warnings.                                      */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Added NULL pointer check in CopyPPBuffer.      */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "setup.h"

#include "constant.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "sysdep.h"
#include "utility.h"

#include "pprint.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DeallocatePrettyPrintData(Environment *);

/****************************************************/
/* InitializePrettyPrintData: Allocates environment */
/*    data for pretty print routines.               */
/****************************************************/
void InitializePrettyPrintData(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,PRETTY_PRINT_DATA,sizeof(struct prettyPrintData),DeallocatePrettyPrintData);

   PrettyPrintData(theEnv)->PPBufferEnabled = true;
  }

/******************************************************/
/* DeallocatePrettyPrintData: Deallocates environment */
/*    data for the pretty print routines.             */
/******************************************************/
static void DeallocatePrettyPrintData(
  Environment *theEnv)
  {
   if (PrettyPrintData(theEnv)->PrettyPrintBuffer != NULL)
     { rm(theEnv,PrettyPrintData(theEnv)->PrettyPrintBuffer,PrettyPrintData(theEnv)->PPBufferMax); }
  }

/*******************************************************/
/* FlushPPBuffer: Resets the pretty print save buffer. */
/*******************************************************/
void FlushPPBuffer(
  Environment *theEnv)
  {
   if (PrettyPrintData(theEnv)->PrettyPrintBuffer == NULL) return;
   PrettyPrintData(theEnv)->PPBackupOnce = 0;
   PrettyPrintData(theEnv)->PPBackupTwice = 0;
   PrettyPrintData(theEnv)->PPBufferPos = 0;
   PrettyPrintData(theEnv)->PrettyPrintBuffer[0] = EOS;
   return;
  }

/*********************************************************************/
/* DestroyPPBuffer: Resets and removes the pretty print save buffer. */
/*********************************************************************/
void DestroyPPBuffer(
  Environment *theEnv)
  {
   PrettyPrintData(theEnv)->PPBackupOnce = 0;
   PrettyPrintData(theEnv)->PPBackupTwice = 0;
   PrettyPrintData(theEnv)->PPBufferPos = 0;
   if (PrettyPrintData(theEnv)->PrettyPrintBuffer != NULL) rm(theEnv,PrettyPrintData(theEnv)->PrettyPrintBuffer,PrettyPrintData(theEnv)->PPBufferMax);
   PrettyPrintData(theEnv)->PrettyPrintBuffer = NULL;
   PrettyPrintData(theEnv)->PPBufferMax = 0;
  }

/*********************************************/
/* SavePPBuffer: Appends a string to the end */
/*   of the pretty print save buffer.        */
/*********************************************/
void SavePPBuffer(
  Environment *theEnv,
  const char *str)
  {
   size_t increment;

   /*==========================================*/
   /* If the pretty print buffer isn't needed, */
   /* then don't bother writing to it.         */
   /*==========================================*/

   if ((PrettyPrintData(theEnv)->PPBufferStatus == false) || (! PrettyPrintData(theEnv)->PPBufferEnabled))
     { return; }

   /*===============================*/
   /* Determine the increment size. */
   /*===============================*/

   increment = 512;
   if (PrettyPrintData(theEnv)->PPBufferPos > increment)
     { increment = PrettyPrintData(theEnv)->PPBufferPos * 3; }

   /*================================================*/
   /* If the pretty print buffer isn't big enough to */
   /* contain the string, then increase its size.    */
   /*================================================*/

   if (strlen(str) + PrettyPrintData(theEnv)->PPBufferPos + 1 >= PrettyPrintData(theEnv)->PPBufferMax)
     {
      PrettyPrintData(theEnv)->PrettyPrintBuffer =
         (char *) genrealloc(theEnv,PrettyPrintData(theEnv)->PrettyPrintBuffer,
                                    PrettyPrintData(theEnv)->PPBufferMax,
                                    PrettyPrintData(theEnv)->PPBufferMax + increment);
      PrettyPrintData(theEnv)->PPBufferMax += increment;
     }

   /*==================================================*/
   /* Remember the previous tokens saved to the pretty */
   /* print buffer in case it is necessary to back up. */
   /*==================================================*/

   PrettyPrintData(theEnv)->PPBackupTwice = PrettyPrintData(theEnv)->PPBackupOnce;
   PrettyPrintData(theEnv)->PPBackupOnce = PrettyPrintData(theEnv)->PPBufferPos;

   /*=============================================*/
   /* Save the string to the pretty print buffer. */
   /*=============================================*/

   PrettyPrintData(theEnv)->PrettyPrintBuffer = AppendToString(theEnv,str,PrettyPrintData(theEnv)->PrettyPrintBuffer,&PrettyPrintData(theEnv)->PPBufferPos,&PrettyPrintData(theEnv)->PPBufferMax);
  }

/***************************************************/
/* PPBackup:  Removes the last string added to the */
/*   pretty print save buffer. Only capable of     */
/*   backing up for the two most recent additions. */
/***************************************************/
void PPBackup(
  Environment *theEnv)
  {
   if ((PrettyPrintData(theEnv)->PPBufferStatus == false) ||
       (PrettyPrintData(theEnv)->PrettyPrintBuffer == NULL) ||
       (! PrettyPrintData(theEnv)->PPBufferEnabled))
     { return; }

   PrettyPrintData(theEnv)->PPBufferPos = PrettyPrintData(theEnv)->PPBackupOnce;
   PrettyPrintData(theEnv)->PPBackupOnce = PrettyPrintData(theEnv)->PPBackupTwice;
   PrettyPrintData(theEnv)->PrettyPrintBuffer[PrettyPrintData(theEnv)->PPBufferPos] = EOS;
  }

/**************************************************/
/* CopyPPBuffer: Makes a copy of the pretty print */
/*   save buffer.                                 */
/**************************************************/
char *CopyPPBuffer(
  Environment *theEnv)
  {
   size_t length;
   char *newString;
   char *theBuffer = PrettyPrintData(theEnv)->PrettyPrintBuffer;

   if (theBuffer == NULL) return NULL;

   length = (1 + strlen(theBuffer)) * (int) sizeof (char);
   newString = (char *) gm2(theEnv,length);

   genstrcpy(newString,theBuffer);
   return(newString);
  }

/************************************************************/
/* GetPPBuffer: Returns a pointer to the PrettyPrintBuffer. */
/************************************************************/
char *GetPPBuffer(
  Environment *theEnv)
  {
   return(PrettyPrintData(theEnv)->PrettyPrintBuffer);
  }

/*******************************************/
/* PPCRAndIndent: Prints white spaces into */
/*   the pretty print buffer.              */
/*******************************************/
void PPCRAndIndent(
  Environment *theEnv)
  {
   int i;
   char buffer[120];

   if ((PrettyPrintData(theEnv)->PPBufferStatus == false) ||
       (! PrettyPrintData(theEnv)->PPBufferEnabled))
     { return; }

   buffer[0] = '\n';

   for (i = 1 ; i <= PrettyPrintData(theEnv)->IndentationDepth ; i++)
     { buffer[i] = ' '; }
   buffer[i] = EOS;

   SavePPBuffer(theEnv,buffer);
  }

/************************************************/
/* IncrementIndentDepth: Increments indentation */
/*   depth for pretty printing.                 */
/************************************************/
void IncrementIndentDepth(
  Environment *theEnv,
  int value)
  {
   PrettyPrintData(theEnv)->IndentationDepth += value;
  }

/************************************************/
/* DecrementIndentDepth: Decrements indentation */
/*   depth for pretty printing.                 */
/************************************************/
void DecrementIndentDepth(
  Environment *theEnv,
  int value)
  {
   PrettyPrintData(theEnv)->IndentationDepth -= value;
  }

/************************************/
/* SetIndentDepth: Sets indentation */
/*   depth for pretty printing.     */
/************************************/
void SetIndentDepth(
  Environment *theEnv,
  int value)
  {
   PrettyPrintData(theEnv)->IndentationDepth = value;
  }

/******************************************/
/* SetPPBufferStatus: Sets PPBufferStatus */
/*   flag to boolean value of ON or OFF.  */
/******************************************/
void SetPPBufferStatus(
  Environment *theEnv,
  bool value)
  {
   PrettyPrintData(theEnv)->PPBufferStatus = value;
  }

/************************************/
/* GetPPBufferStatus: Returns value */
/*   of the PPBufferStatus flag.    */
/************************************/
bool GetPPBufferStatus(
  Environment *theEnv)
  {
   return(PrettyPrintData(theEnv)->PPBufferStatus);
  }

/***********************/
/* SetPPBufferEnabled: */
/***********************/
bool SetPPBufferEnabled(
  Environment *theEnv,
  bool value)
  {
   bool oldValue;

   oldValue = PrettyPrintData(theEnv)->PPBufferEnabled;
   PrettyPrintData(theEnv)->PPBufferEnabled = value;
   return oldValue;
  }

/***********************/
/* GetPPBufferEnabled: */
/***********************/
bool GetPPBufferEnabled(
  Environment *theEnv)
  {
   return PrettyPrintData(theEnv)->PPBufferEnabled;
  }

