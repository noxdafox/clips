   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*               PRETTY PRINT HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for processing the pretty print         */
/*   representation of constructs.                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_pprint

#pragma once

#define _H_pprint

#define PRETTY_PRINT_DATA 52

struct prettyPrintData
  {
   bool PPBufferStatus;
   bool PPBufferEnabled;
   int IndentationDepth;
   size_t PPBufferPos;
   size_t PPBufferMax;
   size_t PPBackupOnce;
   size_t PPBackupTwice;
   char *PrettyPrintBuffer;
  };

#define PrettyPrintData(theEnv) ((struct prettyPrintData *) GetEnvironmentData(theEnv,PRETTY_PRINT_DATA))

   void                           InitializePrettyPrintData(Environment *);
   void                           FlushPPBuffer(Environment *);
   void                           DestroyPPBuffer(Environment *);
   void                           SavePPBuffer(Environment *,const char *);
   void                           PPBackup(Environment *);
   char                          *CopyPPBuffer(Environment *);
   char                          *GetPPBuffer(Environment *);
   void                           PPCRAndIndent(Environment *);
   void                           IncrementIndentDepth(Environment *,int);
   void                           DecrementIndentDepth(Environment *,int);
   void                           SetIndentDepth(Environment *,int);
   void                           SetPPBufferStatus(Environment *,bool);
   bool                           GetPPBufferStatus(Environment *);
   bool                           SetPPBufferEnabled(Environment *,bool);
   bool                           GetPPBufferEnabled(Environment *);

#endif



