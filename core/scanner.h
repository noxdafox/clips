   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*                 SCANNER HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for scanning lexical tokens from an     */
/*   input source.                                           */
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
/*            Support for long long integers.                */
/*                                                           */
/*            Added SetLineCount function.                   */
/*                                                           */
/*            Added UTF-8 support.                           */
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

#ifndef _H_scanner

#pragma once

#define _H_scanner

struct token;

#include "pprint.h"

struct token
  {
   unsigned short type;
   void *value;
   const char *printForm;
  };

#define SCANNER_DATA 57

struct scannerData
  { 
   char *GlobalString;
   size_t GlobalMax;
   size_t GlobalPos;
   long LineCount;
   bool IgnoreCompletionErrors;
  };

#define ScannerData(theEnv) ((struct scannerData *) GetEnvironmentData(theEnv,SCANNER_DATA))

   void                           InitializeScannerData(Environment *);
   void                           GetToken(Environment *,const char *,struct token *);
   void                           CopyToken(struct token *,struct token *);
   void                           ResetLineCount(Environment *);
   long                           GetLineCount(Environment *);
   long                           SetLineCount(Environment *,long);
   void                           IncrementLineCount(Environment *);
   void                           DecrementLineCount(Environment *);

#endif /* _H_scanner */




