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

typedef struct token Token;

typedef enum
  {
   SYMBOL_TOKEN = 1025,
   STRING_TOKEN,
   INSTANCE_NAME_TOKEN,
   FLOAT_TOKEN,
   INTEGER_TOKEN,
   LEFT_PARENTHESIS_TOKEN,
   RIGHT_PARENTHESIS_TOKEN,
   SF_VARIABLE_TOKEN,
   MF_VARIABLE_TOKEN,
   GBL_VARIABLE_TOKEN,
   SF_WILDCARD_TOKEN,
   MF_WILDCARD_TOKEN,
   MF_GBL_VARIABLE_TOKEN,
   NOT_CONSTRAINT_TOKEN,
   AND_CONSTRAINT_TOKEN,
   OR_CONSTRAINT_TOKEN,
   STOP_TOKEN,
   UNKNOWN_VALUE_TOKEN,
  } TokenType;

#include "pprint.h"

struct token
  {
   TokenType tknType;
   union
     {
      void *value;
      CLIPSLexeme *lexemeValue;
      CLIPSFloat *floatValue;
      CLIPSInteger *integerValue;
     };
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
   unsigned short                 TokenTypeToType(TokenType);

#endif /* _H_scanner */




