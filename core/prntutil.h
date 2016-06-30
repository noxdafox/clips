   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*              PRINT UTILITY HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for printing various items      */
/*   and messages.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Link error occurs for the SlotExistError       */
/*            function when OBJECT_SYSTEM is set to 0 in     */
/*            setup.h. DR0865                                */
/*                                                           */
/*            Added DataObjectToString function.             */
/*                                                           */
/*            Added SlotExistError function.                 */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Support for DATA_OBJECT_ARRAY primitive.       */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS.            */
/*                                                           */
/*            Used gensprintf and genstrcat instead of       */
/*            sprintf and strcat.                            */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added code for capturing errors/warnings.      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_prntutil

#pragma once

#define _H_prntutil

#include "moduldef.h"

#include <stdio.h>

#define PRINT_UTILITY_DATA 53

struct printUtilityData
  { 
   intBool PreserveEscapedCharacters;
   intBool AddressesToStrings;
   intBool InstanceAddressesToNames;
  };

#define PrintUtilityData(theEnv) ((struct printUtilityData *) GetEnvironmentData(theEnv,PRINT_UTILITY_DATA))

   void                           InitializePrintUtilityData(void *);
   void                           PrintInChunks(void *,const char *,const char *);
   void                           PrintFloat(void *,const char *,double);
   void                           PrintLongInteger(void *,const char *,long long);
   void                           PrintAtom(void *,const char *,int,void *);
   void                           PrintTally(void *,const char *,long long,const char *,const char *);
   const char                    *FloatToString(void *,double);
   const char                    *LongIntegerToString(void *,long long);
   const char                    *DataObjectToString(void *,DATA_OBJECT *);
   void                           SyntaxErrorMessage(void *,const char *);
   void                           SystemError(void *,const char *,int);
   void                           PrintErrorID(void *,const char *,int,int);
   void                           PrintWarningID(void *,const char *,int,int);
   void                           CantFindItemErrorMessage(void *,const char *,const char *);
   void                           CantDeleteItemErrorMessage(void *,const char *,const char *);
   void                           AlreadyParsedErrorMessage(void *,const char *,const char *);
   void                           LocalVariableErrorMessage(void *,const char *);
   void                           DivideByZeroErrorMessage(void *,const char *);
   void                           SalienceInformationError(void *,const char *,const char *);
   void                           SalienceRangeError(void *,int,int);
   void                           SalienceNonIntegerError(void *);
   void                           CantFindItemInFunctionErrorMessage(void *,const char *,const char *,const char *);
   void                           SlotExistError(void *,const char *,const char *);

#endif /* _H_prntutil */






