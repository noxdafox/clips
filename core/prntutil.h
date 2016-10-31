   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*            Support for typed EXTERNAL_ADDRESS_TYPE.            */
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
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
/*                                                           */
/*************************************************************/

#ifndef _H_prntutil

#pragma once

#define _H_prntutil

#include <stdio.h>

#include "entities.h"

#define PRINT_UTILITY_DATA 53

struct printUtilityData
  {
   bool PreserveEscapedCharacters;
   bool AddressesToStrings;
   bool InstanceAddressesToNames;
  };

#define PrintUtilityData(theEnv) ((struct printUtilityData *) GetEnvironmentData(theEnv,PRINT_UTILITY_DATA))

   void                           InitializePrintUtilityData(Environment *);
   void                           PrintInChunks(Environment *,const char *,const char *);
   void                           PrintFloat(Environment *,const char *,double);
   void                           PrintLongInteger(Environment *,const char *,long long);
   void                           PrintAtom(Environment *,const char *,int,void *);
   void                           PrintTally(Environment *,const char *,long long,const char *,const char *);
   const char                    *FloatToString(Environment *,double);
   const char                    *LongIntegerToString(Environment *,long long);
   const char                    *DataObjectToString(Environment *,UDFValue *);
   void                           SyntaxErrorMessage(Environment *,const char *);
   void                           SystemError(Environment *,const char *,int);
   void                           PrintErrorID(Environment *,const char *,int,bool);
   void                           PrintWarningID(Environment *,const char *,int,bool);
   void                           CantFindItemErrorMessage(Environment *,const char *,const char *);
   void                           CantDeleteItemErrorMessage(Environment *,const char *,const char *);
   void                           AlreadyParsedErrorMessage(Environment *,const char *,const char *);
   void                           LocalVariableErrorMessage(Environment *,const char *);
   void                           DivideByZeroErrorMessage(Environment *,const char *);
   void                           SalienceInformationError(Environment *,const char *,const char *);
   void                           SalienceRangeError(Environment *,int,int);
   void                           SalienceNonIntegerError(Environment *);
   void                           CantFindItemInFunctionErrorMessage(Environment *,const char *,const char *,const char *);
   void                           SlotExistError(Environment *,const char *,const char *);

#endif /* _H_prntutil */






