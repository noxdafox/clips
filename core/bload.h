   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/01/16            */
   /*                                                     */
   /*                 BLOAD HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.31: Data sizes written to binary files for         */
/*            validation when loaded.                        */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_bload

#pragma once

#define _H_bload

#include "utility.h"
#include "extnfunc.h"
#include "exprnbin.h"
#include "symbol.h"
#include "sysdep.h"
#include "symblbin.h"

#define BLOAD_DATA 38

struct bloadData
  {
   const char *BinaryPrefixID;
   const char *BinaryVersionID;
   char *BinarySizes;
   struct functionDefinition **FunctionArray;
   bool BloadActive;
   struct voidCallFunctionItem *BeforeBloadFunctions;
   struct voidCallFunctionItem *AfterBloadFunctions;
   struct boolCallFunctionItem *ClearBloadReadyFunctions;
   struct voidCallFunctionItem *AbortBloadFunctions;
  };

#define BloadData(theEnv) ((struct bloadData *) GetEnvironmentData(theEnv,BLOAD_DATA))

#define FunctionPointer(i) ((((i) == -1L) ? NULL : BloadData(theEnv)->FunctionArray[i]))

   void                    InitializeBloadData(Environment *);
   void                    BloadCommand(Environment *,UDFContext *,UDFValue *);
   bool                    EnvBload(Environment *,const char *);
   void                    BloadandRefresh(Environment *,long,size_t,void (*)(Environment *,void *,long));
   bool                    Bloaded(Environment *);
   void                    AddBeforeBloadFunction(Environment *,const char *,void (*)(Environment *),int);
   void                    AddAfterBloadFunction(Environment *,const char *,void (*)(Environment *),int);
   void                    AddClearBloadReadyFunction(Environment *,const char *,bool (*)(Environment *),int);
   void                    AddAbortBloadFunction(Environment *,const char *,void (*)(Environment *),int);
   void                    CannotLoadWithBloadMessage(Environment *,const char *);

#endif

