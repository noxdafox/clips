   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
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
   struct FunctionDefinition **FunctionArray;
   int BloadActive;
   struct callFunctionItem *BeforeBloadFunctions;
   struct callFunctionItem *AfterBloadFunctions;
   struct callFunctionItem *ClearBloadReadyFunctions;
   struct callFunctionItem *AbortBloadFunctions;
  };

#define BloadData(theEnv) ((struct bloadData *) GetEnvironmentData(theEnv,BLOAD_DATA))

#define FunctionPointer(i) ((struct FunctionDefinition *) (((i) == -1L) ? NULL : BloadData(theEnv)->FunctionArray[i]))

   void                    InitializeBloadData(void *);
   int                     BloadCommand(void *);
   intBool                 EnvBload(void *,const char *);
   void                    BloadandRefresh(void *,long,size_t,void (*)(void *,void *,long));
   intBool                 Bloaded(void *);
   void                    AddBeforeBloadFunction(void *,const char *,void (*)(void *),int);
   void                    AddAfterBloadFunction(void *,const char *,void (*)(void *),int);
   void                    AddClearBloadReadyFunction(void *,const char *,int (*)(void *),int);
   void                    AddAbortBloadFunction(void *,const char *,void (*)(void *),int);
   void                    CannotLoadWithBloadMessage(void *,const char *);

#if ALLOW_ENVIRONMENT_GLOBALS
   int                     Bload(const char *);
#endif

#endif

