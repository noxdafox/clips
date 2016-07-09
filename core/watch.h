   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                  WATCH HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Support functions for the watch and unwatch      */
/*   commands.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added EnvSetWatchItem function.                */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
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
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_watch

#pragma once

#define _H_watch

#include "expressn.h"

#define WATCH_DATA 54

struct watchItem
  {
   const char *name;
   bool *flag;
   int code,priority;
   bool (*accessFunc)(void *,int,bool,struct expr *);
   bool (*printFunc)(void *,const char *,int,struct expr *);
   struct watchItem *next;
  };

struct watchData
  { 
   struct watchItem *ListOfWatchItems;
  };

#define WatchData(theEnv) ((struct watchData *) GetEnvironmentData(theEnv,WATCH_DATA))

   bool                           EnvWatch(void *,const char *);
   bool                           EnvUnwatch(void *,const char *);
   void                           InitializeWatchData(void *);   
   bool                           EnvSetWatchItem(void *,const char *,bool,struct expr *);
   int                            EnvGetWatchItem(void *,const char *);
   bool                           AddWatchItem(void *,const char *,int,bool *,int,
                                                      bool (*)(void *,int,bool,struct expr *),
                                                      bool (*)(void *,const char *,int,struct expr *));
   const char                    *GetNthWatchName(void *,int);
   int                            GetNthWatchValue(void *,int);
   void                           WatchCommand(void *);
   void                           UnwatchCommand(void *);
   void                           ListWatchItemsCommand(void *);
   void                           WatchFunctionDefinitions(void *);
   bool                           GetWatchItemCommand(void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           Watch(const char *);
   bool                           Unwatch(const char *);
   int                            GetWatchItem(const char *);
   int                            SetWatchItem(const char *,unsigned,struct expr *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_watch */



