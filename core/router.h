   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                 ROUTER HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed conversion of '\r' to '\n' from the    */
/*            EnvGetcRouter function.                        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added support for passing context information  */ 
/*            to the router functions.                       */
/*                                                           */
/*      6.30: Fixed issues with passing context to routers.  */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */             
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added STDOUT and STDIN logical name            */
/*            definitions.                                   */
/*                                                           */
/*      6.40: Added EnvInputBufferCount function.            */
/*                                                           */
/*            Added check for reuse of existing router name. */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Changed return values for router functions.    */
/*                                                           */
/*************************************************************/

#ifndef _H_router

#pragma once

#define _H_router

#include "prntutil.h"

#include <stdio.h>

#define WWARNING "wwarning"
#define WERROR "werror"
#define WTRACE "wtrace"
#define WDIALOG "wdialog"
#define WPROMPT  WPROMPT_STRING
#define WDISPLAY "wdisplay"
#define STDOUT "stdout"
#define STDIN "stdin"

#define ROUTER_DATA 46

struct router
  {
   const char *name;
   bool active;
   int priority;
   bool environmentAware;
   void *context;
   bool (*query)(void *,const char *);
   void (*printer)(void *,const char *,const char *);
   void (*exiter)(void *,int);
   int (*charget)(void *,const char *);
   int (*charunget)(void *,int,const char *);
   struct router *next;
  };

struct routerData
  { 
   size_t CommandBufferInputCount;
   bool AwaitingInput;
   const char *LineCountRouter;
   const char *FastCharGetRouter;
   char *FastCharGetString;
   long FastCharGetIndex;
   struct router *ListOfRouters;
   FILE *FastLoadFilePtr;
   FILE *FastSaveFilePtr;
   bool Abort;
  };

#define RouterData(theEnv) ((struct routerData *) GetEnvironmentData(theEnv,ROUTER_DATA))

   void                           InitializeDefaultRouters(void *);
   int                            EnvPrintRouter(void *,const char *,const char *);
   int                            EnvGetcRouter(void *,const char *);
   int                            EnvUngetcRouter(void *,int,const char *);
   void                           EnvExitRouter(void *,int);
   void                           AbortExit(void *);
   bool                           EnvAddRouterWithContext(void *,
                                                   const char *,int,
                                                   bool (*)(void *,const char *),
                                                   void (*)(void *,const char *,const char *),
                                                   int (*)(void *,const char *),
                                                   int (*)(void *,int,const char *),
                                                   void (*)(void *,int),
                                                   void *);
   bool                           EnvAddRouter(void *,
                                                   const char *,int,
                                                   bool (*)(void *,const char *),
                                                   void (*)(void *,const char *,const char *),
                                                   int (*)(void *,const char *),
                                                   int (*)(void *,int,const char *),
                                                   void (*)(void *,int));
   bool                           EnvDeleteRouter(void *,const char *);
   bool                           QueryRouters(void *,const char *);
   bool                           EnvDeactivateRouter(void *,const char *);
   bool                           EnvActivateRouter(void *,const char *);
   void                           SetFastLoad(void *,FILE *);
   void                           SetFastSave(void *,FILE *);
   FILE                          *GetFastLoad(void *);
   FILE                          *GetFastSave(void *);
   void                           UnrecognizedRouterMessage(void *,const char *);
   void                           ExitCommand(void *);
   int                            PrintNRouter(void *,const char *,const char *,unsigned long);
   size_t                         EnvInputBufferCount(void *);
   struct router                 *EnvFindRouter(void *,const char *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           ActivateRouter(const char *);
   bool                           AddRouter(const char *,int,
                                                   bool (*)(const char *),
                                                   void (*)(const char *,const char *),
                                                   int (*)(const char *),
                                                   int (*)(int,const char *),
                                                   void (*)(int));
   bool                           DeactivateRouter(const char *);
   bool                           DeleteRouter(const char *);
   void                           ExitRouter(int);
   int                            GetcRouter(const char *);
   int                            PrintRouter(const char *,const char *);
   int                            UngetcRouter(int,const char *);
   
#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_router */
