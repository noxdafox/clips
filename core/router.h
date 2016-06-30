   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
   int active;
   int priority;
   short int environmentAware;
   void *context;
   int (*query)(void *,const char *);
   int (*printer)(void *,const char *,const char *);
   int (*exiter)(void *,int);
   int (*charget)(void *,const char *);
   int (*charunget)(void *,int,const char *);
   struct router *next;
  };

struct routerData
  { 
   size_t CommandBufferInputCount;
   int AwaitingInput;
   const char *LineCountRouter;
   const char *FastCharGetRouter;
   char *FastCharGetString;
   long FastCharGetIndex;
   struct router *ListOfRouters;
   FILE *FastLoadFilePtr;
   FILE *FastSaveFilePtr;
   int Abort;
  };

#define RouterData(theEnv) ((struct routerData *) GetEnvironmentData(theEnv,ROUTER_DATA))

   void                           InitializeDefaultRouters(void *);
   int                            EnvPrintRouter(void *,const char *,const char *);
   int                            EnvGetcRouter(void *,const char *);
   int                            EnvUngetcRouter(void *,int,const char *);
   void                           EnvExitRouter(void *,int);
   void                           AbortExit(void *);
   intBool                        EnvAddRouterWithContext(void *,
                                                   const char *,int,
                                                   int (*)(void *,const char *),
                                                   int (*)(void *,const char *,const char *),
                                                   int (*)(void *,const char *),
                                                   int (*)(void *,int,const char *),
                                                   int (*)(void *,int),
                                                   void *);
   intBool                        EnvAddRouter(void *,
                                                   const char *,int,
                                                   int (*)(void *,const char *),
                                                   int (*)(void *,const char *,const char *),
                                                   int (*)(void *,const char *),
                                                   int (*)(void *,int,const char *),
                                                   int (*)(void *,int));
   int                            EnvDeleteRouter(void *,const char *);
   int                            QueryRouters(void *,const char *);
   int                            EnvDeactivateRouter(void *,const char *);
   int                            EnvActivateRouter(void *,const char *);
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

   int                            ActivateRouter(const char *);
   intBool                        AddRouter(const char *,int,
                                                   int (*)(const char *),
                                                   int (*)(const char *,const char *),
                                                   int (*)(const char *),
                                                   int (*)(int,const char *),
                                                   int (*)(int));
   int                            DeactivateRouter(const char *);
   int                            DeleteRouter(const char *);
   void                           ExitRouter(int);
   int                            GetcRouter(const char *);
   int                            PrintRouter(const char *,const char *);
   int                            UngetcRouter(int,const char *);
   
#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_router */


