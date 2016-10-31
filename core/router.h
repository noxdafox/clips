   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/01/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_router

#pragma once

#define _H_router

#include <stdio.h>

typedef struct router Router;
typedef bool RouterQueryFunction(Environment *,const char *);
typedef void RouterPrintFunction(Environment *,const char *,const char *);
typedef void RouterExitFunction(Environment *,int);
typedef int RouterGetcFunction(Environment *,const char *);
typedef int RouterUngetcFunction(Environment *,int,const char *);

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
   void *context;
   RouterQueryFunction *queryCallback;
   RouterPrintFunction *printCallback;
   RouterExitFunction *exitCallback;
   RouterGetcFunction *getcCallback;
   RouterUngetcFunction *ungetcCallback;
   Router *next;
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

   void                           InitializeDefaultRouters(Environment *);
   void                           EnvPrintRouter(Environment *,const char *,const char *);
   int                            EnvGetcRouter(Environment *,const char *);
   int                            EnvUngetcRouter(Environment *,int,const char *);
   void                           EnvExitRouter(Environment *,int);
   void                           AbortExit(Environment *);
   bool                           EnvAddRouterWithContext(Environment *,
                                                          const char *,int,
                                                          RouterQueryFunction *,
                                                          RouterPrintFunction *,
                                                          RouterGetcFunction *,
                                                          RouterUngetcFunction *,
                                                          RouterExitFunction *,
                                                          void *);
   bool                           EnvAddRouter(Environment *,
                                               const char *,int,
                                               RouterQueryFunction *,
                                               RouterPrintFunction *,
                                               RouterGetcFunction *,
                                               RouterUngetcFunction *,
                                               RouterExitFunction *);
   bool                           EnvDeleteRouter(Environment *,const char *);
   bool                           QueryRouters(Environment *,const char *);
   bool                           EnvDeactivateRouter(Environment *,const char *);
   bool                           EnvActivateRouter(Environment *,const char *);
   void                           SetFastLoad(Environment *,FILE *);
   void                           SetFastSave(Environment *,FILE *);
   FILE                          *GetFastLoad(Environment *);
   FILE                          *GetFastSave(Environment *);
   void                           UnrecognizedRouterMessage(Environment *,const char *);
   void                           PrintNRouter(Environment *,const char *,const char *,unsigned long);
   size_t                         EnvInputBufferCount(Environment *);
   Router                        *EnvFindRouter(Environment *,const char *);

#endif /* _H_router */
