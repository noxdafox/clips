   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable log to logName        */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS            */
/*                    compilation flag.                      */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added DeallocateMessageHandlerData to          */
/*            deallocate message handler environment data.   */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_msgcom

#pragma once

#define _H_msgcom

#include "msgpass.h"
#include "object.h"

#define MESSAGE_HANDLER_DATA 32

struct messageHandlerData
  { 
   ENTITY_RECORD HandlerGetInfo;
   ENTITY_RECORD HandlerPutInfo;
   SYMBOL_HN *INIT_SYMBOL;
   SYMBOL_HN *DELETE_SYMBOL;
   SYMBOL_HN *CREATE_SYMBOL;
#if DEBUGGING_FUNCTIONS
   bool WatchHandlers;
   bool WatchMessages;
#endif
   const char *hndquals[4];
   SYMBOL_HN *SELF_SYMBOL;
   SYMBOL_HN *CurrentMessageName;
   HANDLER_LINK *CurrentCore;
   HANDLER_LINK *TopOfCore;
   HANDLER_LINK *NextInCore;
   HANDLER_LINK *OldCore;
  };

#define MessageHandlerData(theEnv) ((struct messageHandlerData *) GetEnvironmentData(theEnv,MESSAGE_HANDLER_DATA))

#define INIT_STRING   "init"
#define DELETE_STRING "delete"
#define PRINT_STRING  "print"
#define CREATE_STRING "create"

   void             SetupMessageHandlers(Environment *);
   const char      *EnvGetDefmessageHandlerName(Environment *,Defclass *,int);
   const char      *EnvGetDefmessageHandlerType(Environment *,Defclass *,int);
   int              EnvGetNextDefmessageHandler(Environment *,Defclass *,int);
   DefmessageHandler
                   *GetDefmessageHandlerPointer(Defclass *,int);
#if DEBUGGING_FUNCTIONS
   bool             EnvGetDefmessageHandlerWatch(Environment *,Defclass *,int);
   void             EnvSetDefmessageHandlerWatch(Environment *,bool,Defclass *,int);
#endif
   unsigned         EnvFindDefmessageHandler(Environment *,Defclass *,const char *,const char *);
   bool             EnvIsDefmessageHandlerDeletable(Environment *,Defclass *,int);
   void             UndefmessageHandlerCommand(Environment *,UDFContext *,CLIPSValue *);
   bool             EnvUndefmessageHandler(Environment *,Defclass *,int);
#if DEBUGGING_FUNCTIONS
   void             PPDefmessageHandlerCommand(Environment *,UDFContext *,CLIPSValue *);
   void             ListDefmessageHandlersCommand(Environment *,UDFContext *,CLIPSValue *);
   void             PreviewSendCommand(Environment *,UDFContext *,CLIPSValue *);
   const char      *EnvGetDefmessageHandlerPPForm(Environment *,Defclass *,int);
   void             EnvListDefmessageHandlers(Environment *,const char *,Defclass *,bool);
   void             EnvPreviewSend(Environment *,const char *,Defclass *,const char *);
   long             DisplayHandlersInLinks(Environment *,const char *,PACKED_CLASS_LINKS *,int);
#endif

#endif /* _H_msgcom */





