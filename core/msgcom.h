   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_msgcom
#define _H_msgcom

#ifndef _H_object
#include "object.h"
#endif

#ifndef _H_msgpass
#include "msgpass.h"
#endif

#define MESSAGE_HANDLER_DATA 32

struct messageHandlerData
  { 
   ENTITY_RECORD HandlerGetInfo;
   ENTITY_RECORD HandlerPutInfo;
   SYMBOL_HN *INIT_SYMBOL;
   SYMBOL_HN *DELETE_SYMBOL;
   SYMBOL_HN *CREATE_SYMBOL;
#if DEBUGGING_FUNCTIONS
   unsigned WatchHandlers;
   unsigned WatchMessages;
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

   void             SetupMessageHandlers(void *);
   const char      *EnvGetDefmessageHandlerName(void *,void *,int);
   const char      *EnvGetDefmessageHandlerType(void *,void *,int);
   int              EnvGetNextDefmessageHandler(void *,void *,int);
   HANDLER         *GetDefmessageHandlerPointer(void *,int);
#if DEBUGGING_FUNCTIONS
   unsigned         EnvGetDefmessageHandlerWatch(void *,void *,int);
   void             EnvSetDefmessageHandlerWatch(void *,int,void *,int);
#endif
   unsigned         EnvFindDefmessageHandler(void *,void *,const char *,const char *);
   int              EnvIsDefmessageHandlerDeletable(void *,void *,int);
   void             UndefmessageHandlerCommand(void *);
   int              EnvUndefmessageHandler(void *,void *,int);
#if DEBUGGING_FUNCTIONS
   void             PPDefmessageHandlerCommand(void *);
   void             ListDefmessageHandlersCommand(void *);
   void             PreviewSendCommand(void *); 
   const char      *EnvGetDefmessageHandlerPPForm(void *,void *,int);
   void             EnvListDefmessageHandlers(void *,const char *,void *,int);
   void             EnvPreviewSend(void *,const char *,void *,const char *);
   long             DisplayHandlersInLinks(void *,const char *,PACKED_CLASS_LINKS *,int);
#endif

#if ALLOW_ENVIRONMENT_GLOBALS

   unsigned         FindDefmessageHandler(void *,const char *,const char *);
   const char      *GetDefmessageHandlerName(void *,int);
   const char      *GetDefmessageHandlerType(void *,int);
   int              GetNextDefmessageHandler(void *,int);
   int              IsDefmessageHandlerDeletable(void *,int);
   int              UndefmessageHandler(void *,int);
#if DEBUGGING_FUNCTIONS
   const char      *GetDefmessageHandlerPPForm(void *,int);
   unsigned         GetDefmessageHandlerWatch(void *,int);
   void             ListDefmessageHandlers(const char *,void *,int);
   void             PreviewSend(const char *,void *,const char *);
   void             SetDefmessageHandlerWatch(int,void *,int);
#endif /* DEBUGGING_FUNCTIONS */

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_msgcom */





