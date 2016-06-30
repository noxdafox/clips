   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Message-passing support functions                */
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
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS and        */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_msgfun

#pragma once

#define _H_msgfun

typedef struct handlerSlotReference
  {
   long classID;
   long slotID;
  } HANDLER_SLOT_REFERENCE;

#include "msgpass.h"
#include "object.h"

#define BEGIN_TRACE ">>"
#define END_TRACE   "<<"

/* =================================================================================
   Message-handler types - don't change these values: a string array depends on them
   ================================================================================= */
#define MAROUND        0
#define MBEFORE        1
#define MPRIMARY       2
#define MAFTER         3
#define MERROR         4

#define LOOKUP_HANDLER_INDEX   0
#define LOOKUP_HANDLER_ADDRESS 1

   void             UnboundHandlerErr(void *);
   void             PrintNoHandlerError(void *,const char *);
   int              CheckHandlerArgCount(void *);
   void             SlotAccessViolationError(void *,const char *,intBool,void *);
   void             SlotVisibilityViolationError(void *,SLOT_DESC *,DEFCLASS *);

#if ! RUN_TIME
   void             NewSystemHandler(void *,const char *,const char *,const char *,int);
   HANDLER         *InsertHandlerHeader(void *,DEFCLASS *,SYMBOL_HN *,int);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   HANDLER         *NewHandler(void);
   int              HandlersExecuting(DEFCLASS *);
   int              DeleteHandler(void *,DEFCLASS *,SYMBOL_HN *,int,int);
   void             DeallocateMarkedHandlers(void *,DEFCLASS *);
#endif
   unsigned         HandlerType(void *,const char *,const char *);
   int              CheckCurrentMessage(void *,const char *,int);
   void             PrintHandler(void *,const char *,HANDLER *,int);
   HANDLER         *FindHandlerByAddress(DEFCLASS *,SYMBOL_HN *,unsigned);
   int              FindHandlerByIndex(DEFCLASS *,SYMBOL_HN *,unsigned);
   int              FindHandlerNameGroup(DEFCLASS *,SYMBOL_HN *);
   void             HandlerDeleteError(void *,const char *);

#if DEBUGGING_FUNCTIONS
   void             DisplayCore(void *,const char *,HANDLER_LINK *,int);
   HANDLER_LINK    *FindPreviewApplicableHandlers(void *,DEFCLASS *,SYMBOL_HN *);
   void             WatchMessage(void *,const char *,const char *);
   void             WatchHandler(void *,const char *,HANDLER_LINK *,const char *);
#endif

#endif /* _H_msgfun */







