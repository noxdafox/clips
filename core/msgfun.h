   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/01/16            */
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
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
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

   void             UnboundHandlerErr(Environment *);
   void             PrintNoHandlerError(Environment *,const char *);
   bool             CheckHandlerArgCount(Environment *);
   void             SlotAccessViolationError(Environment *,const char *,Instance *,Defclass *);
   void             SlotVisibilityViolationError(Environment *,SlotDescriptor *,Defclass *,bool);

#if ! RUN_TIME
   void             NewSystemHandler(Environment *,const char *,const char *,const char *,int);
   DefmessageHandler
                   *InsertHandlerHeader(Environment *,Defclass *,CLIPSLexeme *,int);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   DefmessageHandler
                   *NewHandler(void);
   bool             HandlersExecuting(Defclass *);
   bool             DeleteHandler(Environment *,Defclass *,CLIPSLexeme *,int,bool);
   void             DeallocateMarkedHandlers(Environment *,Defclass *);
#endif
   unsigned         HandlerType(Environment *,const char *,const char *);
   bool             CheckCurrentMessage(Environment *,const char *,bool);
   void             PrintHandler(Environment *,const char *,DefmessageHandler *,bool);
   DefmessageHandler
                   *FindHandlerByAddress(Defclass *,CLIPSLexeme *,unsigned);
   int              FindHandlerByIndex(Defclass *,CLIPSLexeme *,unsigned);
   int              FindHandlerNameGroup(Defclass *,CLIPSLexeme *);
   void             HandlerDeleteError(Environment *,const char *);

#if DEBUGGING_FUNCTIONS
   void             DisplayCore(Environment *,const char *,HANDLER_LINK *,int);
   HANDLER_LINK    *FindPreviewApplicableHandlers(Environment *,Defclass *,CLIPSLexeme *);
   void             WatchMessage(Environment *,const char *,const char *);
   void             WatchHandler(Environment *,const char *,HANDLER_LINK *,const char *);
#endif

#endif /* _H_msgfun */







