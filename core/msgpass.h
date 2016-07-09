   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS and        */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: The return value of DirectMessage indicates    */
/*            whether an execution error has occurred.       */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed garbage collection algorithm.          */
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

#ifndef _H_msgpass

#pragma once

#define _H_msgpass

#define GetActiveInstance(theEnv) ((INSTANCE_TYPE *) GetNthMessageArgument(theEnv,0)->value)

#include "object.h"

typedef struct messageHandlerLink
  {
   HANDLER *hnd;
   struct messageHandlerLink *nxt;
   struct messageHandlerLink *nxtInStack;
  } HANDLER_LINK;

   bool             DirectMessage(void *,SYMBOL_HN *,INSTANCE_TYPE *,
                                         DATA_OBJECT *,EXPRESSION *);
   void             EnvSend(void *,DATA_OBJECT *,const char *,const char *,DATA_OBJECT *);
   void             DestroyHandlerLinks(void *,HANDLER_LINK *);
   void             SendCommand(void *,DATA_OBJECT *);
   DATA_OBJECT     *GetNthMessageArgument(void *,int);

   bool             NextHandlerAvailable(void *);
   void             CallNextHandler(void *,DATA_OBJECT *);

   void             FindApplicableOfName(void *,DEFCLASS *,HANDLER_LINK *[],
                                                HANDLER_LINK *[],SYMBOL_HN *);
   HANDLER_LINK    *JoinHandlerLinks(void *,HANDLER_LINK *[],HANDLER_LINK *[],SYMBOL_HN *);

   void             PrintHandlerSlotGetFunction(void *,const char *,void *);
   bool             HandlerSlotGetFunction(void *,void *,DATA_OBJECT *);
   void             PrintHandlerSlotPutFunction(void *,const char *,void *);
   bool             HandlerSlotPutFunction(void *,void *,DATA_OBJECT *);
   void             DynamicHandlerGetSlot(void *,DATA_OBJECT *);
   void             DynamicHandlerPutSlot(void *,DATA_OBJECT *);

#if ALLOW_ENVIRONMENT_GLOBALS

   void             Send(DATA_OBJECT *,const char *,const char *,DATA_OBJECT *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_object */







