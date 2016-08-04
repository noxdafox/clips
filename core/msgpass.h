   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_msgpass

#pragma once

#define _H_msgpass

#define GetActiveInstance(theEnv) ((Instance *) GetNthMessageArgument(theEnv,0)->value)

#include "object.h"

typedef struct messageHandlerLink
  {
   DefmessageHandler *hnd;
   struct messageHandlerLink *nxt;
   struct messageHandlerLink *nxtInStack;
  } HANDLER_LINK;

   bool             DirectMessage(Environment *,SYMBOL_HN *,Instance *,
                                  DATA_OBJECT *,EXPRESSION *);
   void             EnvSend(Environment *,DATA_OBJECT *,const char *,const char *,DATA_OBJECT *);
   void             DestroyHandlerLinks(Environment *,HANDLER_LINK *);
   void             SendCommand(Environment *,DATA_OBJECT *);
   DATA_OBJECT     *GetNthMessageArgument(Environment *,int);

   bool             NextHandlerAvailable(Environment *);
   void             CallNextHandler(Environment *,DATA_OBJECT *);

   void             FindApplicableOfName(Environment *,Defclass *,HANDLER_LINK *[],
                                         HANDLER_LINK *[],SYMBOL_HN *);
   HANDLER_LINK    *JoinHandlerLinks(Environment *,HANDLER_LINK *[],HANDLER_LINK *[],SYMBOL_HN *);

   void             PrintHandlerSlotGetFunction(Environment *,const char *,void *);
   bool             HandlerSlotGetFunction(Environment *,void *,DATA_OBJECT *);
   void             PrintHandlerSlotPutFunction(Environment *,const char *,void *);
   bool             HandlerSlotPutFunction(Environment *,void *,DATA_OBJECT *);
   void             DynamicHandlerGetSlot(Environment *,DATA_OBJECT *);
   void             DynamicHandlerPutSlot(Environment *,DATA_OBJECT *);

#if ALLOW_ENVIRONMENT_GLOBALS

   void             Send(DATA_OBJECT *,const char *,const char *,DATA_OBJECT *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_object */







