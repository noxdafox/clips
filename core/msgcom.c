   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*                OBJECT MESSAGE COMMANDS              */
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
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
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

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#include <string.h>

#include "argacces.h"
#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif
#include "classcom.h"
#include "classfun.h"
#include "classinf.h"
#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "constrct.h"
#include "msgpsr.h"
#endif
#include "envrnmnt.h"
#if ! RUN_TIME
#include "extnfunc.h"
#endif
#include "insfun.h"
#include "insmoddp.h"
#include "msgfun.h"
#include "msgpass.h"
#include "memalloc.h"
#include "prccode.h"
#include "router.h"
#if DEBUGGING_FUNCTIONS
#include "watch.h"
#endif

#include "msgcom.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if ! RUN_TIME
   static void                    CreateSystemHandlers(Environment *);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)
   static bool                    WildDeleteHandler(Environment *,Defclass *,CLIPSLexeme *,const char *);
#endif

#if DEBUGGING_FUNCTIONS
   static bool                    DefmessageHandlerWatchAccess(Environment *,int,bool,EXPRESSION *);
   static bool                    DefmessageHandlerWatchPrint(Environment *,const char *,int,EXPRESSION *);
   static bool                    DefmessageHandlerWatchSupport(Environment *,const char *,const char *,bool,
                                                                void (*)(Environment *,const char *,Defclass *,int),
                                                                void (*)(Environment *,bool,Defclass *,int),
                                                                EXPRESSION *);
   static bool                    WatchClassHandlers(Environment *,Defclass *,const char *,int,const char *,bool,bool,
                                                     void (*)(Environment *,const char *,Defclass *,int),
                                                     void (*)(Environment *,bool,Defclass *,int));
   static void                    PrintHandlerWatchFlag(Environment *,const char *,Defclass *,int);
#endif

   static void                    DeallocateMessageHandlerData(Environment *);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupMessageHandlers
  DESCRIPTION  : Sets up internal symbols and
                 fucntion definitions pertaining to
                 message-handlers.  Also creates
                 system handlers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Functions and data structures
                 initialized
  NOTES        : Should be called before
                 SetupInstanceModDupCommands() in
                 INSMODDP.C
 ***************************************************/
void SetupMessageHandlers(
  Environment *theEnv)
  {
   ENTITY_RECORD handlerGetInfo = { "HANDLER_GET", HANDLER_GET,0,1,1,
                                        PrintHandlerSlotGetFunction,
                                        PrintHandlerSlotGetFunction,NULL,
                                        HandlerSlotGetFunction,
                                        NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL },

                 handlerPutInfo = { "HANDLER_PUT", HANDLER_PUT,0,1,1,
                                        PrintHandlerSlotPutFunction,
                                        PrintHandlerSlotPutFunction,NULL,
                                        HandlerSlotPutFunction,
                                        NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL };

   AllocateEnvironmentData(theEnv,MESSAGE_HANDLER_DATA,sizeof(struct messageHandlerData),DeallocateMessageHandlerData);
   memcpy(&MessageHandlerData(theEnv)->HandlerGetInfo,&handlerGetInfo,sizeof(struct entityRecord));
   memcpy(&MessageHandlerData(theEnv)->HandlerPutInfo,&handlerPutInfo,sizeof(struct entityRecord));

   MessageHandlerData(theEnv)->hndquals[0] = "around";
   MessageHandlerData(theEnv)->hndquals[1] = "before";
   MessageHandlerData(theEnv)->hndquals[2] = "primary";
   MessageHandlerData(theEnv)->hndquals[3] = "after";

   InstallPrimitive(theEnv,&MessageHandlerData(theEnv)->HandlerGetInfo,HANDLER_GET);
   InstallPrimitive(theEnv,&MessageHandlerData(theEnv)->HandlerPutInfo,HANDLER_PUT);

#if ! RUN_TIME
   MessageHandlerData(theEnv)->INIT_SYMBOL = EnvCreateSymbol(theEnv,INIT_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->INIT_SYMBOL);

   MessageHandlerData(theEnv)->DELETE_SYMBOL = EnvCreateSymbol(theEnv,DELETE_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->DELETE_SYMBOL);

   MessageHandlerData(theEnv)->CREATE_SYMBOL = EnvCreateSymbol(theEnv,CREATE_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->CREATE_SYMBOL);

   EnvAddClearFunction(theEnv,"defclass",CreateSystemHandlers,-100);

#if ! BLOAD_ONLY
   MessageHandlerData(theEnv)->SELF_SYMBOL = EnvCreateSymbol(theEnv,SELF_STRING);
   IncrementSymbolCount(MessageHandlerData(theEnv)->SELF_SYMBOL);

   AddConstruct(theEnv,"defmessage-handler","defmessage-handlers",
                ParseDefmessageHandler,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);
   EnvAddUDF(theEnv,"undefmessage-handler","v",2,3,"y",UndefmessageHandlerCommand,"UndefmessageHandlerCommand",NULL);

#endif

   EnvAddUDF(theEnv,"send","*",2,UNBOUNDED,"*;*;y",SendCommand,"SendCommand",NULL);

#if DEBUGGING_FUNCTIONS
   EnvAddUDF(theEnv,"preview-send","v",2,2,"y",PreviewSendCommand,"PreviewSendCommand",NULL);

   EnvAddUDF(theEnv,"ppdefmessage-handler","v",2,3,"y",PPDefmessageHandlerCommand,"PPDefmessageHandlerCommand",NULL);
   EnvAddUDF(theEnv,"list-defmessage-handlers","v",0,2,"y",ListDefmessageHandlersCommand,"ListDefmessageHandlersCommand",NULL);
#endif

   EnvAddUDF(theEnv,"next-handlerp","b",0,0,NULL,NextHandlerAvailableFunction,"NextHandlerAvailableFunction",NULL);
   FuncSeqOvlFlags(theEnv,"next-handlerp",true,false);
   EnvAddUDF(theEnv,"call-next-handler","*",0,0,NULL,CallNextHandler,"CallNextHandler",NULL);
   FuncSeqOvlFlags(theEnv,"call-next-handler",true,false);
   EnvAddUDF(theEnv,"override-next-handler","*",0,UNBOUNDED,NULL,CallNextHandler,"CallNextHandler",NULL);
   FuncSeqOvlFlags(theEnv,"override-next-handler",true,false);

   EnvAddUDF(theEnv,"dynamic-get","*",1,1,"y",DynamicHandlerGetSlot,"DynamicHandlerGetSlot",NULL);
   EnvAddUDF(theEnv,"dynamic-put","*",1,UNBOUNDED,"*;y",DynamicHandlerPutSlot,"DynamicHandlerPutSlot",NULL);
   EnvAddUDF(theEnv,"get","*",1,1,"y",DynamicHandlerGetSlot,"DynamicHandlerGetSlot",NULL);
   EnvAddUDF(theEnv,"put","*",1,UNBOUNDED,"*;y",DynamicHandlerPutSlot,"DynamicHandlerPutSlot",NULL);
#endif

#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,"messages",0,&MessageHandlerData(theEnv)->WatchMessages,36,NULL,NULL);
   AddWatchItem(theEnv,"message-handlers",0,&MessageHandlerData(theEnv)->WatchHandlers,35,
                DefmessageHandlerWatchAccess,DefmessageHandlerWatchPrint);
#endif
  }

/*******************************************************/
/* DeallocateMessageHandlerData: Deallocates environment */
/*    data for the message handler functionality.        */
/******************************************************/
static void DeallocateMessageHandlerData(
  Environment *theEnv)
  {
   HANDLER_LINK *tmp, *mhead, *chead;

   mhead = MessageHandlerData(theEnv)->TopOfCore;
   while (mhead != NULL)
     {
      tmp = mhead;
      mhead = mhead->nxt;
      rtn_struct(theEnv,messageHandlerLink,tmp);
     }

   chead = MessageHandlerData(theEnv)->OldCore;
   while (chead != NULL)
     {
      mhead = chead;
      chead = chead->nxtInStack;

      while (mhead != NULL)
        {
         tmp = mhead;
         mhead = mhead->nxt;
         rtn_struct(theEnv,messageHandlerLink,tmp);
        }
     }
  }

/*****************************************************
  NAME         : EnvGetDefmessageHandlerName
  DESCRIPTION  : Gets the name of a message-handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Name-string of message-handler
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
const char *EnvGetDefmessageHandlerName(
  Environment *theEnv,
  Defclass *theDefclass,
  int theIndex)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   return theDefclass->handlers[theIndex-1].header.name->contents;
  }

/*****************************************************
  NAME         : EnvGetDefmessageHandlerType
  DESCRIPTION  : Gets the type of a message-handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Type-string of message-handler
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
const char *EnvGetDefmessageHandlerType(
  Environment *theEnv,
  Defclass *theDefclass,
  int theIndex)
  {
   return MessageHandlerData(theEnv)->hndquals[theDefclass->handlers[theIndex-1].type];
  }

/**************************************************************
  NAME         : EnvGetNextDefmessageHandler
  DESCRIPTION  : Finds first or next handler for a class
  INPUTS       : 1) The address of the handler's class
                 2) The array index of the current handler (+1)
  RETURNS      : The array index (+1) of the next handler, or 0
                   if there is none
  SIDE EFFECTS : None
  NOTES        : If index == 0, the first handler array index
                 (i.e. 1) returned
 **************************************************************/
int EnvGetNextDefmessageHandler(
  Environment *theEnv,
  Defclass *theDefclass,
  int theIndex)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   if (theIndex == 0)
     { return (theDefclass->handlers != NULL) ? 1 : 0; }

   if (theIndex == theDefclass->handlerCount)
     { return 0; }

   return theIndex+1;
  }

/*****************************************************
  NAME         : GetDefmessageHandlerPointer
  DESCRIPTION  : Returns a pointer to a handler
  INPUTS       : 1) Pointer to a class
                 2) Array index of handler in class's
                    message-handler array (+1)
  RETURNS      : Pointer to the handler.
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************/
DefmessageHandler *GetDefmessageHandlerPointer(
  Defclass *theDefclass,
  int theIndex)
  {
   return &theDefclass->handlers[theIndex-1];
  }

#if DEBUGGING_FUNCTIONS

/*********************************************************
  NAME         : EnvGetDefmessageHandlerWatch
  DESCRIPTION  : Determines if trace messages for calls
                 to this handler will be generated or not
  INPUTS       : 1) A pointer to the class
                 2) The index of the handler
  RETURNS      : True if a trace is active,
                 false otherwise
  SIDE EFFECTS : None
  NOTES        : None
 *********************************************************/
bool EnvGetDefmessageHandlerWatch(
  Environment *theEnv,
  Defclass *theDefclass,
  int theIndex)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   return theDefclass->handlers[theIndex-1].trace;
  }

/*********************************************************
  NAME         : EnvSetDefmessageHandlerWatch
  DESCRIPTION  : Sets the trace to ON/OFF for the
                 calling of the handler
  INPUTS       : 1) True to set the trace on,
                    false to set it off
                 2) A pointer to the class
                 3) The index of the handler
  RETURNS      : Nothing useful
  SIDE EFFECTS : Watch flag for the handler set
  NOTES        : None
 *********************************************************/
void EnvSetDefmessageHandlerWatch(
  Environment *theEnv,
  bool newState,
  Defclass *theClass,
  int theIndex)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   theClass->handlers[theIndex-1].trace = newState;
  }

#endif

/***************************************************
  NAME         : EnvFindDefmessageHandler
  DESCRIPTION  : Determines the index of a specfied
                  message-handler
  INPUTS       : 1) A pointer to the class
                 2) Name-string of the handler
                 3) Handler-type: "around","before",
                    "primary", or "after"
  RETURNS      : The index of the handler
                   (0 if not found)
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
unsigned EnvFindDefmessageHandler(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *hname,
  const char *htypestr)
  {
   unsigned htype;
   CLIPSLexeme *hsym;
   int theIndex;

   htype = HandlerType(theEnv,"handler-lookup",htypestr);
   if (htype == MERROR)
     { return 0; }

   hsym = FindSymbolHN(theEnv,hname,SYMBOL_TYPE);
   if (hsym == NULL)
     { return 0; }

   theIndex = FindHandlerByIndex(theDefclass,hsym,(unsigned) htype);
   return (unsigned) (theIndex+1);
  }

/***************************************************
  NAME         : EnvIsDefmessageHandlerDeletable
  DESCRIPTION  : Determines if a message-handler
                   can be deleted
  INPUTS       : 1) Address of the handler's class
                 2) Index of the handler
  RETURNS      : True if deletable, false otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
bool EnvIsDefmessageHandlerDeletable(
  Environment *theEnv,
  Defclass *theDefclass,
  int theIndex)
  {
   if (! ConstructsDeletable(theEnv))
     { return false; }

   if (theDefclass->handlers[theIndex-1].system == 1)
     { return false; }

#if (! BLOAD_ONLY) && (! RUN_TIME)
   return (HandlersExecuting(theDefclass) == false) ? true : false;
#else
   return false;
#endif
  }

/******************************************************************************
  NAME         : UndefmessageHandlerCommand
  DESCRIPTION  : Deletes a handler from a class
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Handler deleted if possible
  NOTES        : H/L Syntax: (undefmessage-handler <class> <handler> [<type>])
 ******************************************************************************/
void UndefmessageHandlerCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
#if RUN_TIME || BLOAD_ONLY
   PrintErrorID(theEnv,"MSGCOM",3,false);
   EnvPrintRouter(theEnv,WERROR,"Unable to delete message-handlers.\n");
#else
   CLIPSLexeme *mname;
   const char *tname;
   CLIPSValue theArg;
   Defclass *cls;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv))
     {
      PrintErrorID(theEnv,"MSGCOM",3,false);
      EnvPrintRouter(theEnv,WERROR,"Unable to delete message-handlers.\n");
      return;
     }
#endif
   if (! UDFFirstArgument(context,SYMBOL_TYPE,&theArg)) return;

   cls = LookupDefclassByMdlOrScope(theEnv,theArg.lexemeValue->contents);
   if ((cls == NULL) ? (strcmp(theArg.lexemeValue->contents,"*") != 0) : false)
     {
      ClassExistError(theEnv,"undefmessage-handler",theArg.lexemeValue->contents);
      return;
     }
   if (! UDFNextArgument(context,SYMBOL_TYPE,&theArg)) return;

   mname = theArg.lexemeValue;
   if (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,SYMBOL_TYPE,&theArg)) return;

      tname = theArg.lexemeValue->contents;
      if (strcmp(tname,"*") == 0)
        tname = NULL;
     }
   else
     tname = MessageHandlerData(theEnv)->hndquals[MPRIMARY];
   WildDeleteHandler(theEnv,cls,mname,tname);
#endif
  }

/***********************************************************
  NAME         : EnvUndefmessageHandler
  DESCRIPTION  : Deletes a handler from a class
  INPUTS       : 1) Class address    (Can be NULL)
                 2) Handler index (can be 0)
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Handler deleted if possible
  NOTES        : None
 ***********************************************************/
bool EnvUndefmessageHandler(
  Environment *theEnv,
  Defclass *theDefclass,
  int mhi)
  {
#if RUN_TIME || BLOAD_ONLY
   PrintErrorID(theEnv,"MSGCOM",3,false);
   EnvPrintRouter(theEnv,WERROR,"Unable to delete message-handlers.\n");
   return false;
#else

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv))
     {
      PrintErrorID(theEnv,"MSGCOM",3,false);
      EnvPrintRouter(theEnv,WERROR,"Unable to delete message-handlers.\n");
      return false;
     }
#endif
   if (theDefclass == NULL)
     {
      if (mhi != 0)
        {
         PrintErrorID(theEnv,"MSGCOM",1,false);
         EnvPrintRouter(theEnv,WERROR,"Incomplete message-handler specification for deletion.\n");
         return false;
        }
      return WildDeleteHandler(theEnv,NULL,NULL,NULL);
     }

   if (mhi == 0)
     { return WildDeleteHandler(theEnv,theDefclass,NULL,NULL); }

   if (HandlersExecuting(theDefclass))
     {
      HandlerDeleteError(theEnv,EnvGetDefclassName(theEnv,theDefclass));
      return false;
     }

   theDefclass->handlers[mhi-1].mark = 1;
   DeallocateMarkedHandlers(theEnv,theDefclass);
   return true;
#endif
  }

#if DEBUGGING_FUNCTIONS

/*******************************************************************************
  NAME         : PPDefmessageHandlerCommand
  DESCRIPTION  : Displays the pretty-print form (if any) for a handler
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (ppdefmessage-handler <class> <message> [<type>])
 *******************************************************************************/
void PPDefmessageHandlerCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;
   CLIPSLexeme *csym, *msym;
   const char *tname;
   Defclass *cls = NULL;
   unsigned mtype;
   DefmessageHandler *hnd;

   if (! UDFFirstArgument(context,SYMBOL_TYPE,&theArg))
     { return; }

   csym = FindSymbolHN(theEnv,theArg.lexemeValue->contents,SYMBOL_TYPE);

   if (! UDFNextArgument(context,SYMBOL_TYPE,&theArg))
     { return; }

   msym = FindSymbolHN(theEnv,theArg.lexemeValue->contents,SYMBOL_TYPE);

   if (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,SYMBOL_TYPE,&theArg))
        { return; }
      tname = theArg.lexemeValue->contents;
     }
   else
     tname = MessageHandlerData(theEnv)->hndquals[MPRIMARY];
   mtype = HandlerType(theEnv,"ppdefmessage-handler",tname);
   if (mtype == MERROR)
     {
      EnvSetEvaluationError(theEnv,true);
      return;
     }
   if (csym != NULL)
     cls = LookupDefclassByMdlOrScope(theEnv,csym->contents);
   if (((cls == NULL) || (msym == NULL)) ? true :
       ((hnd = FindHandlerByAddress(cls,msym,(unsigned) mtype)) == NULL))
     {
      PrintErrorID(theEnv,"MSGCOM",2,false);
      EnvPrintRouter(theEnv,WERROR,"Unable to find message-handler ");
      EnvPrintRouter(theEnv,WERROR,msym->contents);
      EnvPrintRouter(theEnv,WERROR," ");
      EnvPrintRouter(theEnv,WERROR,tname);
      EnvPrintRouter(theEnv,WERROR," for class ");
      EnvPrintRouter(theEnv,WERROR,csym->contents);
      EnvPrintRouter(theEnv,WERROR," in function ppdefmessage-handler.\n");
      EnvSetEvaluationError(theEnv,true);
      return;
     }
   if (hnd->header.ppForm != NULL)
     PrintInChunks(theEnv,WDISPLAY,hnd->header.ppForm);
  }

/*****************************************************************************
  NAME         : ListDefmessageHandlersCommand
  DESCRIPTION  : Depending on arguments, does lists handlers which
                   match restrictions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : H/L Syntax: (list-defmessage-handlers [<class> [inherit]]))
 *****************************************************************************/
void ListDefmessageHandlersCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;

   if (UDFArgumentCount(context) == 0)
     EnvListDefmessageHandlers(theEnv,WDISPLAY,NULL,0);
   else
     {
      clsptr = ClassInfoFnxArgs(context,"list-defmessage-handlers",&inhp);
      if (clsptr == NULL)
        return;
      EnvListDefmessageHandlers(theEnv,WDISPLAY,clsptr,inhp);
     }
  }

/********************************************************************
  NAME         : PreviewSendCommand
  DESCRIPTION  : Displays a list of the core for a message describing
                   shadows,etc.
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Temporary core created and destroyed
  NOTES        : H/L Syntax: (preview-send <class> <msg>)
 ********************************************************************/
void PreviewSendCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   Defclass *cls;
   CLIPSValue theArg;

   /* =============================
      Get the class for the message
      ============================= */

   if (! UDFFirstArgument(context,SYMBOL_TYPE,&theArg))
     { return; }

   cls = LookupDefclassByMdlOrScope(theEnv,theArg.lexemeValue->contents);

   if (cls == NULL)
     {
      ClassExistError(theEnv,"preview-send",theArg.lexemeValue->contents);
      return;
     }

   if (! UDFNextArgument(context,SYMBOL_TYPE,&theArg))
     { return; }

   EnvPreviewSend(theEnv,WDISPLAY,cls,theArg.lexemeValue->contents);
  }

/********************************************************
  NAME         : EnvGetDefmessageHandlerPPForm
  DESCRIPTION  : Gets a message-handler pretty print form
  INPUTS       : 1) Address of the handler's class
                 2) Index of the handler
  RETURNS      : True if printable, false otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************/
const char *EnvGetDefmessageHandlerPPForm(
  Environment *theEnv,
  Defclass *theDefclass,
  int theIndex)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   return theDefclass->handlers[theIndex-1].header.ppForm;
  }

/*******************************************************************
  NAME         : EnvListDefmessageHandlers
  DESCRIPTION  : Lists message-handlers for a class
  INPUTS       : 1) The logical name of the output
                 2) Class name (NULL to display all handlers)
                 3) A flag indicating whether to list inherited
                    handlers or not
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 *******************************************************************/
void EnvListDefmessageHandlers(
  Environment *theEnv,
  const char *logName,
  Defclass *theDefclass,
  bool inhp)
  {
   long cnt;
   PACKED_CLASS_LINKS plinks;

   if (theDefclass != NULL)
     {
      if (inhp)
        { cnt = DisplayHandlersInLinks(theEnv,logName,&theDefclass->allSuperclasses,0); }
      else
        {
         plinks.classCount = 1;
         plinks.classArray = &theDefclass;
         cnt = DisplayHandlersInLinks(theEnv,logName,&plinks,0);
        }
     }
   else
     {
      plinks.classCount = 1;
      cnt = 0L;
      for (theDefclass = EnvGetNextDefclass(theEnv,NULL) ;
           theDefclass != NULL ;
           theDefclass = EnvGetNextDefclass(theEnv,theDefclass))
        {
         plinks.classArray = &theDefclass;
         cnt += DisplayHandlersInLinks(theEnv,logName,&plinks,0);
        }
     }
   PrintTally(theEnv,logName,cnt,"message-handler","message-handlers");
  }

/********************************************************************
  NAME         : EnvPreviewSend
  DESCRIPTION  : Displays a list of the core for a message describing
                   shadows,etc.
  INPUTS       : 1) Logical name of output
                 2) Class pointer
                 3) Message name-string
  RETURNS      : Nothing useful
  SIDE EFFECTS : Temporary core created and destroyed
  NOTES        : None
 ********************************************************************/
void EnvPreviewSend(
  Environment *theEnv,
  const char *logicalName,
  Defclass *theDefclass,
  const char *msgname)
  {
   HANDLER_LINK *core;
   CLIPSLexeme *msym;

   msym = FindSymbolHN(theEnv,msgname,SYMBOL_TYPE);
   if (msym == NULL)
     { return; }

   core = FindPreviewApplicableHandlers(theEnv,theDefclass,msym);
   if (core != NULL)
     {
      DisplayCore(theEnv,logicalName,core,0);
      DestroyHandlerLinks(theEnv,core);
     }
  }

/****************************************************
  NAME         : DisplayHandlersInLinks
  DESCRIPTION  : Recursively displays all handlers
                  for an array of classes
  INPUTS       : 1) The logical name of the output
                 2) The packed class links
                 3) The index to print from the links
  RETURNS      : The number of handlers printed
  SIDE EFFECTS : None
  NOTES        : Used by DescribeClass()
 ****************************************************/
long DisplayHandlersInLinks(
  Environment *theEnv,
  const char *logName,
  PACKED_CLASS_LINKS *plinks,
  int theIndex)
  {
   long i;
   long cnt;

   cnt = (long) plinks->classArray[theIndex]->handlerCount;
   if (((int) theIndex) < (plinks->classCount - 1))
     cnt += DisplayHandlersInLinks(theEnv,logName,plinks,theIndex + 1);
   for (i = 0 ; i < plinks->classArray[theIndex]->handlerCount ; i++)
     PrintHandler(theEnv,logName,&plinks->classArray[theIndex]->handlers[i],true);
   return(cnt);
  }

#endif

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if ! RUN_TIME

/**********************************************************
  NAME         : CreateSystemHandlers
  DESCRIPTION  : Attachess the system message-handlers
                 after a (clear)
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : System handlers created
  NOTES        : Must be called after CreateSystemClasses()
 **********************************************************/
static void CreateSystemHandlers(
  Environment *theEnv)
  {
   NewSystemHandler(theEnv,USER_TYPE_NAME,INIT_STRING,"init-slots",0);
   NewSystemHandler(theEnv,USER_TYPE_NAME,DELETE_STRING,"delete-instance",0);
   NewSystemHandler(theEnv,USER_TYPE_NAME,CREATE_STRING,"(create-instance)",0);

#if DEBUGGING_FUNCTIONS
   NewSystemHandler(theEnv,USER_TYPE_NAME,PRINT_STRING,"ppinstance",0);
#endif

   NewSystemHandler(theEnv,USER_TYPE_NAME,DIRECT_MODIFY_STRING,"(direct-modify)",1);
   NewSystemHandler(theEnv,USER_TYPE_NAME,MSG_MODIFY_STRING,"(message-modify)",1);
   NewSystemHandler(theEnv,USER_TYPE_NAME,DIRECT_DUPLICATE_STRING,"(direct-duplicate)",2);
   NewSystemHandler(theEnv,USER_TYPE_NAME,MSG_DUPLICATE_STRING,"(message-duplicate)",2);
  }

#endif

#if (! BLOAD_ONLY) && (! RUN_TIME)

/************************************************************
  NAME         : WildDeleteHandler
  DESCRIPTION  : Deletes a handler from a class
  INPUTS       : 1) Class address (Can be NULL)
                 2) Message Handler Name (Can be NULL)
                 3) Type name ("primary", etc.)
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Handler deleted if possible
  NOTES        : None
 ************************************************************/
static bool WildDeleteHandler(
  Environment *theEnv,
  Defclass *cls,
  CLIPSLexeme *msym,
  const char *tname)
  {
   int mtype;

   if (msym == NULL)
     msym = EnvCreateSymbol(theEnv,"*");
   if (tname != NULL)
     {
      mtype = (int) HandlerType(theEnv,"undefmessage-handler",tname);
      if (mtype == MERROR)
        return false;
     }
   else
     mtype = -1;
   if (cls == NULL)
     {
      bool success = true;

      for (cls = EnvGetNextDefclass(theEnv,NULL) ;
           cls != NULL ;
           cls = EnvGetNextDefclass(theEnv,cls))
        if (DeleteHandler(theEnv,cls,msym,mtype,false) == false)
          success = false;
      return(success);
     }
   return(DeleteHandler(theEnv,cls,msym,mtype,true));
  }

#endif

#if DEBUGGING_FUNCTIONS

/******************************************************************
  NAME         : DefmessageHandlerWatchAccess
  DESCRIPTION  : Parses a list of class names passed by
                 AddWatchItem() and sets the traces accordingly
  INPUTS       : 1) A code indicating which trace flag is to be set
                    0 - Watch instance creation/deletion
                    1 - Watch slot changes to instances
                 2) The value to which to set the trace flags
                 3) A list of expressions containing the names
                    of the classes for which to set traces
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : Watch flags set in specified classes
  NOTES        : Accessory function for AddWatchItem()
 ******************************************************************/
static bool DefmessageHandlerWatchAccess(
  Environment *theEnv,
  int code,
  bool newState,
  EXPRESSION *argExprs)
  {
#if MAC_XCD
#pragma unused(code)
#endif
   if (newState)
     return(DefmessageHandlerWatchSupport(theEnv,"watch",NULL,newState,
                                        NULL,EnvSetDefmessageHandlerWatch,argExprs));
   else
     return(DefmessageHandlerWatchSupport(theEnv,"unwatch",NULL,newState,
                                        NULL,EnvSetDefmessageHandlerWatch,argExprs));
  }

/***********************************************************************
  NAME         : DefmessageHandlerWatchPrint
  DESCRIPTION  : Parses a list of class names passed by
                 AddWatchItem() and displays the traces accordingly
  INPUTS       : 1) The logical name of the output
                 2) A code indicating which trace flag is to be examined
                    0 - Watch instance creation/deletion
                    1 - Watch slot changes to instances
                 3) A list of expressions containing the names
                    of the classes for which to examine traces
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : Watch flags displayed for specified classes
  NOTES        : Accessory function for AddWatchItem()
 ***********************************************************************/
static bool DefmessageHandlerWatchPrint(
  Environment *theEnv,
  const char *logName,
  int code,
  EXPRESSION *argExprs)
  {
#if MAC_XCD
#pragma unused(code)
#endif
   return(DefmessageHandlerWatchSupport(theEnv,"list-watch-items",logName,-1,
                                        PrintHandlerWatchFlag,NULL,argExprs));
  }

/*******************************************************
  NAME         : DefmessageHandlerWatchSupport
  DESCRIPTION  : Sets or displays handlers specified
  INPUTS       : 1) The calling function name
                 2) The logical output name for displays
                    (can be NULL)
                 4) The new set state (can be -1)
                 5) The print function (can be NULL)
                 6) The trace function (can be NULL)
                 7) The handlers expression list
  RETURNS      : True if all OK,
                 false otherwise
  SIDE EFFECTS : Handler trace flags set or displayed
  NOTES        : None
 *******************************************************/
static bool DefmessageHandlerWatchSupport(
  Environment *theEnv,
  const char *funcName,
  const char *logName,
  bool newState,
  void (*printFunc)(Environment *,const char *,Defclass *,int),
  void (*traceFunc)(Environment *,bool,Defclass *,int),
  EXPRESSION *argExprs)
  {
   Defmodule *theModule;
   Defclass *theClass;
   const char *theHandlerStr;
   int theType;
   int argIndex = 2;
   CLIPSValue tmpData;

   /* ===============================
      If no handlers are specified,
      show the trace for all handlers
      in all handlers
      =============================== */
   if (argExprs == NULL)
     {
      SaveCurrentModule(theEnv);
      theModule = EnvGetNextDefmodule(theEnv,NULL);
      while (theModule != NULL)
        {
         EnvSetCurrentModule(theEnv,theModule);
         if (traceFunc == NULL)
           {
            EnvPrintRouter(theEnv,logName,EnvGetDefmoduleName(theEnv,theModule));
            EnvPrintRouter(theEnv,logName,":\n");
           }
         theClass = EnvGetNextDefclass(theEnv,NULL);
         while (theClass != NULL)
            {
             if (WatchClassHandlers(theEnv,theClass,NULL,-1,logName,newState,
                                    true,printFunc,traceFunc) == false)
                 return false;
             theClass = EnvGetNextDefclass(theEnv,theClass);
            }
          theModule = EnvGetNextDefmodule(theEnv,theModule);
         }
      RestoreCurrentModule(theEnv);
      return true;
     }

   /* ================================================
      Set or show the traces for the specified handler
      ================================================ */
   while (argExprs != NULL)
     {
      if (EvaluateExpression(theEnv,argExprs,&tmpData))
        return false;
      if (tmpData.header->type != SYMBOL)
        {
         ExpectedTypeError1(theEnv,funcName,argIndex,"class name");
         return false;
        }
      theClass = LookupDefclassByMdlOrScope(theEnv,tmpData.lexemeValue->contents);
      if (theClass == NULL)
        {
         ExpectedTypeError1(theEnv,funcName,argIndex,"class name");
         return false;
        }
      if (GetNextArgument(argExprs) != NULL)
        {
         argExprs = GetNextArgument(argExprs);
         argIndex++;
         if (EvaluateExpression(theEnv,argExprs,&tmpData))
           return false;
         if (tmpData.header->type != SYMBOL)
           {
            ExpectedTypeError1(theEnv,funcName,argIndex,"handler name");
            return false;
           }
         theHandlerStr = tmpData.lexemeValue->contents;
         if (GetNextArgument(argExprs) != NULL)
           {
            argExprs = GetNextArgument(argExprs);
            argIndex++;
            if (EvaluateExpression(theEnv,argExprs,&tmpData))
              return false;
            if (tmpData.header->type != SYMBOL)
              {
               ExpectedTypeError1(theEnv,funcName,argIndex,"handler type");
               return false;
              }
            if ((theType = (int) HandlerType(theEnv,funcName,tmpData.lexemeValue->contents)) == MERROR)
              return false;
           }
         else
           theType = -1;
        }
      else
        {
         theHandlerStr = NULL;
         theType = -1;
        }
      if (WatchClassHandlers(theEnv,theClass,theHandlerStr,theType,logName,
                             newState,false,printFunc,traceFunc) == false)
        {
         ExpectedTypeError1(theEnv,funcName,argIndex,"handler");
         return false;
        }
      argIndex++;
      argExprs = GetNextArgument(argExprs);
     }
   return true;
  }

/*******************************************************
  NAME         : WatchClassHandlers
  DESCRIPTION  : Sets or displays handlers specified
  INPUTS       : 1) The class
                 2) The handler name (or NULL wildcard)
                 3) The handler type (or -1 wildcard)
                 4) The logical output name for displays
                    (can be NULL)
                 5) The new set state (can be -1)
                 6) The print function (can be NULL)
                 7) The trace function (can be NULL)
  RETURNS      : True if all OK,
                 false otherwise
  SIDE EFFECTS : Handler trace flags set or displayed
  NOTES        : None
 *******************************************************/
static bool WatchClassHandlers(
  Environment *theEnv,
  Defclass *theClass,
  const char *theHandlerStr,
  int theType,
  const char *logName,
  bool newState,
  bool indentp,
  void (*printFunc)(Environment *,const char *,Defclass *,int),
  void (*traceFunc)(Environment *,bool,Defclass *,int))
  {
   unsigned theHandler;
   bool found = false;

   theHandler = EnvGetNextDefmessageHandler(theEnv,theClass,0);
   while (theHandler != 0)
     {
      if ((theType == -1) ? true :
          (theType == (int) theClass->handlers[theHandler-1].type))
        {
         if ((theHandlerStr == NULL) ? true :
             (strcmp(theHandlerStr,EnvGetDefmessageHandlerName(theEnv,theClass,theHandler)) == 0))
            {
             if (traceFunc != NULL)
               (*traceFunc)(theEnv,newState,theClass,theHandler);
             else
               {
                if (indentp)
                  EnvPrintRouter(theEnv,logName,"   ");
                (*printFunc)(theEnv,logName,theClass,theHandler);
               }
             found = true;
            }
        }
      theHandler = EnvGetNextDefmessageHandler(theEnv,theClass,theHandler);
     }
   if ((theHandlerStr != NULL) && (theType != -1) && (found == false))
     return false;
   return true;
  }

/***************************************************
  NAME         : PrintHandlerWatchFlag
  DESCRIPTION  : Displays trace value for handler
  INPUTS       : 1) The logical name of the output
                 2) The class
                 3) The handler index
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ***************************************************/
static void PrintHandlerWatchFlag(
  Environment *theEnv,
  const char *logName,
  Defclass *theClass,
  int theHandler)
  {
   EnvPrintRouter(theEnv,logName,EnvGetDefclassName(theEnv,theClass));
   EnvPrintRouter(theEnv,logName," ");
   EnvPrintRouter(theEnv,logName,EnvGetDefmessageHandlerName(theEnv,theClass,theHandler));
   EnvPrintRouter(theEnv,logName," ");
   EnvPrintRouter(theEnv,logName,EnvGetDefmessageHandlerType(theEnv,theClass,theHandler));

   if (EnvGetDefmessageHandlerWatch(theEnv,theClass,theHandler))
     EnvPrintRouter(theEnv,logName," = on\n");
   else
     EnvPrintRouter(theEnv,logName," = off\n");
  }

#endif /* DEBUGGING_FUNCTIONS */

#endif /* OBJECT_SYSTEM */

