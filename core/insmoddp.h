   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*           INSTANCE MODIFY AND DUPLICATE MODULE      */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*      6.30: Added DATA_OBJECT_ARRAY primitive type.        */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            The return value of DirectMessage indicates    */
/*            whether an execution error has occurred.       */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_insmoddp

#pragma once

#define _H_insmoddp

#define DIRECT_MODIFY_STRING    "direct-modify"
#define MSG_MODIFY_STRING       "message-modify"
#define DIRECT_DUPLICATE_STRING "direct-duplicate"
#define MSG_DUPLICATE_STRING    "message-duplicate"

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif

#if (! RUN_TIME)
   void                           SetupInstanceModDupCommands(Environment *);
#endif

   void                           ModifyInstance(Environment *,UDFContext *,CLIPSValue *);
   void                           MsgModifyInstance(Environment *,UDFContext *,CLIPSValue *);
   void                           DuplicateInstance(Environment *,UDFContext *,CLIPSValue *);
   void                           MsgDuplicateInstance(Environment *,UDFContext *,CLIPSValue *);

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           InactiveModifyInstance(Environment *,UDFContext *,CLIPSValue *);
   void                           InactiveMsgModifyInstance(Environment *,UDFContext *,CLIPSValue *);
   void                           InactiveDuplicateInstance(Environment *,UDFContext *,CLIPSValue *);
   void                           InactiveMsgDuplicateInstance(Environment *,UDFContext *,CLIPSValue *);
#endif

   void                           DirectModifyMsgHandler(Environment *,UDFContext *,CLIPSValue *);
   void                           MsgModifyMsgHandler(Environment *,UDFContext *,CLIPSValue *);
   void                           DirectDuplicateMsgHandler(Environment *,UDFContext *,CLIPSValue *);
   void                           MsgDuplicateMsgHandler(Environment *,UDFContext *,CLIPSValue *);

#endif /* _H_insmoddp */







