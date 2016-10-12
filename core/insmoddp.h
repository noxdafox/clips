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
/*            Removed DATA_OBJECT_ARRAY primitive type.      */
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

   void                           ModifyInstance(Environment *,UDFContext *,UDFValue *);
   void                           MsgModifyInstance(Environment *,UDFContext *,UDFValue *);
   void                           DuplicateInstance(Environment *,UDFContext *,UDFValue *);
   void                           MsgDuplicateInstance(Environment *,UDFContext *,UDFValue *);

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           InactiveModifyInstance(Environment *,UDFContext *,UDFValue *);
   void                           InactiveMsgModifyInstance(Environment *,UDFContext *,UDFValue *);
   void                           InactiveDuplicateInstance(Environment *,UDFContext *,UDFValue *);
   void                           InactiveMsgDuplicateInstance(Environment *,UDFContext *,UDFValue *);
#endif

   void                           DirectModifyMsgHandler(Environment *,UDFContext *,UDFValue *);
   void                           MsgModifyMsgHandler(Environment *,UDFContext *,UDFValue *);
   void                           DirectDuplicateMsgHandler(Environment *,UDFContext *,UDFValue *);
   void                           MsgDuplicateMsgHandler(Environment *,UDFContext *,UDFValue *);

#endif /* _H_insmoddp */







