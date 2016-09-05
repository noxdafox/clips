   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*        INSTANCE MODIFY AND DUPLICATE MODULE         */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Instance modify and duplicate support routines  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
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

#include "argacces.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "inscom.h"
#include "insfun.h"
#include "insmngr.h"
#include "inspsr.h"
#include "memalloc.h"
#include "miscfun.h"
#include "msgcom.h"
#include "msgfun.h"
#include "msgpass.h"
#if DEFRULE_CONSTRUCT
#include "network.h"
#include "objrtmch.h"
#endif
#include "prccode.h"
#include "router.h"

#include "insmoddp.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static CLIPSValue             *EvaluateSlotOverrides(Environment *,EXPRESSION *,int *,bool *);
   static void                    DeleteSlotOverrideEvaluations(Environment *,CLIPSValue *,int);
   static void                    ModifyMsgHandlerSupport(Environment *,CLIPSValue *,bool);
   static void                    DuplicateMsgHandlerSupport(Environment *,CLIPSValue *,bool);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if (! RUN_TIME)

/***************************************************
  NAME         : SetupInstanceModDupCommands
  DESCRIPTION  : Defines function interfaces for
                 modify- and duplicate- instance
                 functions
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Functions defined to KB
  NOTES        : None
 ***************************************************/
void SetupInstanceModDupCommands(
  Environment *theEnv)
  {
#if DEFRULE_CONSTRUCT
   EnvAddUDF(theEnv,"modify-instance","*",0,UNBOUNDED,NULL,InactiveModifyInstance,"InactiveModifyInstance",NULL);
   EnvAddUDF(theEnv,"active-modify-instance","*",0,UNBOUNDED,NULL,ModifyInstance,"ModifyInstance",NULL);
   AddFunctionParser(theEnv,"active-modify-instance",ParseInitializeInstance);
   EnvAddUDF(theEnv,"message-modify-instance","*",0,UNBOUNDED,NULL,InactiveMsgModifyInstance,"InactiveMsgModifyInstance",NULL);
   EnvAddUDF(theEnv,"active-message-modify-instance","*",0,UNBOUNDED,NULL,MsgModifyInstance,"MsgModifyInstance",NULL);
   AddFunctionParser(theEnv,"active-message-modify-instance",ParseInitializeInstance);

   EnvAddUDF(theEnv,"duplicate-instance","*",0,UNBOUNDED,NULL,InactiveDuplicateInstance,"InactiveDuplicateInstance",NULL);
   EnvAddUDF(theEnv,"active-duplicate-instance","*",0,UNBOUNDED,NULL,DuplicateInstance,"DuplicateInstance",NULL);
   AddFunctionParser(theEnv,"active-duplicate-instance",ParseInitializeInstance);
   EnvAddUDF(theEnv,"message-duplicate-instance","*",0,UNBOUNDED,NULL,InactiveMsgDuplicateInstance,"InactiveMsgDuplicateInstance",NULL);
   EnvAddUDF(theEnv,"active-message-duplicate-instance","*",0,UNBOUNDED,NULL,MsgDuplicateInstance,"MsgDuplicateInstance",NULL);
   AddFunctionParser(theEnv,"active-message-duplicate-instance",ParseInitializeInstance);
#else
   EnvAddUDF(theEnv,"modify-instance","*",0,UNBOUNDED,NULL,ModifyInstance,"ModifyInstance",NULL);
   EnvAddUDF(theEnv,"message-modify-instance","*",0,UNBOUNDED,NULL,MsgModifyInstance,"MsgModifyInstance",NULL);
   EnvAddUDF(theEnv,"duplicate-instance","*",0,UNBOUNDED,NULL,DuplicateInstance,"DuplicateInstance",NULL);
   EnvAddUDF(theEnv,"message-duplicate-instance","*",0,UNBOUNDED,NULL,MsgDuplicateInstance,"MsgDuplicateInstance",NULL);
#endif

   EnvAddUDF(theEnv,"(direct-modify)","*",0,UNBOUNDED,NULL,DirectModifyMsgHandler,"DirectModifyMsgHandler",NULL);
   EnvAddUDF(theEnv,"(message-modify)","*",0,UNBOUNDED,NULL,MsgModifyMsgHandler,"MsgModifyMsgHandler",NULL);
   EnvAddUDF(theEnv,"(direct-duplicate)","*",0,UNBOUNDED,NULL,DirectDuplicateMsgHandler,"DirectDuplicateMsgHandler",NULL);
   EnvAddUDF(theEnv,"(message-duplicate)","*",0,UNBOUNDED,NULL,MsgDuplicateMsgHandler,"MsgDuplicateMsgHandler",NULL);

   AddFunctionParser(theEnv,"modify-instance",ParseInitializeInstance);
   AddFunctionParser(theEnv,"message-modify-instance",ParseInitializeInstance);
   AddFunctionParser(theEnv,"duplicate-instance",ParseInitializeInstance);
   AddFunctionParser(theEnv,"message-duplicate-instance",ParseInitializeInstance);
  }

#endif

/*************************************************************
  NAME         : ModifyInstance
  DESCRIPTION  : Modifies slots of an instance via the
                 direct-modify message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (modify-instance <instance> <slot-override>*)
 *************************************************************/
void ModifyInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   Instance *ins;
   EXPRESSION theExp;
   CLIPSValue *overrides;
   bool oldOMDMV;
   int overrideCount;
   bool error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */

   overrides = EvaluateSlotOverrides(theEnv,GetFirstArgument()->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(context);
   if (ins == NULL)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the modify
      ====================================== */
   theExp.type = DATA_OBJECT_ARRAY;
   theExp.value = overrides;
   theExp.argList = NULL;
   theExp.nextArg = NULL;

   oldOMDMV = InstanceData(theEnv)->ObjectModDupMsgValid;
   InstanceData(theEnv)->ObjectModDupMsgValid = true;
   DirectMessage(theEnv,FindSymbolHN(theEnv,DIRECT_MODIFY_STRING),ins,returnValue,&theExp);
   InstanceData(theEnv)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
  }

/*************************************************************
  NAME         : MsgModifyInstance
  DESCRIPTION  : Modifies slots of an instance via the
                 direct-modify message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed with put- messages
  NOTES        : H/L Syntax:
                 (message-modify-instance <instance>
                    <slot-override>*)
 *************************************************************/
void MsgModifyInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   Instance *ins;
   EXPRESSION theExp;
   CLIPSValue *overrides;
   bool oldOMDMV;
   int overrideCount;
   bool error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */
   overrides = EvaluateSlotOverrides(theEnv,GetFirstArgument()->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(context);
   if (ins == NULL)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the modify
      ====================================== */
   theExp.type = DATA_OBJECT_ARRAY;
   theExp.value = overrides;
   theExp.argList = NULL;
   theExp.nextArg = NULL;

   oldOMDMV = InstanceData(theEnv)->ObjectModDupMsgValid;
   InstanceData(theEnv)->ObjectModDupMsgValid = true;
   DirectMessage(theEnv,FindSymbolHN(theEnv,MSG_MODIFY_STRING),ins,returnValue,&theExp);
   InstanceData(theEnv)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
  }

/*************************************************************
  NAME         : DuplicateInstance
  DESCRIPTION  : Duplicates an instance via the
                 direct-duplicate message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (duplicate-instance <instance>
                   [to <instance-name>] <slot-override>*)
 *************************************************************/
void DuplicateInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   Instance *ins;
   CLIPSValue newName;
   EXPRESSION theExp[2];
   CLIPSValue *overrides;
   bool oldOMDMV;
   int overrideCount;
   bool error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */
   overrides = EvaluateSlotOverrides(theEnv,GetFirstArgument()->nextArg->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(context);
   if (ins == NULL)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
      return;
     }

   if (! UDFNextArgument(context,INSTANCE_NAME_TYPE | SYMBOL_TYPE,&newName))
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the duplicate
      ====================================== */
   theExp[0].type = INSTANCE_NAME;
   theExp[0].value = newName.value;
   theExp[0].argList = NULL;
   theExp[0].nextArg = &theExp[1];
   theExp[1].type = DATA_OBJECT_ARRAY;
   theExp[1].value = overrides;
   theExp[1].argList = NULL;
   theExp[1].nextArg = NULL;

   oldOMDMV = InstanceData(theEnv)->ObjectModDupMsgValid;
   InstanceData(theEnv)->ObjectModDupMsgValid = true;
   DirectMessage(theEnv,FindSymbolHN(theEnv,DIRECT_DUPLICATE_STRING),ins,returnValue,&theExp[0]);
   InstanceData(theEnv)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
  }

/*************************************************************
  NAME         : MsgDuplicateInstance
  DESCRIPTION  : Duplicates an instance via the
                 message-duplicate message
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed w/ int & put- messages
  NOTES        : H/L Syntax:
                 (duplicate-instance <instance>
                   [to <instance-name>] <slot-override>*)
 *************************************************************/
void MsgDuplicateInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   Instance *ins;
   CLIPSValue newName;
   EXPRESSION theExp[2];
   CLIPSValue *overrides;
   bool oldOMDMV;
   int overrideCount;
   bool error;

   /* ===========================================
      The slot-overrides need to be evaluated now
      to resolve any variable references before a
      new frame is pushed for message-handler
      execution
      =========================================== */
   overrides = EvaluateSlotOverrides(theEnv,GetFirstArgument()->nextArg->nextArg,
                                     &overrideCount,&error);
   if (error)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      return;
     }

   /* ==================================
      Find the instance and make sure it
      wasn't deleted by the overrides
      ================================== */
   ins = CheckInstance(context);
   if (ins == NULL)
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
      return;
     }
   if (! UDFNextArgument(context,INSTANCE_NAME_TYPE | SYMBOL_TYPE,&newName))
     {
      SetpType(returnValue,SYMBOL);
      SetpValue(returnValue,EnvFalseSymbol(theEnv));
      DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
      return;
     }

   /* ======================================
      We are passing the slot override
      expression information along
      to whatever message-handler implements
      the duplicate
      ====================================== */
   theExp[0].type = INSTANCE_NAME;
   theExp[0].value = newName.value;
   theExp[0].argList = NULL;
   theExp[0].nextArg = &theExp[1];
   theExp[1].type = DATA_OBJECT_ARRAY;
   theExp[1].value = overrides;
   theExp[1].argList = NULL;
   theExp[1].nextArg = NULL;

   oldOMDMV = InstanceData(theEnv)->ObjectModDupMsgValid;
   InstanceData(theEnv)->ObjectModDupMsgValid = true;
   DirectMessage(theEnv,FindSymbolHN(theEnv,MSG_DUPLICATE_STRING),ins,returnValue,&theExp[0]);
   InstanceData(theEnv)->ObjectModDupMsgValid = oldOMDMV;

   DeleteSlotOverrideEvaluations(theEnv,overrides,overrideCount);
  }

#if DEFRULE_CONSTRUCT

/**************************************************************
  NAME         : InactiveModifyInstance
  DESCRIPTION  : Modifies slots of an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (modify-instance <instance-name>
                   <slot-override>*)
 **************************************************************/
void InactiveModifyInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool ov;

   ov = SetDelayObjectPatternMatching(theEnv,true);
   ModifyInstance(theEnv,context,returnValue);
   SetDelayObjectPatternMatching(theEnv,ov);
  }

/**************************************************************
  NAME         : InactiveMsgModifyInstance
  DESCRIPTION  : Modifies slots of an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed with put- messages
  NOTES        : H/L Syntax:
                 (message-modify-instance <instance-name>
                   <slot-override>*)
 **************************************************************/
void InactiveMsgModifyInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool ov;

   ov = SetDelayObjectPatternMatching(theEnv,true);
   MsgModifyInstance(theEnv,context,returnValue);
   SetDelayObjectPatternMatching(theEnv,ov);
  }

/*******************************************************************
  NAME         : InactiveDuplicateInstance
  DESCRIPTION  : Duplicates an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed directly
  NOTES        : H/L Syntax:
                 (duplicate-instance <instance> [to <instance-name>]
                   <slot-override>*)
 *******************************************************************/
void InactiveDuplicateInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool ov;

   ov = SetDelayObjectPatternMatching(theEnv,true);
   DuplicateInstance(theEnv,context,returnValue);
   SetDelayObjectPatternMatching(theEnv,ov);
  }

/**************************************************************
  NAME         : InactiveMsgDuplicateInstance
  DESCRIPTION  : Duplicates an instance of a class
                 Pattern-matching is automatically
                 delayed until the slot updates are done
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot updates performed with put- messages
  NOTES        : H/L Syntax:
                 (message-duplicate-instance <instance>
                   [to <instance-name>]
                   <slot-override>*)
 **************************************************************/
void InactiveMsgDuplicateInstance(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool ov;

   ov = SetDelayObjectPatternMatching(theEnv,true);
   MsgDuplicateInstance(theEnv,context,returnValue);
   SetDelayObjectPatternMatching(theEnv,ov);
  }

#endif

/*****************************************************
  NAME         : DirectDuplicateMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler direct-duplicate

                 Implements duplicate-instance message
                 with a series of direct slot
                 placements
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 *****************************************************/
void DirectDuplicateMsgHandler(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   DuplicateMsgHandlerSupport(theEnv,returnValue,false);
  }

/*****************************************************
  NAME         : MsgDuplicateMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler message-duplicate

                 Implements duplicate-instance message
                 with a series of put- messages
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 *****************************************************/
void MsgDuplicateMsgHandler(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   DuplicateMsgHandlerSupport(theEnv,returnValue,true);
  }

/***************************************************
  NAME         : DirectModifyMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler direct-modify

                 Implements modify-instance message
                 with a series of direct slot
                 placements
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 ***************************************************/
void DirectModifyMsgHandler(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   ModifyMsgHandlerSupport(theEnv,returnValue,false);
  }

/***************************************************
  NAME         : MsgModifyMsgHandler
  DESCRIPTION  : Implementation for the USER class
                 handler message-modify

                 Implements modify-instance message
                 with a series of put- messages
  INPUTS       : A data object buffer to hold the
                 result
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slot values updated
  NOTES        : None
 ***************************************************/
void MsgModifyMsgHandler(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   ModifyMsgHandlerSupport(theEnv,returnValue,true);
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : EvaluateSlotOverrides
  DESCRIPTION  : Evaluates the slot-override expressions
                 for modify-instance and duplicate-instance
                 Evaluations are stored in an array of
                 data objects, where the supplementalInfo
                 field points at the name of the slot
                 The data object next fields are used
                 to link the array as well.
  INPUTS       : 1) The slot override expressions
                 2) A buffer to hold the number
                    of slot overrides
                 3) A buffer to hold an error flag
  RETURNS      : The slot override data object array
  SIDE EFFECTS : Data object array allocated and initialized
                 override count and error buffers set
  NOTES        : Slot overrides must be evaluated before
                 calling supporting message-handlers for
                 modify- and duplicate-instance in the
                 event that the overrides contain variable
                 references to an outer frame
 ***********************************************************/
static CLIPSValue *EvaluateSlotOverrides(
  Environment *theEnv,
  EXPRESSION *ovExprs,
  int *ovCnt,
  bool *error)
  {
   CLIPSValue *ovs;
   int ovi;
   void *slotName;

   *error = false;

   /* ==========================================
      There are two expressions chains for every
      slot override: one for the slot name and
      one for the slot value
      ========================================== */
   *ovCnt = CountArguments(ovExprs) / 2;
   if (*ovCnt == 0)
     return NULL;

   /* ===============================================
      Evaluate all the slot override names and values
      and store them in a contiguous array
      =============================================== */
   ovs = (CLIPSValue *) gm2(theEnv,(sizeof(CLIPSValue) * (*ovCnt)));
   ovi = 0;
   while (ovExprs != NULL)
     {
      if (EvaluateExpression(theEnv,ovExprs,&ovs[ovi]))
        goto EvaluateOverridesError;
      if (ovs[ovi].type != SYMBOL)
        {
         ExpectedTypeError1(theEnv,ValueToString(ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)),
                            ovi+1,"slot name");
         EnvSetEvaluationError(theEnv,true);
         goto EvaluateOverridesError;
        }
      slotName = ovs[ovi].value;
      if (ovExprs->nextArg->argList)
        {
         if (EvaluateAndStoreInDataObject(theEnv,false,ovExprs->nextArg->argList,
                                               &ovs[ovi],true) == false)
           goto EvaluateOverridesError;
        }
      else
        {
         SetpDOBegin(&ovs[ovi],1);
         SetpDOEnd(&ovs[ovi],0);
         SetpType(&ovs[ovi],MULTIFIELD);
         SetpValue(&ovs[ovi],ProceduralPrimitiveData(theEnv)->NoParamValue);
        }
      ovs[ovi].supplementalInfo = slotName;
      ovExprs = ovExprs->nextArg->nextArg;
      ovs[ovi].next = (ovExprs != NULL) ? &ovs[ovi+1] : NULL;
      ovi++;
     }
   return(ovs);

EvaluateOverridesError:
   rm(theEnv,ovs,(sizeof(CLIPSValue) * (*ovCnt)));
   *error = true;
   return NULL;
  }

/**********************************************************
  NAME         : DeleteSlotOverrideEvaluations
  DESCRIPTION  : Deallocates slot override evaluation array
  INPUTS       : 1) The data object array
                 2) The number of elements
  RETURNS      : Nothing useful
  SIDE EFFECTS : Deallocates slot override data object
                 array for modify- and duplicate- instance
  NOTES        : None
 **********************************************************/
static void DeleteSlotOverrideEvaluations(
  Environment *theEnv,
  CLIPSValue *ovEvals,
  int ovCnt)
  {
   if (ovEvals != NULL)
     rm(theEnv,ovEvals,(sizeof(CLIPSValue) * ovCnt));
  }

/**********************************************************
  NAME         : ModifyMsgHandlerSupport
  DESCRIPTION  : Support routine for DirectModifyMsgHandler
                 and MsgModifyMsgHandler

                 Performs a series of slot updates
                 directly or with messages
  INPUTS       : 1) A data object buffer to hold the result
                 2) A flag indicating whether to use
                    put- messages or direct placement
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slots updated (messages sent)
  NOTES        : None
 **********************************************************/
static void ModifyMsgHandlerSupport(
  Environment *theEnv,
  CLIPSValue *returnValue,
  bool msgpass)
  {
   CLIPSValue *slotOverrides,*newval,temp,junk;
   EXPRESSION msgExp;
   Instance *ins;
   INSTANCE_SLOT *insSlot;

   returnValue->type = SYMBOL;
   returnValue->value = EnvFalseSymbol(theEnv);
   if (InstanceData(theEnv)->ObjectModDupMsgValid == false)
     {
      PrintErrorID(theEnv,"INSMODDP",1,false);
      EnvPrintRouter(theEnv,WERROR,"Direct/message-modify message valid only in modify-instance.\n");
      EnvSetEvaluationError(theEnv,true);
      return;
     }
   InstanceData(theEnv)->ObjectModDupMsgValid = false;

   ins = GetActiveInstance(theEnv);
   if (ins->garbage)
     {
      StaleInstanceAddress(theEnv,"modify-instance",0);
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   /* =======================================
      Retrieve the slot override data objects
      passed from ModifyInstance - the slot
      name is stored in the supplementalInfo
      field - and the next fields are links
      ======================================= */
   slotOverrides = (CLIPSValue *) GetNthMessageArgument(theEnv,1)->value;

   while (slotOverrides != NULL)
     {
      /* ===========================================================
         No evaluation or error checking needs to be done
         since this has already been done by EvaluateSlotOverrides()
         =========================================================== */
      insSlot = FindInstanceSlot(theEnv,ins,(SYMBOL_HN *) slotOverrides->supplementalInfo);
      if (insSlot == NULL)
        {
         SlotExistError(theEnv,ValueToString(slotOverrides->supplementalInfo),"modify-instance");
         EnvSetEvaluationError(theEnv,true);
         return;
        }
      if (msgpass)
        {
         msgExp.type = slotOverrides->type;
         if (msgExp.type != MULTIFIELD)
           msgExp.value = slotOverrides->value;
         else
           msgExp.value = slotOverrides;
         msgExp.argList = NULL;
         msgExp.nextArg = NULL;
         if (! DirectMessage(theEnv,insSlot->desc->overrideMessage,ins,&temp,&msgExp))
           return;
        }
      else
        {
         if (insSlot->desc->multiple && (slotOverrides->type != MULTIFIELD))
           {
            temp.type = MULTIFIELD;
            temp.value = EnvCreateMultifield(theEnv,1L);
            SetDOBegin(temp,1);
            SetDOEnd(temp,1);
            SetMFType(temp.value,1,(short) slotOverrides->type);
            SetMFValue(temp.value,1,slotOverrides->value);
            newval = &temp;
           }
         else
           newval = slotOverrides;
         if (PutSlotValue(theEnv,ins,insSlot,newval,&junk,"modify-instance") == false)
           return;
        }

      slotOverrides = slotOverrides->next;
     }
   returnValue->value = EnvTrueSymbol(theEnv);
  }

/*************************************************************
  NAME         : DuplicateMsgHandlerSupport
  DESCRIPTION  : Support routine for DirectDuplicateMsgHandler
                 and MsgDuplicateMsgHandler

                 Performs a series of slot updates
                 directly or with messages
  INPUTS       : 1) A data object buffer to hold the result
                 2) A flag indicating whether to use
                    put- messages or direct placement
  RETURNS      : Nothing useful
  SIDE EFFECTS : Slots updated (messages sent)
  NOTES        : None
 *************************************************************/
static void DuplicateMsgHandlerSupport(
  Environment *theEnv,
  CLIPSValue *returnValue,
  bool msgpass)
  {
   Instance *srcins,*dstins;
   SYMBOL_HN *newName;
   CLIPSValue *slotOverrides;
   EXPRESSION *valArg,msgExp;
   long i;
   int oldMkInsMsgPass;
   INSTANCE_SLOT *dstInsSlot;
   CLIPSValue temp,junk,*newval;
   bool success;

   returnValue->type = SYMBOL;
   returnValue->value = EnvFalseSymbol(theEnv);
   if (InstanceData(theEnv)->ObjectModDupMsgValid == false)
     {
      PrintErrorID(theEnv,"INSMODDP",2,false);
      EnvPrintRouter(theEnv,WERROR,"Direct/message-duplicate message valid only in duplicate-instance.\n");
      EnvSetEvaluationError(theEnv,true);
      return;
     }
   InstanceData(theEnv)->ObjectModDupMsgValid = false;

   /* ==================================
      Grab the slot override expressions
      and determine the source instance
      and the name of the new instance
      ================================== */
   srcins = GetActiveInstance(theEnv);
   newName = (SYMBOL_HN *) GetNthMessageArgument(theEnv,1)->value;
   slotOverrides = (CLIPSValue *) GetNthMessageArgument(theEnv,2)->value;
   if (srcins->garbage)
     {
      StaleInstanceAddress(theEnv,"duplicate-instance",0);
      EnvSetEvaluationError(theEnv,true);
      return;
     }
   if (newName == srcins->name)
     {
      PrintErrorID(theEnv,"INSMODDP",3,false);
      EnvPrintRouter(theEnv,WERROR,"Instance copy must have a different name in duplicate-instance.\n");
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   /* ==========================================
      Create an uninitialized new instance of
      the new name (delete old version - if any)
      ========================================== */
   oldMkInsMsgPass = InstanceData(theEnv)->MkInsMsgPass;
   InstanceData(theEnv)->MkInsMsgPass = msgpass;
   dstins = BuildInstance(theEnv,newName,srcins->cls,true);
   InstanceData(theEnv)->MkInsMsgPass = oldMkInsMsgPass;
   if (dstins == NULL)
     return;
   dstins->busy++;

   /* ================================
      Place slot overrides directly or
      with put- messages
      ================================ */
   while (slotOverrides != NULL)
     {
      /* ===========================================================
         No evaluation or error checking needs to be done
         since this has already been done by EvaluateSlotOverrides()
         =========================================================== */
      dstInsSlot = FindInstanceSlot(theEnv,dstins,(SYMBOL_HN *) slotOverrides->supplementalInfo);
      if (dstInsSlot == NULL)
        {
         SlotExistError(theEnv,ValueToString(slotOverrides->supplementalInfo),
                        "duplicate-instance");
         goto DuplicateError;
        }
      if (msgpass)
        {
         msgExp.type = slotOverrides->type;
         if (msgExp.type != MULTIFIELD)
           msgExp.value = slotOverrides->value;
         else
           msgExp.value = slotOverrides;
         msgExp.argList = NULL;
         msgExp.nextArg = NULL;
         if (! DirectMessage(theEnv,dstInsSlot->desc->overrideMessage,dstins,&temp,&msgExp))
           goto DuplicateError;
        }
      else
        {
         if (dstInsSlot->desc->multiple && (slotOverrides->type != MULTIFIELD))
           {
            temp.type = MULTIFIELD;
            temp.value = EnvCreateMultifield(theEnv,1L);
            SetDOBegin(temp,1);
            SetDOEnd(temp,1);
            SetMFType(temp.value,1,(short) slotOverrides->type);
            SetMFValue(temp.value,1,slotOverrides->value);
            newval = &temp;
           }
         else
           newval = slotOverrides;
         if (PutSlotValue(theEnv,dstins,dstInsSlot,newval,&junk,"duplicate-instance") == false)
           goto DuplicateError;
        }
      dstInsSlot->override = true;
      slotOverrides = slotOverrides->next;
     }

   /* =======================================
      Copy values from source instance to new
      directly or with put- messages
      ======================================= */
   for (i = 0 ; i < dstins->cls->localInstanceSlotCount ; i++)
     {
      if (dstins->slots[i].override == false)
        {
         if (msgpass)
           {
            temp.type = (unsigned short)  srcins->slots[i].type;
            temp.value = srcins->slots[i].value;
            if (temp.type == MULTIFIELD)
              {
               SetDOBegin(temp,1);
               SetDOEnd(temp,GetMFLength(temp.value));
              }
            valArg = ConvertValueToExpression(theEnv,&temp);
            success = DirectMessage(theEnv,dstins->slots[i].desc->overrideMessage,
                          dstins,&temp,valArg);
            ReturnExpression(theEnv,valArg);
            if (! success)
              goto DuplicateError;
           }
         else
           {
            temp.type = (unsigned short) srcins->slots[i].type;
            temp.value = srcins->slots[i].value;
            if (srcins->slots[i].type == MULTIFIELD)
              {
               SetDOBegin(temp,1);
               SetDOEnd(temp,GetMFLength(srcins->slots[i].value));
              }
            if (PutSlotValue(theEnv,dstins,&dstins->slots[i],&temp,&junk,"duplicate-instance")
                 == false)
              goto DuplicateError;
           }
        }
     }

   /* =======================================
      Send init message for message-duplicate
      ======================================= */
   if (msgpass)
     {
      for (i = 0 ; i < dstins->cls->instanceSlotCount ; i++)
        dstins->slotAddresses[i]->override = true;
      dstins->initializeInProgress = 1;
      DirectMessage(theEnv,MessageHandlerData(theEnv)->INIT_SYMBOL,dstins,returnValue,NULL);
     }
   dstins->busy--;
   if (dstins->garbage)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv);
      EnvSetEvaluationError(theEnv,true);
     }
   else
     {
      returnValue->type = INSTANCE_NAME;
      returnValue->value = GetFullInstanceName(theEnv,dstins);
     }
   return;

DuplicateError:
   dstins->busy--;
   QuashInstance(theEnv,dstins);
   EnvSetEvaluationError(theEnv,true);
  }

#endif


