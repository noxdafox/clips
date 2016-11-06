   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/18/16             */
   /*                                                     */
   /*           INSTANCE MULTIFIELD_TYPE SLOT MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Access routines for Instance Multifield Slots   */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Changed integer type/precision.                */
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
/*            Removed direct-mv-replace, direct-mv-insert,   */
/*            direct-mv-delete, mv-slot-replace,             */
/*            mv-slot-insert, and mv-slot-delete functions.  */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Eval support for run time and bload only.      */
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
#include "insfun.h"
#include "msgfun.h"
#include "msgpass.h"
#include "multifun.h"
#include "prntutil.h"
#include "router.h"

#include "insmult.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define INSERT         0
#define REPLACE        1
#define DELETE_OP      2

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static Instance               *CheckMultifieldSlotInstance(UDFContext *);
   static InstanceSlot           *CheckMultifieldSlotModify(Environment *,int,const char *,Instance *,
                                                            Expression *,long *,long *,UDFValue *);
   static void                    AssignSlotToDataObject(UDFValue *,InstanceSlot *);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***************************************************
  NAME         : SetupInstanceMultifieldCommands
  DESCRIPTION  : Defines function interfaces for
                 manipulating instance multislots
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Functions defined to KB
  NOTES        : None
 ***************************************************/
void SetupInstanceMultifieldCommands(
  Environment *theEnv)
  {
#if (! RUN_TIME)
   EnvAddUDF(theEnv,"slot-direct-replace$","b",4,UNBOUNDED,"*;y;l;l",DirectMVReplaceCommand,"DirectMVReplaceCommand",NULL);
   EnvAddUDF(theEnv,"slot-direct-insert$","b",3,UNBOUNDED,"*;y;l",DirectMVInsertCommand,"DirectMVInsertCommand",NULL);
   EnvAddUDF(theEnv,"slot-direct-delete$","b",3,3,"l;y",DirectMVDeleteCommand,"DirectMVDeleteCommand",NULL);
   EnvAddUDF(theEnv,"slot-replace$","*",5,UNBOUNDED,"*;iny;y;l;l",MVSlotReplaceCommand,"MVSlotReplaceCommand",NULL);
   EnvAddUDF(theEnv,"slot-insert$","*",4,UNBOUNDED,"*;iny;y;l",MVSlotInsertCommand,"MVSlotInsertCommand",NULL);
   EnvAddUDF(theEnv,"slot-delete$","*",4,4,"l;iny;y",MVSlotDeleteCommand,"MVSlotDeleteCommand",NULL);
#endif
  }

/***********************************************************************************
  NAME         : MVSlotReplaceCommand
  DESCRIPTION  : Allows user to replace a specified field of a multi-value slot
                 The slot is directly read (w/o a get- message) and the new
                   slot-value is placed via a put- message.
                 This function is not valid for single-value slots.
  INPUTS       : Caller's result buffer
  RETURNS      : True if multi-value slot successfully modified,
                 false otherwise
  SIDE EFFECTS : Put messsage sent for slot
  NOTES        : H/L Syntax : (slot-replace$ <instance> <slot>
                                 <range-begin> <range-end> <value>)
 ***********************************************************************************/
void MVSlotReplaceCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue newval,newseg,oldseg;
   Instance *ins;
   InstanceSlot *sp;
   long rb,re;
   Expression arg;

   returnValue->lexemeValue = FalseSymbol(theEnv);
   ins = CheckMultifieldSlotInstance(context);
   if (ins == NULL)
     return;
   sp = CheckMultifieldSlotModify(theEnv,REPLACE,"slot-replace$",ins,
                            GetFirstArgument()->nextArg,&rb,&re,&newval);
   if (sp == NULL)
     return;
   AssignSlotToDataObject(&oldseg,sp);
   if (ReplaceMultiValueField(theEnv,&newseg,&oldseg,rb,re,&newval,"slot-replace$") == false)
     return;
   arg.type = MULTIFIELD_TYPE;
   arg.value = &newseg;
   arg.nextArg = NULL;
   arg.argList = NULL;
   DirectMessage(theEnv,sp->desc->overrideMessage,ins,returnValue,&arg);
  }

/***********************************************************************************
  NAME         : MVSlotInsertCommand
  DESCRIPTION  : Allows user to insert a specified field of a multi-value slot
                 The slot is directly read (w/o a get- message) and the new
                   slot-value is placed via a put- message.
                 This function is not valid for single-value slots.
  INPUTS       : Caller's result buffer
  RETURNS      : True if multi-value slot successfully modified, false otherwise
  SIDE EFFECTS : Put messsage sent for slot
  NOTES        : H/L Syntax : (slot-insert$ <instance> <slot> <index> <value>)
 ***********************************************************************************/
void MVSlotInsertCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue newval,newseg,oldseg;
   Instance *ins;
   InstanceSlot *sp;
   long theIndex;
   Expression arg;

   returnValue->lexemeValue = FalseSymbol(theEnv);
   ins = CheckMultifieldSlotInstance(context);
   if (ins == NULL)
     return;
   sp = CheckMultifieldSlotModify(theEnv,INSERT,"slot-insert$",ins,
                            GetFirstArgument()->nextArg,&theIndex,NULL,&newval);
   if (sp == NULL)
     return;
   AssignSlotToDataObject(&oldseg,sp);
   if (InsertMultiValueField(theEnv,&newseg,&oldseg,theIndex,&newval,"slot-insert$") == false)
     return;
   arg.type = MULTIFIELD_TYPE;
   arg.value = &newseg;
   arg.nextArg = NULL;
   arg.argList = NULL;
   DirectMessage(theEnv,sp->desc->overrideMessage,ins,returnValue,&arg);
  }

/***********************************************************************************
  NAME         : MVSlotDeleteCommand
  DESCRIPTION  : Allows user to delete a specified field of a multi-value slot
                 The slot is directly read (w/o a get- message) and the new
                   slot-value is placed via a put- message.
                 This function is not valid for single-value slots.
  INPUTS       : Caller's result buffer
  RETURNS      : True if multi-value slot successfully modified, false otherwise
  SIDE EFFECTS : Put message sent for slot
  NOTES        : H/L Syntax : (slot-delete$ <instance> <slot>
                                 <range-begin> <range-end>)
 ***********************************************************************************/
void MVSlotDeleteCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue newseg,oldseg;
   Instance *ins;
   InstanceSlot *sp;
   long rb,re;
   Expression arg;

   returnValue->lexemeValue = FalseSymbol(theEnv);
   ins = CheckMultifieldSlotInstance(context);
   if (ins == NULL)
     return;
   sp = CheckMultifieldSlotModify(theEnv,DELETE_OP,"slot-delete$",ins,
                            GetFirstArgument()->nextArg,&rb,&re,NULL);
   if (sp == NULL)
     return;
   AssignSlotToDataObject(&oldseg,sp);
   if (DeleteMultiValueField(theEnv,&newseg,&oldseg,rb,re,"slot-delete$") == false)
     return;
   arg.type = MULTIFIELD_TYPE;
   arg.value = &newseg;
   arg.nextArg = NULL;
   arg.argList = NULL;
   DirectMessage(theEnv,sp->desc->overrideMessage,ins,returnValue,&arg);
  }

/*****************************************************************
  NAME         : DirectMVReplaceCommand
  DESCRIPTION  : Directly replaces a slot's value
  INPUTS       : None
  RETURNS      : True if put OK, false otherwise
  SIDE EFFECTS : Slot modified
  NOTES        : H/L Syntax: (direct-slot-replace$ <slot>
                                <range-begin> <range-end> <value>)
 *****************************************************************/
void DirectMVReplaceCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   InstanceSlot *sp;
   Instance *ins;
   long rb,re;
   UDFValue newval,newseg,oldseg;

   if (CheckCurrentMessage(theEnv,"direct-slot-replace$",true) == false)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   ins = GetActiveInstance(theEnv);
   sp = CheckMultifieldSlotModify(theEnv,REPLACE,"direct-slot-replace$",ins,
                            GetFirstArgument(),&rb,&re,&newval);
   if (sp == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   AssignSlotToDataObject(&oldseg,sp);
   if (! ReplaceMultiValueField(theEnv,&newseg,&oldseg,rb,re,&newval,"direct-slot-replace$"))
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   if (PutSlotValue(theEnv,ins,sp,&newseg,&newval,"function direct-slot-replace$"))
     { returnValue->lexemeValue = TrueSymbol(theEnv); }
   else
     { returnValue->lexemeValue = FalseSymbol(theEnv); }
  }

/************************************************************************
  NAME         : DirectMVInsertCommand
  DESCRIPTION  : Directly inserts a slot's value
  INPUTS       : None
  RETURNS      : True if put OK, false otherwise
  SIDE EFFECTS : Slot modified
  NOTES        : H/L Syntax: (direct-slot-insert$ <slot> <index> <value>)
 ************************************************************************/
void DirectMVInsertCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   InstanceSlot *sp;
   Instance *ins;
   long theIndex;
   UDFValue newval,newseg,oldseg;

   if (CheckCurrentMessage(theEnv,"direct-slot-insert$",true) == false)
     { 
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return; 
     }

   ins = GetActiveInstance(theEnv);
   sp = CheckMultifieldSlotModify(theEnv,INSERT,"direct-slot-insert$",ins,
                            GetFirstArgument(),&theIndex,NULL,&newval);
   if (sp == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   AssignSlotToDataObject(&oldseg,sp);
   if (! InsertMultiValueField(theEnv,&newseg,&oldseg,theIndex,&newval,"direct-slot-insert$"))
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return; 
     }

   if (PutSlotValue(theEnv,ins,sp,&newseg,&newval,"function direct-slot-insert$"))
     { returnValue->lexemeValue = TrueSymbol(theEnv); }
   else
     { returnValue->lexemeValue = FalseSymbol(theEnv); }
  }

/*****************************************************************
  NAME         : DirectMVDeleteCommand
  DESCRIPTION  : Directly deletes a slot's value
  INPUTS       : None
  RETURNS      : True if put OK, false otherwise
  SIDE EFFECTS : Slot modified
  NOTES        : H/L Syntax: (direct-slot-delete$ <slot>
                                <range-begin> <range-end>)
 *****************************************************************/
void DirectMVDeleteCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   InstanceSlot *sp;
   Instance *ins;
   long rb,re;
   UDFValue newseg,oldseg;

   if (CheckCurrentMessage(theEnv,"direct-slot-delete$",true) == false)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   ins = GetActiveInstance(theEnv);
   sp = CheckMultifieldSlotModify(theEnv,DELETE_OP,"direct-slot-delete$",ins,
                                  GetFirstArgument(),&rb,&re,NULL);
   if (sp == NULL)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   AssignSlotToDataObject(&oldseg,sp);
   if (! DeleteMultiValueField(theEnv,&newseg,&oldseg,rb,re,"direct-slot-delete$"))
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   if (PutSlotValue(theEnv,ins,sp,&newseg,&oldseg,"function direct-slot-delete$"))
     { returnValue->lexemeValue = TrueSymbol(theEnv); }
   else
     { returnValue->lexemeValue = FalseSymbol(theEnv); }
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/**********************************************************************
  NAME         : CheckMultifieldSlotInstance
  DESCRIPTION  : Gets the instance for the functions slot-replace$,
                    insert and delete
  INPUTS       : The function name
  RETURNS      : The instance address, NULL on errors
  SIDE EFFECTS : None
  NOTES        : None
 **********************************************************************/
static Instance *CheckMultifieldSlotInstance(
  UDFContext *context)
  {
   Instance *ins;
   UDFValue temp;
   Environment *theEnv = context->environment;

   if (! UDFFirstArgument(context,INSTANCE_BITS | SYMBOL_BIT,&temp))
     { return NULL; }

   if (temp.header->type == INSTANCE_ADDRESS_TYPE)
     {
      ins = temp.instanceValue;
      if (ins->garbage == 1)
        {
         StaleInstanceAddress(theEnv,UDFContextFunctionName(context),0);
         EnvSetEvaluationError(theEnv,true);
         return NULL;
        }
     }
   else
     {
      ins = FindInstanceBySymbol(theEnv,temp.lexemeValue);
      if (ins == NULL)
        NoInstanceError(theEnv,temp.lexemeValue->contents,UDFContextFunctionName(context));
     }
   return ins;
  }

/*********************************************************************
  NAME         : CheckMultifieldSlotModify
  DESCRIPTION  : For the functions slot-replace$, insert, & delete
                    as well as direct-slot-replace$, insert, & delete
                    this function gets the slot, index, and optional
                    field-value for these functions
  INPUTS       : 1) A code indicating the type of operation
                      INSERT    (0) : Requires one index
                      REPLACE   (1) : Requires two indices
                      DELETE_OP (2) : Requires two indices
                 2) Function name-string
                 3) Instance address
                 4) Argument expression chain
                 5) Caller's buffer for index (or beginning of range)
                 6) Caller's buffer for end of range
                     (can be NULL for INSERT)
                 7) Caller's new-field value buffer
                     (can be NULL for DELETE_OP)
  RETURNS      : The address of the instance-slot,
                    NULL on errors
  SIDE EFFECTS : Caller's index buffer set
                 Caller's new-field value buffer set (if not NULL)
                   Will allocate an ephemeral segment to store more
                     than 1 new field value
                 EvaluationError set on errors
  NOTES        : Assume the argument chain is at least 2
                   expressions deep - slot, index, and optional values
 *********************************************************************/
static InstanceSlot *CheckMultifieldSlotModify(
  Environment *theEnv,
  int code,
  const char *func,
  Instance *ins,
  Expression *args,
  long *rb,
  long *re,
  UDFValue *newval)
  {
   UDFValue temp;
   InstanceSlot *sp;
   int start;

   start = (args == GetFirstArgument()) ? 1 : 2;
   EvaluationData(theEnv)->EvaluationError = false;
   EvaluateExpression(theEnv,args,&temp);
   if (temp.header->type != SYMBOL_TYPE)
     {
      ExpectedTypeError1(theEnv,func,start,"symbol");
      EnvSetEvaluationError(theEnv,true);
      return NULL;
     }
   sp = FindInstanceSlot(theEnv,ins,temp.lexemeValue);
   if (sp == NULL)
     {
      SlotExistError(theEnv,temp.lexemeValue->contents,func);
      return NULL;
     }
   if (sp->desc->multiple == 0)
     {
      PrintErrorID(theEnv,"INSMULT",1,false);
      EnvPrintRouter(theEnv,WERROR,"Function ");
      EnvPrintRouter(theEnv,WERROR,func);
      EnvPrintRouter(theEnv,WERROR," cannot be used on single-field slot ");
      EnvPrintRouter(theEnv,WERROR,sp->desc->slotName->name->contents);
      EnvPrintRouter(theEnv,WERROR," in instance ");
      EnvPrintRouter(theEnv,WERROR,ins->name->contents);
      EnvPrintRouter(theEnv,WERROR,".\n");
      EnvSetEvaluationError(theEnv,true);
      return NULL;
     }
   EvaluateExpression(theEnv,args->nextArg,&temp);
   if (temp.header->type != INTEGER_TYPE)
     {
      ExpectedTypeError1(theEnv,func,start+1,"integer");
      EnvSetEvaluationError(theEnv,true);
      return NULL;
     }
   args = args->nextArg->nextArg;
   *rb = (long) temp.integerValue->contents;
   if ((code == REPLACE) || (code == DELETE_OP))
     {
      EvaluateExpression(theEnv,args,&temp);
      if (temp.header->type != INTEGER_TYPE)
        {
         ExpectedTypeError1(theEnv,func,start+2,"integer");
         EnvSetEvaluationError(theEnv,true);
         return NULL;
        }
      *re = (long) temp.integerValue->contents;
      args = args->nextArg;
     }
   if ((code == INSERT) || (code == REPLACE))
     {
      if (EvaluateAndStoreInDataObject(theEnv,1,args,newval,true) == false)
        return NULL;
     }
   return(sp);
  }

/***************************************************
  NAME         : AssignSlotToDataObject
  DESCRIPTION  : Assigns the value of a multifield
                 slot to a data object
  INPUTS       : 1) The data object buffer
                 2) The instance slot
  RETURNS      : Nothing useful
  SIDE EFFECTS : Data object fields set
  NOTES        : Assumes slot is a multislot
 ***************************************************/
static void AssignSlotToDataObject(
  UDFValue *theDataObject,
  InstanceSlot *theSlot)
  {
   theDataObject->value = theSlot->value;
   theDataObject->begin = 0;
   theDataObject->range = theSlot->multifieldValue->length;
  }

#endif
