   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*          INSTANCE PRIMITIVE SUPPORT MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose:  Creation and Deletion of Instances Routines     */
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
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Newly created instances can no longer use      */
/*            a preexisting instance name of another class   */
/*            [INSMNGR16].                                   */
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

#if DEFRULE_CONSTRUCT
#include "network.h"
#include "drive.h"
#include "objrtmch.h"
#include "lgcldpnd.h"
#endif

#include "classcom.h"
#include "classfun.h"
#include "engine.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "insfun.h"
#include "memalloc.h"
#include "miscfun.h"
#include "modulutl.h"
#include "msgcom.h"
#include "msgfun.h"
#include "prccode.h"
#include "prntutil.h"
#include "router.h"
#include "sysdep.h"
#include "utility.h"

#include "insmngr.h"

#include "inscom.h"
#include "watch.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define MAKE_TRACE   "==>"
#define UNMAKE_TRACE "<=="

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static Instance               *NewInstance(Environment *);
   static Instance               *InstanceLocationInfo(Environment *,Defclass *,CLIPSLexeme *,Instance **,
                                                       unsigned *);
   static void                    InstallInstance(Environment *,Instance *,bool);
   static void                    BuildDefaultSlots(Environment *,bool);
   static bool                    CoreInitializeInstance(Environment *,Instance *,Expression *);
   static bool                    CoreInitializeInstanceCV(Environment *,Instance *,CLIPSValue *);
   static bool                    InsertSlotOverrides(Environment *,Instance *,Expression *);
   static bool                    InsertSlotOverridesCV(Environment *,Instance *,CLIPSValue *);
   static void                    EvaluateClassDefaults(Environment *,Instance *);
   static bool                    IMModifySlots(Environment *,Instance *,CLIPSValue *);

#if DEBUGGING_FUNCTIONS
   static void                    PrintInstanceWatch(Environment *,const char *,Instance *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : InitializeInstanceCommand
  DESCRIPTION  : Initializes an instance of a class
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance intialized
  NOTES        : H/L Syntax:
                 (active-initialize-instance <instance-name>
                    <slot-override>*)
 ***********************************************************/
void InitializeInstanceCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   Instance *ins;

   returnValue->lexemeValue = FalseSymbol(theEnv);
   ins = CheckInstance(context);
   if (ins == NULL)
     return;
   if (CoreInitializeInstance(theEnv,ins,GetFirstArgument()->nextArg) == true)
     { returnValue->value = ins->name; }
  }

/****************************************************************
  NAME         : MakeInstanceCommand
  DESCRIPTION  : Creates and initializes an instance of a class
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance intialized
  NOTES        : H/L Syntax:
                 (active-make-instance <instance-name> of <class>
                    <slot-override>*)
  CHANGES      : It's now possible to create an instance of a
                 class that's not in scope if the module name
                 is specified.
 ****************************************************************/
void MakeInstanceCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   CLIPSLexeme *iname;
   Instance *ins;
   UDFValue temp;
   Defclass *cls;

   returnValue->lexemeValue = FalseSymbol(theEnv);
   EvaluateExpression(theEnv,GetFirstArgument(),&temp);
   
   if (temp.header->type == INSTANCE_NAME_TYPE)
     { iname = temp.lexemeValue; }
   else if (temp.header->type == SYMBOL_TYPE)
     { iname = EnvCreateInstanceName(theEnv,temp.lexemeValue->contents); }
   else
     {
      PrintErrorID(theEnv,"INSMNGR",1,false);
      EnvPrintRouter(theEnv,WERROR,"Expected a valid name for new instance.\n");
      EnvSetEvaluationError(theEnv,true);
      return;
     }

   if (GetFirstArgument()->nextArg->type == DEFCLASS_PTR)
     cls = (Defclass *) GetFirstArgument()->nextArg->value;
   else
     {
      EvaluateExpression(theEnv,GetFirstArgument()->nextArg,&temp);
      if (temp.header->type != SYMBOL_TYPE)
        {
         PrintErrorID(theEnv,"INSMNGR",2,false);
         EnvPrintRouter(theEnv,WERROR,"Expected a valid class name for new instance.\n");
         EnvSetEvaluationError(theEnv,true);
         return;
        }

      cls = LookupDefclassByMdlOrScope(theEnv,temp.lexemeValue->contents); // Module or scope is now allowed

      if (cls == NULL)
        {
         ClassExistError(theEnv,ExpressionFunctionCallName(EvaluationData(theEnv)->CurrentExpression)->contents,
                         temp.lexemeValue->contents);
         EnvSetEvaluationError(theEnv,true);
         return;
        }
     }

   ins = BuildInstance(theEnv,iname,cls,true);
   if (ins == NULL)
     return;

   if (CoreInitializeInstance(theEnv,ins,GetFirstArgument()->nextArg->nextArg) == true)
     { returnValue->value = GetFullInstanceName(theEnv,ins); }
   else
     QuashInstance(theEnv,ins);
  }

/***************************************************
  NAME         : GetFullInstanceName
  DESCRIPTION  : If this function is called while
                 the current module is other than
                 the one in which the instance
                 resides, then the module name is
                 prepended to the instance name.
                 Otherwise - the base name only is
                 returned.
  INPUTS       : The instance
  RETURNS      : The instance name symbol (with
                 module name and :: prepended)
  SIDE EFFECTS : Temporary buffer allocated possibly
                 and new symbol created
  NOTES        : Used to differentiate between
                 instances of the same name in
                 different modules.
                 Instances are now global in scope so
                 each instance name must belong to a
                 single instance. It's no longer
                 necessary to return the full instance
                 name.
 ***************************************************/
CLIPSLexeme *GetFullInstanceName(
  Environment *theEnv,
  Instance *ins)
  {
   if (ins == &InstanceData(theEnv)->DummyInstance)
     { return EnvCreateInstanceName(theEnv,"Dummy Instance"); }

   return ins->name;
  }

/***************************************************
  NAME         : BuildInstance
  DESCRIPTION  : Creates an uninitialized instance
  INPUTS       : 1) Name of the instance
                 2) Class pointer
                 3) Flag indicating whether init
                    message will be called for
                    this instance or not
  RETURNS      : The address of the new instance,
                   NULL on errors (or when a
                   a logical basis in a rule was
                   deleted int the same RHS in
                   which the instance creation
                   occurred)
  SIDE EFFECTS : Old definition (if any) is deleted
  NOTES        : None
 ***************************************************/
Instance *BuildInstance(
  Environment *theEnv,
  CLIPSLexeme *iname,
  Defclass *cls,
  bool initMessage)
  {
   Instance *ins,*iprv;
   unsigned hashTableIndex;
   unsigned modulePosition;
   CLIPSLexeme *moduleName;
   UDFValue temp;

#if DEFRULE_CONSTRUCT
   if (EngineData(theEnv)->JoinOperationInProgress && cls->reactive)
     {
      PrintErrorID(theEnv,"INSMNGR",10,false);
      EnvPrintRouter(theEnv,WERROR,"Cannot create instances of reactive classes while\n");
      EnvPrintRouter(theEnv,WERROR,"  pattern-matching is in process.\n");
      EnvSetEvaluationError(theEnv,true);
      return NULL;
     }
#endif
   if (cls->abstract)
     {
      PrintErrorID(theEnv,"INSMNGR",3,false);
      EnvPrintRouter(theEnv,WERROR,"Cannot create instances of abstract class ");
      EnvPrintRouter(theEnv,WERROR,DefclassName(cls));
      EnvPrintRouter(theEnv,WERROR,".\n");
      EnvSetEvaluationError(theEnv,true);
      return NULL;
     }
   modulePosition = FindModuleSeparator(iname->contents);
   if (modulePosition)
     {
      moduleName = ExtractModuleName(theEnv,modulePosition,iname->contents);
      if ((moduleName == NULL) ||
          (moduleName != cls->header.whichModule->theModule->header.name))
        {
         PrintErrorID(theEnv,"INSMNGR",11,true);
         EnvPrintRouter(theEnv,WERROR,"Invalid module specifier in new instance name.\n");
         EnvSetEvaluationError(theEnv,true);
         return NULL;
        }
      iname = ExtractConstructName(theEnv,modulePosition,iname->contents,INSTANCE_NAME_TYPE);
     }
   ins = InstanceLocationInfo(theEnv,cls,iname,&iprv,&hashTableIndex);

   if (ins != NULL)
     {
      if (ins->cls != cls)
        {
         PrintErrorID(theEnv,"INSMNGR",16,false);
         EnvPrintRouter(theEnv,WERROR,"The instance name ");
         EnvPrintRouter(theEnv,WERROR,iname->contents);
         EnvPrintRouter(theEnv,WERROR," is in use by an instance of class ");
         EnvPrintRouter(theEnv,WERROR,ins->cls->header.name->contents);
         EnvPrintRouter(theEnv,WERROR,".\n");
         EnvSetEvaluationError(theEnv,true);
         return NULL;
        }

      if (ins->installed == 0)
        {
         PrintErrorID(theEnv,"INSMNGR",4,false);
         EnvPrintRouter(theEnv,WERROR,"The instance ");
         EnvPrintRouter(theEnv,WERROR,iname->contents);
         EnvPrintRouter(theEnv,WERROR," has a slot-value which depends on the instance definition.\n");
         EnvSetEvaluationError(theEnv,true);
         return NULL;
        }
      ins->busy++;
      IncrementSymbolCount(iname);
      if (ins->garbage == 0)
        {
         if (InstanceData(theEnv)->MkInsMsgPass)
           DirectMessage(theEnv,MessageHandlerData(theEnv)->DELETE_SYMBOL,ins,NULL,NULL);
         else
           QuashInstance(theEnv,ins);
        }
      ins->busy--;
      DecrementSymbolCount(theEnv,iname);
      if (ins->garbage == 0)
        {
         PrintErrorID(theEnv,"INSMNGR",5,false);
         EnvPrintRouter(theEnv,WERROR,"Unable to delete old instance ");
         EnvPrintRouter(theEnv,WERROR,iname->contents);
         EnvPrintRouter(theEnv,WERROR,".\n");
         EnvSetEvaluationError(theEnv,true);
         return NULL;
        }
     }

   /* =============================================================
      Create the base instance from the defaults of the inheritance
      precedence list
      ============================================================= */
   InstanceData(theEnv)->CurrentInstance = NewInstance(theEnv);

#if DEFRULE_CONSTRUCT
   /* ==============================================
      Add this new instance as a dependent to
      any currently active basis - if the partial
      match was deleted, abort the instance creation
      ============================================== */
   if (AddLogicalDependencies(theEnv,(struct patternEntity *) InstanceData(theEnv)->CurrentInstance,false)
        == false)
     {
      rtn_struct(theEnv,instance,InstanceData(theEnv)->CurrentInstance);
      InstanceData(theEnv)->CurrentInstance = NULL;
      return NULL;
     }
#endif

   InstanceData(theEnv)->CurrentInstance->name = iname;
   InstanceData(theEnv)->CurrentInstance->cls = cls;
   BuildDefaultSlots(theEnv,initMessage);

   /* ============================================================
      Put the instance in the instance hash table and put it on its
        class's instance list
      ============================================================ */
   InstanceData(theEnv)->CurrentInstance->hashTableIndex = hashTableIndex;
   if (iprv == NULL)
     {
      InstanceData(theEnv)->CurrentInstance->nxtHash = InstanceData(theEnv)->InstanceTable[hashTableIndex];
      if (InstanceData(theEnv)->InstanceTable[hashTableIndex] != NULL)
        InstanceData(theEnv)->InstanceTable[hashTableIndex]->prvHash = InstanceData(theEnv)->CurrentInstance;
      InstanceData(theEnv)->InstanceTable[hashTableIndex] = InstanceData(theEnv)->CurrentInstance;
     }
   else
     {
      InstanceData(theEnv)->CurrentInstance->nxtHash = iprv->nxtHash;
      if (iprv->nxtHash != NULL)
        iprv->nxtHash->prvHash = InstanceData(theEnv)->CurrentInstance;
      iprv->nxtHash = InstanceData(theEnv)->CurrentInstance;
      InstanceData(theEnv)->CurrentInstance->prvHash = iprv;
     }

   /* ======================================
      Put instance in global and class lists
      ====================================== */
   if (InstanceData(theEnv)->CurrentInstance->cls->instanceList == NULL)
     InstanceData(theEnv)->CurrentInstance->cls->instanceList = InstanceData(theEnv)->CurrentInstance;
   else
     InstanceData(theEnv)->CurrentInstance->cls->instanceListBottom->nxtClass = InstanceData(theEnv)->CurrentInstance;
   InstanceData(theEnv)->CurrentInstance->prvClass = InstanceData(theEnv)->CurrentInstance->cls->instanceListBottom;
   InstanceData(theEnv)->CurrentInstance->cls->instanceListBottom = InstanceData(theEnv)->CurrentInstance;

   if (InstanceData(theEnv)->InstanceList == NULL)
     InstanceData(theEnv)->InstanceList = InstanceData(theEnv)->CurrentInstance;
   else
     InstanceData(theEnv)->InstanceListBottom->nxtList = InstanceData(theEnv)->CurrentInstance;
   InstanceData(theEnv)->CurrentInstance->prvList = InstanceData(theEnv)->InstanceListBottom;
   InstanceData(theEnv)->InstanceListBottom = InstanceData(theEnv)->CurrentInstance;
   InstanceData(theEnv)->ChangesToInstances = true;

   /* ==============================================================================
      Install the instance's name and slot-value symbols (prevent them from becoming
      ephemeral) - the class name and slot names are accounted for by the class
      ============================================================================== */
   InstallInstance(theEnv,InstanceData(theEnv)->CurrentInstance,true);

   ins = InstanceData(theEnv)->CurrentInstance;
   InstanceData(theEnv)->CurrentInstance = NULL;

   if (InstanceData(theEnv)->MkInsMsgPass)
     { DirectMessage(theEnv,MessageHandlerData(theEnv)->CREATE_SYMBOL,ins,&temp,NULL); }

#if DEFRULE_CONSTRUCT
   if (ins->cls->reactive)
     ObjectNetworkAction(theEnv,OBJECT_ASSERT,ins,-1);
#endif

   return(ins);
  }

/*****************************************************************************
  NAME         : InitSlotsCommand
  DESCRIPTION  : Calls Kernel Expression Evaluator EvaluateExpression
                   for each expression-value of an instance expression

                 Evaluates default slots only - slots that were specified
                 by overrides (sp->override == 1) are ignored)
  INPUTS       : 1) Instance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Each UDFValue slot in the instance's slot array is replaced
                   by the evaluation (by EvaluateExpression) of the expression
                   in the slot list.  The old expression-values
                   are deleted.
  NOTES        : H/L Syntax: (init-slots <instance>)
 *****************************************************************************/
void InitSlotsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   EvaluationData(theEnv)->EvaluationError = false;

   if (CheckCurrentMessage(theEnv,"init-slots",true) == false)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   EvaluateClassDefaults(theEnv,GetActiveInstance(theEnv));

   if (! EvaluationData(theEnv)->EvaluationError)
     { returnValue->instanceValue = GetActiveInstance(theEnv); }
   else
     { returnValue->lexemeValue = FalseSymbol(theEnv); }
  }

/******************************************************
  NAME         : QuashInstance
  DESCRIPTION  : Deletes an instance if it is not in
                   use, otherwise sticks it on the
                   garbage list
  INPUTS       : The instance
  RETURNS      : 1 if successful, 0 otherwise
  SIDE EFFECTS : Instance deleted or added to garbage
  NOTES        : Even though the instance is removed
                   from the class list, hash table and
                   instance list, its links remain
                   unchanged so that outside loops
                   can still determine where the next
                   node in the list is (assuming the
                   instance was garbage collected).
 ******************************************************/
bool QuashInstance(
  Environment *theEnv,
  Instance *ins)
  {
   int iflag;
   IGARBAGE *gptr;

#if DEFRULE_CONSTRUCT
   if (EngineData(theEnv)->JoinOperationInProgress && ins->cls->reactive)
     {
      PrintErrorID(theEnv,"INSMNGR",12,false);
      EnvPrintRouter(theEnv,WERROR,"Cannot delete instances of reactive classes while\n");
      EnvPrintRouter(theEnv,WERROR,"  pattern-matching is in process.\n");
      EnvSetEvaluationError(theEnv,true);
      return false;
     }
#endif
   if (ins->garbage == 1)
     return false;
   if (ins->installed == 0)
     {
      PrintErrorID(theEnv,"INSMNGR",6,false);
      EnvPrintRouter(theEnv,WERROR,"Cannot delete instance ");
      EnvPrintRouter(theEnv,WERROR,ins->name->contents);
      EnvPrintRouter(theEnv,WERROR," during initialization.\n");
      EnvSetEvaluationError(theEnv,true);
      return false;
     }
#if DEBUGGING_FUNCTIONS
   if (ins->cls->traceInstances)
     PrintInstanceWatch(theEnv,UNMAKE_TRACE,ins);
#endif

#if DEFRULE_CONSTRUCT
   RemoveEntityDependencies(theEnv,(struct patternEntity *) ins);

   if (ins->cls->reactive)
     ObjectNetworkAction(theEnv,OBJECT_RETRACT,ins,-1);
#endif

   if (ins->prvHash != NULL)
     ins->prvHash->nxtHash = ins->nxtHash;
   else
     InstanceData(theEnv)->InstanceTable[ins->hashTableIndex] = ins->nxtHash;
   if (ins->nxtHash != NULL)
     ins->nxtHash->prvHash = ins->prvHash;

   if (ins->prvClass != NULL)
     ins->prvClass->nxtClass = ins->nxtClass;
   else
     ins->cls->instanceList = ins->nxtClass;
   if (ins->nxtClass != NULL)
     ins->nxtClass->prvClass = ins->prvClass;
   else
     ins->cls->instanceListBottom = ins->prvClass;

   if (ins->prvList != NULL)
     ins->prvList->nxtList = ins->nxtList;
   else
     InstanceData(theEnv)->InstanceList = ins->nxtList;
   if (ins->nxtList != NULL)
     ins->nxtList->prvList = ins->prvList;
   else
     InstanceData(theEnv)->InstanceListBottom = ins->prvList;

   iflag = ins->installed;
   InstallInstance(theEnv,ins,false);

   /* ==============================================
      If the instance is the basis for an executing
      rule, don't bother deleting its slots yet, for
      they may still be needed by pattern variables
      ============================================== */
#if DEFRULE_CONSTRUCT
   if ((iflag == 1)
       && (ins->header.busyCount == 0))
#else
   if (iflag == 1)
#endif
     RemoveInstanceData(theEnv,ins);

   if ((ins->busy == 0) &&
       (InstanceData(theEnv)->MaintainGarbageInstances == false)
#if DEFRULE_CONSTRUCT
        && (ins->header.busyCount == 0)
#endif
       )
     {
      DecrementSymbolCount(theEnv,ins->name);
      rtn_struct(theEnv,instance,ins);
     }
   else
     {
      gptr = get_struct(theEnv,igarbage);
      ins->garbage = 1;
      gptr->ins = ins;
      gptr->nxt = InstanceData(theEnv)->InstanceGarbageList;
      InstanceData(theEnv)->InstanceGarbageList = gptr;
      UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
     }
   InstanceData(theEnv)->ChangesToInstances = true;
   return true;
  }


#if DEFRULE_CONSTRUCT

/****************************************************
  NAME         : InactiveInitializeInstance
  DESCRIPTION  : Initializes an instance of a class
                 Pattern-matching is automatically
                 delayed until the instance is
                 completely initialized
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance intialized
  NOTES        : H/L Syntax:
                 (initialize-instance <instance-name>
                    <slot-override>*)
 ****************************************************/
void InactiveInitializeInstance(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   bool ov;

   ov = SetDelayObjectPatternMatching(theEnv,true);
   InitializeInstanceCommand(theEnv,context,returnValue);
   SetDelayObjectPatternMatching(theEnv,ov);
  }

/**************************************************************
  NAME         : InactiveMakeInstance
  DESCRIPTION  : Creates and initializes an instance of a class
                 Pattern-matching is automatically
                 delayed until the instance is
                 completely initialized
  INPUTS       : The address of the result value
  RETURNS      : Nothing useful
  SIDE EFFECTS : Instance intialized
  NOTES        : H/L Syntax:
                 (make-instance <instance-name> of <class>
                    <slot-override>*)
 **************************************************************/
void InactiveMakeInstance(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   bool ov;

   ov = SetDelayObjectPatternMatching(theEnv,true);
   MakeInstanceCommand(theEnv,context,returnValue);
   SetDelayObjectPatternMatching(theEnv,ov);
  }

#endif

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/********************************************************
  NAME         : NewInstance
  DESCRIPTION  : Allocates and initializes a new instance
  INPUTS       : None
  RETURNS      : The address of the new instance
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************/
static Instance *NewInstance(
  Environment *theEnv)
  {
   Instance *instance;

   instance = get_struct(theEnv,instance);
#if DEFRULE_CONSTRUCT
   instance->header.theInfo = &InstanceData(theEnv)->InstanceInfo;

   instance->header.dependents = NULL;
   instance->header.busyCount = 0;
   instance->header.timeTag = 0L;

   instance->partialMatchList = NULL;
   instance->basisSlots = NULL;
   instance->reteSynchronized = false;
#endif
   instance->header.th.type = INSTANCE_ADDRESS_TYPE;
   instance->busy = 0;
   instance->installed = 0;
   instance->garbage = 0;
   instance->initSlotsCalled = 0;
   instance->initializeInProgress = 0;
   instance->name = NULL;
   instance->hashTableIndex = 0;
   instance->cls = NULL;
   instance->slots = NULL;
   instance->slotAddresses = NULL;
   instance->prvClass = NULL;
   instance->nxtClass = NULL;
   instance->prvHash = NULL;
   instance->nxtHash = NULL;
   instance->prvList = NULL;
   instance->nxtList = NULL;
   return(instance);
  }

/*****************************************************************
  NAME         : InstanceLocationInfo
  DESCRIPTION  : Determines where a specified instance belongs
                   in the instance hash table
  INPUTS       : 1) The class of the new instance
                 2) The symbol for the name of the instance
                 3) Caller's buffer for previous node address
                 4) Caller's buffer for hash value
  RETURNS      : The address of the found instance, NULL otherwise
  SIDE EFFECTS : None
  NOTES        : Instance names only have to be unique within
                 a module.
                 Change: instance names must be unique regardless
                 of module.
 *****************************************************************/
static Instance *InstanceLocationInfo(
  Environment *theEnv,
  Defclass *cls,
  CLIPSLexeme *iname,
  Instance **prv,
  unsigned *hashTableIndex)
  {
   Instance *ins;

   *hashTableIndex = HashInstance(iname);
   ins = InstanceData(theEnv)->InstanceTable[*hashTableIndex];

   /* ========================================
      Make sure all instances of the same name
      are grouped together regardless of what
      module their classes are in
      ======================================== */
   *prv = NULL;
   while (ins != NULL)
     {
      if (ins->name == iname)
        { return(ins); }
      *prv = ins;
      ins = ins->nxtHash;
     }

   /*
   while ((ins != NULL) ? (ins->name != iname) : false)
     {
      *prv = ins;
      ins = ins->nxtHash;
     }
   while ((ins != NULL) ? (ins->name == iname) : false)
     {
      if (ins->cls->header.whichModule->theModule ==
          cls->header.whichModule->theModule)
        return(ins);
      *prv = ins;
      ins = ins->nxtHash;
     }
   */
   return NULL;
  }

/********************************************************
  NAME         : InstallInstance
  DESCRIPTION  : Prevent name and slot value symbols
                   from being ephemeral (all others
                   taken care of by class defn)
  INPUTS       : 1) The address of the instance
                 2) A flag indicating whether to
                    install or deinstall
  RETURNS      : Nothing useful
  SIDE EFFECTS : Symbol counts incremented or decremented
  NOTES        : Slot symbol installations are handled
                   by PutSlotValue
 ********************************************************/
static void InstallInstance(
  Environment *theEnv,
  Instance *ins,
  bool set)
  {
   if (set == true)
     {
      if (ins->installed)
        return;
#if DEBUGGING_FUNCTIONS
      if (ins->cls->traceInstances)
        PrintInstanceWatch(theEnv,MAKE_TRACE,ins);
#endif
      ins->installed = 1;
      IncrementSymbolCount(ins->name);
      IncrementDefclassBusyCount(theEnv,ins->cls);
      InstanceData(theEnv)->GlobalNumberOfInstances++;
     }
   else
     {
      if (! ins->installed)
        return;
      ins->installed = 0;
      InstanceData(theEnv)->GlobalNumberOfInstances--;

      /* =======================================
         Class counts is decremented by
         RemoveInstanceData() when slot data is
         truly deleted - and name count is
         deleted by CleanupInstances() or
         QuashInstance() when instance is
         truly deleted
         ======================================= */
     }
  }

/****************************************************************
  NAME         : BuildDefaultSlots
  DESCRIPTION  : The current instance's address is
                   in the global variable CurrentInstance.
                   This builds the slots and the default values
                   from the direct class of the instance and its
                   inheritances.
  INPUTS       : Flag indicating whether init message will be
                 called for this instance or not
  RETURNS      : Nothing useful
  SIDE EFFECTS : Allocates the slot array for
                   the current instance
  NOTES        : The current instance's address is
                 stored in a global variable
 ****************************************************************/
static void BuildDefaultSlots(
  Environment *theEnv,
  bool initMessage)
  {
   unsigned i,j;
   unsigned scnt;
   unsigned lscnt;
   InstanceSlot *dst = NULL,**adst;
   SlotDescriptor **src;

   scnt = InstanceData(theEnv)->CurrentInstance->cls->instanceSlotCount;
   lscnt = InstanceData(theEnv)->CurrentInstance->cls->localInstanceSlotCount;
   if (scnt > 0)
     {
      InstanceData(theEnv)->CurrentInstance->slotAddresses = adst =
         (InstanceSlot **) gm2(theEnv,(sizeof(InstanceSlot *) * scnt));
      if (lscnt != 0)
        InstanceData(theEnv)->CurrentInstance->slots = dst =
           (InstanceSlot *) gm2(theEnv,(sizeof(InstanceSlot) * lscnt));
      src = InstanceData(theEnv)->CurrentInstance->cls->instanceTemplate;

      /* ==================================================
         A map of slot addresses is created - shared slots
         point at values in the class, and local slots
         point at values in the instance

         Also - slots are always given an initial value
         (since slots cannot be unbound). If there is
         already an instance of a class with a shared slot,
         that value is left alone
         ================================================== */
      for (i = 0 , j = 0 ; i < scnt ; i++)
        {
         if (src[i]->shared)
           {
            src[i]->sharedCount++;
            adst[i] = &(src[i]->sharedValue);
           }
         else
           {
            dst[j].desc = src[i];
            dst[j].value = NULL;
            adst[i] = &dst[j++];
           }
         if (adst[i]->value == NULL)
           {
            adst[i]->valueRequired = initMessage;
            if (adst[i]->desc->multiple)
              {
               adst[i]->type = MULTIFIELD_TYPE;
               adst[i]->value = CreateUnmanagedMultifield(theEnv,0L);
               MultifieldInstall(theEnv,adst[i]->multifieldValue);
              }
            else
              {
               adst[i]->type = SYMBOL_TYPE;
               adst[i]->value = EnvCreateSymbol(theEnv,"nil");
               AtomInstall(theEnv,(int) adst[i]->type,adst[i]->value);
              }
           }
         else
           adst[i]->valueRequired = false;
         adst[i]->override = false;
        }
     }
  }

/*******************************************************************
  NAME         : CoreInitializeInstance
  DESCRIPTION  : Performs the core work for initializing an instance
  INPUTS       : 1) The instance address
                 2) Slot override expressions
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : EvaluationError set on errors - slots evaluated
  NOTES        : None
 *******************************************************************/
static bool CoreInitializeInstance(
  Environment *theEnv,
  Instance *ins,
  Expression *ovrexp)
  {
   UDFValue temp;

   if (ins->installed == 0)
     {
      PrintErrorID(theEnv,"INSMNGR",7,false);
      EnvPrintRouter(theEnv,WERROR,"Instance ");
      EnvPrintRouter(theEnv,WERROR,ins->name->contents);
      EnvPrintRouter(theEnv,WERROR," is already being initialized.\n");
      EnvSetEvaluationError(theEnv,true);
      return false;
     }

   /* =======================================================
      Replace all default-slot values with any slot-overrides
      ======================================================= */
   ins->busy++;
   ins->installed = 0;

   /* =================================================================
      If the slots are initialized properly - the initializeInProgress
      flag will be turned off.
      ================================================================= */
   ins->initializeInProgress = 1;
   ins->initSlotsCalled = 0;

   if (InsertSlotOverrides(theEnv,ins,ovrexp) == false)
      {
       ins->installed = 1;
       ins->busy--;
       return false;
      }

   /* =================================================================
      Now that all the slot expressions are established - replace them
      with their evaluation
      ================================================================= */

   if (InstanceData(theEnv)->MkInsMsgPass)
     DirectMessage(theEnv,MessageHandlerData(theEnv)->INIT_SYMBOL,ins,&temp,NULL);
   else
     EvaluateClassDefaults(theEnv,ins);

   ins->busy--;
   ins->installed = 1;
   if (EvaluationData(theEnv)->EvaluationError)
     {
      PrintErrorID(theEnv,"INSMNGR",8,false);
      EnvPrintRouter(theEnv,WERROR,"An error occurred during the initialization of instance ");
      EnvPrintRouter(theEnv,WERROR,ins->name->contents);
      EnvPrintRouter(theEnv,WERROR,".\n");
      return false;
     }

   ins->initializeInProgress = 0;
   return (ins->initSlotsCalled == 0) ? false : true;
  }

/*******************************************************************
  NAME         : CoreInitializeInstanceCV
  DESCRIPTION  : Performs the core work for initializing an instance
  INPUTS       : 1) The instance address
                 2) Slot override CLIPSValues
  RETURNS      : True if all OK, false otherwise
  SIDE EFFECTS : EvaluationError set on errors - slots evaluated
  NOTES        : None
 *******************************************************************/
static bool CoreInitializeInstanceCV(
  Environment *theEnv,
  Instance *ins,
  CLIPSValue *overrides)
  {
   UDFValue temp;

   if (ins->installed == 0)
     {
      PrintErrorID(theEnv,"INSMNGR",7,false);
      EnvPrintRouter(theEnv,WERROR,"Instance ");
      EnvPrintRouter(theEnv,WERROR,ins->name->contents);
      EnvPrintRouter(theEnv,WERROR," is already being initialized.\n");
      EnvSetEvaluationError(theEnv,true);
      return false;
     }
     
   /*==========================================================*/
   /* Replace all default-slot values with any slot-overrides. */
   /*==========================================================*/
   
   ins->busy++;
   ins->installed = 0;
   
   /*===============================================*/
   /* If the slots are initialized properly - the   */
   /* initializeInProgress flag will be turned off. */
   /*===============================================*/
   
   ins->initializeInProgress = 1;
   ins->initSlotsCalled = 0;

   if (InsertSlotOverridesCV(theEnv,ins,overrides) == false)
      {
       ins->installed = 1;
       ins->busy--;
       return false;
      }

   /*====================================================*/
   /* Now that all the slot expressions are established, */
   /* replace them  with their evaluation.               */
   /*====================================================*/

   if (InstanceData(theEnv)->MkInsMsgPass)
     { DirectMessage(theEnv,MessageHandlerData(theEnv)->INIT_SYMBOL,ins,&temp,NULL); }
   else
     { EvaluateClassDefaults(theEnv,ins); }

   ins->busy--;
   ins->installed = 1;
   if (EvaluationData(theEnv)->EvaluationError)
     {
      PrintErrorID(theEnv,"INSMNGR",8,false);
      EnvPrintRouter(theEnv,WERROR,"An error occurred during the initialization of instance ");
      EnvPrintRouter(theEnv,WERROR,ins->name->contents);
      EnvPrintRouter(theEnv,WERROR,".\n");
      return false;
     }
     
   ins->initializeInProgress = 0;
   return (ins->initSlotsCalled == 0) ? false : true;
  }

/**********************************************************
  NAME         : InsertSlotOverrides
  DESCRIPTION  : Replaces value-expression for a slot
  INPUTS       : 1) The instance address
                 2) The address of the beginning of the
                    list of slot-expressions
  RETURNS      : True if all okay, false otherwise
  SIDE EFFECTS : Old slot expression deallocated
  NOTES        : Assumes symbols not yet installed
                 EVALUATES the slot-name expression but
                    simply copies the slot value-expression
 **********************************************************/
static bool InsertSlotOverrides(
  Environment *theEnv,
  Instance *ins,
  Expression *slot_exp)
  {
   InstanceSlot *slot;
   UDFValue temp, junk;

   EvaluationData(theEnv)->EvaluationError = false;
   while (slot_exp != NULL)
     {
      if ((EvaluateExpression(theEnv,slot_exp,&temp) == true) ? true :
          (temp.header->type != SYMBOL_TYPE))
        {
         PrintErrorID(theEnv,"INSMNGR",9,false);
         EnvPrintRouter(theEnv,WERROR,"Expected a valid slot name for slot-override.\n");
         EnvSetEvaluationError(theEnv,true);
         return false;
        }
      slot = FindInstanceSlot(theEnv,ins,temp.lexemeValue);
      if (slot == NULL)
        {
         PrintErrorID(theEnv,"INSMNGR",13,false);
         EnvPrintRouter(theEnv,WERROR,"Slot ");
         EnvPrintRouter(theEnv,WERROR,temp.lexemeValue->contents);
         EnvPrintRouter(theEnv,WERROR," does not exist in instance ");
         EnvPrintRouter(theEnv,WERROR,ins->name->contents);
         EnvPrintRouter(theEnv,WERROR,".\n");
         EnvSetEvaluationError(theEnv,true);
         return false;
        }

      if (InstanceData(theEnv)->MkInsMsgPass)
        { DirectMessage(theEnv,slot->desc->overrideMessage,
                       ins,NULL,slot_exp->nextArg->argList); }
      else if (slot_exp->nextArg->argList)
        {
         if (EvaluateAndStoreInDataObject(theEnv,(int) slot->desc->multiple,
                               slot_exp->nextArg->argList,&temp,true))
             PutSlotValue(theEnv,ins,slot,&temp,&junk,"function make-instance");
        }
      else
        {
         temp.begin = 0;
         temp.range = 0;
         temp.value = ProceduralPrimitiveData(theEnv)->NoParamValue;
         PutSlotValue(theEnv,ins,slot,&temp,&junk,"function make-instance");
        }

      if (EvaluationData(theEnv)->EvaluationError)
        return false;
      slot->override = true;
      slot_exp = slot_exp->nextArg->nextArg;
     }
   return true;
  }

/**********************************************************
  NAME         : InsertSlotOverridesCV
  DESCRIPTION  : Replaces value-expression for a slot
  INPUTS       : 1) The instance address
                 2) The address of the beginning of the
                    list of slot-expressions
  RETURNS      : True if all okay, false otherwise
  SIDE EFFECTS : Old slot expression deallocated
  NOTES        : Assumes symbols not yet installed
                 EVALUATES the slot-name expression but
                    simply copies the slot value-expression
 **********************************************************/
static bool InsertSlotOverridesCV(
  Environment *theEnv,
  Instance *ins,
  CLIPSValue *overrides)
  {
   int i;
   InstanceSlot *slot;
   UDFValue temp, junk;

   EvaluationData(theEnv)->EvaluationError = false;

   for (i = 0; i < ins->cls->slotCount; i++)
     {
      if (overrides[i].value == VoidConstant(theEnv)) continue;
      
      slot = ins->slotAddresses[i];
      CLIPSToUDFValue(&overrides[i],&temp);
      PutSlotValue(theEnv,ins,slot,&temp,&junk,"InstanceBuilder call");
      
      if (EvaluationData(theEnv)->EvaluationError)
        { return false; }
        
      slot->override = true;
     }
     
   return true;
  }

/*****************************************************************************
  NAME         : EvaluateClassDefaults
  DESCRIPTION  : Evaluates default slots only - slots that were specified
                 by overrides (sp->override == 1) are ignored)
  INPUTS       : 1) Instance address
  RETURNS      : Nothing useful
  SIDE EFFECTS : Each UDFValue slot in the instance's slot array is replaced
                   by the evaluation (by EvaluateExpression) of the expression
                   in the slot list.  The old expression-values
                   are deleted.
  NOTES        : None
 *****************************************************************************/
static void EvaluateClassDefaults(
  Environment *theEnv,
  Instance *ins)
  {
   InstanceSlot *slot;
   UDFValue temp,junk;
   long i;

   if (ins->initializeInProgress == 0)
     {
      PrintErrorID(theEnv,"INSMNGR",15,false);
      EnvSetEvaluationError(theEnv,true);
      EnvPrintRouter(theEnv,WERROR,"init-slots not valid in this context.\n");
      return;
     }
   for (i = 0 ; i < ins->cls->instanceSlotCount ; i++)
     {
      slot = ins->slotAddresses[i];

      /* ===========================================================
         Slot-overrides are just a short-hand for put-slots, so they
         should be done with messages.  Defaults are from the class
         definition and can be placed directly.
         =========================================================== */
      if (!slot->override)
        {
         if (slot->desc->dynamicDefault)
           {
            if (EvaluateAndStoreInDataObject(theEnv,(int) slot->desc->multiple,
                                             (Expression *) slot->desc->defaultValue,
                                             &temp,true))
              PutSlotValue(theEnv,ins,slot,&temp,&junk,"function init-slots");
           }
         else if (((slot->desc->shared == 0) || (slot->desc->sharedCount == 1)) &&
                  (slot->desc->noDefault == 0))
           DirectPutSlotValue(theEnv,ins,slot,(UDFValue *) slot->desc->defaultValue,&junk);
         else if (slot->valueRequired)
           {
            PrintErrorID(theEnv,"INSMNGR",14,false);
            EnvPrintRouter(theEnv,WERROR,"Override required for slot ");
            EnvPrintRouter(theEnv,WERROR,slot->desc->slotName->name->contents);
            EnvPrintRouter(theEnv,WERROR," in instance ");
            EnvPrintRouter(theEnv,WERROR,ins->name->contents);
            EnvPrintRouter(theEnv,WERROR,".\n");
            EnvSetEvaluationError(theEnv,true);
           }
         slot->valueRequired = false;
         if (ins->garbage == 1)
           {
            EnvPrintRouter(theEnv,WERROR,ins->name->contents);
            EnvPrintRouter(theEnv,WERROR," instance deleted by slot-override evaluation.\n");
            EnvSetEvaluationError(theEnv,true);
           }
         if (EvaluationData(theEnv)->EvaluationError)
            return;
        }
      slot->override = false;
     }
   ins->initSlotsCalled = 1;
  }

#if DEBUGGING_FUNCTIONS

/***************************************************
  NAME         : PrintInstanceWatch
  DESCRIPTION  : Prints out a trace message for the
                 creation/deletion of an instance
  INPUTS       : 1) The trace string indicating if
                    this is a creation or deletion
                 2) The instance
  RETURNS      : Nothing usful
  SIDE EFFECTS : Watch message printed
  NOTES        : None
 ***************************************************/
static void PrintInstanceWatch(
  Environment *theEnv,
  const char *traceString,
  Instance *theInstance)
  {
   EnvPrintRouter(theEnv,WTRACE,traceString);
   EnvPrintRouter(theEnv,WTRACE," instance ");
   PrintInstanceNameAndClass(theEnv,WTRACE,theInstance,true);
  }

#endif

/*****************************/
/* EnvCreateInstanceBuilder: */
/*****************************/
InstanceBuilder *EnvCreateInstanceBuilder(
  Environment *theEnv,
  const char *defclassName)
  {
   InstanceBuilder *theIB;
   Defclass *theDefclass;
   int i;
   
   theDefclass = EnvFindDefclass(theEnv,defclassName);
   if (theDefclass == NULL) return NULL;
      
   theIB = get_struct(theEnv,instanceBuilder);
   if (theIB == NULL) return NULL;
   
   theIB->ibEnv = theEnv;
   theIB->ibDefclass = theDefclass;
      
   theIB->ibValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * theDefclass->slotCount);

   for (i = 0; i < theDefclass->slotCount; i++)
     { theIB->ibValueArray[i].voidValue = VoidConstant(theEnv); }

   return theIB;
  }

/********************/
/* IBPutSlotInteger */
/********************/
bool IBPutSlotInteger(
  InstanceBuilder *theIB,
  const char *slotName,
  CLIPSInteger *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.integerValue = slotValue;
   return IBPutSlot(theIB,slotName,&theValue);
  }

/*******************/
/* IBPutSlotLexeme */
/*******************/
bool IBPutSlotLexeme(
  InstanceBuilder *theIB,
  const char *slotName,
  CLIPSLexeme *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.lexemeValue = slotValue;
   return IBPutSlot(theIB,slotName,&theValue);
  }

/******************/
/* IBPutSlotFloat */
/******************/
bool IBPutSlotFloat(
  InstanceBuilder *theIB,
  const char *slotName,
  CLIPSFloat *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.floatValue = slotValue;
   return IBPutSlot(theIB,slotName,&theValue);
  }

/*****************/
/* IBPutSlotFact */
/*****************/
bool IBPutSlotFact(
  InstanceBuilder *theIB,
  const char *slotName,
  Fact *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.factValue = slotValue;
   return IBPutSlot(theIB,slotName,&theValue);
  }

/*********************/
/* IBPutSlotInstance */
/*********************/
bool IBPutSlotInstance(
  InstanceBuilder *theIB,
  const char *slotName,
  Instance *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.instanceValue = slotValue;
   return IBPutSlot(theIB,slotName,&theValue);
  }

/****************************/
/* IBPutSlotExternalAddress */
/****************************/
bool IBPutSlotExternalAddress(
  InstanceBuilder *theIB,
  const char *slotName,
  CLIPSExternalAddress *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.externalAddressValue = slotValue;
   return IBPutSlot(theIB,slotName,&theValue);
  }

/***********************/
/* IBPutSlotMultifield */
/***********************/
bool IBPutSlotMultifield(
  InstanceBuilder *theIB,
  const char *slotName,
  Multifield *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.multifieldValue = slotValue;
   return IBPutSlot(theIB,slotName,&theValue);
  }

/**************/
/* IBPutSlot: */
/**************/
bool IBPutSlot(
  InstanceBuilder *theIB,
  const char *slotName,
  CLIPSValue *slotValue)
  {
   Environment *theEnv = theIB->ibEnv;
   short whichSlot;
   CLIPSValue oldValue;
   int i;
      
   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   whichSlot = FindInstanceTemplateSlot(theEnv,theIB->ibDefclass,EnvCreateSymbol(theIB->ibEnv,slotName));
   if (whichSlot == -1)
     { return false; }
     
   /*===================================*/
   /* Create the value array if needed. */
   /*===================================*/
     
   if (theIB->ibValueArray == NULL)
     {
      theIB->ibValueArray = (CLIPSValue *) gm3(theIB->ibEnv,sizeof(CLIPSValue) * theIB->ibDefclass->slotCount);
      for (i = 0; i < theIB->ibDefclass->slotCount; i++)
        { theIB->ibValueArray[i].voidValue = theIB->ibEnv->VoidConstant; }
     }

   /*=====================*/
   /* Set the slot value. */
   /*=====================*/
   
   oldValue.value = theIB->ibValueArray[whichSlot].value;
   
   if (oldValue.header->type == MULTIFIELD_TYPE)
     {
      if (MultifieldsEqual(oldValue.multifieldValue,slotValue->multifieldValue))
        { return true; }
     }
   else
     {
      if (oldValue.value == slotValue->value)
        { return true; }
     }
   
   CVAtomDeinstall(theEnv,oldValue.value);
   
   if (oldValue.header->type == MULTIFIELD_TYPE)
     { ReturnMultifield(theEnv,oldValue.multifieldValue); }

   if (slotValue->header->type == MULTIFIELD_TYPE)
     { theIB->ibValueArray[whichSlot].multifieldValue = CopyMultifield(theEnv,slotValue->multifieldValue); }
   else
     { theIB->ibValueArray[whichSlot].value = slotValue->value; }
      
   CVAtomInstall(theEnv,theIB->ibValueArray[whichSlot].value);
   
   return true;
  }

/***********/
/* IBMake: */
/***********/
Instance *IBMake(
  InstanceBuilder *theIB,
  const char *instanceName)
  {
   Environment *theEnv = theIB->ibEnv;
   Instance *theInstance;
   CLIPSLexeme *instanceLexeme;
   UDFValue rv;
   int i;

   if (instanceName == NULL)
     {
      GensymStar(theIB->ibEnv,&rv);
      instanceLexeme = rv.lexemeValue;
     }
   else
     { instanceLexeme = EnvCreateInstanceName(theEnv,instanceName); }
   
   theInstance = BuildInstance(theEnv,instanceLexeme,theIB->ibDefclass,true);
   if (theInstance == NULL) return NULL;
   
   if (CoreInitializeInstanceCV(theIB->ibEnv,theInstance,theIB->ibValueArray) == false)
     {
      QuashInstance(theIB->ibEnv,theInstance);
      return NULL;
     }
 
   for (i = 0; i < theIB->ibDefclass->slotCount; i++)
     {
      if (theIB->ibValueArray[i].voidValue != VoidConstant(theEnv))
        {
         CVAtomDeinstall(theEnv,theIB->ibValueArray[i].value);

         if (theIB->ibValueArray[i].header->type == MULTIFIELD_TYPE)
           { ReturnMultifield(theEnv,theIB->ibValueArray[i].multifieldValue); }

         theIB->ibValueArray[i].voidValue = VoidConstant(theEnv);
        }
     }

   return theInstance;
  }

/**************/
/* IBDispose: */
/**************/
void IBDispose(
  InstanceBuilder *theIB)
  {
   Environment *theEnv = theIB->ibEnv;

   IBAbort(theIB);
   
   if (theIB->ibValueArray != NULL)
     { rm3(theEnv,theIB->ibValueArray,sizeof(CLIPSValue) * theIB->ibDefclass->slotCount); }
   
   rtn_struct(theEnv,instanceBuilder,theIB);
  }

/************/
/* IBAbort: */
/************/
void IBAbort(
  InstanceBuilder *theIB)
  {
   Environment *theEnv = theIB->ibEnv;
   int i;
   
   for (i = 0; i < theIB->ibDefclass->slotCount; i++)
     {
      CVAtomDeinstall(theEnv,theIB->ibValueArray[i].value);
      
      if (theIB->ibValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theIB->ibValueArray[i].multifieldValue); }
        
      theIB->ibValueArray[i].voidValue = VoidConstant(theEnv);
     }
  }
  
/*****************/
/* IBSetDefclass */
/*****************/
bool IBSetDefclass(
  InstanceBuilder *theIB,
  const char *defclassName)
  {
   Defclass *theDefclass;
   Environment *theEnv = theIB->ibEnv;
   int i;
   
   IBAbort(theIB);
   
   theDefclass = EnvFindDefclass(theIB->ibEnv,defclassName);
   
   if (theDefclass == NULL) return false;

   if (theIB->ibValueArray != NULL)
     { rm3(theEnv,theIB->ibValueArray,sizeof(CLIPSValue) * theIB->ibDefclass->slotCount); }

   theIB->ibDefclass = theDefclass;
   
   theIB->ibValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * theDefclass->slotCount);

   for (i = 0; i < theDefclass->slotCount; i++)
     { theIB->ibValueArray[i].voidValue = VoidConstant(theEnv); }

   return true;
  }

/******************************/
/* EnvCreateInstanceModifier: */
/******************************/
InstanceModifier *EnvCreateInstanceModifier(
  Environment *theEnv,
  Instance *oldInstance)
  {
   InstanceModifier *theIM;
   int i;

   if (oldInstance->garbage) return NULL;
   if (oldInstance->cls->slotCount == 0) return NULL;

   theIM = get_struct(theEnv,instanceModifier);
   if (theIM == NULL) return NULL;

   theIM->imEnv = theEnv;
   theIM->imOldInstance = oldInstance;

   EnvIncrementInstanceCount(theEnv,oldInstance);

   theIM->imValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * oldInstance->cls->slotCount);

   for (i = 0; i < oldInstance->cls->slotCount; i++)
     { theIM->imValueArray[i].voidValue = VoidConstant(theEnv); }

   theIM->changeMap = (char *) gm2(theEnv,CountToBitMapSize(oldInstance->cls->slotCount));
   ClearBitString((void *) theIM->changeMap,CountToBitMapSize(oldInstance->cls->slotCount));

   return theIM;
  }

/********************/
/* IMPutSlotInteger */
/********************/
bool IMPutSlotInteger(
  InstanceModifier *theFM,
  const char *slotName,
  CLIPSInteger *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.integerValue = slotValue;
   return IMPutSlot(theFM,slotName,&theValue);
  }

/*******************/
/* IMPutSlotLexeme */
/*******************/
bool IMPutSlotLexeme(
  InstanceModifier *theFM,
  const char *slotName,
  CLIPSLexeme *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.lexemeValue = slotValue;
   return IMPutSlot(theFM,slotName,&theValue);
  }

/******************/
/* IMPutSlotFloat */
/******************/
bool IMPutSlotFloat(
  InstanceModifier *theFM,
  const char *slotName,
  CLIPSFloat *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.floatValue = slotValue;
   return IMPutSlot(theFM,slotName,&theValue);
  }

/*****************/
/* IMPutSlotFact */
/*****************/
bool IMPutSlotFact(
  InstanceModifier *theFM,
  const char *slotName,
  Fact *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.factValue = slotValue;
   return IMPutSlot(theFM,slotName,&theValue);
  }

/*********************/
/* IMPutSlotInstance */
/*********************/
bool IMPutSlotInstance(
  InstanceModifier *theFM,
  const char *slotName,
  Instance *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.instanceValue = slotValue;
   return IMPutSlot(theFM,slotName,&theValue);
  }

/****************************/
/* IMPutSlotExternalAddress */
/****************************/
bool IMPutSlotExternalAddress(
  InstanceModifier *theFM,
  const char *slotName,
  CLIPSExternalAddress *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.externalAddressValue = slotValue;
   return IMPutSlot(theFM,slotName,&theValue);
  }

/***********************/
/* IMPutSlotMultifield */
/***********************/
bool IMPutSlotMultifield(
  InstanceModifier *theFM,
  const char *slotName,
  Multifield *slotValue)
  {
   CLIPSValue theValue;
   
   theValue.multifieldValue = slotValue;
   return IMPutSlot(theFM,slotName,&theValue);
  }

/**************/
/* IMPutSlot: */
/**************/
bool IMPutSlot(
  InstanceModifier *theIM,
  const char *slotName,
  CLIPSValue *slotValue)
  {
   Environment *theEnv = theIM->imEnv;
   short whichSlot;
   CLIPSValue oldValue;
   CLIPSValue oldInstanceValue;
   int i;

   /*===================================*/
   /* Make sure the slot name requested */
   /* corresponds to a valid slot name. */
   /*===================================*/

   whichSlot = FindInstanceTemplateSlot(theEnv,theIM->imOldInstance->cls,EnvCreateSymbol(theIM->imEnv,slotName));
   if (whichSlot == -1)
     { return false; }

   /*=============================================*/
   /* Make sure a single field value is not being */
   /* stored in a multifield slot or vice versa.  */
   /*=============================================*/
/*
   if (((theSlot->multislot == 0) && (slotValue->header->type == MULTIFIELD_TYPE)) ||
       ((theSlot->multislot == 1) && (slotValue->header->type != MULTIFIELD_TYPE)))
     { return false; }
*/
   if (theIM->imValueArray == NULL)
     {
      theIM->imValueArray = (CLIPSValue *) gm3(theIM->imEnv,sizeof(CLIPSValue) * theIM->imOldInstance->cls->slotCount);
      for (i = 0; i < theIM->imOldInstance->cls->slotCount; i++)
        { theIM->imValueArray[i].voidValue = theIM->imEnv->VoidConstant; }
     }

   if (theIM->changeMap == NULL)
     {
      theIM->changeMap = (char *) gm2(theIM->imEnv,CountToBitMapSize(theIM->imOldInstance->cls->slotCount));
      ClearBitString((void *) theIM->changeMap,CountToBitMapSize(theIM->imOldInstance->cls->slotCount));
     }
     
   /*=====================*/
   /* Set the slot value. */
   /*=====================*/

   oldValue.value = theIM->imValueArray[whichSlot].value;
   oldInstanceValue.value = theIM->imOldInstance->slotAddresses[whichSlot]->value;

   if (oldInstanceValue.header->type == MULTIFIELD_TYPE)
     {
      if (MultifieldsEqual(oldInstanceValue.multifieldValue,slotValue->multifieldValue))
        {
         CVAtomDeinstall(theIM->imEnv,oldValue.value);
         if (oldValue.header->type == MULTIFIELD_TYPE)
           { ReturnMultifield(theIM->imEnv,oldValue.multifieldValue); }
         theIM->imValueArray[whichSlot].voidValue = theIM->imEnv->VoidConstant;
         ClearBitMap(theIM->changeMap,whichSlot);
         return true;
        }

      if (MultifieldsEqual(oldValue.multifieldValue,slotValue->multifieldValue))
        { return true; }
     }
   else
     {
      if (slotValue->value == oldInstanceValue.value)
        {
         CVAtomDeinstall(theIM->imEnv,oldValue.value);
         theIM->imValueArray[whichSlot].voidValue = theIM->imEnv->VoidConstant;
         ClearBitMap(theIM->changeMap,whichSlot);
         return true;
        }
        
      if (oldValue.value == slotValue->value)
        { return true; }
     }

   SetBitMap(theIM->changeMap,whichSlot);

   CVAtomDeinstall(theIM->imEnv,oldValue.value);

   if (oldValue.header->type == MULTIFIELD_TYPE)
     { ReturnMultifield(theIM->imEnv,oldValue.multifieldValue); }
      
   if (slotValue->header->type == MULTIFIELD_TYPE)
     { theIM->imValueArray[whichSlot].multifieldValue = CopyMultifield(theIM->imEnv,slotValue->multifieldValue); }
   else
     { theIM->imValueArray[whichSlot].value = slotValue->value; }

   CVAtomInstall(theIM->imEnv,theIM->imValueArray[whichSlot].value);

   return true;
  }

/*************/
/* IMModify: */
/*************/
Instance *IMModify(
  InstanceModifier *theIM)
  {
   Instance *rv = theIM->imOldInstance;
   bool ov;

   if (! BitStringHasBitsSet(theIM->changeMap,CountToBitMapSize(theIM->imOldInstance->cls->slotCount)))
     { return theIM->imOldInstance; }
     
   ov = SetDelayObjectPatternMatching(theIM->imEnv,true);
   IMModifySlots(theIM->imEnv,theIM->imOldInstance,theIM->imValueArray);
   SetDelayObjectPatternMatching(theIM->imEnv,ov);
   
   IMAbort(theIM);
   
   return rv;
  }

/*****************/
/* IMModifySlots */
/*****************/
static bool IMModifySlots(
  Environment *theEnv,
  Instance *theInstance,
  CLIPSValue *overrides)
  {
   UDFValue temp, junk;
   InstanceSlot *insSlot;
   int i;

   for (i = 0; i < theInstance->cls->slotCount; i++)
     {
      if (overrides[i].value == VoidConstant(theEnv))
        { continue; }
        
      insSlot = theInstance->slotAddresses[i];
      
      if (insSlot->desc->multiple && (overrides[i].header->type != MULTIFIELD_TYPE))
        {
         temp.value = EnvCreateMultifield(theEnv,1L);
         temp.begin = 0;
         temp.range = 1;
         temp.multifieldValue->theFields[0].value = overrides[i].value;
        }
      else
        { CLIPSToUDFValue(&overrides[i],&temp); }
        
      if (PutSlotValue(theEnv,theInstance,insSlot,&temp,&junk,"InstanceModifier call") == false)
        { return false; }
     }
     
   return true;
  }

/**************/
/* IMDispose: */
/**************/
void IMDispose(
  InstanceModifier *theIM)
  {
   Environment *theEnv = theIM->imEnv;
   int i;

   /*========================*/
   /* Clear the value array. */
   /*========================*/
   
   for (i = 0; i < theIM->imOldInstance->cls->slotCount; i++)
     {
      CVAtomDeinstall(theEnv,theIM->imValueArray[i].value);

      if (theIM->imValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theIM->imValueArray[i].multifieldValue); }
     }
   
   /*=====================================*/
   /* Return the value and change arrays. */
   /*=====================================*/
   
   if (theIM->imValueArray != NULL)
     { rm3(theEnv,theIM->imValueArray,sizeof(CLIPSValue) * theIM->imOldInstance->cls->slotCount); }
      
   if (theIM->changeMap != NULL)
     { rm(theEnv,(void *) theIM->changeMap,CountToBitMapSize(theIM->imOldInstance->cls->slotCount)); }

   /*========================================*/
   /* Return the InstanceModifier structure. */
   /*========================================*/
   
   EnvDecrementInstanceCount(theEnv,theIM->imOldInstance);
   
   rtn_struct(theEnv,instanceModifier,theIM);
  }

/************/
/* IMAbort: */
/************/
void IMAbort(
  InstanceModifier *theIM)
  {
   Environment *theEnv = theIM->imEnv;
   int i;
   
   for (i = 0; i < theIM->imOldInstance->cls->slotCount; i++)
     {
      CVAtomDeinstall(theEnv,theIM->imValueArray[i].value);

      if (theIM->imValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theIM->imValueArray[i].multifieldValue); }
        
      theIM->imValueArray[i].voidValue = theIM->imEnv->VoidConstant;
     }
     
   ClearBitString((void *) theIM->changeMap,CountToBitMapSize(theIM->imOldInstance->cls->slotCount));
  }

/******************/
/* IMSetInstance: */
/******************/
bool IMSetInstance(
  InstanceModifier *theIM,
  Instance *oldInstance)
  {
   Environment *theEnv = theIM->imEnv;
   unsigned short currentSlotCount = theIM->imOldInstance->cls->slotCount;
   int i;
   
   /*=================================================*/
   /* Modifiers can only be created for non-retracted */
   /* deftemplate facts with at least one slot.       */
   /*=================================================*/
   
   if (oldInstance->garbage) return false;
   if (oldInstance->cls->slotCount == 0) return false;

   /*========================*/
   /* Clear the value array. */
   /*========================*/
   
   for (i = 0; i < theIM->imOldInstance->cls->slotCount; i++)
     {
      CVAtomDeinstall(theEnv,theIM->imValueArray[i].value);

      if (theIM->imValueArray[i].header->type == MULTIFIELD_TYPE)
        { ReturnMultifield(theEnv,theIM->imValueArray[i].multifieldValue); }
     }

   /*==================================================*/
   /* Resize the value and change arrays if necessary. */
   /*==================================================*/
   
   if (oldInstance->cls->slotCount != currentSlotCount)
     {
      if (theIM->imValueArray != NULL)
        { rm3(theEnv,theIM->imValueArray,sizeof(CLIPSValue) * currentSlotCount); }
      
      if (theIM->changeMap != NULL)
        { rm(theEnv,(void *) theIM->changeMap,currentSlotCount); }
        
      theIM->imValueArray = (CLIPSValue *) gm3(theEnv,sizeof(CLIPSValue) * oldInstance->cls->slotCount);
      theIM->changeMap = (char *) gm2(theEnv,CountToBitMapSize(oldInstance->cls->slotCount));
     }
   
   /*=================================*/
   /* Update the fact being modified. */
   /*=================================*/
   
   EnvDecrementInstanceCount(theEnv,theIM->imOldInstance);
   theIM->imOldInstance = oldInstance;
   EnvIncrementInstanceCount(theEnv,theIM->imOldInstance);
   
   /*=========================================*/
   /* Initialize the value and change arrays. */
   /*=========================================*/
   
   for (i = 0; i < theIM->imOldInstance->cls->slotCount; i++)
     { theIM->imValueArray[i].voidValue = theIM->imEnv->VoidConstant; }
   
   ClearBitString((void *) theIM->changeMap,CountToBitMapSize(theIM->imOldInstance->cls->slotCount));

   /*================================================================*/
   /* Return true to indicate the modifier was successfully created. */
   /*================================================================*/
   
   return true;
  }

#endif



