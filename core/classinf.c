   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*        CLASS INFO PROGRAMMATIC ACCESS MODULE        */
   /*******************************************************/

/**************************************************************/
/* Purpose: Class Information Interface Support Routines      */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Dantes                                       */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859   */
/*                                                            */
/*            Changed name of variable exp to theExp          */
/*            because of Unix compiler warnings of shadowed   */
/*            definitions.                                    */
/*                                                            */
/*      6.24: Added allowed-classes slot facet.               */
/*                                                            */
/*            Converted INSTANCE_PATTERN_MATCHING to          */
/*            DEFRULE_CONSTRUCT.                              */
/*                                                            */
/*            Renamed BOOLEAN macro type to intBool.          */
/*                                                            */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior  */
/*            (MAC_MCW, IBM_MCW) are no longer supported.     */
/*                                                            */
/*            Changed integer type/precision.                 */
/*                                                            */
/*            Added const qualifiers to remove C++            */
/*            deprecation warnings.                           */
/*                                                            */
/*            Converted API macros to function calls.         */
/*                                                            */
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
/**************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#include <stdio.h>
#include <string.h>

#include "argacces.h"
#include "classcom.h"
#include "classexm.h"
#include "classfun.h"
#include "classini.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "insfun.h"
#include "msgcom.h"
#include "msgfun.h"
#include "multifld.h"
#include "prntutil.h"

#include "classinf.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    SlotInfoSupportFunction(UDFContext *,CLIPSValue *,const char *,void (*)(Environment *,Defclass *,const char *,CLIPSValue *));
   static unsigned                CountSubclasses(Defclass *,bool,int);
   static unsigned                StoreSubclasses(Multifield *,unsigned,Defclass *,int,int,bool);
   static SlotDescriptor         *SlotInfoSlot(Environment *,CLIPSValue *,Defclass *,const char *,const char *);

/*********************************************************************
  NAME         : ClassAbstractPCommand
  DESCRIPTION  : Determines if direct instances of a class can be made
  INPUTS       : None
  RETURNS      : True (1) if class is abstract, false (0) if concrete
  SIDE EFFECTS : None
  NOTES        : Syntax: (class-abstractp <class>)
 *********************************************************************/
void ClassAbstractPCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;
   Defclass *cls;

   returnValue->type = SYMBOL;

   if (! UDFFirstArgument(context,SYMBOL_TYPE,&theArg))
     { return; }

   cls = LookupDefclassByMdlOrScope(theEnv,DOToString(theArg));
   if (cls == NULL)
     {
      ClassExistError(theEnv,"class-abstractp",ValueToString(theArg.value));
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   if (EnvClassAbstractP(theEnv,cls))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

#if DEFRULE_CONSTRUCT

/*****************************************************************
  NAME         : ClassReactivePCommand
  DESCRIPTION  : Determines if instances of a class can match rule
                 patterns
  INPUTS       : None
  RETURNS      : True (1) if class is reactive, false (0)
                 if non-reactive
  SIDE EFFECTS : None
  NOTES        : Syntax: (class-reactivep <class>)
 *****************************************************************/
void ClassReactivePCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   CLIPSValue theArg;
   Defclass *cls;

   returnValue->type = SYMBOL;

   if (! UDFFirstArgument(context,SYMBOL_TYPE,&theArg))
     { return; }

   cls = LookupDefclassByMdlOrScope(theEnv,DOToString(theArg));
   if (cls == NULL)
     {
      ClassExistError(theEnv,"class-reactivep",ValueToString(theArg.value));
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }

   if (EnvClassReactiveP(theEnv,cls))
     { returnValue->value = EnvTrueSymbol(theEnv); }
   else
     { returnValue->value = EnvFalseSymbol(theEnv); }
  }

#endif

/***********************************************************
  NAME         : ClassInfoFnxArgs
  DESCRIPTION  : Examines arguments for:
                   class-slots, get-defmessage-handler-list,
                   class-superclasses and class-subclasses
  INPUTS       : 1) Name of function
                 2) A buffer to hold a flag indicating if
                    the inherit keyword was specified
  RETURNS      : Pointer to the class on success,
                   NULL on errors
  SIDE EFFECTS : inhp flag set
                 error flag set
  NOTES        : None
 ***********************************************************/
Defclass *ClassInfoFnxArgs(
  UDFContext *context,
  const char *fnx,
  bool *inhp)
  {
   Defclass *clsptr;
   CLIPSValue theArg;
   Environment *theEnv = context->environment;

   *inhp = false;

   if (! UDFFirstArgument(context,SYMBOL_TYPE,&theArg))
     { return NULL; }

   clsptr = LookupDefclassByMdlOrScope(theEnv,DOToString(theArg));
   if (clsptr == NULL)
     {
      ClassExistError(theEnv,fnx,ValueToString(theArg.value));
      return NULL;
     }

   if (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,SYMBOL_TYPE,&theArg))
        { return NULL; }

      if (strcmp(ValueToString(theArg.value),"inherit") == 0)
        { *inhp = true; }
      else
        {
         SyntaxErrorMessage(theEnv,fnx);
         EnvSetEvaluationError(theEnv,true);
         return NULL;
        }
     }

   return clsptr;
  }

/********************************************************************
  NAME         : ClassSlotsCommand
  DESCRIPTION  : Groups slot info for a class into a multifield value
                   for dynamic perusal
  INPUTS       : Data object buffer to hold the slots of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the slots of the class
  NOTES        : Syntax: (class-slots <class> [inherit])
 ********************************************************************/
void ClassSlotsCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;

   clsptr = ClassInfoFnxArgs(context,"class-slots",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   EnvClassSlots(theEnv,clsptr,returnValue,inhp);
  }

/************************************************************************
  NAME         : ClassSuperclassesCommand
  DESCRIPTION  : Groups superclasses for a class into a multifield value
                   for dynamic perusal
  INPUTS       : Data object buffer to hold the superclasses of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the superclasses of the class
  NOTES        : Syntax: (class-superclasses <class> [inherit])
 ************************************************************************/
void ClassSuperclassesCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;

   clsptr = ClassInfoFnxArgs(context,"class-superclasses",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   EnvClassSuperclasses(theEnv,clsptr,returnValue,inhp);
  }

/************************************************************************
  NAME         : ClassSubclassesCommand
  DESCRIPTION  : Groups subclasses for a class into a multifield value
                   for dynamic perusal
  INPUTS       : Data object buffer to hold the subclasses of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the subclasses of the class
  NOTES        : Syntax: (class-subclasses <class> [inherit])
 ************************************************************************/
void ClassSubclassesCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;

   clsptr = ClassInfoFnxArgs(context,"class-subclasses",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   EnvClassSubclasses(theEnv,clsptr,returnValue,inhp);
  }

/***********************************************************************
  NAME         : GetDefmessageHandlersListCmd
  DESCRIPTION  : Groups message-handlers for a class into a multifield
                   value for dynamic perusal
  INPUTS       : Data object buffer to hold the handlers of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the message-handlers of the class
  NOTES        : Syntax: (get-defmessage-handler-list <class> [inherit])
 ***********************************************************************/
void GetDefmessageHandlersListCmd(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;

   if (! UDFHasNextArgument(context))
     { EnvGetDefmessageHandlerList(theEnv,NULL,returnValue,false); }
   else
     {
      clsptr = ClassInfoFnxArgs(context,"get-defmessage-handler-list",&inhp);
      if (clsptr == NULL)
        {
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         return;
        }
      EnvGetDefmessageHandlerList(theEnv,clsptr,returnValue,inhp);
     }
  }

/*********************************
 Slot Information Access Functions
 *********************************/
void SlotFacetsCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-facets",EnvSlotFacets);
  }

void SlotSourcesCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-sources",EnvSlotSources);
  }

void SlotTypesCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-types",EnvSlotTypes);
  }

void SlotAllowedValuesCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-allowed-values",EnvSlotAllowedValues);
  }

void SlotAllowedClassesCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-allowed-classes",EnvSlotAllowedClasses);
  }

void SlotRangeCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-range",EnvSlotRange);
  }

void SlotCardinalityCommand(
  Environment *theEnv,
  UDFContext *context,
  CLIPSValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-cardinality",EnvSlotCardinality);
  }

/********************************************************************
  NAME         : EnvClassAbstractP
  DESCRIPTION  : Determines if a class is abstract or not
  INPUTS       : Generic pointer to class
  RETURNS      : 1 if class is abstract, 0 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
bool EnvClassAbstractP(
  Environment *theEnv,
  Defclass *theDefclass)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   return theDefclass->abstract;
  }

#if DEFRULE_CONSTRUCT

/********************************************************************
  NAME         : EnvClassReactiveP
  DESCRIPTION  : Determines if a class is reactive or not
  INPUTS       : Generic pointer to class
  RETURNS      : 1 if class is reactive, 0 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
bool EnvClassReactiveP(
  Environment *theEnv,
  Defclass *theDefclass)
  {
#if MAC_XCD
#pragma unused(theEnv)
#endif

   return theDefclass->reactive;
  }

#endif

/********************************************************************
  NAME         : EnvClassSlots
  DESCRIPTION  : Groups slot info for a class into a multifield value
                   for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the slots of the class
                 3) Include (1) or exclude (0) inherited slots
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the slots of the class
  NOTES        : None
 ********************************************************************/
void EnvClassSlots(
  Environment *theEnv,
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   long size;
   long i;

   size = inhp ? theDefclass->instanceSlotCount : theDefclass->slotCount;

   returnValue->type = MULTIFIELD;
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,size);
   returnValue->value = EnvCreateMultifield(theEnv,size);

   if (size == 0)
     { return; }

   if (inhp)
     {
      for (i = 0 ; i < theDefclass->instanceSlotCount ; i++)
        {
         SetMFType(returnValue->value,i+1,SYMBOL);
         SetMFValue(returnValue->value,i+1,theDefclass->instanceTemplate[i]->slotName->name);
        }
     }
   else
     {
      for (i = 0 ; i < theDefclass->slotCount ; i++)
        {
         SetMFType(returnValue->value,i+1,SYMBOL);
         SetMFValue(returnValue->value,i+1,theDefclass->slots[i].slotName->name);
        }
     }
  }

/************************************************************************
  NAME         : EnvGetDefmessageHandlerList
  DESCRIPTION  : Groups handler info for a class into a multifield value
                   for dynamic perusal
  INPUTS       : 1) Generic pointer to class (NULL to get handlers for
                    all classes)
                 2) Data object buffer to hold the handlers of the class
                 3) Include (1) or exclude (0) inherited handlers
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names and types of
                    the message-handlers of the class
  NOTES        : None
 ************************************************************************/
void EnvGetDefmessageHandlerList(
  Environment *theEnv,
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   Defclass *cls,*svcls,*svnxt,*supcls;
   long j;
   int classi, classiLimit;
   unsigned long i, sublen, len;

   if (theDefclass == NULL)
     {
      inhp = 0;
      cls = EnvGetNextDefclass(theEnv,NULL);
      svnxt = EnvGetNextDefclass(theEnv,cls);
     }
   else
     {
      cls = theDefclass;
      svnxt = EnvGetNextDefclass(theEnv,theDefclass);
      SetNextDefclass(cls,NULL);
     }

   for (svcls = cls , i = 0 ;
        cls != NULL ;
        cls = EnvGetNextDefclass(theEnv,cls))
     {
      classiLimit = inhp ? cls->allSuperclasses.classCount : 1;
      for (classi = 0 ; classi < classiLimit ; classi++)
        { i += cls->allSuperclasses.classArray[classi]->handlerCount; }
     }

   len = i * 3;
   returnValue->type = MULTIFIELD;
   SetpDOBegin(returnValue,1);
   SetpDOEnd(returnValue,len);
   returnValue->value = EnvCreateMultifield(theEnv,len);

   for (cls = svcls , sublen = 0 ;
        cls != NULL ;
        cls = EnvGetNextDefclass(theEnv,cls))
     {
      classiLimit = inhp ? cls->allSuperclasses.classCount : 1;
      for (classi = 0 ; classi < classiLimit ; classi++)
        {
         supcls = cls->allSuperclasses.classArray[classi];

         if (inhp == 0)
           { i = sublen + 1; }
         else
           { i = len - (supcls->handlerCount * 3) - sublen + 1; }

         for (j = 0 ; j < supcls->handlerCount ; j++)
           {
            SetMFType(returnValue->value,i,SYMBOL);
            SetMFValue(returnValue->value,i++,GetDefclassNamePointer(supcls));
            SetMFType(returnValue->value,i,SYMBOL);
            SetMFValue(returnValue->value,i++,supcls->handlers[j].name);
            SetMFType(returnValue->value,i,SYMBOL);
            SetMFValue(returnValue->value,i++,EnvAddSymbol(theEnv,MessageHandlerData(theEnv)->hndquals[supcls->handlers[j].type]));
           }

         sublen += supcls->handlerCount * 3;
        }
     }

   if (svcls != NULL)
     { SetNextDefclass(svcls,svnxt); }
  }

/***************************************************************************
  NAME         : EnvClassSuperclasses
  DESCRIPTION  : Groups the names of superclasses into a multifield
                   value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the superclasses of the class
                 3) Include (1) or exclude (0) indirect superclasses
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names of
                    the superclasses of the class
  NOTES        : None
 ***************************************************************************/
void EnvClassSuperclasses(
  Environment *theEnv,
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   PACKED_CLASS_LINKS *plinks;
   unsigned offset;
   long i,j;

   if (inhp)
     {
      plinks = &theDefclass->allSuperclasses;
      offset = 1;
     }
   else
     {
      plinks = &theDefclass->directSuperclasses;
      offset = 0;
     }

   returnValue->type = MULTIFIELD;
   returnValue->begin = 0;
   SetpDOEnd(returnValue,plinks->classCount - offset);
   returnValue->value = EnvCreateMultifield(theEnv,returnValue->end + 1U);

   if (returnValue->end == -1)
     { return; }

   for (i = offset , j = 1 ; i < plinks->classCount ; i++ , j++)
     {
      SetMFType(returnValue->value,j,SYMBOL);
      SetMFValue(returnValue->value,j,GetDefclassNamePointer(plinks->classArray[i]));
     }
  }

/**************************************************************************
  NAME         : EnvClassSubclasses
  DESCRIPTION  : Groups the names of subclasses for a class into a
                   multifield value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the sublclasses of the class
                 3) Include (1) or exclude (0) indirect subclasses
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the names
                    the subclasses of the class
  NOTES        : None
 **************************************************************************/
void EnvClassSubclasses(
  Environment *theEnv,
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   int i; // Bug fix 2014-07-18: Previously unsigned and SetpDOEnd decremented to -1.
   int id;

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   i = CountSubclasses(theDefclass,inhp,id);

   ReleaseTraversalID(theEnv);

   returnValue->type = MULTIFIELD;
   returnValue->begin = 0;
   SetpDOEnd(returnValue,i);
   returnValue->value = EnvCreateMultifield(theEnv,i);

   if (i == 0)
     { return; }

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   StoreSubclasses((Multifield *) returnValue->value,1,theDefclass,inhp,id,true);
   ReleaseTraversalID(theEnv);
  }

/**************************************************************************
  NAME         : ClassSubclassAddresses
  DESCRIPTION  : Groups the class addresses of subclasses for a class into a
                   multifield value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Data object buffer to hold the sublclasses of the class
                 3) Include (1) or exclude (0) indirect subclasses
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the subclass
                    addresss of the class
  NOTES        : None
 **************************************************************************/
void ClassSubclassAddresses(
  Environment *theEnv,
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   int i; // Bug fix 2014-07-18: Previously unsigned and SetpDOEnd decremented to -1.
   int id;

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   i = CountSubclasses(theDefclass,inhp,id);

   ReleaseTraversalID(theEnv);

   returnValue->type = MULTIFIELD;
   returnValue->begin = 0;
   SetpDOEnd(returnValue,i);
   returnValue->value = EnvCreateMultifield(theEnv,i);

   if (i == 0)
     { return; }

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   StoreSubclasses((Multifield *) returnValue->value,1,theDefclass,inhp,id,false);
   ReleaseTraversalID(theEnv);
  }
/**************************************************************************
  NAME         : Slot...  Slot information access functions
  DESCRIPTION  : Groups the sources/facets/types/allowed-values/range or
                   cardinality  of a slot for a class into a multifield
                   value for dynamic perusal
  INPUTS       : 1) Generic pointer to class
                 2) Name of the slot
                 3) Data object buffer to hold the attributes of the class
  RETURNS      : Nothing useful
  SIDE EFFECTS : Creates a multifield storing the attributes for the slot
                   of the class
  NOTES        : None
 **************************************************************************/

/*****************/
/* EnvSlotFacets */
/*****************/
void EnvSlotFacets(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   int i;
   SlotDescriptor *sp;

   if ((sp = SlotInfoSlot(theEnv,returnValue,theDefclass,sname,"slot-facets")) == NULL)
     { return; }

#if DEFRULE_CONSTRUCT
   returnValue->end = 9;
   returnValue->value = EnvCreateMultifield(theEnv,10L);
   for (i = 1 ; i <= 10 ; i++)
     SetMFType(returnValue->value,i,SYMBOL);
#else
   returnValue->end = 8;
   returnValue->value = EnvCreateMultifield(theEnv,9L);
   for (i = 1 ; i <= 9 ; i++)
     { SetMFType(returnValue->value,i,SYMBOL); }
#endif

   if (sp->multiple)
     { SetMFValue(returnValue->value,1,EnvAddSymbol(theEnv,"MLT")); }
   else
     { SetMFValue(returnValue->value,1,EnvAddSymbol(theEnv,"SGL")); }

   if (sp->noDefault)
     SetMFValue(returnValue->value,2,EnvAddSymbol(theEnv,"NIL"));
   else
     {
      if (sp->dynamicDefault)
        { SetMFValue(returnValue->value,2,EnvAddSymbol(theEnv,"DYN")); }
      else
        { SetMFValue(returnValue->value,2,EnvAddSymbol(theEnv,"STC")); }
     }

   if (sp->noInherit)
     SetMFValue(returnValue->value,3,EnvAddSymbol(theEnv,"NIL"));
   else
     SetMFValue(returnValue->value,3,EnvAddSymbol(theEnv,"INH"));

   if (sp->initializeOnly)
     SetMFValue(returnValue->value,4,EnvAddSymbol(theEnv,"INT"));
   else if (sp->noWrite)
     SetMFValue(returnValue->value,4,EnvAddSymbol(theEnv,"R"));
   else
     SetMFValue(returnValue->value,4,EnvAddSymbol(theEnv,"RW"));

   if (sp->shared)
     SetMFValue(returnValue->value,5,EnvAddSymbol(theEnv,"SHR"));
   else
     SetMFValue(returnValue->value,5,EnvAddSymbol(theEnv,"LCL"));

#if DEFRULE_CONSTRUCT
   if (sp->reactive)
     SetMFValue(returnValue->value,6,EnvAddSymbol(theEnv,"RCT"));
   else
     SetMFValue(returnValue->value,6,EnvAddSymbol(theEnv,"NIL"));

   if (sp->composite)
     SetMFValue(returnValue->value,7,EnvAddSymbol(theEnv,"CMP"));
   else
     SetMFValue(returnValue->value,7,EnvAddSymbol(theEnv,"EXC"));

   if (sp->publicVisibility)
     SetMFValue(returnValue->value,8,EnvAddSymbol(theEnv,"PUB"));
   else
     SetMFValue(returnValue->value,8,EnvAddSymbol(theEnv,"PRV"));

   SetMFValue(returnValue->value,9,EnvAddSymbol(theEnv,GetCreateAccessorString(sp)));
   SetMFValue(returnValue->value,10,sp->noWrite ? EnvAddSymbol(theEnv,"NIL") : (void *) sp->overrideMessage);
#else
   if (sp->composite)
     SetMFValue(returnValue->value,6,EnvAddSymbol(theEnv,"CMP"));
   else
     SetMFValue(returnValue->value,6,EnvAddSymbol(theEnv,"EXC"));

   if (sp->publicVisibility)
     SetMFValue(returnValue->value,7,EnvAddSymbol(theEnv,"PUB"));
   else
     SetMFValue(returnValue->value,7,EnvAddSymbol(theEnv,"PRV"));

   SetMFValue(returnValue->value,8,EnvAddSymbol(theEnv,GetCreateAccessorString(sp)));
   SetMFValue(returnValue->value,9,sp->noWrite ? EnvAddSymbol(theEnv,"NIL") : (void *) sp->overrideMessage);
#endif
  }

/******************/
/* EnvSlotSources */
/******************/
void EnvSlotSources(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   unsigned i;
   int classi;
   SlotDescriptor *sp, *csp;
   CLASS_LINK *ctop,*ctmp;
   Defclass *cls;

   if ((sp = SlotInfoSlot(theEnv,returnValue,theDefclass,sname,"slot-sources")) == NULL)
     return;
   i = 1;
   ctop = get_struct(theEnv,classLink);
   ctop->cls = sp->cls;
   ctop->nxt = NULL;
   if (sp->composite)
     {
      for (classi = 1 ; classi < sp->cls->allSuperclasses.classCount ; classi++)
        {
         cls = sp->cls->allSuperclasses.classArray[classi];
         csp = FindClassSlot(cls,sp->slotName->name);
         if ((csp != NULL) ? (csp->noInherit == 0) : false)
           {
            ctmp = get_struct(theEnv,classLink);
            ctmp->cls = cls;
            ctmp->nxt = ctop;
            ctop = ctmp;
            i++;
            if (csp->composite == 0)
              break;
           }
        }
     }
   SetpDOEnd(returnValue,i);
   returnValue->value = EnvCreateMultifield(theEnv,i);
   for (ctmp = ctop , i = 1 ; ctmp != NULL ; ctmp = ctmp->nxt , i++)
     {
      SetMFType(returnValue->value,i,SYMBOL);
      SetMFValue(returnValue->value,i,GetDefclassNamePointer(ctmp->cls));
     }
   DeleteClassLinks(theEnv,ctop);
  }

/****************/
/* EnvSlotTypes */
/****************/
void EnvSlotTypes(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   unsigned i,j;
   SlotDescriptor *sp;
   char typemap[2];
   unsigned msize;

   if ((sp = SlotInfoSlot(theEnv,returnValue,theDefclass,sname,"slot-types")) == NULL)
     return;
   if ((sp->constraint != NULL) ? sp->constraint->anyAllowed : true)
     {
      typemap[0] = typemap[1] = (char) 0xFF;
      ClearBitMap(typemap,MULTIFIELD);
      msize = 8;
     }
   else
     {
      typemap[0] = typemap[1] = (char) 0x00;
      msize = 0;
      if (sp->constraint->symbolsAllowed)
        {
         msize++;
         SetBitMap(typemap,SYMBOL);
        }
      if (sp->constraint->stringsAllowed)
        {
         msize++;
         SetBitMap(typemap,STRING);
        }
      if (sp->constraint->floatsAllowed)
        {
         msize++;
         SetBitMap(typemap,FLOAT);
        }
      if (sp->constraint->integersAllowed)
        {
         msize++;
         SetBitMap(typemap,INTEGER);
        }
      if (sp->constraint->instanceNamesAllowed)
        {
         msize++;
         SetBitMap(typemap,INSTANCE_NAME);
        }
      if (sp->constraint->instanceAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,INSTANCE_ADDRESS);
        }
      if (sp->constraint->externalAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,EXTERNAL_ADDRESS);
        }
      if (sp->constraint->factAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,FACT_ADDRESS);
        }
     }
   SetpDOEnd(returnValue,msize);
   returnValue->value = EnvCreateMultifield(theEnv,msize);
   i = 1;
   j = 0;
   while (i <= msize)
     {
      if (TestBitMap(typemap,j))
       {
        SetMFType(returnValue->value,i,SYMBOL);
        SetMFValue(returnValue->value,i,
                   GetDefclassNamePointer(DefclassData(theEnv)->PrimitiveClassMap[j]));
        i++;
       }
      j++;
     }
  }

/************************/
/* EnvSlotAllowedValues */
/************************/
void EnvSlotAllowedValues(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   int i;
   SlotDescriptor *sp;
   EXPRESSION *theExp;

   if ((sp = SlotInfoSlot(theEnv,returnValue,theDefclass,sname,"slot-allowed-values")) == NULL)
     return;
   if ((sp->constraint != NULL) ? (sp->constraint->restrictionList == NULL) : true)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   returnValue->end = ExpressionSize(sp->constraint->restrictionList) - 1;
   returnValue->value = EnvCreateMultifield(theEnv,(unsigned long) (returnValue->end + 1));
   i = 1;
   theExp = sp->constraint->restrictionList;
   while (theExp != NULL)
     {
      SetMFType(returnValue->value,i,theExp->type);
      SetMFValue(returnValue->value,i,theExp->value);
      theExp = theExp->nextArg;
      i++;
     }
  }

/*************************/
/* EnvSlotAllowedClasses */
/*************************/
void EnvSlotAllowedClasses(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   int i;
   SlotDescriptor *sp;
   EXPRESSION *theExp;

   if ((sp = SlotInfoSlot(theEnv,returnValue,theDefclass,sname,"slot-allowed-classes")) == NULL)
     return;
   if ((sp->constraint != NULL) ? (sp->constraint->classList == NULL) : true)
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
   returnValue->end = ExpressionSize(sp->constraint->classList) - 1;
   returnValue->value = EnvCreateMultifield(theEnv,(unsigned long) (returnValue->end + 1));
   i = 1;
   theExp = sp->constraint->classList;
   while (theExp != NULL)
     {
      SetMFType(returnValue->value,i,theExp->type);
      SetMFValue(returnValue->value,i,theExp->value);
      theExp = theExp->nextArg;
      i++;
     }
  }

/****************/
/* EnvSlotRange */
/****************/
void EnvSlotRange(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   SlotDescriptor *sp;

   if ((sp = SlotInfoSlot(theEnv,returnValue,theDefclass,sname,"slot-range")) == NULL)
     return;
   if ((sp->constraint == NULL) ? false :
       (sp->constraint->anyAllowed || sp->constraint->floatsAllowed ||
        sp->constraint->integersAllowed))
     {
      returnValue->end = 1;
      returnValue->value = EnvCreateMultifield(theEnv,2L);
      SetMFType(returnValue->value,1,sp->constraint->minValue->type);
      SetMFValue(returnValue->value,1,sp->constraint->minValue->value);
      SetMFType(returnValue->value,2,sp->constraint->maxValue->type);
      SetMFValue(returnValue->value,2,sp->constraint->maxValue->value);
     }
   else
     {
      returnValue->type = SYMBOL;
      returnValue->value = EnvFalseSymbol(theEnv);
      return;
     }
  }

/**********************/
/* EnvSlotCardinality */
/**********************/
void EnvSlotCardinality(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   SlotDescriptor *sp;

   if ((sp = SlotInfoSlot(theEnv,returnValue,theDefclass,sname,"slot-cardinality")) == NULL)
     return;
   if (sp->multiple == 0)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   returnValue->end = 1;
   returnValue->value = EnvCreateMultifield(theEnv,2L);
   if (sp->constraint != NULL)
     {
      SetMFType(returnValue->value,1,sp->constraint->minFields->type);
      SetMFValue(returnValue->value,1,sp->constraint->minFields->value);
      SetMFType(returnValue->value,2,sp->constraint->maxFields->type);
      SetMFValue(returnValue->value,2,sp->constraint->maxFields->value);
     }
   else
     {
      SetMFType(returnValue->value,1,INTEGER);
      SetMFValue(returnValue->value,1,SymbolData(theEnv)->Zero);
      SetMFType(returnValue->value,2,SYMBOL);
      SetMFValue(returnValue->value,2,SymbolData(theEnv)->PositiveInfinity);
     }
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*****************************************************
  NAME         : SlotInfoSupportFunction
  DESCRIPTION  : Support routine for slot-sources,
                   slot-facets, et. al.
  INPUTS       : 1) Data object buffer
                 2) Name of the H/L caller
                 3) Pointer to support function to call
  RETURNS      : Nothing useful
  SIDE EFFECTS : Support function called and data
                  object buffer set
  NOTES        : None
 *****************************************************/
static void SlotInfoSupportFunction(
  UDFContext *context,
  CLIPSValue *returnValue,
  const char *fnxname,
  void (*fnx)(Environment *,Defclass *,const char *,CLIPSValue *))
  {
   SYMBOL_HN *ssym;
   Defclass *cls;

   ssym = CheckClassAndSlot(context,fnxname,&cls);
   if (ssym == NULL)
     {
      EnvSetMultifieldErrorValue(context->environment,returnValue);
      return;
     }
   (*fnx)(context->environment,cls,ValueToString(ssym),returnValue);
  }

/*****************************************************************
  NAME         : CountSubclasses
  DESCRIPTION  : Counts the number of direct or indirect
                   subclasses for a class
  INPUTS       : 1) Address of class
                 2) Include (1) or exclude (0) indirect subclasses
                 3) Traversal id
  RETURNS      : The number of subclasses
  SIDE EFFECTS : None
  NOTES        : None
 *****************************************************************/
static unsigned CountSubclasses(
  Defclass *cls,
  bool inhp,
  int tvid)
  {
   long i,cnt;
   Defclass *subcls;

   for (cnt = 0 , i = 0 ; i < cls->directSubclasses.classCount ; i++)
     {
      subcls = cls->directSubclasses.classArray[i];
      if (TestTraversalID(subcls->traversalRecord,tvid) == 0)
        {
         cnt++;
         SetTraversalID(subcls->traversalRecord,tvid);
         if (inhp && (subcls->directSubclasses.classCount != 0))
           cnt += CountSubclasses(subcls,inhp,tvid);
        }
     }
   return(cnt);
  }

/*********************************************************************
  NAME         : StoreSubclasses
  DESCRIPTION  : Stores the names of direct or indirect
                   subclasses for a class in a mutlifield
  INPUTS       : 1) Caller's multifield buffer
                 2) Starting index
                 3) Address of the class
                 4) Include (1) or exclude (0) indirect subclasses
                 5) Traversal id
  RETURNS      : The number of subclass names stored in the multifield
  SIDE EFFECTS : Multifield set with subclass names
  NOTES        : Assumes multifield is big enough to hold subclasses
 *********************************************************************/
static unsigned StoreSubclasses(
  Multifield *mfval,
  unsigned si,
  Defclass *cls,
  int inhp,
  int tvid,
  bool storeName)
  {
   long i,classi;
   Defclass *subcls;

   for (i = si , classi = 0 ; classi < cls->directSubclasses.classCount ; classi++)
     {
      subcls = cls->directSubclasses.classArray[classi];
      if (TestTraversalID(subcls->traversalRecord,tvid) == 0)
        {
         SetTraversalID(subcls->traversalRecord,tvid);
         if (storeName)
           {
            SetMFType(mfval,i,SYMBOL);
            SetMFValue(mfval,i++,GetDefclassNamePointer(subcls));
           }
         else
           {
            SetMFType(mfval,i,DEFCLASS_PTR);
            SetMFValue(mfval,i++,subcls);
           }

         if (inhp && (subcls->directSubclasses.classCount != 0))
           i += StoreSubclasses(mfval,i,subcls,inhp,tvid,storeName);
        }
     }
   return(i - si);
  }

/*********************************************************
  NAME         : SlotInfoSlot
  DESCRIPTION  : Runtime support routine for slot-sources,
                   slot-facets, et. al. which looks up
                   a slot
  INPUTS       : 1) Data object buffer
                 2) Class pointer
                 3) Name-string of slot to find
                 4) The name of the calling function
  RETURNS      : Nothing useful
  SIDE EFFECTS : Support function called and data object
                  buffer initialized
  NOTES        : None
 *********************************************************/
static SlotDescriptor *SlotInfoSlot(
  Environment *theEnv,
  CLIPSValue *returnValue,
  Defclass *cls,
  const char *sname,
  const char *fnxname)
  {
   SYMBOL_HN *ssym;
   int i;

   if ((ssym = FindSymbolHN(theEnv,sname)) == NULL)
     {
      EnvSetEvaluationError(theEnv,true);
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return NULL;
     }
   i = FindInstanceTemplateSlot(theEnv,cls,ssym);
   if (i == -1)
     {
      SlotExistError(theEnv,sname,fnxname);
      EnvSetEvaluationError(theEnv,true);
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return NULL;
     }
   returnValue->type = MULTIFIELD;
   returnValue->begin = 0;
   return(cls->instanceTemplate[i]);
  }

#endif
