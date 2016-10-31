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

   static void                    SlotInfoSupportFunction(UDFContext *,UDFValue *,const char *,void (*)(Defclass *,const char *,CLIPSValue *));
   static unsigned                CountSubclasses(Defclass *,bool,int);
   static unsigned                StoreSubclasses(Multifield *,unsigned,Defclass *,int,int,bool);
   static SlotDescriptor         *SlotInfoSlot(Environment *,UDFValue *,Defclass *,const char *,const char *);

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
  UDFValue *returnValue)
  {
   UDFValue theArg;
   Defclass *cls;

   if (! UDFFirstArgument(context,SYMBOL_BIT,&theArg))
     { return; }

   cls = LookupDefclassByMdlOrScope(theEnv,theArg.lexemeValue->contents);
   if (cls == NULL)
     {
      ClassExistError(theEnv,"class-abstractp",theArg.lexemeValue->contents);
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,(ClassAbstractP(cls)));
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
  UDFValue *returnValue)
  {
   UDFValue theArg;
   Defclass *cls;

   if (! UDFFirstArgument(context,SYMBOL_BIT,&theArg))
     { return; }

   cls = LookupDefclassByMdlOrScope(theEnv,theArg.lexemeValue->contents);
   if (cls == NULL)
     {
      ClassExistError(theEnv,"class-reactivep",theArg.lexemeValue->contents);
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   returnValue->lexemeValue = EnvCreateBoolean(theEnv,ClassReactiveP(cls));
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
   UDFValue theArg;
   Environment *theEnv = context->environment;

   *inhp = false;

   if (! UDFFirstArgument(context,SYMBOL_BIT,&theArg))
     { return NULL; }

   clsptr = LookupDefclassByMdlOrScope(theEnv,theArg.lexemeValue->contents);
   if (clsptr == NULL)
     {
      ClassExistError(theEnv,fnx,theArg.lexemeValue->contents);
      return NULL;
     }

   if (UDFHasNextArgument(context))
     {
      if (! UDFNextArgument(context,SYMBOL_BIT,&theArg))
        { return NULL; }

      if (strcmp(theArg.lexemeValue->contents,"inherit") == 0)
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
  UDFValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;
   CLIPSValue result;

   clsptr = ClassInfoFnxArgs(context,"class-slots",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   ClassSlots(clsptr,&result,inhp);
   CLIPSToUDFValue(&result,returnValue);
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
  UDFValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;
   CLIPSValue result;

   clsptr = ClassInfoFnxArgs(context,"class-superclasses",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   ClassSuperclasses(clsptr,&result,inhp);
   CLIPSToUDFValue(&result,returnValue);
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
  UDFValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;
   CLIPSValue result;

   clsptr = ClassInfoFnxArgs(context,"class-subclasses",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   ClassSubclasses(clsptr,&result,inhp);
   CLIPSToUDFValue(&result,returnValue);
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
  UDFValue *returnValue)
  {
   bool inhp;
   Defclass *clsptr;
   CLIPSValue result;

   if (! UDFHasNextArgument(context))
     {
      EnvGetDefmessageHandlerList(theEnv,NULL,&result,false);
      CLIPSToUDFValue(&result,returnValue);
     }
   else
     {
      clsptr = ClassInfoFnxArgs(context,"get-defmessage-handler-list",&inhp);
      if (clsptr == NULL)
        {
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         return;
        }
        
      EnvGetDefmessageHandlerList(theEnv,clsptr,&result,inhp);
      CLIPSToUDFValue(&result,returnValue);
     }
  }

/*********************************
 Slot Information Access Functions
 *********************************/
void SlotFacetsCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-facets",SlotFacets);
  }

void SlotSourcesCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-sources",SlotSources);
  }

void SlotTypesCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-types",SlotTypes);
  }

void SlotAllowedValuesCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-allowed-values",SlotAllowedValues);
  }

void SlotAllowedClassesCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-allowed-classes",SlotAllowedClasses);
  }

void SlotRangeCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-range",SlotRange);
  }

void SlotCardinalityCommand(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   SlotInfoSupportFunction(context,returnValue,"slot-cardinality",SlotCardinality);
  }

/********************************************************************
  NAME         : ClassAbstractP
  DESCRIPTION  : Determines if a class is abstract or not
  INPUTS       : Generic pointer to class
  RETURNS      : 1 if class is abstract, 0 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
bool ClassAbstractP(
  Defclass *theDefclass)
  {
   return theDefclass->abstract;
  }

#if DEFRULE_CONSTRUCT

/********************************************************************
  NAME         : ClassReactiveP
  DESCRIPTION  : Determines if a class is reactive or not
  INPUTS       : Generic pointer to class
  RETURNS      : 1 if class is reactive, 0 otherwise
  SIDE EFFECTS : None
  NOTES        : None
 ********************************************************************/
bool ClassReactiveP(
  Defclass *theDefclass)
  {
   return theDefclass->reactive;
  }

#endif

/********************************************************************
  NAME         : ClassSlots
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
void ClassSlots(
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   long size;
   long i;
   Environment *theEnv = theDefclass->header.env;

   size = inhp ? theDefclass->instanceSlotCount : theDefclass->slotCount;

   returnValue->value = EnvCreateMultifield(theEnv,size);

   if (size == 0)
     { return; }

   if (inhp)
     {
      for (i = 0 ; i < theDefclass->instanceSlotCount ; i++)
        {
         returnValue->multifieldValue->theFields[i].value =
            theDefclass->instanceTemplate[i]->slotName->name;
        }
     }
   else
     {
      for (i = 0 ; i < theDefclass->slotCount ; i++)
        {
         returnValue->multifieldValue->theFields[i].value =
            theDefclass->slots[i].slotName->name;
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
           { i = sublen; }
         else
           { i = len - (supcls->handlerCount * 3) - sublen; }

         for (j = 0 ; j < supcls->handlerCount ; j++)
           {
            returnValue->multifieldValue->theFields[i++].value = GetDefclassNamePointer(supcls);
            returnValue->multifieldValue->theFields[i++].value = supcls->handlers[j].header.name;
            returnValue->multifieldValue->theFields[i++].value = EnvCreateSymbol(theEnv,MessageHandlerData(theEnv)->hndquals[supcls->handlers[j].type]);
           }

         sublen += supcls->handlerCount * 3;
        }
     }

   if (svcls != NULL)
     { SetNextDefclass(svcls,svnxt); }
  }

/***************************************************************************
  NAME         : ClassSuperclasses
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
void ClassSuperclasses(
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   PACKED_CLASS_LINKS *plinks;
   unsigned offset;
   long i,j;
   Environment *theEnv = theDefclass->header.env;
   
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

   returnValue->value = EnvCreateMultifield(theEnv,(plinks->classCount - offset));

   if (returnValue->multifieldValue->length == 0)
     { return; }

   for (i = offset , j = 0 ; i < plinks->classCount ; i++ , j++)
     {
      returnValue->multifieldValue->theFields[j].value = GetDefclassNamePointer(plinks->classArray[i]);
     }
  }

/**************************************************************************
  NAME         : ClassSubclasses
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
void ClassSubclasses(
  Defclass *theDefclass,
  CLIPSValue *returnValue,
  bool inhp)
  {
   int i; // Bug fix 2014-07-18: Previously unsigned and SetpDOEnd decremented to -1.
   int id;
   Environment *theEnv = theDefclass->header.env;

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   i = CountSubclasses(theDefclass,inhp,id);

   ReleaseTraversalID(theEnv);

   returnValue->value = EnvCreateMultifield(theEnv,i);

   if (i == 0)
     { return; }

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   StoreSubclasses(returnValue->multifieldValue,0,theDefclass,inhp,id,true);
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
  UDFValue *returnValue,
  bool inhp)
  {
   int i; // Bug fix 2014-07-18: Previously unsigned and SetpDOEnd decremented to -1.
   int id;

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   i = CountSubclasses(theDefclass,inhp,id);

   ReleaseTraversalID(theEnv);

   returnValue->begin = 0;
   returnValue->range = i;
   returnValue->value = EnvCreateMultifield(theEnv,i);

   if (i == 0)
     { return; }

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }

   StoreSubclasses(returnValue->multifieldValue,0,theDefclass,inhp,id,false);
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

/**************/
/* SlotFacets */
/**************/
void SlotFacets(
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   SlotDescriptor *sp;
   UDFValue result;
   Environment *theEnv = theDefclass->header.env;

   if ((sp = SlotInfoSlot(theEnv,&result,theDefclass,sname,"slot-facets")) == NULL)
     {
      NormalizeMultifield(theEnv,&result);
      returnValue->value = result.value;
      return;
     }

#if DEFRULE_CONSTRUCT
   returnValue->value = EnvCreateMultifield(theEnv,10L);
#else
   returnValue->value = EnvCreateMultifield(theEnv,9L);
#endif

   if (sp->multiple)
     { returnValue->multifieldValue->theFields[0].lexemeValue = EnvCreateSymbol(theEnv,"MLT"); }
   else
     { returnValue->multifieldValue->theFields[0].lexemeValue = EnvCreateSymbol(theEnv,"SGL"); }

   if (sp->noDefault)
     returnValue->multifieldValue->theFields[1].lexemeValue = EnvCreateSymbol(theEnv,"NIL");
   else
     {
      if (sp->dynamicDefault)
        { returnValue->multifieldValue->theFields[1].lexemeValue = EnvCreateSymbol(theEnv,"DYN"); }
      else
        { returnValue->multifieldValue->theFields[1].lexemeValue = EnvCreateSymbol(theEnv,"STC"); }
     }

   if (sp->noInherit)
     returnValue->multifieldValue->theFields[2].lexemeValue = EnvCreateSymbol(theEnv,"NIL");
   else
     returnValue->multifieldValue->theFields[2].lexemeValue = EnvCreateSymbol(theEnv,"INH");

   if (sp->initializeOnly)
     returnValue->multifieldValue->theFields[3].lexemeValue = EnvCreateSymbol(theEnv,"INT");
   else if (sp->noWrite)
     returnValue->multifieldValue->theFields[3].lexemeValue = EnvCreateSymbol(theEnv,"R");
   else
     returnValue->multifieldValue->theFields[3].lexemeValue = EnvCreateSymbol(theEnv,"RW");

   if (sp->shared)
     returnValue->multifieldValue->theFields[4].lexemeValue = EnvCreateSymbol(theEnv,"SHR");
   else
     returnValue->multifieldValue->theFields[4].lexemeValue = EnvCreateSymbol(theEnv,"LCL");

#if DEFRULE_CONSTRUCT
   if (sp->reactive)
     returnValue->multifieldValue->theFields[5].lexemeValue = EnvCreateSymbol(theEnv,"RCT");
   else
     returnValue->multifieldValue->theFields[5].lexemeValue = EnvCreateSymbol(theEnv,"NIL");

   if (sp->composite)
     returnValue->multifieldValue->theFields[6].lexemeValue = EnvCreateSymbol(theEnv,"CMP");
   else
     returnValue->multifieldValue->theFields[6].lexemeValue = EnvCreateSymbol(theEnv,"EXC");

   if (sp->publicVisibility)
     returnValue->multifieldValue->theFields[7].lexemeValue = EnvCreateSymbol(theEnv,"PUB");
   else
     returnValue->multifieldValue->theFields[7].lexemeValue = EnvCreateSymbol(theEnv,"PRV");

   returnValue->multifieldValue->theFields[8].lexemeValue = EnvCreateSymbol(theEnv,GetCreateAccessorString(sp));
   returnValue->multifieldValue->theFields[9].lexemeValue = (sp->noWrite ? EnvCreateSymbol(theEnv,"NIL") : sp->overrideMessage);
#else
   if (sp->composite)
     returnValue->multifieldValue->theFields[5].lexemeValue = EnvCreateSymbol(theEnv,"CMP");
   else
     returnValue->multifieldValue->theFields[5].lexemeValue = EnvCreateSymbol(theEnv,"EXC");

   if (sp->publicVisibility)
     returnValue->multifieldValue->theFields[6].lexemeValue = EnvCreateSymbol(theEnv,"PUB");
   else
     returnValue->multifieldValue->theFields[6].lexemeValue = EnvCreateSymbol(theEnv,"PRV"));

   returnValue->multifieldValue->theFields[7].lexemeValue = EnvCreateSymbol(theEnv,GetCreateAccessorString(sp));
   returnValue->multifieldValue->theFields[8].lexemeValue = (sp->noWrite ? EnvCreateSymbol(theEnv,"NIL") : sp->overrideMessage);
#endif
  }

/***************/
/* SlotSources */
/***************/
void SlotSources(
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   unsigned i;
   int classi;
   SlotDescriptor *sp, *csp;
   CLASS_LINK *ctop,*ctmp;
   Defclass *cls;
   UDFValue result;
   Environment *theEnv = theDefclass->header.env;

   if ((sp = SlotInfoSlot(theEnv,&result,theDefclass,sname,"slot-sources")) == NULL)
     {
      NormalizeMultifield(theEnv,&result);
      returnValue->value = result.value;
      return;
     }
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

   returnValue->value = EnvCreateMultifield(theEnv,i);
   for (ctmp = ctop , i = 0 ; ctmp != NULL ; ctmp = ctmp->nxt , i++)
     {
      returnValue->multifieldValue->theFields[i].value = GetDefclassNamePointer(ctmp->cls);
     }
   DeleteClassLinks(theEnv,ctop);
  }

/*************/
/* SlotTypes */
/*************/
void SlotTypes(
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   unsigned i,j;
   SlotDescriptor *sp;
   char typemap[2];
   unsigned msize;
   UDFValue result;
   Environment *theEnv = theDefclass->header.env;
   
   if ((sp = SlotInfoSlot(theEnv,&result,theDefclass,sname,"slot-types")) == NULL)
     {
      NormalizeMultifield(theEnv,&result);
      returnValue->value = result.value;
      return;
     }
     
   if ((sp->constraint != NULL) ? sp->constraint->anyAllowed : true)
     {
      typemap[0] = typemap[1] = (char) 0xFF;
      ClearBitMap(typemap,MULTIFIELD_TYPE);
      msize = 8;
     }
   else
     {
      typemap[0] = typemap[1] = (char) 0x00;
      msize = 0;
      if (sp->constraint->symbolsAllowed)
        {
         msize++;
         SetBitMap(typemap,SYMBOL_TYPE);
        }
      if (sp->constraint->stringsAllowed)
        {
         msize++;
         SetBitMap(typemap,STRING_TYPE);
        }
      if (sp->constraint->floatsAllowed)
        {
         msize++;
         SetBitMap(typemap,FLOAT_TYPE);
        }
      if (sp->constraint->integersAllowed)
        {
         msize++;
         SetBitMap(typemap,INTEGER_TYPE);
        }
      if (sp->constraint->instanceNamesAllowed)
        {
         msize++;
         SetBitMap(typemap,INSTANCE_NAME_TYPE);
        }
      if (sp->constraint->instanceAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,INSTANCE_ADDRESS_TYPE);
        }
      if (sp->constraint->externalAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,EXTERNAL_ADDRESS_TYPE);
        }
      if (sp->constraint->factAddressesAllowed)
        {
         msize++;
         SetBitMap(typemap,FACT_ADDRESS_TYPE);
        }
     }

   returnValue->value = EnvCreateMultifield(theEnv,msize);
   i = 0;
   j = 0;
   while (i < msize)
     {
      if (TestBitMap(typemap,j))
       {
        returnValue->multifieldValue->theFields[i].value =
                   GetDefclassNamePointer(DefclassData(theEnv)->PrimitiveClassMap[j]);
        i++;
       }
      j++;
     }
  }

/*********************/
/* SlotAllowedValues */
/*********************/
void SlotAllowedValues(
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   int i;
   SlotDescriptor *sp;
   Expression *theExp;
   UDFValue result;
   Environment *theEnv = theDefclass->header.env;

   if ((sp = SlotInfoSlot(theEnv,&result,theDefclass,sname,"slot-allowed-values")) == NULL)
     {
      NormalizeMultifield(theEnv,&result);
      returnValue->value = result.value;
      return;
     }

   if ((sp->constraint != NULL) ? (sp->constraint->restrictionList == NULL) : true)
     {
      returnValue->value = FalseSymbol(theEnv);
      return;
     }

   returnValue->value = EnvCreateMultifield(theEnv,(unsigned long) ExpressionSize(sp->constraint->restrictionList));
   i = 0;
   theExp = sp->constraint->restrictionList;
   while (theExp != NULL)
     {
      returnValue->multifieldValue->theFields[i].value = theExp->value;
      theExp = theExp->nextArg;
      i++;
     }
  }

/**********************/
/* SlotAllowedClasses */
/**********************/
void SlotAllowedClasses(
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   int i;
   SlotDescriptor *sp;
   Expression *theExp;
   UDFValue result;
   Environment *theEnv = theDefclass->header.env;

   if ((sp = SlotInfoSlot(theEnv,&result,theDefclass,sname,"slot-allowed-classes")) == NULL)
     {
      NormalizeMultifield(theEnv,&result);
      returnValue->value = result.value;
      return;
     }
   if ((sp->constraint != NULL) ? (sp->constraint->classList == NULL) : true)
     {
      returnValue->value = FalseSymbol(theEnv);
      return;
     }
   returnValue->value = EnvCreateMultifield(theEnv,(unsigned long) ExpressionSize(sp->constraint->classList));
   i = 0;
   theExp = sp->constraint->classList;
   while (theExp != NULL)
     {
      returnValue->multifieldValue->theFields[i].value = theExp->value;
      theExp = theExp->nextArg;
      i++;
     }
  }

/*************/
/* SlotRange */
/*************/
void SlotRange(
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   SlotDescriptor *sp;
   UDFValue result;
   Environment *theEnv = theDefclass->header.env;

   if ((sp = SlotInfoSlot(theEnv,&result,theDefclass,sname,"slot-range")) == NULL)
     {
      NormalizeMultifield(theEnv,&result);
      returnValue->value = result.value;
      return;
     }
   if ((sp->constraint == NULL) ? false :
       (sp->constraint->anyAllowed || sp->constraint->floatsAllowed ||
        sp->constraint->integersAllowed))
     {
      returnValue->value = EnvCreateMultifield(theEnv,2L);
      returnValue->multifieldValue->theFields[0].value = sp->constraint->minValue->value;
      returnValue->multifieldValue->theFields[1].value = sp->constraint->maxValue->value;
     }
   else
     {
      returnValue->value = FalseSymbol(theEnv);
      return;
     }
  }

/*******************/
/* SlotCardinality */
/*******************/
void SlotCardinality(
  Defclass *theDefclass,
  const char *sname,
  CLIPSValue *returnValue)
  {
   SlotDescriptor *sp;
   UDFValue result;
   Environment *theEnv = theDefclass->header.env;
     
   if ((sp = SlotInfoSlot(theEnv,&result,theDefclass,sname,"slot-cardinality")) == NULL)
     {
      NormalizeMultifield(theEnv,&result);
      returnValue->value = result.value;
      return;
     }
     
   if (sp->multiple == 0)
     {
      returnValue->multifieldValue = EnvCreateMultifield(theEnv,0L);
      return;
     }

   returnValue->value = EnvCreateMultifield(theEnv,2L);
   if (sp->constraint != NULL)
     {
      returnValue->multifieldValue->theFields[0].value = sp->constraint->minFields->value;
      returnValue->multifieldValue->theFields[1].value = sp->constraint->maxFields->value;
     }
   else
     {
      returnValue->multifieldValue->theFields[0].value = SymbolData(theEnv)->Zero;
      returnValue->multifieldValue->theFields[1].value = SymbolData(theEnv)->PositiveInfinity;
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
  UDFValue *returnValue,
  const char *fnxname,
  void (*fnx)(Defclass *,const char *,CLIPSValue *))
  {
   CLIPSLexeme *ssym;
   Defclass *cls;
   CLIPSValue result;

   ssym = CheckClassAndSlot(context,fnxname,&cls);
   if (ssym == NULL)
     {
      EnvSetMultifieldErrorValue(context->environment,returnValue);
      return;
     }
   (*fnx)(cls,ssym->contents,&result);
   CLIPSToUDFValue(&result,returnValue);
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
            mfval->theFields[i++].value = GetDefclassNamePointer(subcls);
           }
         else
           {
            mfval->theFields[i++].value = subcls;
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
  UDFValue *returnValue,
  Defclass *cls,
  const char *sname,
  const char *fnxname)
  {
   CLIPSLexeme *ssym;
   int i;

   if ((ssym = FindSymbolHN(theEnv,sname,SYMBOL_BIT)) == NULL)
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
   returnValue->begin = 0;
   return(cls->instanceTemplate[i]);
  }

#endif
