   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
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

   static void                    SlotInfoSupportFunction(Environment *,DATA_OBJECT *,const char *,void (*)(Environment *,Defclass *,const char *,DATA_OBJECT *));
   static unsigned                CountSubclasses(Defclass *,bool,int);
   static unsigned                StoreSubclasses(Multifield *,unsigned,Defclass *,int,int,bool);
   static SlotDescriptor         *SlotInfoSlot(Environment *,DATA_OBJECT *,Defclass *,const char *,const char *);

/*********************************************************************
  NAME         : ClassAbstractPCommand
  DESCRIPTION  : Determines if direct instances of a class can be made
  INPUTS       : None
  RETURNS      : True (1) if class is abstract, false (0) if concrete
  SIDE EFFECTS : None
  NOTES        : Syntax: (class-abstractp <class>)
 *********************************************************************/
bool ClassAbstractPCommand(
  Environment *theEnv)
  {
   DATA_OBJECT tmp;
   Defclass *cls;
   
   if (EnvArgTypeCheck(theEnv,"class-abstractp",1,SYMBOL,&tmp) == false)
     return false;
   cls = LookupDefclassByMdlOrScope(theEnv,DOToString(tmp));
   if (cls == NULL)
     {
      ClassExistError(theEnv,"class-abstractp",ValueToString(tmp.value));
      return false;
     }
   return EnvClassAbstractP(theEnv,cls);
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
bool ClassReactivePCommand(
  Environment *theEnv)
  {
   DATA_OBJECT tmp;
   Defclass *cls;
   
   if (EnvArgTypeCheck(theEnv,"class-reactivep",1,SYMBOL,&tmp) == false)
     return false;
   cls = LookupDefclassByMdlOrScope(theEnv,DOToString(tmp));
   if (cls == NULL)
     {
      ClassExistError(theEnv,"class-reactivep",ValueToString(tmp.value));
      return false;
     }
   return EnvClassReactiveP(theEnv,cls);
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
  Environment *theEnv,
  const char *fnx,
  bool *inhp)
  {
   Defclass *clsptr;
   DATA_OBJECT tmp;

   *inhp = false;
   if (EnvRtnArgCount(theEnv) == 0)
     {
      ExpectedCountError(theEnv,fnx,AT_LEAST,1);
      EnvSetEvaluationError(theEnv,true);
      return NULL;
     }
     
   if (EnvArgTypeCheck(theEnv,fnx,1,SYMBOL,&tmp) == false)
     { return NULL; }
     
   clsptr = LookupDefclassByMdlOrScope(theEnv,DOToString(tmp));
   if (clsptr == NULL)
     {
      ClassExistError(theEnv,fnx,ValueToString(tmp.value));
      return NULL;
     }
     
   if (EnvRtnArgCount(theEnv) == 2)
     {
      if (EnvArgTypeCheck(theEnv,fnx,2,SYMBOL,&tmp) == false)
        { return NULL; }
        
      if (strcmp(ValueToString(tmp.value),"inherit") == 0)
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
  DATA_OBJECT *result)
  {
   bool inhp;
   Defclass *clsptr;
   
   clsptr = ClassInfoFnxArgs(theEnv,"class-slots",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,result);
      return;
     }
   EnvClassSlots(theEnv,clsptr,result,inhp);
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
  DATA_OBJECT *result)
  {
   bool inhp;
   Defclass *clsptr;
   
   clsptr = ClassInfoFnxArgs(theEnv,"class-superclasses",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,result);
      return;
     }
   EnvClassSuperclasses(theEnv,clsptr,result,inhp);
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
  DATA_OBJECT *result)
  {
   bool inhp;
   Defclass *clsptr;
     
   clsptr = ClassInfoFnxArgs(theEnv,"class-subclasses",&inhp);
   if (clsptr == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,result);
      return;
     }
   EnvClassSubclasses(theEnv,clsptr,result,inhp);
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
  DATA_OBJECT *result)
  {
   bool inhp;
   Defclass *clsptr;
   
   if (EnvRtnArgCount(theEnv) == 0)
      EnvGetDefmessageHandlerList(theEnv,NULL,result,0);
   else
     {
      clsptr = ClassInfoFnxArgs(theEnv,"get-defmessage-handler-list",&inhp);
      if (clsptr == NULL)
        {
         EnvSetMultifieldErrorValue(theEnv,result);
         return;
        }
      EnvGetDefmessageHandlerList(theEnv,clsptr,result,inhp);
     }
  }

/*********************************
 Slot Information Access Functions
 *********************************/
void SlotFacetsCommand(
  Environment *theEnv,
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(theEnv,result,"slot-facets",EnvSlotFacets);
  }

void SlotSourcesCommand(
  Environment *theEnv,
  DATA_OBJECT *result)
  {   
   SlotInfoSupportFunction(theEnv,result,"slot-sources",EnvSlotSources);
  }

void SlotTypesCommand(
  Environment *theEnv,
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(theEnv,result,"slot-types",EnvSlotTypes);
  }

void SlotAllowedValuesCommand(
  Environment *theEnv,
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(theEnv,result,"slot-allowed-values",EnvSlotAllowedValues);
  }

void SlotAllowedClassesCommand(
  Environment *theEnv,
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(theEnv,result,"slot-allowed-classes",EnvSlotAllowedClasses);
  }

void SlotRangeCommand(
  Environment *theEnv,
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(theEnv,result,"slot-range",EnvSlotRange);
  }

void SlotCardinalityCommand(
  Environment *theEnv,
  DATA_OBJECT *result)
  {
   SlotInfoSupportFunction(theEnv,result,"slot-cardinality",EnvSlotCardinality);
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
  DATA_OBJECT *result,
  bool inhp)
  {
   long size;
   long i;

   size = inhp ? theDefclass->instanceSlotCount : theDefclass->slotCount;
   
   result->type = MULTIFIELD;
   SetpDOBegin(result,1);
   SetpDOEnd(result,size);
   result->value = EnvCreateMultifield(theEnv,size);
   
   if (size == 0)
     { return; }
   
   if (inhp)
     {
      for (i = 0 ; i < theDefclass->instanceSlotCount ; i++)
        {
         SetMFType(result->value,i+1,SYMBOL);
         SetMFValue(result->value,i+1,theDefclass->instanceTemplate[i]->slotName->name);
        }
     }
   else
     {
      for (i = 0 ; i < theDefclass->slotCount ; i++)
        {
         SetMFType(result->value,i+1,SYMBOL);
         SetMFValue(result->value,i+1,theDefclass->slots[i].slotName->name);
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
  DATA_OBJECT *result,
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
   result->type = MULTIFIELD;
   SetpDOBegin(result,1);
   SetpDOEnd(result,len);
   result->value = EnvCreateMultifield(theEnv,len);
   
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
            SetMFType(result->value,i,SYMBOL);
            SetMFValue(result->value,i++,GetDefclassNamePointer(supcls));
            SetMFType(result->value,i,SYMBOL);
            SetMFValue(result->value,i++,supcls->handlers[j].name);
            SetMFType(result->value,i,SYMBOL);
            SetMFValue(result->value,i++,EnvAddSymbol(theEnv,MessageHandlerData(theEnv)->hndquals[supcls->handlers[j].type]));
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
  DATA_OBJECT *result,
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
     
   result->type = MULTIFIELD;
   result->begin = 0;
   SetpDOEnd(result,plinks->classCount - offset);
   result->value = EnvCreateMultifield(theEnv,result->end + 1U);
   
   if (result->end == -1)
     { return; }
     
   for (i = offset , j = 1 ; i < plinks->classCount ; i++ , j++)
     {
      SetMFType(result->value,j,SYMBOL);
      SetMFValue(result->value,j,GetDefclassNamePointer(plinks->classArray[i]));
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
  DATA_OBJECT *result,
  bool inhp)
  {
   int i; // Bug fix 2014-07-18: Previously unsigned and SetpDOEnd decremented to -1.
   int id;

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }
     
   i = CountSubclasses(theDefclass,inhp,id);
   
   ReleaseTraversalID(theEnv);
   
   result->type = MULTIFIELD;
   result->begin = 0;
   SetpDOEnd(result,i);
   result->value = EnvCreateMultifield(theEnv,i);
   
   if (i == 0)
     { return; }
   
   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }
     
   StoreSubclasses(result->value,1,theDefclass,inhp,id,true);
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
  DATA_OBJECT *result,
  bool inhp)
  {
   int i; // Bug fix 2014-07-18: Previously unsigned and SetpDOEnd decremented to -1.
   int id;

   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }
     
   i = CountSubclasses(theDefclass,inhp,id);
   
   ReleaseTraversalID(theEnv);
   
   result->type = MULTIFIELD;
   result->begin = 0;
   SetpDOEnd(result,i);
   result->value = EnvCreateMultifield(theEnv,i);
   
   if (i == 0)
     { return; }
     
   if ((id = GetTraversalID(theEnv)) == -1)
     { return; }
     
   StoreSubclasses(result->value,1,theDefclass,inhp,id,false);
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
  DATA_OBJECT *result)
  {
   int i;
   SlotDescriptor *sp;

   if ((sp = SlotInfoSlot(theEnv,result,theDefclass,sname,"slot-facets")) == NULL)
     { return; }
     
#if DEFRULE_CONSTRUCT
   result->end = 9;
   result->value = EnvCreateMultifield(theEnv,10L);
   for (i = 1 ; i <= 10 ; i++)
     SetMFType(result->value,i,SYMBOL);
#else
   result->end = 8;
   result->value = EnvCreateMultifield(theEnv,9L);
   for (i = 1 ; i <= 9 ; i++)
     { SetMFType(result->value,i,SYMBOL); }
#endif

   if (sp->multiple)
     { SetMFValue(result->value,1,EnvAddSymbol(theEnv,"MLT")); }
   else
     { SetMFValue(result->value,1,EnvAddSymbol(theEnv,"SGL")); }

   if (sp->noDefault)
     SetMFValue(result->value,2,EnvAddSymbol(theEnv,"NIL"));
   else
     {
      if (sp->dynamicDefault)
        { SetMFValue(result->value,2,EnvAddSymbol(theEnv,"DYN")); }
      else
        { SetMFValue(result->value,2,EnvAddSymbol(theEnv,"STC")); }
     }
   
   if (sp->noInherit)    
     SetMFValue(result->value,3,EnvAddSymbol(theEnv,"NIL"));
   else
     SetMFValue(result->value,3,EnvAddSymbol(theEnv,"INH"));
   
   if (sp->initializeOnly)
     SetMFValue(result->value,4,EnvAddSymbol(theEnv,"INT"));
   else if (sp->noWrite)
     SetMFValue(result->value,4,EnvAddSymbol(theEnv,"R"));
   else
     SetMFValue(result->value,4,EnvAddSymbol(theEnv,"RW"));

   if (sp->shared)     
     SetMFValue(result->value,5,EnvAddSymbol(theEnv,"SHR"));
   else
     SetMFValue(result->value,5,EnvAddSymbol(theEnv,"LCL"));

#if DEFRULE_CONSTRUCT
   if (sp->reactive)   
     SetMFValue(result->value,6,EnvAddSymbol(theEnv,"RCT"));
   else
     SetMFValue(result->value,6,EnvAddSymbol(theEnv,"NIL"));
   
   if (sp->composite)
     SetMFValue(result->value,7,EnvAddSymbol(theEnv,"CMP"));
   else
     SetMFValue(result->value,7,EnvAddSymbol(theEnv,"EXC"));

   if (sp->publicVisibility)   
     SetMFValue(result->value,8,EnvAddSymbol(theEnv,"PUB"));
   else
     SetMFValue(result->value,8,EnvAddSymbol(theEnv,"PRV"));
   
   SetMFValue(result->value,9,EnvAddSymbol(theEnv,GetCreateAccessorString(sp)));
   SetMFValue(result->value,10,sp->noWrite ? EnvAddSymbol(theEnv,"NIL") : (void *) sp->overrideMessage);
#else
   if (sp->composite)
     SetMFValue(result->value,6,EnvAddSymbol(theEnv,"CMP"));
   else
     SetMFValue(result->value,6,EnvAddSymbol(theEnv,"EXC"));

   if (sp->publicVisibility)
     SetMFValue(result->value,7,EnvAddSymbol(theEnv,"PUB"));
   else
     SetMFValue(result->value,7,EnvAddSymbol(theEnv,"PRV"));

   SetMFValue(result->value,8,EnvAddSymbol(theEnv,GetCreateAccessorString(sp)));
   SetMFValue(result->value,9,sp->noWrite ? EnvAddSymbol(theEnv,"NIL") : (void *) sp->overrideMessage);
#endif
  }

/******************/
/* EnvSlotSources */
/******************/
void EnvSlotSources(
  Environment *theEnv,
  Defclass *theDefclass,
  const char *sname,
  DATA_OBJECT *result)
  {
   unsigned i;
   int classi;
   SlotDescriptor *sp, *csp;
   CLASS_LINK *ctop,*ctmp;
   Defclass *cls;

   if ((sp = SlotInfoSlot(theEnv,result,theDefclass,sname,"slot-sources")) == NULL)
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
   SetpDOEnd(result,i);
   result->value = EnvCreateMultifield(theEnv,i);
   for (ctmp = ctop , i = 1 ; ctmp != NULL ; ctmp = ctmp->nxt , i++)
     {
      SetMFType(result->value,i,SYMBOL);
      SetMFValue(result->value,i,GetDefclassNamePointer(ctmp->cls));
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
  DATA_OBJECT *result)
  {
   unsigned i,j;
   SlotDescriptor *sp;
   char typemap[2];
   unsigned msize;

   if ((sp = SlotInfoSlot(theEnv,result,theDefclass,sname,"slot-types")) == NULL)
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
   SetpDOEnd(result,msize);
   result->value = EnvCreateMultifield(theEnv,msize);
   i = 1;
   j = 0;
   while (i <= msize)
     {
      if (TestBitMap(typemap,j))
       {
        SetMFType(result->value,i,SYMBOL);
        SetMFValue(result->value,i,
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
  DATA_OBJECT *result)
  {
   int i;
   SlotDescriptor *sp;
   EXPRESSION *theExp;

   if ((sp = SlotInfoSlot(theEnv,result,theDefclass,sname,"slot-allowed-values")) == NULL)
     return;
   if ((sp->constraint != NULL) ? (sp->constraint->restrictionList == NULL) : true)
     {
      result->type = SYMBOL;
      result->value = EnvFalseSymbol(theEnv);
      return;
     }
   result->end = ExpressionSize(sp->constraint->restrictionList) - 1;
   result->value = EnvCreateMultifield(theEnv,(unsigned long) (result->end + 1));
   i = 1;
   theExp = sp->constraint->restrictionList;
   while (theExp != NULL)
     {
      SetMFType(result->value,i,theExp->type);
      SetMFValue(result->value,i,theExp->value);
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
  DATA_OBJECT *result)
  {
   int i;
   SlotDescriptor *sp;
   EXPRESSION *theExp;

   if ((sp = SlotInfoSlot(theEnv,result,theDefclass,sname,"slot-allowed-classes")) == NULL)
     return;
   if ((sp->constraint != NULL) ? (sp->constraint->classList == NULL) : true)
     {
      result->type = SYMBOL;
      result->value = EnvFalseSymbol(theEnv);
      return;
     }
   result->end = ExpressionSize(sp->constraint->classList) - 1;
   result->value = EnvCreateMultifield(theEnv,(unsigned long) (result->end + 1));
   i = 1;
   theExp = sp->constraint->classList;
   while (theExp != NULL)
     {
      SetMFType(result->value,i,theExp->type);
      SetMFValue(result->value,i,theExp->value);
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
  DATA_OBJECT *result)
  {
   SlotDescriptor *sp;

   if ((sp = SlotInfoSlot(theEnv,result,theDefclass,sname,"slot-range")) == NULL)
     return;
   if ((sp->constraint == NULL) ? false :
       (sp->constraint->anyAllowed || sp->constraint->floatsAllowed ||
        sp->constraint->integersAllowed))
     {
      result->end = 1;
      result->value = EnvCreateMultifield(theEnv,2L);
      SetMFType(result->value,1,sp->constraint->minValue->type);
      SetMFValue(result->value,1,sp->constraint->minValue->value);
      SetMFType(result->value,2,sp->constraint->maxValue->type);
      SetMFValue(result->value,2,sp->constraint->maxValue->value);
     }
   else
     {
      result->type = SYMBOL;
      result->value = EnvFalseSymbol(theEnv);
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
  DATA_OBJECT *result)
  {
   SlotDescriptor *sp;

   if ((sp = SlotInfoSlot(theEnv,result,theDefclass,sname,"slot-cardinality")) == NULL)
     return;
   if (sp->multiple == 0)
     {
      EnvSetMultifieldErrorValue(theEnv,result);
      return;
     }
   result->end = 1;
   result->value = EnvCreateMultifield(theEnv,2L);
   if (sp->constraint != NULL)
     {
      SetMFType(result->value,1,sp->constraint->minFields->type);
      SetMFValue(result->value,1,sp->constraint->minFields->value);
      SetMFType(result->value,2,sp->constraint->maxFields->type);
      SetMFValue(result->value,2,sp->constraint->maxFields->value);
     }
   else
     {
      SetMFType(result->value,1,INTEGER);
      SetMFValue(result->value,1,SymbolData(theEnv)->Zero);
      SetMFType(result->value,2,SYMBOL);
      SetMFValue(result->value,2,SymbolData(theEnv)->PositiveInfinity);
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
  Environment *theEnv,
  DATA_OBJECT *result,
  const char *fnxname,
  void (*fnx)(Environment *,Defclass *,const char *,DATA_OBJECT *))
  {
   SYMBOL_HN *ssym;
   Defclass *cls;

   ssym = CheckClassAndSlot(theEnv,fnxname,&cls);
   if (ssym == NULL)
     {
      EnvSetMultifieldErrorValue(theEnv,result);
      return;
     }
   (*fnx)(theEnv,cls,ValueToString(ssym),result);
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
  DATA_OBJECT *result,
  Defclass *cls,
  const char *sname,
  const char *fnxname)
  {
   SYMBOL_HN *ssym;
   int i;

   if ((ssym = FindSymbolHN(theEnv,sname)) == NULL)
     {
      EnvSetEvaluationError(theEnv,true);
      EnvSetMultifieldErrorValue(theEnv,result);
      return NULL;
     }
   i = FindInstanceTemplateSlot(theEnv,cls,ssym);
   if (i == -1)
     {
      SlotExistError(theEnv,sname,fnxname);
      EnvSetEvaluationError(theEnv,true);
      EnvSetMultifieldErrorValue(theEnv,result);
      return NULL;
     }
   result->type = MULTIFIELD;
   result->begin = 0;
   return(cls->instanceTemplate[i]);
  }

/*##################################*/
/* Additional Environment Functions */
/*##################################*/

#if ALLOW_ENVIRONMENT_GLOBALS

bool ClassAbstractP(
  Defclass *clsptr)
  {
   return EnvClassAbstractP(GetCurrentEnvironment(),clsptr);
  }

#if DEFRULE_CONSTRUCT
bool ClassReactiveP(
  Defclass *clsptr)
  {
   return EnvClassReactiveP(GetCurrentEnvironment(),clsptr);
  }
#endif

void ClassSlots(
  Defclass *clsptr,
  DATA_OBJECT *result,
  bool inhp)
  {
   EnvClassSlots(GetCurrentEnvironment(),clsptr,result,inhp);
  }

void ClassSubclasses(
  Defclass *clsptr,
  DATA_OBJECT *result,
  bool inhp)
  {
   EnvClassSubclasses(GetCurrentEnvironment(),clsptr,result,inhp);
  }

void ClassSuperclasses(
  Defclass *clsptr,
  DATA_OBJECT *result,
  bool inhp)
  {
   EnvClassSuperclasses(GetCurrentEnvironment(),clsptr,result,inhp);
  }

void SlotAllowedValues(
  Defclass *clsptr,
  const char *sname,
  DATA_OBJECT *result)
  {
   EnvSlotAllowedValues(GetCurrentEnvironment(),clsptr,sname,result);
  }

void SlotAllowedClasses(
  Defclass *clsptr,
  const char *sname,
  DATA_OBJECT *result)
  {
   EnvSlotAllowedClasses(GetCurrentEnvironment(),clsptr,sname,result);
  }

void SlotCardinality(
  Defclass *clsptr,
  const char *sname,
  DATA_OBJECT *result)
  {
   EnvSlotCardinality(GetCurrentEnvironment(),clsptr,sname,result);
  }

void SlotFacets(
  Defclass *clsptr,
  const char *sname,
  DATA_OBJECT *result)
  {
   EnvSlotFacets(GetCurrentEnvironment(),clsptr,sname,result);
  }

void SlotRange(
  Defclass *clsptr,
  const char *sname,
  DATA_OBJECT *result)
  {
   EnvSlotRange(GetCurrentEnvironment(),clsptr,sname,result);
  }

void SlotSources(
  Defclass *clsptr,
  const char *sname,
  DATA_OBJECT *result)
  {
   EnvSlotSources(GetCurrentEnvironment(),clsptr,sname,result);
  }

void SlotTypes(
  Defclass *clsptr,
  const char *sname,
  DATA_OBJECT *result)
  {
   EnvSlotTypes(GetCurrentEnvironment(),clsptr,sname,result);
  }

void GetDefmessageHandlerList(
  Defclass *clsptr,
  DATA_OBJECT *result,
  bool inhp)
  {
   EnvGetDefmessageHandlerList(GetCurrentEnvironment(),clsptr,result,inhp);
  }

#endif


#endif
