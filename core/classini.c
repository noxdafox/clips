   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/01/16             */
   /*                                                     */
   /*               CLASS INITIALIZATION MODULE           */
   /*******************************************************/

/**************************************************************/
/* Purpose: Defclass Initialization Routines                  */
/*                                                            */
/* Principal Programmer(s):                                   */
/*      Brian L. Dantes                                       */
/*                                                            */
/* Contributing Programmer(s):                                */
/*                                                            */
/* Revision History:                                          */
/*                                                            */
/*      6.23: Corrected compilation errors for files          */
/*            generated by constructs-to-c. DR0861            */
/*                                                            */
/*      6.24: Added allowed-classes slot facet.               */
/*                                                            */
/*            Converted INSTANCE_PATTERN_MATCHING to          */
/*            DEFRULE_CONSTRUCT.                              */
/*                                                            */
/*            Corrected code to remove run-time program       */
/*            compiler warning.                               */
/*                                                            */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior  */
/*            (MAC_MCW, IBM_MCW) are no longer supported.     */
/*                                                            */
/*            Changed integer type/precision.                 */
/*                                                            */
/*            Support for hashed alpha memories.              */
/*                                                            */
/*            Added const qualifiers to remove C++            */
/*            deprecation warnings.                           */
/*                                                            */
/*            Changed find construct functionality so that    */
/*            imported modules are search when locating a     */
/*            named construct.                                */
/*                                                            */
/*      6.40: Pragma once and other inclusion changes.        */
/*                                                            */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Removed initial-object support.                 */
/*                                                            */
/**************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if OBJECT_SYSTEM

#include <stdio.h>

#include "classcom.h"
#include "classexm.h"
#include "classfun.h"
#include "classinf.h"
#include "classpsr.h"
#include "cstrccom.h"
#include "cstrcpsr.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "inscom.h"
#include "memalloc.h"
#include "modulpsr.h"
#include "modulutl.h"
#include "msgcom.h"
#include "watch.h"

#if DEFINSTANCES_CONSTRUCT
#include "defins.h"
#endif

#if INSTANCE_SET_QUERIES
#include "insquery.h"
#endif

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
#include "bload.h"
#include "objbin.h"
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
#include "objcmp.h"
#endif

#if DEFRULE_CONSTRUCT
#include "objrtbld.h"
#include "objrtfnx.h"
#include "objrtmch.h"
#endif

#if RUN_TIME
#include "insfun.h"
#include "msgfun.h"
#include "pattern.h"
#endif

#include "classini.h"

/* =========================================
   *****************************************
                   CONSTANTS
   =========================================
   ***************************************** */
#define SUPERCLASS_RLN       "is-a"
#define NAME_RLN             "name"

/* =========================================
   *****************************************
      INTERNALLY VISIBLE FUNCTION HEADERS
   =========================================
   ***************************************** */

   static void                    SetupDefclasses(Environment *);
   static void                    DeallocateDefclassData(Environment *);

#if (! RUN_TIME)
   static void                    DestroyDefclassAction(Environment *,ConstructHeader *,void *);
   static Defclass               *AddSystemClass(Environment *,const char *,Defclass *);
   static void                   *AllocateModule(Environment *);
   static void                    ReturnModule(Environment *,void *);
#else
   static void                    SearchForHashedPatternNodes(Environment *,OBJECT_PATTERN_NODE *);
#endif

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT
   static void                    UpdateDefclassesScope(Environment *,void *);
#endif

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/**********************************************************
  NAME         : SetupObjectSystem
  DESCRIPTION  : Initializes all COOL constructs, functions,
                   and data structures
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : COOL initialized
  NOTES        : Order of setup calls is important
 **********************************************************/
void SetupObjectSystem(
  Environment *theEnv)
  {
   EntityRecord defclassEntityRecord = { "DEFCLASS_PTR", DEFCLASS_PTR,1,0,0,
                                              NULL,NULL,NULL,NULL,NULL,
                                              (EntityBusyCountFunction *) DecrementDefclassBusyCount,
                                              (EntityBusyCountFunction *) IncrementDefclassBusyCount,
                                              NULL,NULL,NULL,NULL,NULL };

   AllocateEnvironmentData(theEnv,DEFCLASS_DATA,sizeof(struct defclassData),NULL);
   AddEnvironmentCleanupFunction(theEnv,"defclasses",DeallocateDefclassData,-500);

   memcpy(&DefclassData(theEnv)->DefclassEntityRecord,&defclassEntityRecord,sizeof(struct entityRecord));

#if ! RUN_TIME
   DefclassData(theEnv)->ClassDefaultsModeValue = CONVENIENCE_MODE;
   DefclassData(theEnv)->ISA_SYMBOL = CreateSymbol(theEnv,SUPERCLASS_RLN);
   IncrementLexemeCount(DefclassData(theEnv)->ISA_SYMBOL);
   DefclassData(theEnv)->NAME_SYMBOL = CreateSymbol(theEnv,NAME_RLN);
   IncrementLexemeCount(DefclassData(theEnv)->NAME_SYMBOL);
#endif

   SetupDefclasses(theEnv);
   SetupInstances(theEnv);
   SetupMessageHandlers(theEnv);

#if DEFINSTANCES_CONSTRUCT
   SetupDefinstances(theEnv);
#endif

#if INSTANCE_SET_QUERIES
   SetupQuery(theEnv);
#endif

#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
   SetupObjectsBload(theEnv);
#endif

#if CONSTRUCT_COMPILER && (! RUN_TIME)
   SetupObjectsCompiler(theEnv);
#endif

#if DEFRULE_CONSTRUCT
   SetupObjectPatternStuff(theEnv);
#endif
  }

/***************************************************/
/* DeallocateDefclassData: Deallocates environment */
/*    data for the defclass construct.             */
/***************************************************/
static void DeallocateDefclassData(
  Environment *theEnv)
  {
#if ! RUN_TIME
   SLOT_NAME *tmpSNPPtr, *nextSNPPtr;
   int i;
   struct defclassModule *theModuleItem;
   Defmodule *theModule;
   bool bloaded = false;

#if BLOAD || BLOAD_AND_BSAVE
   if (Bloaded(theEnv)) bloaded = true;
#endif

   /*=============================*/
   /* Destroy all the defclasses. */
   /*=============================*/

   if (! bloaded)
     {
      DoForAllConstructs(theEnv,DestroyDefclassAction,DefclassData(theEnv)->DefclassModuleIndex,false,NULL);

      for (theModule = GetNextDefmodule(theEnv,NULL);
           theModule != NULL;
           theModule = GetNextDefmodule(theEnv,theModule))
        {
         theModuleItem = (struct defclassModule *)
                         GetModuleItem(theEnv,theModule,
                                       DefclassData(theEnv)->DefclassModuleIndex);
         rtn_struct(theEnv,defclassModule,theModuleItem);
        }
     }

   /*==========================*/
   /* Remove the class tables. */
   /*==========================*/

   if (! bloaded)
     {
      if (DefclassData(theEnv)->ClassIDMap != NULL)
        {
         genfree(theEnv,DefclassData(theEnv)->ClassIDMap,DefclassData(theEnv)->AvailClassID * sizeof(Defclass *));
        }
     }

   if (DefclassData(theEnv)->ClassTable != NULL)
     {
      genfree(theEnv,DefclassData(theEnv)->ClassTable,sizeof(Defclass *) * CLASS_TABLE_HASH_SIZE);
     }

   /*==============================*/
   /* Free up the slot name table. */
   /*==============================*/

   if (! bloaded)
     {
      for (i = 0; i < SLOT_NAME_TABLE_HASH_SIZE; i++)
        {
         tmpSNPPtr = DefclassData(theEnv)->SlotNameTable[i];

         while (tmpSNPPtr != NULL)
           {
            nextSNPPtr = tmpSNPPtr->nxt;
            rtn_struct(theEnv,slotName,tmpSNPPtr);
            tmpSNPPtr = nextSNPPtr;
           }
        }
     }

   if (DefclassData(theEnv)->SlotNameTable != NULL)
     {
      genfree(theEnv,DefclassData(theEnv)->SlotNameTable,sizeof(SLOT_NAME *) * SLOT_NAME_TABLE_HASH_SIZE);
     }
#else
   Defclass *cls;
   void *tmpexp;
   unsigned int i;
   int j;

   if (DefclassData(theEnv)->ClassTable != NULL)
     {
      for (j = 0 ; j < CLASS_TABLE_HASH_SIZE ; j++)
        for (cls = DefclassData(theEnv)->ClassTable[j] ; cls != NULL ; cls = cls->nxtHash)
          {
           for (i = 0 ; i < cls->slotCount ; i++)
             {
              if ((cls->slots[i].defaultValue != NULL) && (cls->slots[i].dynamicDefault == 0))
                {
                 tmpexp = ((UDFValue *) cls->slots[i].defaultValue)->supplementalInfo;
                 rtn_struct(theEnv,udfValue,cls->slots[i].defaultValue);
                 cls->slots[i].defaultValue = tmpexp;
                }
             }
          }
     }
#endif
  }

#if ! RUN_TIME
/*********************************************************/
/* DestroyDefclassAction: Action used to remove defclass */
/*   as a result of DestroyEnvironment.                  */
/*********************************************************/
static void DestroyDefclassAction(
  Environment *theEnv,
  ConstructHeader *theConstruct,
  void *buffer)
  {
#if MAC_XCD
#pragma unused(buffer)
#endif
   Defclass *theDefclass = (Defclass *) theConstruct;

   if (theDefclass == NULL) return;

#if (! BLOAD_ONLY)
   DestroyDefclass(theEnv,theDefclass);
#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif
  }
#endif

#if RUN_TIME

/***************************************************
  NAME         : ObjectsRunTimeInitialize
  DESCRIPTION  : Initializes objects system lists
                   in a run-time module
  INPUTS       : 1) Pointer to new class hash table
                 2) Pointer to new slot name table
  RETURNS      : Nothing useful
  SIDE EFFECTS : Global pointers set
  NOTES        : None
 ***************************************************/
void ObjectsRunTimeInitialize(
  Environment *theEnv,
  Defclass *ctable[],
  SLOT_NAME *sntable[],
  Defclass **cidmap,
  unsigned mid)
  {
   Defclass *cls;
   void *tmpexp;
   unsigned int i,j;

   if (DefclassData(theEnv)->ClassTable != NULL)
     {
      for (j = 0 ; j < CLASS_TABLE_HASH_SIZE ; j++)
        for (cls = DefclassData(theEnv)->ClassTable[j] ; cls != NULL ; cls = cls->nxtHash)
          {
           cls->header.env = theEnv;
           
           for (i = 0 ; i < cls->slotCount ; i++)
             {
              /* =====================================================================
                 For static default values, the data object value needs to deinstalled
                 and deallocated, and the expression needs to be restored (which was
                 temporarily stored in the supplementalInfo field of the data object)
                 ===================================================================== */
              if ((cls->slots[i].defaultValue != NULL) && (cls->slots[i].dynamicDefault == 0))
                {
                 tmpexp = ((UDFValue *) cls->slots[i].defaultValue)->supplementalInfo;
                 DecrementUDFValueReferenceCount(theEnv,(UDFValue *) cls->slots[i].defaultValue);
                 rtn_struct(theEnv,udfValue,cls->slots[i].defaultValue);
                 cls->slots[i].defaultValue = tmpexp;
                }
             }

           for (i = 0; i < cls->handlerCount; i++)
             { cls->handlers[i].header.env = theEnv; }
          }
     }

   InstanceQueryData(theEnv)->QUERY_DELIMITER_SYMBOL = FindSymbolHN(theEnv,QUERY_DELIMITER_STRING,SYMBOL_BIT);
   MessageHandlerData(theEnv)->INIT_SYMBOL = FindSymbolHN(theEnv,INIT_STRING,SYMBOL_BIT);
   MessageHandlerData(theEnv)->DELETE_SYMBOL = FindSymbolHN(theEnv,DELETE_STRING,SYMBOL_BIT);
   MessageHandlerData(theEnv)->CREATE_SYMBOL = FindSymbolHN(theEnv,CREATE_STRING,SYMBOL_BIT);
   DefclassData(theEnv)->ISA_SYMBOL = FindSymbolHN(theEnv,SUPERCLASS_RLN,SYMBOL_BIT);
   DefclassData(theEnv)->NAME_SYMBOL = FindSymbolHN(theEnv,NAME_RLN,SYMBOL_BIT);

   DefclassData(theEnv)->ClassTable = (Defclass **) ctable;
   DefclassData(theEnv)->SlotNameTable = (SLOT_NAME **) sntable;
   DefclassData(theEnv)->ClassIDMap = (Defclass **) cidmap;
   DefclassData(theEnv)->MaxClassID = (unsigned short) mid;
   DefclassData(theEnv)->PrimitiveClassMap[FLOAT_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,FLOAT_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[INTEGER_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,INTEGER_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[STRING_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,STRING_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[SYMBOL_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,SYMBOL_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[MULTIFIELD_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,MULTIFIELD_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[EXTERNAL_ADDRESS_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,EXTERNAL_ADDRESS_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[FACT_ADDRESS_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,FACT_ADDRESS_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_NAME_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,INSTANCE_NAME_TYPE_NAME);
   DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE] =
     LookupDefclassByMdlOrScope(theEnv,INSTANCE_ADDRESS_TYPE_NAME);

   for (j = 0 ; j < CLASS_TABLE_HASH_SIZE ; j++)
     for (cls = DefclassData(theEnv)->ClassTable[j] ; cls != NULL ; cls = cls->nxtHash)
     {
      for (i = 0 ; i < cls->slotCount ; i++)
        {
         if ((cls->slots[i].defaultValue != NULL) && (cls->slots[i].dynamicDefault == 0))
           {
            tmpexp = cls->slots[i].defaultValue;
            cls->slots[i].defaultValue = get_struct(theEnv,udfValue);
            EvaluateAndStoreInDataObject(theEnv,(int) cls->slots[i].multiple,(Expression *) tmpexp,
                                         (UDFValue *) cls->slots[i].defaultValue,true);
            IncrementUDFValueReferenceCount(theEnv,(UDFValue *) cls->slots[i].defaultValue);
            ((UDFValue *) cls->slots[i].defaultValue)->supplementalInfo = tmpexp;
           }
        }
     }

   SearchForHashedPatternNodes(theEnv,ObjectReteData(theEnv)->ObjectPatternNetworkPointer);
  }

/********************************/
/* SearchForHashedPatternNodes: */
/********************************/
static void SearchForHashedPatternNodes(
   Environment *theEnv,
   OBJECT_PATTERN_NODE *theNode)
   {
    while (theNode != NULL)
      {
       if ((theNode->lastLevel != NULL) && (theNode->lastLevel->selector))
        { AddHashedPatternNode(theEnv,theNode->lastLevel,theNode,theNode->networkTest->type,theNode->networkTest->value); }

       SearchForHashedPatternNodes(theEnv,theNode->nextLevel);

       theNode = theNode->rightNode;
      }
   }

#else

/***************************************************************
  NAME         : CreateSystemClasses
  DESCRIPTION  : Creates the built-in system classes
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : System classes inserted in the
                   class hash table
  NOTES        : The binary/load save indices for the primitive
                   types (integer, float, symbol and string,
                   multifield, external-address and fact-address)
                   are very important.  Need to be able to refer
                   to types with the same index regardless of
                   whether the object system is installed or
                   not.  Thus, the bsave/blaod indices of these
                   classes match their integer codes.
                WARNING!!: Assumes no classes exist yet!
 ***************************************************************/
void CreateSystemClasses(
  Environment *theEnv,
  void *context)
  {
   Defclass *user,*any,*primitive,*number,*lexeme,*address,*instance;

   /* ===================================
      Add canonical slot name entries for
      the is-a and name fields - used for
      object patterns
      =================================== */
   AddSlotName(theEnv,DefclassData(theEnv)->ISA_SYMBOL,ISA_ID,true);
   AddSlotName(theEnv,DefclassData(theEnv)->NAME_SYMBOL,NAME_ID,true);

   /* =========================================================
      Bsave Indices for non-primitive classes start at 9
               Object is 9, Primitive is 10, Number is 11,
               Lexeme is 12, Address is 13, and Instance is 14.
      because: float = 0, integer = 1, symbol = 2, string = 3,
               multifield = 4, and external-address = 5 and
               fact-address = 6, instance-adress = 7 and
               instance-name = 8.
      ========================================================= */
   any = AddSystemClass(theEnv,OBJECT_TYPE_NAME,NULL);
   primitive = AddSystemClass(theEnv,PRIMITIVE_TYPE_NAME,any);
   user = AddSystemClass(theEnv,USER_TYPE_NAME,any);

   number = AddSystemClass(theEnv,NUMBER_TYPE_NAME,primitive);
   DefclassData(theEnv)->PrimitiveClassMap[INTEGER_TYPE] = AddSystemClass(theEnv,INTEGER_TYPE_NAME,number);
   DefclassData(theEnv)->PrimitiveClassMap[FLOAT_TYPE] = AddSystemClass(theEnv,FLOAT_TYPE_NAME,number);
   lexeme = AddSystemClass(theEnv,LEXEME_TYPE_NAME,primitive);
   DefclassData(theEnv)->PrimitiveClassMap[SYMBOL_TYPE] = AddSystemClass(theEnv,SYMBOL_TYPE_NAME,lexeme);
   DefclassData(theEnv)->PrimitiveClassMap[STRING_TYPE] = AddSystemClass(theEnv,STRING_TYPE_NAME,lexeme);
   DefclassData(theEnv)->PrimitiveClassMap[MULTIFIELD_TYPE] = AddSystemClass(theEnv,MULTIFIELD_TYPE_NAME,primitive);
   address = AddSystemClass(theEnv,ADDRESS_TYPE_NAME,primitive);
   DefclassData(theEnv)->PrimitiveClassMap[EXTERNAL_ADDRESS_TYPE] = AddSystemClass(theEnv,EXTERNAL_ADDRESS_TYPE_NAME,address);
   DefclassData(theEnv)->PrimitiveClassMap[FACT_ADDRESS_TYPE] = AddSystemClass(theEnv,FACT_ADDRESS_TYPE_NAME,address);
   instance = AddSystemClass(theEnv,INSTANCE_TYPE_NAME,primitive);
   DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE] = AddSystemClass(theEnv,INSTANCE_ADDRESS_TYPE_NAME,instance);
   DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_NAME_TYPE] = AddSystemClass(theEnv,INSTANCE_NAME_TYPE_NAME,instance);

   /* ================================================================================
       INSTANCE-ADDRESS is-a INSTANCE and ADDRESS.  The links between INSTANCE-ADDRESS
       and ADDRESS still need to be made.
       =============================================================================== */
   AddClassLink(theEnv,&DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE]->directSuperclasses,address,-1);
   AddClassLink(theEnv,&DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE]->allSuperclasses,address,2);
   AddClassLink(theEnv,&address->directSubclasses,DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE],-1);

   /* =======================================================================
      The order of the class in the list MUST correspond to their type codes!
      See CONSTANT.H
      ======================================================================= */
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[FLOAT_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[INTEGER_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[SYMBOL_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[STRING_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[MULTIFIELD_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[EXTERNAL_ADDRESS_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[FACT_ADDRESS_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_ADDRESS_TYPE]->header);
   AddConstructToModule(&DefclassData(theEnv)->PrimitiveClassMap[INSTANCE_NAME_TYPE]->header);
   AddConstructToModule(&any->header);
   AddConstructToModule(&primitive->header);
   AddConstructToModule(&number->header);
   AddConstructToModule(&lexeme->header);
   AddConstructToModule(&address->header);
   AddConstructToModule(&instance->header);
   AddConstructToModule(&user->header);

   for (any = GetNextDefclass(theEnv,NULL) ;
        any != NULL ;
        any = GetNextDefclass(theEnv,any))
     AssignClassID(theEnv,any);
  }

#endif

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/*********************************************************
  NAME         : SetupDefclasses
  DESCRIPTION  : Initializes Class Hash Table,
                   Function Parsers, and Data Structures
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS :
  NOTES        : None
 *********************************************************/
static void SetupDefclasses(
  Environment *theEnv)
  {
   InstallPrimitive(theEnv,&DefclassData(theEnv)->DefclassEntityRecord,DEFCLASS_PTR);

   DefclassData(theEnv)->DefclassModuleIndex =
                RegisterModuleItem(theEnv,"defclass",
#if (! RUN_TIME)
                                    AllocateModule,
                                    ReturnModule,
#else
                                    NULL,NULL,
#endif
#if BLOAD_AND_BSAVE || BLOAD || BLOAD_ONLY
                                    BloadDefclassModuleReference,
#else
                                    NULL,
#endif
#if CONSTRUCT_COMPILER && (! RUN_TIME)
                                    DefclassCModuleReference,
#else
                                    NULL,
#endif
                                    (FindConstructFunction *) FindDefclassInModule);

   DefclassData(theEnv)->DefclassConstruct =  AddConstruct(theEnv,"defclass","defclasses",
#if (! BLOAD_ONLY) && (! RUN_TIME)
                                     ParseDefclass,
#else
                                     NULL,
#endif
                                     (FindConstructFunction *) FindDefclass,
                                     GetConstructNamePointer,GetConstructPPForm,
                                     GetConstructModuleItem,
                                     (GetNextConstructFunction *) GetNextDefclass,
                                     SetNextConstruct,
                                     (IsConstructDeletableFunction *) DefclassIsDeletable,
                                     (DeleteConstructFunction *) Undefclass,
#if (! RUN_TIME)
                                     (FreeConstructFunction *) RemoveDefclass
#else
                                     NULL
#endif
                                     );

   AddClearReadyFunction(theEnv,"defclass",InstancesPurge,0,NULL);

#if ! RUN_TIME
   AddClearFunction(theEnv,"defclass",CreateSystemClasses,0,NULL);
   InitializeClasses(theEnv);

#if ! BLOAD_ONLY
#if DEFMODULE_CONSTRUCT
   AddPortConstructItem(theEnv,"defclass",SYMBOL_TOKEN);
   AddAfterModuleDefinedFunction(theEnv,"defclass",UpdateDefclassesScope,0,NULL);
#endif
   AddUDF(theEnv,"undefclass","v",1,1,"y",UndefclassCommand,"UndefclassCommand",NULL);

   AddSaveFunction(theEnv,"defclass",SaveDefclasses,10,NULL);
#endif

#if DEBUGGING_FUNCTIONS
   AddUDF(theEnv,"list-defclasses","v",0,1,"y",ListDefclassesCommand,"ListDefclassesCommand",NULL);
   AddUDF(theEnv,"ppdefclass","v",1,1,"y",PPDefclassCommand,"PPDefclassCommand",NULL);
   AddUDF(theEnv,"describe-class","v",1,1,"y",DescribeClassCommand,"DescribeClassCommand",NULL);
   AddUDF(theEnv,"browse-classes","v",0,1,"y",BrowseClassesCommand,"BrowseClassesCommand",NULL);
#endif

   AddUDF(theEnv,"get-defclass-list","m",0,1,"y",GetDefclassListFunction,"GetDefclassListFunction",NULL);
   AddUDF(theEnv,"superclassp","b",2,2,"y",SuperclassPCommand,"SuperclassPCommand",NULL);
   AddUDF(theEnv,"subclassp","b",2,2,"y",SubclassPCommand,"SubclassPCommand",NULL);
   AddUDF(theEnv,"class-existp","b", 1,1,"y",ClassExistPCommand,"ClassExistPCommand",NULL);
   AddUDF(theEnv,"message-handler-existp","b",2,3,"y",MessageHandlerExistPCommand,"MessageHandlerExistPCommand",NULL);
   AddUDF(theEnv,"class-abstractp","b",1,1,"y",ClassAbstractPCommand,"ClassAbstractPCommand",NULL);
#if DEFRULE_CONSTRUCT
   AddUDF(theEnv,"class-reactivep","b",1,1,"y",ClassReactivePCommand,"ClassReactivePCommand",NULL);
#endif
   AddUDF(theEnv,"class-slots","m",1,2,"y",ClassSlotsCommand,"ClassSlotsCommand",NULL);
   AddUDF(theEnv,"class-superclasses","m",1,2,"y",ClassSuperclassesCommand,"ClassSuperclassesCommand",NULL);
   AddUDF(theEnv,"class-subclasses","m",1,2,"y",ClassSubclassesCommand,"ClassSubclassesCommand",NULL);
   AddUDF(theEnv,"get-defmessage-handler-list","m",0,2,"y",GetDefmessageHandlersListCmd,"GetDefmessageHandlersListCmd",NULL);
   AddUDF(theEnv,"slot-existp","b",2,3,"y",SlotExistPCommand,"SlotExistPCommand",NULL);
   AddUDF(theEnv,"slot-facets","m",2,2,"y",SlotFacetsCommand,"SlotFacetsCommand",NULL);
   AddUDF(theEnv,"slot-sources","m",2,2,"y",SlotSourcesCommand,"SlotSourcesCommand",NULL);
   AddUDF(theEnv,"slot-types","m",2,2,"y",SlotTypesCommand,"SlotTypesCommand",NULL);
   AddUDF(theEnv,"slot-allowed-values","m",2,2,"y",SlotAllowedValuesCommand,"SlotAllowedValuesCommand",NULL);
   AddUDF(theEnv,"slot-allowed-classes","m",2,2,"y",SlotAllowedClassesCommand,"SlotAllowedClassesCommand",NULL);
   AddUDF(theEnv,"slot-range","m",2,2,"y",SlotRangeCommand,"SlotRangeCommand",NULL);
   AddUDF(theEnv,"slot-cardinality","m",2,2,"y",SlotCardinalityCommand,"SlotCardinalityCommand",NULL);
   AddUDF(theEnv,"slot-writablep","b",2,2,"y",SlotWritablePCommand,"SlotWritablePCommand",NULL);
   AddUDF(theEnv,"slot-initablep","b",2,2,"y",SlotInitablePCommand,"SlotInitablePCommand",NULL);
   AddUDF(theEnv,"slot-publicp","b",2,2,"y",SlotPublicPCommand,"SlotPublicPCommand",NULL);
   AddUDF(theEnv,"slot-direct-accessp","b",2,2,"y",SlotDirectAccessPCommand,"SlotDirectAccessPCommand",NULL);
   AddUDF(theEnv,"slot-default-value","*",2,2,"y",SlotDefaultValueCommand,"SlotDefaultValueCommand",NULL);
   AddUDF(theEnv,"defclass-module","y",1,1,"y",GetDefclassModuleCommand,"GetDefclassModuleCommand",NULL);
   AddUDF(theEnv,"get-class-defaults-mode","y",0,0,NULL,GetClassDefaultsModeCommand,"GetClassDefaultsModeCommand",NULL);
   AddUDF(theEnv,"set-class-defaults-mode","y",1,1,"y",SetClassDefaultsModeCommand,"SetClassDefaultsModeCommand",NULL);
#endif

#if DEBUGGING_FUNCTIONS
   AddWatchItem(theEnv,"instances",0,&DefclassData(theEnv)->WatchInstances,75,DefclassWatchAccess,DefclassWatchPrint);
   AddWatchItem(theEnv,"slots",1,&DefclassData(theEnv)->WatchSlots,74,DefclassWatchAccess,DefclassWatchPrint);
#endif
  }

#if (! RUN_TIME)

/*********************************************************
  NAME         : AddSystemClass
  DESCRIPTION  : Performs all necessary allocations
                   for adding a system class
  INPUTS       : 1) The name-string of the system class
                 2) The address of the parent class
                    (NULL if none)
  RETURNS      : The address of the new system class
  SIDE EFFECTS : Allocations performed
  NOTES        : Assumes system-class name is unique
                 Also assumes SINGLE INHERITANCE for
                   system classes to simplify precedence
                   list determination
                 Adds classes to has table but NOT to
                  class list (this is responsibility
                  of caller)
 *********************************************************/
static Defclass *AddSystemClass(
  Environment *theEnv,
  const char *name,
  Defclass *parent)
  {
   Defclass *sys;
   long i;
   char defaultScopeMap[1];

   sys = NewClass(theEnv,CreateSymbol(theEnv,name));
   sys->abstract = 1;
#if DEFRULE_CONSTRUCT
   sys->reactive = 0;
#endif
   IncrementLexemeCount(sys->header.name);
   sys->installed = 1;
   sys->system = 1;
   sys->hashTableIndex = HashClass(sys->header.name);

   AddClassLink(theEnv,&sys->allSuperclasses,sys,-1);
   if (parent != NULL)
     {
      AddClassLink(theEnv,&sys->directSuperclasses,parent,-1);
      AddClassLink(theEnv,&parent->directSubclasses,sys,-1);
      AddClassLink(theEnv,&sys->allSuperclasses,parent,-1);
      for (i = 1 ; i < parent->allSuperclasses.classCount ; i++)
        AddClassLink(theEnv,&sys->allSuperclasses,parent->allSuperclasses.classArray[i],-1);
     }
   sys->nxtHash = DefclassData(theEnv)->ClassTable[sys->hashTableIndex];
   DefclassData(theEnv)->ClassTable[sys->hashTableIndex] = sys;

   /* =========================================
      Add default scope maps for a system class
      There is only one module (MAIN) so far -
      which has an id of 0
      ========================================= */
   ClearBitString(defaultScopeMap,(int) sizeof(char));
   SetBitMap(defaultScopeMap,0);
#if DEFMODULE_CONSTRUCT
   sys->scopeMap = (CLIPSBitMap *) AddBitMap(theEnv,defaultScopeMap,(int) sizeof(char));
   IncrementBitMapCount(sys->scopeMap);
#endif
   return(sys);
  }

/*****************************************************
  NAME         : AllocateModule
  DESCRIPTION  : Creates and initializes a
                 list of defclasses for a new module
  INPUTS       : None
  RETURNS      : The new defclass module
  SIDE EFFECTS : Defclass module created
  NOTES        : None
 *****************************************************/
static void *AllocateModule(
  Environment *theEnv)
  {
   return (void *) get_struct(theEnv,defclassModule);
  }

/***************************************************
  NAME         : ReturnModule
  DESCRIPTION  : Removes a defclass module and
                 all associated defclasses
  INPUTS       : The defclass module
  RETURNS      : Nothing useful
  SIDE EFFECTS : Module and defclasses deleted
  NOTES        : None
 ***************************************************/
static void ReturnModule(
  Environment *theEnv,
  void *theItem)
  {
   FreeConstructHeaderModule(theEnv,(struct defmoduleItemHeader *) theItem,DefclassData(theEnv)->DefclassConstruct);
   DeleteSlotName(theEnv,FindIDSlotNameHash(theEnv,ISA_ID));
   DeleteSlotName(theEnv,FindIDSlotNameHash(theEnv,NAME_ID));
   rtn_struct(theEnv,defclassModule,theItem);
  }

#endif

#if (! BLOAD_ONLY) && (! RUN_TIME) && DEFMODULE_CONSTRUCT

/***************************************************
  NAME         : UpdateDefclassesScope
  DESCRIPTION  : This function updates the scope
                 bitmaps for existing classes when
                 a new module is defined
  INPUTS       : None
  RETURNS      : Nothing
  SIDE EFFECTS : Class scope bitmaps are updated
  NOTES        : None
 ***************************************************/
static void UpdateDefclassesScope(
  Environment *theEnv,
  void *context)
  {
   unsigned i;
   Defclass *theDefclass;
   int newModuleID,count;
   char *newScopeMap;
   unsigned newScopeMapSize;
   const char *className;
   Defmodule *matchModule;

   newModuleID = (int) GetCurrentModule(theEnv)->header.bsaveID;
   newScopeMapSize = (sizeof(char) * ((GetNumberOfDefmodules(theEnv) / BITS_PER_BYTE) + 1));
   newScopeMap = (char *) gm2(theEnv,newScopeMapSize);
   for (i = 0 ; i < CLASS_TABLE_HASH_SIZE ; i++)
     for (theDefclass = DefclassData(theEnv)->ClassTable[i] ;
          theDefclass != NULL ;
          theDefclass = theDefclass->nxtHash)
       {
        matchModule = theDefclass->header.whichModule->theModule;
        className = theDefclass->header.name->contents;
        ClearBitString(newScopeMap,newScopeMapSize);
        GenCopyMemory(char,theDefclass->scopeMap->size,
                   newScopeMap,theDefclass->scopeMap->contents);
        DecrementBitMapReferenceCount(theEnv,theDefclass->scopeMap);
        if (theDefclass->system)
          SetBitMap(newScopeMap,newModuleID);
        else if (FindImportedConstruct(theEnv,"defclass",matchModule,
                                       className,&count,true,NULL) != NULL)
          SetBitMap(newScopeMap,newModuleID);
        theDefclass->scopeMap = (CLIPSBitMap *) AddBitMap(theEnv,newScopeMap,newScopeMapSize);
        IncrementBitMapCount(theDefclass->scopeMap);
       }
   rm(theEnv,newScopeMap,newScopeMapSize);
  }

#endif

#endif
