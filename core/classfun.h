   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*             CLASS FUNCTIONS HEADER FILE             */
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
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Used genstrcpy and genstrcat instead of strcpy */
/*            and strcat.                                    */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_classfun

#pragma once

#define _H_classfun

#include "object.h"

#define TestTraversalID(traversalRecord,id) TestBitMap(traversalRecord,id)
#define SetTraversalID(traversalRecord,id) SetBitMap(traversalRecord,id)
#define ClearTraversalID(traversalRecord,id) ClearBitMap(traversalRecord,id)

#define CLASS_TABLE_HASH_SIZE     167
#define SLOT_NAME_TABLE_HASH_SIZE 167

#define INITIAL_OBJECT_CLASS_NAME "INITIAL-OBJECT"

#define ISA_ID  0
#define NAME_ID 1

   void                           IncrementDefclassBusyCount(Environment *,Defclass *);
   void                           DecrementDefclassBusyCount(Environment *,Defclass *);
   bool                           InstancesPurge(Environment *);

#if ! RUN_TIME
   void                           InitializeClasses(Environment *);
#endif
   SlotDescriptor                *FindClassSlot(Defclass *,CLIPSLexeme *);
   void                           ClassExistError(Environment *,const char *,const char *);
   void                           DeleteClassLinks(Environment *,CLASS_LINK *);
   void                           PrintClassName(Environment *,const char *,Defclass *,bool);

#if DEBUGGING_FUNCTIONS || ((! BLOAD_ONLY) && (! RUN_TIME))
   void                           PrintPackedClassLinks(Environment *,const char *,const char *,PACKED_CLASS_LINKS *);
#endif

#if ! RUN_TIME
   void                           PutClassInTable(Environment *,Defclass *);
   void                           RemoveClassFromTable(Environment *,Defclass *);
   void                           AddClassLink(Environment *,PACKED_CLASS_LINKS *,Defclass *,int);
   void                           DeleteSubclassLink(Environment *,Defclass *,Defclass *);
   void                           DeleteSuperclassLink(Environment *,Defclass *,Defclass *);
   Defclass                      *NewClass(Environment *,CLIPSLexeme *);
   void                           DeletePackedClassLinks(Environment *,PACKED_CLASS_LINKS *,bool);
   void                           AssignClassID(Environment *,Defclass *);
   SLOT_NAME                     *AddSlotName(Environment *,CLIPSLexeme *,int,bool);
   void                           DeleteSlotName(Environment *,SLOT_NAME *);
   void                           RemoveDefclass(Environment *,Defclass *);
   void                           InstallClass(Environment *,Defclass *,bool);
#endif
   void                           DestroyDefclass(Environment *,Defclass *);

#if (! BLOAD_ONLY) && (! RUN_TIME)
   bool                           IsClassBeingUsed(Defclass *);
   bool                           RemoveAllUserClasses(Environment *);
   bool                           DeleteClassUAG(Environment *,Defclass *);
   void                           MarkBitMapSubclasses(char *,Defclass *,int);
#endif

   short                          FindSlotNameID(Environment *,CLIPSLexeme *);
   CLIPSLexeme                   *FindIDSlotName(Environment *,int);
   SLOT_NAME                     *FindIDSlotNameHash(Environment *,int);
   int                            GetTraversalID(Environment *);
   void                           ReleaseTraversalID(Environment *);
   unsigned                       HashClass(CLIPSLexeme *);

#define DEFCLASS_DATA 21

#define PRIMITIVE_CLASSES 9

struct defclassData
  {
   Construct *DefclassConstruct;
   int DefclassModuleIndex;
   ENTITY_RECORD DefclassEntityRecord;
   Defclass *PrimitiveClassMap[PRIMITIVE_CLASSES];
   Defclass **ClassIDMap;
   Defclass **ClassTable;
   unsigned short MaxClassID;
   unsigned short AvailClassID;
   SLOT_NAME **SlotNameTable;
   CLIPSLexeme *ISA_SYMBOL;
   CLIPSLexeme *NAME_SYMBOL;
#if DEFRULE_CONSTRUCT
   CLIPSLexeme *INITIAL_OBJECT_SYMBOL;
#endif
#if DEBUGGING_FUNCTIONS
   bool WatchInstances;
   bool WatchSlots;
#endif
   unsigned short CTID;
   struct token ObjectParseToken;
   unsigned short ClassDefaultsMode;
  };

#define DefclassData(theEnv) ((struct defclassData *) GetEnvironmentData(theEnv,DEFCLASS_DATA))

#endif









