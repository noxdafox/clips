   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                                                     */
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
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added support for hashed alpha memories.       */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_objrtfnx

#pragma once

#define _H_objrtfnx

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM

#include "conscomp.h"
#include "evaluatn.h"
#include "match.h"
#include "object.h"
#include "objrtmch.h"

struct ObjectMatchVar1
  {
   unsigned short whichSlot;
   unsigned short whichPattern;
   unsigned short whichField;
   unsigned objectAddress : 1;
   unsigned allFields     : 1;
   unsigned lhs           : 1;
   unsigned rhs           : 1;
  };

struct ObjectMatchVar2
  {
   unsigned short whichSlot;
   unsigned short whichPattern;
   unsigned short beginningOffset;
   unsigned short endOffset;
   unsigned fromBeginning   : 1;
   unsigned fromEnd         : 1;
   unsigned lhs           : 1;
   unsigned rhs           : 1;
  };

struct ObjectMatchLength
  {
   unsigned minLength : 15;
   unsigned exactly   : 1;
  };

struct ObjectCmpPNConstant
  {
   unsigned short offset;
   unsigned pass          : 1;
   unsigned fail          : 1;
   unsigned general       : 1;
   unsigned fromBeginning : 1;
  };

struct ObjectCmpPNSingleSlotVars1
  {
   unsigned short firstSlot;
   unsigned short secondSlot;
   unsigned pass       : 1;
   unsigned fail       : 1;
  };

struct ObjectCmpPNSingleSlotVars2
  {
   unsigned short firstSlot;
   unsigned short secondSlot;
   unsigned short offset;
   unsigned pass          : 1;
   unsigned fail          : 1;
   unsigned fromBeginning : 1;
  };

struct ObjectCmpPNSingleSlotVars3
  {
   unsigned short firstSlot;
   unsigned short secondSlot;
   unsigned short firstOffset;
   unsigned short secondOffset;
   unsigned pass                : 1;
   unsigned fail                : 1;
   unsigned firstFromBeginning  : 1;
   unsigned secondFromBeginning : 1;
  };

struct ObjectCmpJoinSingleSlotVars1
  {
   unsigned short firstSlot;
   unsigned short secondSlot;
   unsigned short firstPattern;
   unsigned short secondPattern;
   unsigned pass          : 1;
   unsigned fail          : 1;
   unsigned int firstPatternLHS : 1;
   unsigned int firstPatternRHS : 1;
   unsigned int secondPatternLHS : 1;
   unsigned int secondPatternRHS : 1;
  };

struct ObjectCmpJoinSingleSlotVars2
  {
   unsigned short firstSlot;
   unsigned short secondSlot;
   unsigned short firstPattern;
   unsigned short secondPattern;
   unsigned short offset;
   unsigned pass          : 1;
   unsigned fromBeginning : 1;
   unsigned fail          : 1;
   unsigned int firstPatternLHS : 1;
   unsigned int firstPatternRHS : 1;
   unsigned int secondPatternLHS : 1;
   unsigned int secondPatternRHS : 1;
  };

struct ObjectCmpJoinSingleSlotVars3
  {
   unsigned short firstSlot;
   unsigned short secondSlot;
   unsigned short firstPattern;
   unsigned short secondPattern;
   unsigned short firstOffset;
   unsigned short secondOffset;
   unsigned pass                : 1;
   unsigned fail                : 1;
   unsigned firstFromBeginning  : 1;
   unsigned secondFromBeginning : 1;
   unsigned int firstPatternLHS : 1;
   unsigned int firstPatternRHS : 1;
   unsigned int secondPatternLHS : 1;
   unsigned int secondPatternRHS : 1;
  };

#define OBJECT_RETE_DATA 35

struct objectReteData
  {
   Instance *CurrentPatternObject;
   InstanceSlot *CurrentPatternObjectSlot;
   unsigned CurrentObjectSlotLength;
   struct multifieldMarker *CurrentPatternObjectMarks;
   struct entityRecord ObjectGVInfo1;
   struct entityRecord ObjectGVInfo2;
   struct entityRecord ObjectGVPNInfo1;
   struct entityRecord ObjectGVPNInfo2;
   struct entityRecord ObjectCmpConstantInfo;
   struct entityRecord LengthTestInfo;
   struct entityRecord PNSimpleCompareInfo1;
   struct entityRecord PNSimpleCompareInfo2;
   struct entityRecord PNSimpleCompareInfo3;
   struct entityRecord JNSimpleCompareInfo1;
   struct entityRecord JNSimpleCompareInfo2;
   struct entityRecord JNSimpleCompareInfo3;
   OBJECT_MATCH_ACTION *ObjectMatchActionQueue;
   OBJECT_PATTERN_NODE *ObjectPatternNetworkPointer;
   OBJECT_ALPHA_NODE *ObjectPatternNetworkTerminalPointer;
   bool DelayObjectPatternMatching;
   unsigned long long CurrentObjectMatchTimeTag;
   long long UseEntityTimeTag;
#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM && CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *ObjectPatternCodeItem;
#endif
  };

#define ObjectReteData(theEnv) ((struct objectReteData *) GetEnvironmentData(theEnv,OBJECT_RETE_DATA))

   void                    InstallObjectPrimitives(Environment *);
   bool                    ObjectCmpConstantFunction(Environment *,void *,UDFValue *);

#endif /* DEFRULE_CONSTRUCT && OBJECT_SYSTEM */

#endif /* _H_objrtfnx */






