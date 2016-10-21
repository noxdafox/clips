   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*           DEFRULE BSAVE/BLOAD HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    defrule construct.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed CONFLICT_RESOLUTION_STRATEGIES,        */
/*            DYNAMIC_SALIENCE, and LOGICAL_DEPENDENCIES     */
/*            compilation flags.                             */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Added support for alpha memories.              */
/*                                                           */
/*            Added salience groups to improve performance   */
/*            with large numbers of activations of different */
/*            saliences.                                     */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_rulebin

#pragma once

#define _H_rulebin

#if (! RUN_TIME)

#include "cstrcbin.h"
#include "modulbin.h"
#include "network.h"

struct bsaveDefrule
  {
   struct bsaveConstructHeader header;
   int salience;
   int localVarCnt;
   unsigned int complexity      : 12;
   unsigned int autoFocus       :  1;
   long dynamicSalience;
   long actions;
   long logicalJoin;
   long lastJoin;
   long disjunct;
  };

struct bsavePatternNodeHeader
  {
   long entryJoin;
   long rightHash;
   unsigned int singlefieldNode : 1;
   unsigned int multifieldNode : 1;
   unsigned int stopNode : 1;
   unsigned int blocked : 1;
   unsigned int initialize : 1;
   unsigned int marked : 1;
   unsigned int beginSlot : 1;
   unsigned int endSlot : 1;
   unsigned int selector : 1;
  };

struct bsaveDefruleModule
  {
   struct bsaveDefmoduleItemHeader header;
  };

struct bsaveJoinLink
  {
   char enterDirection;
   long join;
   long next;
  };

struct bsaveJoinNode
  {
   unsigned int firstJoin : 1;
   unsigned int logicalJoin : 1;
   unsigned int joinFromTheRight : 1;
   unsigned int patternIsNegated : 1;
   unsigned int patternIsExists : 1;
   unsigned int rhsType : 3;
   unsigned int depth : 7;
   long networkTest;
   long secondaryNetworkTest;
   long leftHash;
   long rightHash;
   long rightSideEntryStructure;
   long nextLinks;
   long lastLevel;
   long rightMatchNode;
   long ruleToActivate;
  };

#define RULEBIN_DATA 20

struct defruleBinaryData
  {
   long NumberOfDefruleModules;
   long NumberOfDefrules;
   long NumberOfJoins;
   long NumberOfLinks;
   long RightPrimeIndex;
   long LeftPrimeIndex;
   struct defruleModule *ModuleArray;
   Defrule *DefruleArray;
   struct joinNode *JoinArray;
   struct joinLink *LinkArray;
  };

#define DefruleBinaryData(theEnv) ((struct defruleBinaryData *) GetEnvironmentData(theEnv,RULEBIN_DATA))

#define BloadDefrulePointer(x,i) ((Defrule *) ((i == -1L) ? NULL : &x[i]))
#define BsaveJoinIndex(joinPtr) ((joinPtr == NULL) ? -1L :  ((struct joinNode *) joinPtr)->bsaveID)
#define BloadJoinPointer(i) ((struct joinNode *) ((i == -1L) ? NULL : &DefruleBinaryData(theEnv)->JoinArray[i]))
#define BsaveJoinLinkIndex(linkPtr) ((linkPtr == NULL) ? -1L :  ((struct joinLink *) linkPtr)->bsaveID)
#define BloadJoinLinkPointer(i) ((struct joinLink *) ((i == -1L) ? NULL : &DefruleBinaryData(theEnv)->LinkArray[i]))

   void                           DefruleBinarySetup(Environment *);
   void                           UpdatePatternNodeHeader(Environment *,struct patternNodeHeader *,
                                                                 struct bsavePatternNodeHeader *);
   void                           AssignBsavePatternHeaderValues(Environment *,struct bsavePatternNodeHeader *,
                                                                        struct patternNodeHeader *);
   void                          *BloadDefruleModuleReference(Environment *,int);

#endif /* (! RUN_TIME) */

#endif /* _H_rulebin */





