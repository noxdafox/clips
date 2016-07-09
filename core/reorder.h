   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                 REORDER HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines necessary for converting the   */
/*   the LHS of a rule into an appropriate form suitable for */
/*   the KB Rete topology. This includes transforming the    */
/*   LHS so there is at most one "or" CE (and this is the    */
/*   first CE of the LHS if it is used), adding initial      */
/*   patterns to the LHS (if no LHS is specified or a "test" */
/*   or "not" CE is the first pattern within an "and" CE),   */
/*   removing redundant CEs, and determining appropriate     */
/*   information on nesting for implementing joins from the  */
/*   right.                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Support for join network changes.              */
/*                                                           */
/*            Changes to the algorithm for processing        */
/*            not/and CE groups.                             */
/*                                                           */
/*            Additional optimizations for combining         */
/*            conditional elements.                          */
/*                                                           */
/*            Added support for hashed alpha memories.       */
/*                                                           */
/*      6.31: Removed the marked flag used for not/and       */
/*            unification.                                   */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_reorder

#pragma once

#define _H_reorder

struct lhsParseNode;

#include "expressn.h"
#include "pattern.h"
#include "ruledef.h"

/***********************************************************************/
/* lhsParseNode structure: Stores information about the intermediate   */
/*   parsed representation of the lhs of a rule.                       */
/***********************************************************************/
struct lhsParseNode
  {
   unsigned short type;
   void *value;
   unsigned int negated : 1;
   unsigned int exists : 1;
   unsigned int existsNand : 1;
   unsigned int logical : 1;
   unsigned int multifieldSlot : 1;
   unsigned int bindingVariable : 1;
   unsigned int derivedConstraints : 1;
   unsigned int userCE : 1;
   unsigned int whichCE : 7;
   //unsigned int marked : 1;
   unsigned int withinMultifieldSlot : 1;
   unsigned short multiFieldsBefore;
   unsigned short multiFieldsAfter;
   unsigned short singleFieldsBefore;
   unsigned short singleFieldsAfter;
   struct constraintRecord *constraints;
   struct lhsParseNode *referringNode;
   struct patternParser *patternType;
   short pattern;
   short index;
   struct symbolHashNode *slot;
   short slotNumber;
   int beginNandDepth;
   int endNandDepth;
   int joinDepth;
   struct expr *networkTest;
   struct expr *externalNetworkTest;
   struct expr *secondaryNetworkTest;
   struct expr *externalLeftHash;
   struct expr *externalRightHash;
   struct expr *constantSelector;
   struct expr *constantValue;
   struct expr *leftHash;
   struct expr *rightHash;
   struct expr *betaHash;
   struct lhsParseNode *expression;
   struct lhsParseNode *secondaryExpression;
   void *userData;
   struct lhsParseNode *right;
   struct lhsParseNode *bottom;
  };

   struct lhsParseNode           *ReorderPatterns(void *,struct lhsParseNode *,bool *);
   struct lhsParseNode           *CopyLHSParseNodes(void *,struct lhsParseNode *);
   void                           CopyLHSParseNode(void *,struct lhsParseNode *,struct lhsParseNode *,bool);
   struct lhsParseNode           *GetLHSParseNode(void *);
   void                           ReturnLHSParseNodes(void *,struct lhsParseNode *);
   struct lhsParseNode           *ExpressionToLHSParseNodes(void *,struct expr *);
   struct expr                   *LHSParseNodesToExpression(void *,struct lhsParseNode *);
   void                           AddInitialPatterns(void *,struct lhsParseNode *);
   bool                           IsExistsSubjoin(struct lhsParseNode *,int);
   struct lhsParseNode           *CombineLHSParseNodes(void *,struct lhsParseNode *,struct lhsParseNode *);
   //void                           AssignPatternMarkedFlag(struct lhsParseNode *,short);

#endif /* _H_reorder */





