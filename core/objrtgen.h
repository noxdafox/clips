   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*      6.30: Added support for hashed memories and other    */
/*            join network changes.                          */
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

#ifndef _H_objrtgen

#pragma once

#define _H_objrtgen

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM && (! RUN_TIME) && (! BLOAD_ONLY)

#include "expressn.h"
#include "reorder.h"

   void             ReplaceGetJNObjectValue(Environment *,EXPRESSION *,struct lhsParseNode *,int);
   EXPRESSION      *GenGetJNObjectValue(Environment *,struct lhsParseNode *,int);
   EXPRESSION      *ObjectJNVariableComparison(Environment *,struct lhsParseNode *,struct lhsParseNode *,bool);
   EXPRESSION      *GenObjectPNConstantCompare(Environment *,struct lhsParseNode *);
   void             ReplaceGetPNObjectValue(Environment *,EXPRESSION *,struct lhsParseNode *);
   EXPRESSION      *GenGetPNObjectValue(Environment *,struct lhsParseNode *);
   EXPRESSION      *ObjectPNVariableComparison(Environment *,struct lhsParseNode *,struct lhsParseNode *);
   void             GenObjectLengthTest(Environment *,struct lhsParseNode *);
   void             GenObjectZeroLengthTest(Environment *,struct lhsParseNode *);

#endif /* DEFRULE_CONSTRUCT && OBJECT_SYSTEM && (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* _H_objrtgen */




