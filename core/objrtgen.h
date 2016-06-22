   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_objrtgen
#define _H_objrtgen

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM && (! RUN_TIME) && (! BLOAD_ONLY)

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_reorder
#include "reorder.h"
#endif

   void             ReplaceGetJNObjectValue(void *,EXPRESSION *,struct lhsParseNode *,int);
   EXPRESSION      *GenGetJNObjectValue(void *,struct lhsParseNode *,int);
   EXPRESSION      *ObjectJNVariableComparison(void *,struct lhsParseNode *,struct lhsParseNode *,int);
   EXPRESSION      *GenObjectPNConstantCompare(void *,struct lhsParseNode *);
   void             ReplaceGetPNObjectValue(void *,EXPRESSION *,struct lhsParseNode *);
   EXPRESSION      *GenGetPNObjectValue(void *,struct lhsParseNode *); 
   EXPRESSION      *ObjectPNVariableComparison(void *,struct lhsParseNode *,struct lhsParseNode *);
   void             GenObjectLengthTest(void *,struct lhsParseNode *);
   void             GenObjectZeroLengthTest(void *,struct lhsParseNode *);

#endif /* DEFRULE_CONSTRUCT && OBJECT_SYSTEM && (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* _H_objrtgen */




