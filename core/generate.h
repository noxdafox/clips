   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                GENERATE HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for converting field           */
/*   constraints to expressions which can be used            */
/*   in the pattern and join networks.                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Added support for hashed comparisons to        */
/*            constants.                                     */
/*                                                           */
/*            Reimplemented algorithm for comparisons to     */
/*            variables contained within not/and CEs.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_generate

#define _H_generate

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_reorder
#include "reorder.h"
#endif
#ifndef _H_analysis
#include "analysis.h"
#endif

   void                           FieldConversion(void *,struct lhsParseNode *,struct lhsParseNode *,struct nandFrame *);
   struct expr                   *GetvarReplace(void *,struct lhsParseNode *,int,struct nandFrame *);
   void                           AddNandUnification(void *,struct lhsParseNode *,struct nandFrame *);

#endif /* _H_generate */



