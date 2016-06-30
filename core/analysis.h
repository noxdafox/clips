   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                ANALYSIS HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Analyzes LHS patterns to check for semantic      */
/*   errors and to determine variable comparisons and other  */
/*   tests which must be performed either in the pattern or  */
/*   join networks.                                          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Join network rework and optimizations.         */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_analysis

#pragma once

#define _H_analysis

#include "expressn.h"
#include "reorder.h"

/*****************************************************/
/* nandFrame structure: Stores information about the */
/*   current position in the nesting of not/and CEs  */
/*   as the patterns of a rule are analyzed.         */
/*****************************************************/
struct nandFrame
  {
   int depth;
   struct lhsParseNode *nandCE;
   struct nandFrame *next;
  };

   intBool                        VariableAnalysis(void *,struct lhsParseNode *);

#endif

