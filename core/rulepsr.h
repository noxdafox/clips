   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*               RULE PARSING HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Coordinates parsing of a rule.                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed DYNAMIC_SALIENCE, INCREMENTAL_RESET,   */
/*            and LOGICAL_DEPENDENCIES compilation flags.    */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            GetConstructNameAndComment API change.         */
/*                                                           */
/*            Added support for hashed memories.             */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_rulepsr

#pragma once

#define _H_rulepsr

   int                            ParseDefrule(void *,const char *);
   struct lhsParseNode           *FindVariable(struct symbolHashNode *,
                                                      struct lhsParseNode *);
#if DEVELOPER && DEBUGGING_FUNCTIONS
   void                           DumpRuleAnalysis(void *,struct lhsParseNode *);
#endif

#endif /* _H_rulepsr */


