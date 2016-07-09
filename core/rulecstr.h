   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*             RULE CONSTRAINTS HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for detecting constraint       */
/*   conflicts in the LHS and RHS of rules.                  */
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
/*      6.30: Support for long long integers.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_rulecstr

#pragma once

#define _H_rulecstr

   struct lhsParseNode           *GetExpressionVarConstraints(void *,struct lhsParseNode *);
   struct lhsParseNode           *DeriveVariableConstraints(void *,struct lhsParseNode *);
   bool                           ProcessConnectedConstraints(void *,struct lhsParseNode *,struct lhsParseNode *,struct lhsParseNode *);
   void                           ConstraintReferenceErrorMessage(void *,
                                                                struct symbolHashNode *,
                                                                struct lhsParseNode *,
                                                                int,int,
                                                                struct symbolHashNode *,
                                                                int);
   bool                           CheckRHSForConstraintErrors(void *,struct expr *,struct lhsParseNode *);

#endif /* _H_rulecstr */

