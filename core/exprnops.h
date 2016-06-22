   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*          EXPRESSION OPERATIONS HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides utility routines for manipulating and   */
/*   examining expressions.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Add NegateExpression function.                 */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_exprnops

#define _H_exprnops

#ifndef _H_expressn
#include "expressn.h"
#endif

   intBool                        ConstantExpression(struct expr *);
   void                           PrintExpression(void *,const char *,struct expr *);
   long                           ExpressionSize(struct expr *);
   int                            CountArguments(struct expr *);
   struct expr                   *CopyExpression(void *,struct expr *);
   intBool                        ExpressionContainsVariables(struct expr *,int);
   intBool                        IdenticalExpression(struct expr *,struct expr *);
   struct expr                   *GenConstant(void *,unsigned short,void *);
#if ! RUN_TIME
   int                            CheckArgumentAgainstRestriction(void *,struct expr *,int);
#endif
   intBool                        ConstantType(int);
   struct expr                   *CombineExpressions(void *,struct expr *,struct expr *);
   struct expr                   *AppendExpressions(struct expr *,struct expr *);
   struct expr                   *NegateExpression(void *,struct expr *);

#endif /* _H_exprnops */


