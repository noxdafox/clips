   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*           EXPRESSION BLOAD/BSAVE HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for the  */
/*    expression data structure.                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_exprnbin

#pragma once

#define _H_exprnbin

#include <stdio.h>

#include "expressn.h"

#define ExpressionPointer(i) ((struct expr *) (((i) == -1L) ? NULL : &ExpressionData(theEnv)->ExpressionArray[i]))
#define HashedExpressionPointer(i) ExpressionPointer(i)

   void                        AllocateExpressions(Environment *);
   void                        RefreshExpressions(Environment *);
   void                        ClearBloadedExpressions(Environment *);
   void                        FindHashedExpressions(Environment *);
   void                        BsaveHashedExpressions(Environment *,FILE *);
   void                        BsaveConstructExpressions(Environment *,FILE *);
   void                        BsaveExpression(Environment *,struct expr *,FILE *);

#endif /* _H_exprnbin */







