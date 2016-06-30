   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_exprnbin

#pragma once

#define _H_exprnbin

#include <stdio.h>

#include "expressn.h"

#define ExpressionPointer(i) ((struct expr *) (((i) == -1L) ? NULL : &ExpressionData(theEnv)->ExpressionArray[i]))
#define HashedExpressionPointer(i) ExpressionPointer(i)

   void                        AllocateExpressions(void *);
   void                        RefreshExpressions(void *);
   void                        ClearBloadedExpressions(void *);
   void                        FindHashedExpressions(void *);
   void                        BsaveHashedExpressions(void *,FILE *);
   void                        BsaveConstructExpressions(void *,FILE *);
   void                        BsaveExpression(void *,struct expr *,FILE *);

#endif /* _H_exprnbin */







