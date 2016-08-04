   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*            CONSTRAINT UTILITY HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Utility routines for manipulating, initializing, */
/*   creating, copying, and comparing constraint records.    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*                                                           */
/* Revision History:                                         */
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

#ifndef _H_cstrnutl

#pragma once

#define _H_cstrnutl

#include "constrnt.h"

   struct constraintRecord       *GetConstraintRecord(Environment *);
   int                            CompareNumbers(Environment *,int,void *,int,void *);
   struct constraintRecord       *CopyConstraintRecord(Environment *,CONSTRAINT_RECORD *);
   bool                           SetConstraintType(int,CONSTRAINT_RECORD *);
   void                           SetAnyAllowedFlags(CONSTRAINT_RECORD *,bool);
   void                           SetAnyRestrictionFlags(CONSTRAINT_RECORD *,bool);
   CONSTRAINT_RECORD             *ArgumentTypeToConstraintRecord(Environment *,int);
   CONSTRAINT_RECORD             *FunctionCallToConstraintRecord(Environment *,void *);
   CONSTRAINT_RECORD             *ExpressionToConstraintRecord(Environment *,struct expr *);

#endif /* _H_cstrnutl */


