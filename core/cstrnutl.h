   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_cstrnutl
#define _H_cstrnutl

#ifndef _H_constrnt
#include "constrnt.h"
#endif

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

   struct constraintRecord       *GetConstraintRecord(void *);
   int                            CompareNumbers(void *,int,void *,int,void *);
   struct constraintRecord       *CopyConstraintRecord(void *,CONSTRAINT_RECORD *);
   int                            SetConstraintType(int,CONSTRAINT_RECORD *);
   void                           SetAnyAllowedFlags(CONSTRAINT_RECORD *,int);
   void                           SetAnyRestrictionFlags(CONSTRAINT_RECORD *,int);
   CONSTRAINT_RECORD             *ArgumentTypeToConstraintRecord(void *,int);
   CONSTRAINT_RECORD             *FunctionCallToConstraintRecord(void *,void *);
   CONSTRAINT_RECORD             *ExpressionToConstraintRecord(void *,struct expr *);

#endif /* _H_cstrnutl */


