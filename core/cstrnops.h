   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*           CONSTRAINT OPERATIONS HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for performing operations on  */
/*   constraint records including computing the intersection */
/*   and union of constraint records.                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrnops
#define _H_cstrnops

#if (! RUN_TIME)

#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif

   struct constraintRecord       *IntersectConstraints(void *,struct constraintRecord *,struct constraintRecord *);
#if (! BLOAD_ONLY)
   struct constraintRecord       *UnionConstraints(void *,struct constraintRecord *,struct constraintRecord *);
   void                           RemoveConstantFromConstraint(void *,int,void *,CONSTRAINT_RECORD *);
#endif

#endif /* (! RUN_TIME) */

#endif /* _H_cstrnops */
