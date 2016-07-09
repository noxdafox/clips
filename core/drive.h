   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                  DRIVE HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Handles join network activity associated with    */
/*   with the addition of a data entity such as a fact or    */
/*   instance.                                               */
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
/*      6.30: Added support for hashed memories.             */
/*                                                           */
/*            Added additional developer statistics to help  */
/*            analyze join network performance.              */
/*                                                           */
/*            Removed pseudo-facts used in not CE.           */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_drive

#pragma once

#define _H_drive

#include "expressn.h"
#include "match.h"
#include "network.h"

   void                           NetworkAssert(void *,struct partialMatch *,struct joinNode *);
   bool                           EvaluateJoinExpression(void *,struct expr *,struct joinNode *);
   void                           NetworkAssertLeft(void *,struct partialMatch *,struct joinNode *,int);
   void                           NetworkAssertRight(void *,struct partialMatch *,struct joinNode *,int);
   void                           PPDrive(void *,struct partialMatch *,struct partialMatch *,struct joinNode *,int);
   unsigned long                  BetaMemoryHashValue(void *,struct expr *,struct partialMatch *,struct partialMatch *,struct joinNode *);
   bool                           EvaluateSecondaryNetworkTest(void *,struct partialMatch *,struct joinNode *);
   void                           EPMDrive(void *,struct partialMatch *,struct joinNode *,int);
   
#endif /* _H_drive */





