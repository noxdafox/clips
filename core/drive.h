   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_drive

#pragma once

#define _H_drive

#include "expressn.h"
#include "match.h"
#include "network.h"

   void                           NetworkAssert(Environment *,struct partialMatch *,struct joinNode *);
   bool                           EvaluateJoinExpression(Environment *,struct expr *,struct joinNode *);
   void                           NetworkAssertLeft(Environment *,struct partialMatch *,struct joinNode *,int);
   void                           NetworkAssertRight(Environment *,struct partialMatch *,struct joinNode *,int);
   void                           PPDrive(Environment *,struct partialMatch *,struct partialMatch *,struct joinNode *,int);
   unsigned long                  BetaMemoryHashValue(Environment *,struct expr *,struct partialMatch *,struct partialMatch *,struct joinNode *);
   bool                           EvaluateSecondaryNetworkTest(Environment *,struct partialMatch *,struct joinNode *);
   void                           EPMDrive(Environment *,struct partialMatch *,struct joinNode *,int);
   
#endif /* _H_drive */





