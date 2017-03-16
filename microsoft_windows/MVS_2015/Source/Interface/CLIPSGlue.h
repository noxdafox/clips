   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  02/17/17            */
   /*                                                     */
   /*              COMMAND LINE HEADER FILE               */
   /*******************************************************/

#ifndef _H_CLIPSGlue

#pragma once

#define _H_CLIPSGlue

#include "setup.h"
#include "ruledef.h"

   void                           SetupCLIPSGlue(Environment *);
   void                           MatchesAction(Defrule *,char *);

#endif