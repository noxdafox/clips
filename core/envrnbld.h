   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/18/16            */
   /*                                                     */
   /*             ENVRNMNT BUILD HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for supporting multiple environments.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Added to separate environment creation and     */
/*            deletion code.                                 */
/*                                                           */
/*************************************************************/

#ifndef _H_envrnbld

#pragma once

#define _H_envrnbld

#include <stdbool.h>

#include "envrnmnt.h"
#include "extnfunc.h"

   Environment                   *CreateEnvironment(void);
   Environment                   *CreateRuntimeEnvironment(CLIPSLexeme **,CLIPSFloat **,
                                                           CLIPSInteger **,struct bitMapHashNode **,
                                                           struct functionDefinition *);
   bool                           DestroyEnvironment(Environment *);

#endif /* _H_envrnbld */

