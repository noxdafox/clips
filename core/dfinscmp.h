   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Definstances Construct Compiler Code             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added support for path name argument to        */
/*            constructs-to-c.                               */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_dfinscmp

#pragma once

#define _H_dfinscmp

#if DEFINSTANCES_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME)

#include <stdio.h>

   void                           SetupDefinstancesCompiler(Environment *);
   void                           DefinstancesCModuleReference(Environment *,FILE *,int,int,int);

#endif /* DEFINSTANCES_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME) */

#endif /* _H_dfinscmp */


