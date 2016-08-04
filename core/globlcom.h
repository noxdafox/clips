   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*            DEFGLOBAL COMMANDS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
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

#ifndef _H_globlcom

#pragma once

#define _H_globlcom

   void                           DefglobalCommandDefinitions(Environment *);
   bool                           SetResetGlobalsCommand(Environment *);
   bool                           EnvSetResetGlobals(Environment *,bool);
   bool                           GetResetGlobalsCommand(Environment *);
   bool                           EnvGetResetGlobals(Environment *);
   void                           ShowDefglobalsCommand(Environment *);
   void                           EnvShowDefglobals(Environment *,const char *,Defmodule *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           GetResetGlobals(void);
   bool                           SetResetGlobals(bool);
#if DEBUGGING_FUNCTIONS
   void                           ShowDefglobals(const char *,Defmodule *);
#endif

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_globlcom */

