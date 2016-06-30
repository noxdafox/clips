   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*         DEFMODULE BASIC COMMANDS HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements core commands for the deffacts        */
/*   construct such as clear, reset, save, undeffacts,       */
/*   ppdeffacts, list-deffacts, and get-deffacts-list.       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
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
/*************************************************************/

#ifndef _H_modulbsc

#pragma once

#define _H_modulbsc

#include "evaluatn.h"

   void                           DefmoduleBasicCommands(void *);
   void                           EnvGetDefmoduleList(void *,DATA_OBJECT_PTR);
   void                           PPDefmoduleCommand(void *);
   int                            PPDefmodule(void *,const char *,const char *);
   void                           ListDefmodulesCommand(void *);
   void                           EnvListDefmodules(void *,const char *);

#if ALLOW_ENVIRONMENT_GLOBALS

   void                           GetDefmoduleList(DATA_OBJECT_PTR);
#if DEBUGGING_FUNCTIONS
   void                           ListDefmodules(const char *);
#endif

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_modulbsc */

