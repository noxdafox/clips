   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Deffunction Construct Compiler Code              */
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
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_dffnxcmp
#define _H_dffnxcmp

#if DEFFUNCTION_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME)

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#include "dffnxfun.h"

   void                           SetupDeffunctionCompiler(void *);
   void                           PrintDeffunctionReference(void *,FILE *,DEFFUNCTION *,int,int);
   void                           DeffunctionCModuleReference(void *,FILE *,int,int,int);

#endif /* DEFFUNCTION_CONSTRUCT && CONSTRUCT_COMPILER && (! RUN_TIME) */

#endif /* _H_dffnxcmp */


