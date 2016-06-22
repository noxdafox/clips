   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Object System Construct Compiler Code            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added support for path name argument to        */
/*            constructs-to-c.                               */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_objcmp
#define _H_objcmp

#ifndef _STDIO_INCLUDED_
#include <stdio.h>
#define _STDIO_INCLUDED_
#endif

#ifndef _H_conscomp
#include "conscomp.h"
#endif
#ifndef _H_object
#include "object.h"
#endif

#define OBJECT_COMPILER_DATA 36

struct objectCompilerData
  { 
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *ObjectCodeItem;
#endif
  };

#define ObjectCompilerData(theEnv) ((struct objectCompilerData *) GetEnvironmentData(theEnv,OBJECT_COMPILER_DATA))

   void                    SetupObjectsCompiler(void *);
   void                    PrintClassReference(void *,FILE *,DEFCLASS *,int,int);
   void                    DefclassCModuleReference(void *,FILE *,int,int,int);

#endif /* _H_objcmp */

