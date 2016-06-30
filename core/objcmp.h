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
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_objcmp

#pragma once

#define _H_objcmp

#include <stdio.h>

#include "conscomp.h"
#include "object.h"

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

