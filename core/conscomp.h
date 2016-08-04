   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*           CONSTRUCT COMPILER HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Modifications to use the system constant       */
/*            FILENAME_MAX to check file name lengths.       */
/*            DR0856                                         */
/*                                                           */
/*            Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */
/*      6.24: Used EnvClear rather than Clear in             */
/*            InitCImage initialization code.                */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Removed SHORT_LINK_NAMES code as this option   */
/*            is no longer supported.                        */
/*                                                           */
/*            Support for run-time programs directly passing */
/*            the hash tables for initialization.            */
/*                                                           */
/*      6.30: Added path name argument to constructs-to-c.   */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW, MAC_MCW, */
/*            IBM_TBC, IBM_MSC, IBM_ICB, IBM_ZTC, and        */
/*            IBM_SC).                                       */
/*                                                           */
/*            Use genstrcpy instead of strcpy.               */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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

#ifndef _H_conscomp

#pragma once

#define _H_conscomp

#define ArbitraryPrefix(codeItem,i)    (codeItem)->arrayNames[(i)]

#define ModulePrefix(codeItem)         (codeItem)->arrayNames[0]
#define ConstructPrefix(codeItem)      (codeItem)->arrayNames[1]

#include <stdio.h>

#include "constrct.h"
#include "extnfunc.h"
#include "symblcmp.h"
#include "moduldef.h"

#define CONSTRUCT_COMPILER_DATA 41

struct CodeGeneratorItem
  {
   const char *name;
   void (*beforeFunction)(Environment *);
   void (*initFunction)(Environment *,FILE *,int,int);
   bool (*generateFunction)(Environment *,const char *,const char *,char *,int,FILE *,int,int);
   int priority;
   char **arrayNames;
   int arrayCount;
   struct CodeGeneratorItem *next;
  };

struct constructCompilerData
  { 
   int ImageID;
   FILE *HeaderFP;
   int MaxIndices;
   FILE *ExpressionFP;
   FILE *FixupFP;
   const char *FilePrefix;
   const char *PathName;
   char *FileNameBuffer;
   bool ExpressionHeader;
   long ExpressionCount;
   int ExpressionVersion;
   int CodeGeneratorCount;
   struct CodeGeneratorItem *ListOfCodeGeneratorItems;
  };

#define ConstructCompilerData(theEnv) ((struct constructCompilerData *) GetEnvironmentData(theEnv,CONSTRUCT_COMPILER_DATA))

struct CodeGeneratorFile
 {
  const char *filePrefix;
  const char *pathName;
  char *fileNameBuffer;
  int id,version;
 };

   void                      InitializeConstructCompilerData(Environment *);
   void                      ConstructsToCCommandDefinition(Environment *);
   FILE                     *NewCFile(Environment *,const char *,const char *,char *,int,int,bool);
   int                       ExpressionToCode(Environment *,FILE *,struct expr *);
   void                      PrintFunctionReference(Environment *,FILE *,struct FunctionDefinition *);
   struct CodeGeneratorItem *AddCodeGeneratorItem(Environment *,const char *,int,
                                                  void (*)(Environment *),
                                                  void (*)(Environment *,FILE *,int,int),
                                                  bool (*)(Environment *,const char *,const char *,char *,int,FILE *,int,int),int);
   FILE                     *CloseFileIfNeeded(Environment *,FILE *,int *,int *,int,bool *,struct CodeGeneratorFile *);
   FILE                     *OpenFileIfNeeded(Environment *,FILE *,const char *,const char *,char *,int,int,int *,
                                              int,FILE *,const char *,char *,bool,struct CodeGeneratorFile *);
   void                      MarkConstructBsaveIDs(Environment *,int);
   void                      ConstructHeaderToCode(Environment *,FILE *,struct constructHeader *,int,int,
                                                   int,const char *,const char *);
   void                      ConstructModuleToCode(Environment *,FILE *,Defmodule *,int,int,
                                                   int,const char *);
   void                      PrintHashedExpressionReference(Environment *,FILE *,struct expr *,int,int);

#endif




