   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
   /*                                                     */
   /*            SYSTEM DEPENDENT HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Isolation of system dependent routines.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Modified GenOpen to check the file length      */
/*            against the system constant FILENAME_MAX.      */
/*                                                           */
/*      6.24: Support for run-time programs directly passing */
/*            the hash tables for initialization.            */
/*                                                           */
/*            Made gensystem functional for Xcode.           */
/*                                                           */
/*            Added BeforeOpenFunction and AfterOpenFunction */
/*            hooks.                                         */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Updated UNIX_V gentime functionality.          */
/*                                                           */
/*            Removed GenOpen check against FILENAME_MAX.    */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, IBM_ICB, IBM_TBC, IBM_ZTC, and        */
/*            IBM_SC).                                       */
/*                                                           */
/*            Renamed IBM_MSC and WIN_MVC compiler flags     */
/*            and IBM_GCC to WIN_GCC.                        */
/*                                                           */
/*            Added LINUX and DARWIN compiler flags.         */
/*                                                           */
/*            Removed HELP_FUNCTIONS compilation flag and    */
/*            associated functionality.                      */
/*                                                           */
/*            Removed EMACS_EDITOR compilation flag and      */
/*            associated functionality.                      */
/*                                                           */
/*            Combined BASIC_IO and EXT_IO compilation       */
/*            flags into the single IO_FUNCTIONS flag.       */
/*                                                           */
/*            Changed the EX_MATH compilation flag to        */
/*            EXTENDED_MATH_FUNCTIONS.                       */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS_TYPE.            */
/*                                                           */
/*            GenOpen function checks for UTF-8 Byte Order   */
/*            Marker.                                        */
/*                                                           */
/*            Added gengetchar, genungetchar, genprintfile,  */
/*            genstrcpy, genstrncpy, genstrcat, genstrncat,  */
/*            and gensprintf functions.                      */
/*                                                           */
/*            Added SetJmpBuffer function.                   */
/*                                                           */
/*            Added environment argument to genexit.         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Added genchdir function for changing the       */
/*            current directory.                             */
/*                                                           */
/*            Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_sysdep

#pragma once

#define _H_sysdep

#include <stdio.h>
#include <setjmp.h>

   void                        SetRedrawFunction(Environment *,void (*)(Environment *));
   void                        SetPauseEnvFunction(Environment *,void (*)(Environment *));
   void                        SetContinueEnvFunction(Environment *,void (*)(Environment *,int));
   void                        (*GetRedrawFunction(Environment *))(Environment *);
   void                        (*GetPauseEnvFunction(Environment *))(Environment *);
   void                        (*GetContinueEnvFunction(Environment *))(Environment *,int);
   double                      gentime(void);
   void                        gensystem(Environment *,const char *);
   int                         GenOpenReadBinary(Environment *,const char *,const char *);
   void                        GetSeekCurBinary(Environment *,long);
   void                        GetSeekSetBinary(Environment *,long);
   void                        GenTellBinary(Environment *,long *);
   void                        GenCloseBinary(Environment *);
   void                        GenReadBinary(Environment *,void *,size_t);
   FILE                       *GenOpen(Environment *,const char *,const char *);
   int                         GenClose(Environment *,FILE *);
   void                        genexit(Environment *,int);
   int                         genrand(void);
   void                        genseed(int);
   bool                        genremove(const char *);
   bool                        genrename(const char *,const char *);
   char                       *gengetcwd(char *,int);
   void                        GenWrite(void *,size_t,FILE *);
   int                       (*EnvSetBeforeOpenFunction(Environment *,int (*)(Environment *)))(Environment *);
   int                       (*EnvSetAfterOpenFunction(Environment *,int (*)(Environment *)))(Environment *);
   int                         gensprintf(char *,const char *,...);
   char                       *genstrcpy(char *,const char *);
   char                       *genstrncpy(char *,const char *,size_t);
   char                       *genstrcat(char *,const char *);
   char                       *genstrncat(char *,const char *,size_t);
   int                         genchdir(const char *);
   void                        SetJmpBuffer(Environment *,jmp_buf *);
   void                        genprintfile(Environment *,FILE *,const char *);
   int                         gengetchar(Environment *);
   int                         genungetchar(Environment *,int);
   void                        InitializeSystemDependentData(Environment *);
   void                        InitializeNonportableFeatures(Environment *);

#endif /* _H_sysdep */





