   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/17/16            */
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
/*            Support for typed EXTERNAL_ADDRESS.            */
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
/*************************************************************/

#ifndef _H_sysdep
#define _H_sysdep

#ifndef _STDIO_INCLUDED_
#define _STDIO_INCLUDED_
#include <stdio.h>
#endif

#include <setjmp.h>

#if WIN_MVC
#include <dos.h>
#endif

   void                        SetRedrawFunction(void *,void (*)(void *));
   void                        SetPauseEnvFunction(void *,void (*)(void *));
   void                        SetContinueEnvFunction(void *,void (*)(void *,int));
   void                        (*GetRedrawFunction(void *))(void *);
   void                        (*GetPauseEnvFunction(void *))(void *);
   void                        (*GetContinueEnvFunction(void *))(void *,int);
   double                      gentime(void);
   void                        gensystem(void *theEnv,const char *);
   void                        VMSSystem(char *);
   int                         GenOpenReadBinary(void *,const char *,const char *);
   void                        GetSeekCurBinary(void *,long);
   void                        GetSeekSetBinary(void *,long);
   void                        GenTellBinary(void *,long *);
   void                        GenCloseBinary(void *);
   void                        GenReadBinary(void *,void *,size_t);
   FILE                       *GenOpen(void *,const char *,const char *);
   int                         GenClose(void *,FILE *);
   void                        genexit(void *,int);
   int                         genrand(void);
   void                        genseed(int);
   int                         genremove(const char *);
   int                         genrename(const char *,const char *);
   char                       *gengetcwd(char *,int);
   void                        GenWrite(void *,size_t,FILE *);
   int                       (*EnvSetBeforeOpenFunction(void *,int (*)(void *)))(void *);
   int                       (*EnvSetAfterOpenFunction(void *,int (*)(void *)))(void *);
   int                         gensprintf(char *,const char *,...);
   char                       *genstrcpy(char *,const char *);
   char                       *genstrncpy(char *,const char *,size_t);
   char                       *genstrcat(char *,const char *);
   char                       *genstrncat(char *,const char *,size_t);
   int                         genchdir(const char *);
   void                        SetJmpBuffer(void *,jmp_buf *);
   void                        genprintfile(void *,FILE *,const char *);
   int                         gengetchar(void *);
   int                         genungetchar(void *,int);
   void                        InitializeSystemDependentData(void *);
   void                        InitializeNonportableFeatures(void *);
 
#endif /* _H_sysdep */





