   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*             FILE I/O ROUTER HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow files to be used */
/*   as input and output sources.                            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Added pragmas to remove compilation warnings.  */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Used gengetc and genungetchar rather than      */
/*            getc and ungetc.                               */
/*                                                           */
/*            Replaced BASIC_IO and ADVANCED_IO compiler     */
/*            flags with the single IO_FUNCTIONS compiler    */
/*            flag.                                          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_filertr

#pragma once

#define _H_filertr

#include <stdio.h>

#define FILE_ROUTER_DATA 47
   
struct fileRouter
  {
   const char *logicalName;
   FILE *stream;
   struct fileRouter *next;
  };

struct fileRouterData
  { 
   struct fileRouter *ListOfFileRouters;
  };

#define FileRouterData(theEnv) ((struct fileRouterData *) GetEnvironmentData(theEnv,FILE_ROUTER_DATA))

   void                           InitializeFileRouter(void *);
   FILE                          *FindFptr(void *,const char *);
   int                            OpenAFile(void *,const char *,const char *,const char *);
   int                            CloseAllFiles(void *);
   int                            CloseFile(void *,const char *);
   int                            FindFile(void *,const char *);

#endif /* _H_filertr */






