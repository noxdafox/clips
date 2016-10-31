   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/06/16            */
   /*                                                     */
   /*              CONSTRUCT PARSER MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parsing routines and utilities for parsing       */
/*   constructs.                                             */
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
/*            Made the construct redefinition message more   */
/*            prominent.                                     */
/*                                                           */
/*            Added pragmas to remove compilation warnings.  */
/*                                                           */
/*      6.30: Added code for capturing errors/warnings.      */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW, MAC_MCW, */
/*            and IBM_TBC).                                  */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            GetConstructNameAndComment API change.         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrcpsr

#pragma once

#define _H_cstrcpsr

#if (! RUN_TIME) && (! BLOAD_ONLY)
   int                            EnvLoad(Environment *,const char *);
   int                            LoadConstructsFromLogicalName(Environment *,const char *);
   int                            ParseConstruct(Environment *,const char *,const char *);
   void                           ImportExportConflictMessage(Environment *,const char *,const char *,
                                                              const char *,const char *);
   void                           FlushParsingMessages(Environment *);
   char                          *EnvGetParsingFileName(Environment *);
   void                           EnvSetParsingFileName(Environment *,const char *);
   char                          *EnvGetErrorFileName(Environment *);
   void                           EnvSetErrorFileName(Environment *,const char *);
   char                          *EnvGetWarningFileName(Environment *);
   void                           EnvSetWarningFileName(Environment *,const char *);
   void                           CreateErrorCaptureRouter(Environment *);
   void                           DeleteErrorCaptureRouter(Environment *);
#endif

#endif







