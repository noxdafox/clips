   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/04/16            */
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
/*************************************************************/

#ifndef _H_cstrcpsr

#pragma once

#define _H_cstrcpsr

#include "evaluatn.h"
#include "scanner.h"
#include "constrct.h"

#if ALLOW_ENVIRONMENT_GLOBALS
   int                            Load(const char *);
#endif

   int                            EnvLoad(void *,const char *);
   int                            LoadConstructsFromLogicalName(void *,const char *);
   int                            ParseConstruct(void *,const char *,const char *);
   void                           RemoveConstructFromModule(void *,struct constructHeader *);
   struct symbolHashNode         *GetConstructNameAndComment(void *,const char *,
                                                                    struct token *,const char *,
                                                                    void *(*)(void *,const char *),
                                                                    bool (*)(void *,void *),
                                                                    const char *,bool,bool,bool,bool);
   void                           ImportExportConflictMessage(void *,const char *,const char *,const char *,const char *);
#if (! RUN_TIME) && (! BLOAD_ONLY)
   void                           FlushParsingMessages(void *);
   char                          *EnvGetParsingFileName(void *);
   void                           EnvSetParsingFileName(void *,const char *);
   char                          *EnvGetErrorFileName(void *);
   void                           EnvSetErrorFileName(void *,const char *);
   char                          *EnvGetWarningFileName(void *);
   void                           EnvSetWarningFileName(void *,const char *);
   void                           CreateErrorCaptureRouter(void *);
   void                           DeleteErrorCaptureRouter(void *);
#endif

#endif







