   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*             TEXT PROCESSING HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Modified error messages so that they were      */
/*            directly printed rather than storing them in   */
/*            a string buffer which might not be large       */
/*            enough to contain the entire message. DR0855   */
/*            Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Added get-region function.                     */
/*                                                           */
/*            Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*      6.30: Removed HELP_FUNCTIONS compilation flag and    */
/*            associated functionality.                      */
/*                                                           */
/*            Used genstrcpy and genstrncpy instead of       */
/*            strcpy and strncpy.                            */
/*                                                           */             
/*            Support for long long integers.                */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_TBC).         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_textpro

#pragma once

#define _H_textpro

#if TEXTPRO_FUNCTIONS
   void                           FetchCommand(void *,DATA_OBJECT *);
   int                            PrintRegionCommand(void *);
   void                          *GetRegionCommand(void *);
   int                                   TossCommand(void *);
#endif

   void                           HelpFunctionDefinitions(void *);

#endif /* _H_textpro */





