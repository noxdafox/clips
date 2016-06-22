   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*            DEFGLOBAL COMMANDS HEADER FILE           */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_globlcom
#define _H_globlcom

   void                           DefglobalCommandDefinitions(void *);
   int                            SetResetGlobalsCommand(void *);
   intBool                        EnvSetResetGlobals(void *,int);
   int                            GetResetGlobalsCommand(void *);
   intBool                        EnvGetResetGlobals(void *);
   void                           ShowDefglobalsCommand(void *);
   void                           EnvShowDefglobals(void *,const char *,void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                        GetResetGlobals(void);
   intBool                        SetResetGlobals(int);
#if DEBUGGING_FUNCTIONS
   void                           ShowDefglobals(const char *,void *);
#endif

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_globlcom */

