   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*              FILE COMMANDS HEADER FILE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for file commands including    */
/*   batch, dribble-on, dribble-off, save, load, bsave, and  */
/*   bload.                                                  */
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
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added code for capturing errors/warnings.      */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */             
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_filecom

#pragma once

#define _H_filecom

   void                           FileCommandDefinitions(void *);
   intBool                        EnvDribbleOn(void *,const char *);
   intBool                        EnvDribbleActive(void *);
   intBool                        EnvDribbleOff(void *);
   void                           SetDribbleStatusFunction(void *,int (*)(void *,int));
   int                            LLGetcBatch(void *,const char *,int);
   int                            Batch(void *,const char *);
   int                            OpenBatch(void *,const char *,int);
   int                            OpenStringBatch(void *,const char *,const char *,int);
   int                            RemoveBatch(void *);
   intBool                        BatchActive(void *);
   void                           CloseAllBatchSources(void *);
   int                            BatchCommand(void *);
   int                            BatchStarCommand(void *);
   int                            EnvBatchStar(void *,const char *);
   int                            LoadCommand(void *);
   int                            LoadStarCommand(void *);
   int                            SaveCommand(void *);
   int                            DribbleOnCommand(void *);
   int                            DribbleOffCommand(void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                        DribbleActive(void);
   intBool                        DribbleOn(const char *);
   intBool                        DribbleOff(void);
   int                            BatchStar(const char *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_filecom */






