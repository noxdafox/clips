   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_filecom

#pragma once

#define _H_filecom

   void                           FileCommandDefinitions(Environment *);
   bool                           EnvDribbleOn(Environment *,const char *);
   bool                           EnvDribbleActive(Environment *);
   bool                           EnvDribbleOff(Environment *);
   void                           SetDribbleStatusFunction(Environment *,int (*)(Environment *,bool));
   int                            LLGetcBatch(Environment *,const char *,bool);
   bool                           Batch(Environment *,const char *);
   bool                           OpenBatch(Environment *,const char *,bool);
   bool                           OpenStringBatch(Environment *,const char *,const char *,bool);
   bool                           RemoveBatch(Environment *);
   bool                           BatchActive(Environment *);
   void                           CloseAllBatchSources(Environment *);
   bool                           BatchCommand(Environment *);
   bool                           BatchStarCommand(Environment *);
   bool                           EnvBatchStar(Environment *,const char *);
   bool                           LoadCommand(Environment *);
   bool                           LoadStarCommand(Environment *);
   bool                           SaveCommand(Environment *);
   bool                           DribbleOnCommand(Environment *);
   bool                           DribbleOffCommand(Environment *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           DribbleActive(void);
   bool                           DribbleOn(const char *);
   bool                           DribbleOff(void);
   bool                           BatchStar(const char *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_filecom */






