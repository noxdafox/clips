   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*      6.40: Split from filecom.c                           */
/*                                                           */
/*************************************************************/

#ifndef _H_fileutil

#pragma once

#define _H_fileutil

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
   bool                           EnvBatchStar(Environment *,const char *);

#endif /* _H_fileutil */






