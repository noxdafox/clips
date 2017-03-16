   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.20  10/02/01            */
   /*                                                     */
   /*                STATUS HEADER FILE                   */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides dialogs for Windows interface.          */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Christopher J. Ortiz                                  */
/*                                                            */
/* Contributing Programmer(s):                                */
/*      Gary D. Riley                                         */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*************************************************************/

#ifndef _H_statusData

#define _H_statusData

#include "utility.h"

struct statusWindowData
  {
   char *baseName;
   int noLines;
   int lastLine;
   int lineSize;
   void (*getPPForm)(void *,StringBuilder *);
   void *(*getNextValue)(void *,void *);
   bool (*getChanged)(Environment *);
   void (*setChanged)(Environment *,bool);
   int (*getCount)(void *);
  };

#endif

