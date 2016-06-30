   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*           DEFMODULE BSAVE/BLOAD HEADER FILE         */
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
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_modulbin

#pragma once

#define _H_modulbin

#include "moduldef.h"

struct bsaveDefmodule
  {
   unsigned long name;
   long importList;
   long exportList;
   long next;
   long bsaveID;
  };

struct bsaveDefmoduleItemHeader
  {
   long theModule;
   long firstItem;
   long lastItem;
  };

struct bsavePortItem
  {
   long moduleName;
   long constructType;
   long constructName;
   long next;
  };

#define ModulePointer(i) ((struct defmodule *) (&DefmoduleData(theEnv)->DefmoduleArray[i]))

   void                           DefmoduleBinarySetup(void *);
   void                           UpdateDefmoduleItemHeader
                                                 (void *,struct bsaveDefmoduleItemHeader *,
                                                  struct defmoduleItemHeader *,int,void *);

#if BLOAD_AND_BSAVE
   void                           AssignBsaveDefmdlItemHdrVals
                                                 (struct bsaveDefmoduleItemHeader *,
                                                  struct defmoduleItemHeader *);
#endif

#endif /* _H_modulbin */



