   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_modulbin

#pragma once

#define _H_modulbin

#include "moduldef.h"
#include "modulbin.h"
#include "cstrcbin.h"

#if (! RUN_TIME)

struct bsaveDefmodule
  {
   struct bsaveConstructHeader header;
   long importList;
   long exportList;
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

#define ModulePointer(i) ((Defmodule *) (&DefmoduleData(theEnv)->DefmoduleArray[i]))

   void                           DefmoduleBinarySetup(Environment *);
   void                           UpdateDefmoduleItemHeader
                                                 (Environment *,struct bsaveDefmoduleItemHeader *,
                                                  struct defmoduleItemHeader *,int,void *);

#if BLOAD_AND_BSAVE
   void                           AssignBsaveDefmdlItemHdrVals
                                                 (struct bsaveDefmoduleItemHeader *,
                                                  struct defmoduleItemHeader *);
#endif

#endif /* RUN_TIME */

#endif /* _H_modulbin */



