   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                                                     */
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
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_clsltpsr

#pragma once

#define _H_clsltpsr

#if OBJECT_SYSTEM && (! BLOAD_ONLY) && (! RUN_TIME)

#define MATCH_RLN            "pattern-match"
#define REACTIVE_RLN         "reactive"
#define NONREACTIVE_RLN      "non-reactive"

#include "object.h"

typedef struct tempSlotLink
  {
   SLOT_DESC *desc;
   struct tempSlotLink *nxt;
  } TEMP_SLOT_LINK;

TEMP_SLOT_LINK *ParseSlot(void *,const char *,TEMP_SLOT_LINK *,PACKED_CLASS_LINKS *,int,int);
void DeleteSlots(void *,TEMP_SLOT_LINK *);

#endif

#endif





