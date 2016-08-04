   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*          INSTANCE PRIMITIVE SUPPORT MODULE          */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_insmngr

#pragma once

#define _H_insmngr

#include "object.h"

   void                           InitializeInstanceCommand(Environment *,DATA_OBJECT *);
   void                           MakeInstanceCommand(Environment *,DATA_OBJECT *);
   SYMBOL_HN                     *GetFullInstanceName(Environment *,Instance *);
   Instance                      *BuildInstance(Environment *,SYMBOL_HN *,Defclass *,bool);
   void                           InitSlotsCommand(Environment *,DATA_OBJECT *);
   bool                           QuashInstance(Environment *,Instance *);

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           InactiveInitializeInstance(Environment *,DATA_OBJECT *);
   void                           InactiveMakeInstance(Environment *,DATA_OBJECT *);
#endif

#endif /* _H_insmngr */







