   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*            INSTANCE PRIMITIVE SUPPORT MODULE        */
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
/*************************************************************/

#ifndef _H_insmngr
#define _H_insmngr

#ifndef _H_object
#include "object.h"
#endif

   void                           InitializeInstanceCommand(void *,DATA_OBJECT *);
   void                           MakeInstanceCommand(void *,DATA_OBJECT *);
   SYMBOL_HN                     *GetFullInstanceName(void *,INSTANCE_TYPE *);
   INSTANCE_TYPE                 *BuildInstance(void *,SYMBOL_HN *,DEFCLASS *,intBool);
   void                           InitSlotsCommand(void *,DATA_OBJECT *);
   intBool                        QuashInstance(void *,INSTANCE_TYPE *);

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           InactiveInitializeInstance(void *,DATA_OBJECT *);
   void                           InactiveMakeInstance(void *,DATA_OBJECT *);
#endif

#endif /* _H_insmngr */







