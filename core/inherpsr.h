   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_inherpsr

#pragma once

#define _H_inherpsr

#if OBJECT_SYSTEM && (! BLOAD_ONLY) && (! RUN_TIME)

#include "object.h"

   PACKED_CLASS_LINKS            *ParseSuperclasses(Environment *,const char *,SYMBOL_HN *);
   PACKED_CLASS_LINKS            *FindPrecedenceList(Environment *,Defclass *,PACKED_CLASS_LINKS *);
   void                           PackClassLinks(Environment *,PACKED_CLASS_LINKS *,CLASS_LINK *);

#endif /* OBJECT_SYSTEM && (! BLOAD_ONLY) && (! RUN_TIME) */

#endif /* _H_inherpsr */



