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
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_cstrcbin

#pragma once

#define _H_cstrcbin

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE

struct bsaveConstructHeader
  {
   long name;
   long whichModule;
   long next;
  };

#include "constrct.h"

#if BLOAD_AND_BSAVE
void MarkConstructHeaderNeededItems(struct constructHeader *,long);
void AssignBsaveConstructHeaderVals(struct bsaveConstructHeader *,
                                             struct constructHeader *);
#endif

void UpdateConstructHeader(void *,
                                  struct bsaveConstructHeader *,
                                  struct constructHeader *,int,void *,int,void *);
void UnmarkConstructHeader(void *,struct constructHeader *);

#endif

#endif




