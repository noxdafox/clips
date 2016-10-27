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
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
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
void MarkConstructHeaderNeededItems(ConstructHeader *,long);
void AssignBsaveConstructHeaderVals(struct bsaveConstructHeader *,
                                    ConstructHeader *);
#endif

void UpdateConstructHeader(Environment *,struct bsaveConstructHeader *,
                           ConstructHeader *,ConstructType,int,void *,int,
                           void *);
void UnmarkConstructHeader(Environment *,ConstructHeader *);

#endif

#endif




