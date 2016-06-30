   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*        SYMBOL CONSTRUCT COMPILER HEADER FILE        */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for       */
/*   atomic data values: symbols, integers, floats, and      */
/*   bit maps.                                               */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*      6.30: Added support for path name argument to        */
/*            constructs-to-c.                               */
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

#ifndef _H_symblcmp

#pragma once

#define _H_symblcmp

#include <stdio.h>

#include "symbol.h"

   void                     PrintSymbolReference(void *,FILE *,SYMBOL_HN *);
   void                     PrintFloatReference(void *,FILE *,FLOAT_HN *);
   void                     PrintIntegerReference(void *,FILE *,INTEGER_HN *);
   void                     PrintBitMapReference(void *,FILE *,BITMAP_HN *);
   void                     AtomicValuesToCode(void *,const char *,const char *,char *);

#endif /* _H_symblcmp */


