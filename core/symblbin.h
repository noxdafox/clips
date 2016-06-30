   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*           SYMBOL BINARY SAVE HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the binary save/load feature for      */
/*    atomic data values.                                    */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_symblbin

#pragma once

#define _H_symblbin

#include <stdio.h>

#include "symbol.h"

#define BitMapPointer(i) ((BITMAP_HN *) (SymbolData(theEnv)->BitMapArray[i]))
#define SymbolPointer(i) ((SYMBOL_HN *) (SymbolData(theEnv)->SymbolArray[i]))
#define FloatPointer(i) ((FLOAT_HN *) (SymbolData(theEnv)->FloatArray[i]))
#define IntegerPointer(i) ((INTEGER_HN *) (SymbolData(theEnv)->IntegerArray[i]))

   void                    MarkNeededAtomicValues(void);
   void                    WriteNeededAtomicValues(void *,FILE *);
   void                    ReadNeededAtomicValues(void *);
   void                    InitAtomicValueNeededFlags(void *);
   void                    FreeAtomicValueStorage(void *);
   void                    WriteNeededSymbols(void *,FILE *);
   void                    WriteNeededFloats(void *,FILE *);
   void                    WriteNeededIntegers(void *,FILE *);
   void                    ReadNeededSymbols(void *);
   void                    ReadNeededFloats(void *);
   void                    ReadNeededIntegers(void *);

#endif /* _H_symblbin */



