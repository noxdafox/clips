   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                 SYMBOL HEADER FILE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Manages the atomic data value hash tables for    */
/*   storing symbols, integers, floats, and bit maps.        */
/*   Contains routines for adding entries, examining the     */
/*   hash tables, and performing garbage collection to       */
/*   remove entries no longer in use.                        */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: CLIPS crashing on AMD64 processor in the       */
/*            function used to generate a hash value for     */
/*            integers. DR0871                               */
/*                                                           */
/*            Support for run-time programs directly passing */
/*            the hash tables for initialization.            */
/*                                                           */
/*            Corrected code generating compilation          */
/*            warnings.                                      */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for hashing EXTERNAL_ADDRESS data      */
/*            type.                                          */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Added support for external address hash table  */
/*            and subtyping.                                 */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added ValueToPointer and EnvValueToPointer     */
/*            macros.                                        */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_symbol

#pragma once

#define _H_symbol

#include <stdlib.h>

typedef struct voidHashNode CLIPSVoid;
typedef struct symbolHashNode CLIPSLexeme;
typedef struct floatHashNode CLIPSFloat;
typedef struct integerHashNode CLIPSInteger;
typedef struct bitMapHashNode BITMAP_HN;
typedef struct externalAddressHashNode EXTERNAL_ADDRESS_HN;
typedef struct genericHashNode GENERIC_HN;

#include "evaluatn.h"

#ifndef SYMBOL_HASH_SIZE
#define SYMBOL_HASH_SIZE       63559L
#endif

#ifndef FLOAT_HASH_SIZE
#define FLOAT_HASH_SIZE         8191
#endif

#ifndef INTEGER_HASH_SIZE
#define INTEGER_HASH_SIZE       8191
#endif

#ifndef BITMAP_HASH_SIZE
#define BITMAP_HASH_SIZE        8191
#endif

#ifndef EXTERNAL_ADDRESS_HASH_SIZE
#define EXTERNAL_ADDRESS_HASH_SIZE        8191
#endif

/***************************/
/* voidHashNode STRUCTURE: */
/***************************/
struct voidHashNode
  {
   TypeHeader th;
  };

/*****************************/
/* symbolHashNode STRUCTURE: */
/*****************************/
struct symbolHashNode
  {
   TypeHeader th;
   struct symbolHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededSymbol : 1;
   unsigned int bucket : 29;
   const char *contents;
  };

/****************************/
/* floatHashNode STRUCTURE: */
/****************************/
struct floatHashNode
  {
   TypeHeader th;
   struct floatHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededFloat : 1;
   unsigned int bucket : 29;
   double contents;
  };

/******************************/
/* integerHashNode STRUCTURE: */
/******************************/
struct integerHashNode
  {
   TypeHeader th;
   struct integerHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededInteger : 1;
   unsigned int bucket : 29;
   long long contents;
  };

/*****************************/
/* bitMapHashNode STRUCTURE: */
/*****************************/
struct bitMapHashNode
  {
   TypeHeader th;
   struct bitMapHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededBitMap : 1;
   unsigned int bucket : 29;
   const char *contents;
   unsigned short size;
  };

/**************************************/
/* externalAddressHashNode STRUCTURE: */
/**************************************/
struct externalAddressHashNode
  {
   TypeHeader th;
   struct externalAddressHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int neededPointer : 1;
   unsigned int bucket : 29;
   void *externalAddress;
   unsigned short type;
  };

/******************************/
/* genericHashNode STRUCTURE: */
/******************************/
struct genericHashNode
  {
   TypeHeader th;
   struct genericHashNode *next;
   long count;
   unsigned int permanent : 1;
   unsigned int markedEphemeral : 1;
   unsigned int needed : 1;
   unsigned int bucket : 29;
  };

/**********************************************************/
/* EPHEMERON STRUCTURE: Data structure used to keep track */
/*   of ephemeral symbols, floats, and integers.          */
/*                                                        */
/*   associatedValue: Contains a pointer to the storage   */
/*   structure for the symbol, float, or integer which is */
/*   ephemeral.                                           */
/*                                                        */
/*   next: Contains a pointer to the next ephemeral item  */
/*   in a list of ephemeral items.                        */
/**********************************************************/
struct ephemeron
  {
   GENERIC_HN *associatedValue;
   struct ephemeron *next;
  };

/**************************/
/* symbolMatch STRUCTURE: */
/**************************/
struct symbolMatch
  {
   CLIPSLexeme *match;
   struct symbolMatch *next;
  };

#define ValueToBitMap(target) ((void *) ((struct bitMapHashNode *) (target))->contents)
#define ValueToPointer(target) ((void *) target)
#define ValueToExternalAddress(target) ((void *) ((struct externalAddressHashNode *) (target))->externalAddress)

#define IncrementSymbolCount(theValue) (((CLIPSLexeme *) theValue)->count++)
#define IncrementFloatCount(theValue) (((CLIPSFloat *) theValue)->count++)
#define IncrementIntegerCount(theValue) (((CLIPSInteger *) theValue)->count++)
#define IncrementBitMapCount(theValue) (((BITMAP_HN *) theValue)->count++)
#define IncrementExternalAddressCount(theValue) (((EXTERNAL_ADDRESS_HN *) theValue)->count++)

/*==================*/
/* ENVIRONMENT DATA */
/*==================*/

#define SYMBOL_DATA 49

struct symbolData
  {
   CLIPSLexeme *PositiveInfinity;
   CLIPSLexeme *NegativeInfinity;
   CLIPSInteger *Zero;
   CLIPSLexeme **SymbolTable;
   CLIPSFloat **FloatTable;
   CLIPSInteger **IntegerTable;
   BITMAP_HN **BitMapTable;
   EXTERNAL_ADDRESS_HN **ExternalAddressTable;
#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE || BLOAD_INSTANCES || BSAVE_INSTANCES
   long NumberOfSymbols;
   long NumberOfFloats;
   long NumberOfIntegers;
   long NumberOfBitMaps;
   long NumberOfExternalAddresses;
   CLIPSLexeme **SymbolArray;
   CLIPSFloat **FloatArray;
   CLIPSInteger **IntegerArray;
   BITMAP_HN **BitMapArray;
   EXTERNAL_ADDRESS_HN **ExternalAddressArray;
#endif
  };

#define SymbolData(theEnv) ((struct symbolData *) GetEnvironmentData(theEnv,SYMBOL_DATA))

   void                           InitializeAtomTables(Environment *,CLIPSLexeme **,CLIPSFloat **,
                                                              CLIPSInteger **,struct bitMapHashNode **,
                                                              struct externalAddressHashNode **);
   CLIPSLexeme                   *EnvAddSymbol(Environment *,const char *,unsigned short);
   CLIPSLexeme                   *FindSymbolHN(Environment *,const char *,unsigned short);
   CLIPSFloat                    *EnvCreateFloat(Environment *,double);
   CLIPSInteger                  *EnvCreateInteger(Environment *,long long);
   void                          *EnvAddBitMap(Environment *,void *,unsigned);
   void                          *EnvAddExternalAddress(Environment *,void *,unsigned);
   CLIPSInteger                  *FindLongHN(Environment *,long long);
   unsigned long                  HashSymbol(const char *,unsigned long);
   unsigned long                  HashFloat(double,unsigned long);
   unsigned long                  HashInteger(long long,unsigned long);
   unsigned long                  HashBitMap(const char *,unsigned long,unsigned);
   unsigned long                  HashExternalAddress(void *,unsigned long);
   void                           DecrementSymbolCount(Environment *,CLIPSLexeme *);
   void                           DecrementFloatCount(Environment *,CLIPSFloat *);
   void                           DecrementIntegerCount(Environment *,CLIPSInteger *);
   void                           DecrementBitMapCount(Environment *,struct bitMapHashNode *);
   void                           DecrementExternalAddressCount(Environment *,struct externalAddressHashNode *);
   void                           RemoveEphemeralAtoms(Environment *);
   CLIPSLexeme                  **GetSymbolTable(Environment *);
   void                           SetSymbolTable(Environment *,CLIPSLexeme **);
   CLIPSFloat                   **GetFloatTable(Environment *);
   void                           SetFloatTable(Environment *,CLIPSFloat **);
   CLIPSInteger                 **GetIntegerTable(Environment *);
   void                           SetIntegerTable(Environment *,CLIPSInteger **);
   struct bitMapHashNode        **GetBitMapTable(Environment *);
   void                           SetBitMapTable(Environment *,struct bitMapHashNode **);
   struct externalAddressHashNode
                                **GetExternalAddressTable(Environment *);
   void                           SetExternalAddressTable(Environment *,struct externalAddressHashNode **);
   void                           RefreshSpecialSymbols(Environment *);
   struct symbolMatch            *FindSymbolMatches(Environment *,const char *,unsigned *,size_t *);
   void                           ReturnSymbolMatches(Environment *,struct symbolMatch *);
   CLIPSLexeme                   *GetNextSymbolMatch(Environment *,const char *,size_t,CLIPSLexeme *,bool,size_t *);
   void                           ClearBitString(void *,unsigned);
   void                           SetAtomicValueIndices(Environment *,bool);
   void                           RestoreAtomicValueBuckets(Environment *);
   void                           EphemerateValue(Environment *,void *);
   CLIPSLexeme                   *EnvCreateSymbol(Environment *,const char *);
   CLIPSLexeme                   *EnvCreateString(Environment *,const char *);
   CLIPSLexeme                   *EnvCreateInstanceName(Environment *,const char *);
   CLIPSLexeme                   *EnvCreateBoolean(Environment *,bool);

#endif /* _H_symbol */



