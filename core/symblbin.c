   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
   /*                                                     */
   /*                 SYMBOL BSAVE MODULE                 */
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
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE || BLOAD_INSTANCES || BSAVE_INSTANCES

#include "argacces.h"
#include "bload.h"
#include "bsave.h"
#include "cstrnbin.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "moduldef.h"
#include "router.h"

#include "symblbin.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                        ReadNeededBitMaps(Environment *);
#if BLOAD_AND_BSAVE || BSAVE_INSTANCES
   static void                        WriteNeededBitMaps(Environment *,FILE *);
#endif

#if BLOAD_AND_BSAVE || BSAVE_INSTANCES

/**********************************************/
/* WriteNeededAtomicValues: Save all symbols, */
/*   floats, integers, and bitmaps needed by  */
/*   this binary image to the binary file.    */
/**********************************************/
void WriteNeededAtomicValues(
  Environment *theEnv,
  FILE *fp)
  {
   WriteNeededSymbols(theEnv,fp);
   WriteNeededFloats(theEnv,fp);
   WriteNeededIntegers(theEnv,fp);
   WriteNeededBitMaps(theEnv,fp);
  }

/********************************************************/
/* InitAtomicValueNeededFlags: Initializes all symbols, */
/*   floats, integers, and bitmaps as being unneeded by */
/*   the binary image being saved.                      */
/********************************************************/
void InitAtomicValueNeededFlags(
  Environment *theEnv)
  {
   unsigned long i;
   CLIPSLexeme *symbolPtr, **symbolArray;
   CLIPSFloat *floatPtr, **floatArray;
   CLIPSInteger *integerPtr, **integerArray;
   BITMAP_HN *bitMapPtr, **bitMapArray;

   /*===============*/
   /* Mark symbols. */
   /*===============*/

   symbolArray = GetSymbolTable(theEnv);

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      symbolPtr = symbolArray[i];
      while (symbolPtr != NULL)
        {
         symbolPtr->neededSymbol = false;
         symbolPtr = symbolPtr->next;
        }
     }

   /*==============*/
   /* Mark floats. */
   /*==============*/

   floatArray = GetFloatTable(theEnv);

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      floatPtr = floatArray[i];
      while (floatPtr != NULL)
        {
         floatPtr->neededFloat = false;
         floatPtr = floatPtr->next;
        }
     }

   /*================*/
   /* Mark integers. */
   /*================*/

   integerArray = GetIntegerTable(theEnv);

   for (i = 0; i < INTEGER_HASH_SIZE; i++)
     {
      integerPtr = integerArray[i];
      while (integerPtr != NULL)
        {
         integerPtr->neededInteger = false;
         integerPtr = integerPtr->next;
        }
     }

   /*===============*/
   /* Mark bitmaps. */
   /*===============*/

   bitMapArray = GetBitMapTable(theEnv);

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      bitMapPtr = bitMapArray[i];
      while (bitMapPtr != NULL)
        {
         bitMapPtr->neededBitMap = false;
         bitMapPtr = bitMapPtr->next;
        }
     }
  }

/*****************************************************************/
/* WriteNeededSymbols: Stores all of the symbols in the symbol   */
/*   table needed for this binary image in the binary save file. */
/*****************************************************************/
void WriteNeededSymbols(
  Environment *theEnv,
  FILE *fp)
  {
   unsigned long i;
   size_t length;
   CLIPSLexeme **symbolArray;
   CLIPSLexeme *symbolPtr;
   unsigned long int numberOfUsedSymbols = 0;
   size_t size = 0;

   /*=================================*/
   /* Get a copy of the symbol table. */
   /*=================================*/

   symbolArray = GetSymbolTable(theEnv);

   /*======================================================*/
   /* Get the number of symbols and the total string size. */
   /*======================================================*/

   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i];
           symbolPtr != NULL;
           symbolPtr = symbolPtr->next)
        {
         if (symbolPtr->neededSymbol)
           {
            numberOfUsedSymbols++;
            size += strlen(symbolPtr->contents) + 1;
           }
        }
     }

   /*=============================================*/
   /* Write out the symbols and the string sizes. */
   /*=============================================*/

   GenWrite(&numberOfUsedSymbols,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&size,(unsigned long) sizeof(unsigned long int),fp);

   /*=============================*/
   /* Write out the symbol types. */
   /*=============================*/
   
   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i];
           symbolPtr != NULL;
           symbolPtr = symbolPtr->next)
        {
         if (symbolPtr->neededSymbol)
           { GenWrite(&symbolPtr->th.type,sizeof(unsigned short),fp); }
        }
     }
     
   /*========================*/
   /* Write out the symbols. */
   /*========================*/
   
   for (i = 0; i < SYMBOL_HASH_SIZE; i++)
     {
      for (symbolPtr = symbolArray[i];
           symbolPtr != NULL;
           symbolPtr = symbolPtr->next)
        {
         if (symbolPtr->neededSymbol)
           {
            length = strlen(symbolPtr->contents) + 1;
            GenWrite((void *) symbolPtr->contents,(unsigned long) length,fp);
           }
        }
     }
  }

/*****************************************************************/
/* WriteNeededFloats: Stores all of the floats in the float      */
/*   table needed for this binary image in the binary save file. */
/*****************************************************************/
void WriteNeededFloats(
  Environment *theEnv,
  FILE *fp)
  {
   int i;
   CLIPSFloat **floatArray;
   CLIPSFloat *floatPtr;
   unsigned long int numberOfUsedFloats = 0;

   /*================================*/
   /* Get a copy of the float table. */
   /*================================*/

   floatArray = GetFloatTable(theEnv);

   /*===========================*/
   /* Get the number of floats. */
   /*===========================*/

   for (i = 0; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i];
           floatPtr != NULL;
           floatPtr = floatPtr->next)
        { if (floatPtr->neededFloat) numberOfUsedFloats++; }
     }

   /*======================================================*/
   /* Write out the number of floats and the float values. */
   /*======================================================*/

   GenWrite(&numberOfUsedFloats,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0 ; i < FLOAT_HASH_SIZE; i++)
     {
      for (floatPtr = floatArray[i];
           floatPtr != NULL;
           floatPtr = floatPtr->next)
        {
         if (floatPtr->neededFloat)
           { GenWrite(&floatPtr->contents,
                      (unsigned long) sizeof(floatPtr->contents),fp); }
        }
     }
  }

/******************************************************************/
/* WriteNeededIntegers: Stores all of the integers in the integer */
/*   table needed for this binary image in the binary save file.  */
/******************************************************************/
void WriteNeededIntegers(
  Environment *theEnv,
  FILE *fp)
  {
   int i;
   CLIPSInteger **integerArray;
   CLIPSInteger *integerPtr;
   unsigned long int numberOfUsedIntegers = 0;

   /*==================================*/
   /* Get a copy of the integer table. */
   /*==================================*/

   integerArray = GetIntegerTable(theEnv);

   /*=============================*/
   /* Get the number of integers. */
   /*=============================*/

   for (i = 0 ; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i];
           integerPtr != NULL;
           integerPtr = integerPtr->next)
        {
         if (integerPtr->neededInteger) numberOfUsedIntegers++;
        }
     }

   /*==========================================================*/
   /* Write out the number of integers and the integer values. */
   /*==========================================================*/

   GenWrite(&numberOfUsedIntegers,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0 ; i < INTEGER_HASH_SIZE; i++)
     {
      for (integerPtr = integerArray[i];
           integerPtr != NULL;
           integerPtr = integerPtr->next)
        {
         if (integerPtr->neededInteger)
           {
            GenWrite(&integerPtr->contents,
                     (unsigned long) sizeof(integerPtr->contents),fp);
           }
        }
     }
  }

/*****************************************************************/
/* WriteNeededBitMaps: Stores all of the bitmaps in the bitmap   */
/*   table needed for this binary image in the binary save file. */
/*****************************************************************/
static void WriteNeededBitMaps(
  Environment *theEnv,
  FILE *fp)
  {
   int i;
   BITMAP_HN **bitMapArray;
   BITMAP_HN *bitMapPtr;
   unsigned long int numberOfUsedBitMaps = 0, size = 0;
   unsigned short tempSize;

   /*=================================*/
   /* Get a copy of the bitmap table. */
   /*=================================*/

   bitMapArray = GetBitMapTable(theEnv);

   /*======================================================*/
   /* Get the number of bitmaps and the total bitmap size. */
   /*======================================================*/

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i];
           bitMapPtr != NULL;
           bitMapPtr = bitMapPtr->next)
        {
         if (bitMapPtr->neededBitMap)
           {
            numberOfUsedBitMaps++;
            size += (unsigned long) (bitMapPtr->size + sizeof(unsigned short));
           }
        }
     }

   /*========================================*/
   /* Write out the bitmaps and their sizes. */
   /*========================================*/

   GenWrite(&numberOfUsedBitMaps,(unsigned long) sizeof(unsigned long int),fp);
   GenWrite(&size,(unsigned long) sizeof(unsigned long int),fp);

   for (i = 0; i < BITMAP_HASH_SIZE; i++)
     {
      for (bitMapPtr = bitMapArray[i];
           bitMapPtr != NULL;
           bitMapPtr = bitMapPtr->next)
        {
         if (bitMapPtr->neededBitMap)
           {
            tempSize = (unsigned short) bitMapPtr->size;
            GenWrite(&tempSize,(unsigned long) sizeof(unsigned short),fp);
            GenWrite((void *) bitMapPtr->contents,(unsigned long) bitMapPtr->size,fp);
           }
        }
     }
  }

#endif /* BLOAD_AND_BSAVE || BSAVE_INSTANCES */

/*********************************************/
/* ReadNeededAtomicValues: Read all symbols, */
/*   floats, integers, and bitmaps needed by */
/*   this binary image from the binary file. */
/*********************************************/
void ReadNeededAtomicValues(
  Environment *theEnv)
  {
   ReadNeededSymbols(theEnv);
   ReadNeededFloats(theEnv);
   ReadNeededIntegers(theEnv);
   ReadNeededBitMaps(theEnv);
  }

/*******************************************/
/* ReadNeededSymbols: Reads in the symbols */
/*   used by the binary image.             */
/*******************************************/
void ReadNeededSymbols(
  Environment *theEnv)
  {
   char *symbolNames, *namePtr;
   unsigned long space;
   unsigned short *types;
   long i;

   /*=================================================*/
   /* Determine the number of symbol names to be read */
   /* and space required for them.                    */
   /*=================================================*/

   GenReadBinary(theEnv,&SymbolData(theEnv)->NumberOfSymbols,(unsigned long) sizeof(long int));
   GenReadBinary(theEnv,&space,(unsigned long) sizeof(unsigned long int));
   if (SymbolData(theEnv)->NumberOfSymbols == 0)
     {
      SymbolData(theEnv)->SymbolArray = NULL;
      return;
     }

   /*=======================================*/
   /* Allocate area for strings to be read. */
   /*=======================================*/
   
   types = (unsigned short *) gm3(theEnv,sizeof(unsigned short) * SymbolData(theEnv)->NumberOfSymbols);
   GenReadBinary(theEnv,types,sizeof(unsigned short) * SymbolData(theEnv)->NumberOfSymbols);

   symbolNames = (char *) gm3(theEnv,(long) space);
   GenReadBinary(theEnv,symbolNames,space);

   /*================================================*/
   /* Store the symbol pointers in the symbol array. */
   /*================================================*/

   SymbolData(theEnv)->SymbolArray = (CLIPSLexeme **)
                 gm3(theEnv,(long) sizeof(CLIPSLexeme *) * SymbolData(theEnv)->NumberOfSymbols);
   namePtr = symbolNames;
   for (i = 0; i < SymbolData(theEnv)->NumberOfSymbols; i++)
     {
      if (types[i] == SYMBOL)
        { SymbolData(theEnv)->SymbolArray[i] = EnvCreateSymbol(theEnv,namePtr); }
      else if (types[i] == STRING)
        { SymbolData(theEnv)->SymbolArray[i] = EnvCreateString(theEnv,namePtr); }
      else
        { SymbolData(theEnv)->SymbolArray[i] = EnvCreateInstanceName(theEnv,namePtr); }

      namePtr += strlen(namePtr) + 1;
     }

   /*=======================*/
   /* Free the name buffer. */
   /*=======================*/

   rm3(theEnv,types,sizeof(unsigned short) * SymbolData(theEnv)->NumberOfSymbols);
   rm3(theEnv,symbolNames,(long) space);
  }

/*****************************************/
/* ReadNeededFloats: Reads in the floats */
/*   used by the binary image.           */
/*****************************************/
void ReadNeededFloats(
  Environment *theEnv)
  {
   double *floatValues;
   long i;

   /*============================================*/
   /* Determine the number of floats to be read. */
   /*============================================*/

   GenReadBinary(theEnv,&SymbolData(theEnv)->NumberOfFloats,(unsigned long) sizeof(long int));
   if (SymbolData(theEnv)->NumberOfFloats == 0)
     {
      SymbolData(theEnv)->FloatArray = NULL;
      return;
     }

   /*===============================*/
   /* Allocate area for the floats. */
   /*===============================*/

   floatValues = (double *) gm3(theEnv,(long) sizeof(double) * SymbolData(theEnv)->NumberOfFloats);
   GenReadBinary(theEnv,floatValues,(unsigned long) (sizeof(double) * SymbolData(theEnv)->NumberOfFloats));

   /*======================================*/
   /* Store the floats in the float array. */
   /*======================================*/

   SymbolData(theEnv)->FloatArray = (CLIPSFloat **)
               gm3(theEnv,(long) sizeof(CLIPSFloat *) * SymbolData(theEnv)->NumberOfFloats);
   for (i = 0; i < SymbolData(theEnv)->NumberOfFloats; i++)
     { SymbolData(theEnv)->FloatArray[i] = (CLIPSFloat *) EnvCreateFloat(theEnv,floatValues[i]); }

   /*========================*/
   /* Free the float buffer. */
   /*========================*/

   rm3(theEnv,floatValues,(long) (sizeof(double) * SymbolData(theEnv)->NumberOfFloats));
  }

/*********************************************/
/* ReadNeededIntegers: Reads in the integers */
/*   used by the binary image.               */
/*********************************************/
void ReadNeededIntegers(
  Environment *theEnv)
  {
   long long *integerValues;
   long i;

   /*==============================================*/
   /* Determine the number of integers to be read. */
   /*==============================================*/

   GenReadBinary(theEnv,&SymbolData(theEnv)->NumberOfIntegers,(unsigned long) sizeof(unsigned long int));
   if (SymbolData(theEnv)->NumberOfIntegers == 0)
     {
      SymbolData(theEnv)->IntegerArray = NULL;
      return;
     }

   /*=================================*/
   /* Allocate area for the integers. */
   /*=================================*/

   integerValues = (long long *) gm3(theEnv,(long) (sizeof(long long) * SymbolData(theEnv)->NumberOfIntegers));
   GenReadBinary(theEnv,integerValues,(unsigned long) (sizeof(long long) * SymbolData(theEnv)->NumberOfIntegers));

   /*==========================================*/
   /* Store the integers in the integer array. */
   /*==========================================*/

   SymbolData(theEnv)->IntegerArray = (CLIPSInteger **)
           gm3(theEnv,(long) (sizeof(CLIPSInteger *) * SymbolData(theEnv)->NumberOfIntegers));
   for (i = 0; i < SymbolData(theEnv)->NumberOfIntegers; i++)
     { SymbolData(theEnv)->IntegerArray[i] = (CLIPSInteger *) EnvCreateInteger(theEnv,integerValues[i]); }

   /*==========================*/
   /* Free the integer buffer. */
   /*==========================*/

   rm3(theEnv,integerValues,(long) (sizeof(long long) * SymbolData(theEnv)->NumberOfIntegers));
  }

/*******************************************/
/* ReadNeededBitMaps: Reads in the bitmaps */
/*   used by the binary image.             */
/*******************************************/
static void ReadNeededBitMaps(
  Environment *theEnv)
  {
   char *bitMapStorage, *bitMapPtr;
   unsigned long space;
   long i;
   unsigned short *tempSize;

   /*=======================================*/
   /* Determine the number of bitmaps to be */
   /* read and space required for them.     */
   /*=======================================*/

   GenReadBinary(theEnv,&SymbolData(theEnv)->NumberOfBitMaps,(unsigned long) sizeof(long int));
   GenReadBinary(theEnv,&space,(unsigned long) sizeof(unsigned long int));
   if (SymbolData(theEnv)->NumberOfBitMaps == 0)
     {
      SymbolData(theEnv)->BitMapArray = NULL;
      return;
     }

   /*=======================================*/
   /* Allocate area for bitmaps to be read. */
   /*=======================================*/

   bitMapStorage = (char *) gm3(theEnv,(long) space);
   GenReadBinary(theEnv,bitMapStorage,space);

   /*================================================*/
   /* Store the bitMap pointers in the bitmap array. */
   /*================================================*/

   SymbolData(theEnv)->BitMapArray = (BITMAP_HN **)
                 gm3(theEnv,(long) sizeof(BITMAP_HN *) *  SymbolData(theEnv)->NumberOfBitMaps);
   bitMapPtr = bitMapStorage;
   for (i = 0; i < SymbolData(theEnv)->NumberOfBitMaps; i++)
     {
      tempSize = (unsigned short *) bitMapPtr;
      SymbolData(theEnv)->BitMapArray[i] = (BITMAP_HN *) EnvAddBitMap(theEnv,bitMapPtr+sizeof(unsigned short),*tempSize);
      bitMapPtr += *tempSize + sizeof(unsigned short);
     }

   /*=========================*/
   /* Free the bitmap buffer. */
   /*=========================*/

   rm3(theEnv,bitMapStorage,(long) space);
  }

/**********************************************************/
/* FreeAtomicValueStorage: Returns the memory allocated   */
/*   for storing the pointers to atomic data values used  */
/*   in refreshing expressions and other data structures. */
/**********************************************************/
void FreeAtomicValueStorage(
  Environment *theEnv)
  {
   if (SymbolData(theEnv)->SymbolArray != NULL)
     rm3(theEnv,SymbolData(theEnv)->SymbolArray,(long) sizeof(CLIPSLexeme *) * SymbolData(theEnv)->NumberOfSymbols);
   if (SymbolData(theEnv)->FloatArray != NULL)
     rm3(theEnv,SymbolData(theEnv)->FloatArray,(long) sizeof(CLIPSFloat *) * SymbolData(theEnv)->NumberOfFloats);
   if (SymbolData(theEnv)->IntegerArray != NULL)
     rm3(theEnv,SymbolData(theEnv)->IntegerArray,(long) sizeof(CLIPSInteger *) * SymbolData(theEnv)->NumberOfIntegers);
   if (SymbolData(theEnv)->BitMapArray != NULL)
     rm3(theEnv,SymbolData(theEnv)->BitMapArray,(long) sizeof(BITMAP_HN *) * SymbolData(theEnv)->NumberOfBitMaps);

   SymbolData(theEnv)->SymbolArray = NULL;
   SymbolData(theEnv)->FloatArray = NULL;
   SymbolData(theEnv)->IntegerArray = NULL;
   SymbolData(theEnv)->BitMapArray = NULL;
   SymbolData(theEnv)->NumberOfSymbols = 0;
   SymbolData(theEnv)->NumberOfFloats = 0;
   SymbolData(theEnv)->NumberOfIntegers = 0;
   SymbolData(theEnv)->NumberOfBitMaps = 0;
  }

#endif /* BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE || BLOAD_INSTANCES || BSAVE_INSTANCES */
