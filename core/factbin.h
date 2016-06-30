   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*            FACT BLOAD/BSAVE HEADER FILE             */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_factbin

#pragma once

#define _H_factbin

#include "factbld.h"

#define FACTBIN_DATA 62

struct factBinaryData
  { 
   struct factPatternNode *FactPatternArray;
   long NumberOfPatterns;
  };
  
#define FactBinaryData(theEnv) ((struct factBinaryData *) GetEnvironmentData(theEnv,FACTBIN_DATA))

   void                           FactBinarySetup(void *);

#define BsaveFactPatternIndex(patPtr) ((patPtr == NULL) ? -1L : ((struct factPatternNode *) patPtr)->bsaveID)
#define BloadFactPatternPointer(i) ((struct factPatternNode *) ((i == -1L) ? NULL : &FactBinaryData(theEnv)->FactPatternArray[i]))

#endif

