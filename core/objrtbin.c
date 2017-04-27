   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/01/16             */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Binary Load/Save Functions Defrule               */
/*          Object Pattern Network                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            ResetObjectMatchTimeTags did not pass in the   */
/*            environment argument when BLOAD_ONLY was set.  */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Added support for hashed comparisons to        */
/*            constants.                                     */
/*                                                           */
/*            Added support for hashed alpha memories.       */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

/* =========================================
   *****************************************
               EXTERNAL DEFINITIONS
   =========================================
   ***************************************** */
#include "setup.h"

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM && (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE)

#include "bload.h"
#include "bsave.h"
#include "envrnmnt.h"
#include "memalloc.h"
#include "insfun.h"
#include "objrtmch.h"
#include "pattern.h"
#include "reteutil.h"
#include "rulebin.h"

#include "objrtbin.h"

/* =========================================
   *****************************************
               MACROS AND TYPES
   =========================================
   ***************************************** */
typedef unsigned long UNLN;

typedef struct bsaveObjectPatternNode
  {
   unsigned multifieldNode : 1;
   unsigned endSlot        : 1;
   unsigned selector       : 1;
   unsigned whichField     : 8;
   unsigned short leaveFields;
   unsigned short slotNameID;
   unsigned long networkTest,
        nextLevel,
        lastLevel,
        leftNode,
        rightNode,
        alphaNode;
  } BSAVE_OBJECT_PATTERN_NODE;

typedef struct bsaveObjectAlphaNode
  {
   struct bsavePatternNodeHeader header;
   unsigned long classbmp,
        slotbmp,
        patternNode,
        nxtInGroup,
        nxtTerminal;
  } BSAVE_OBJECT_ALPHA_NODE;

#define BsaveObjectPatternIndex(op) ((op != NULL) ? op->bsaveID : ULONG_MAX)
#define BsaveObjectAlphaIndex(ap)   ((ap != NULL) ? ap->bsaveID : ULONG_MAX)

#define ObjectPatternPointer(i) ((i == ULONG_MAX) ? NULL : (OBJECT_PATTERN_NODE *) &ObjectReteBinaryData(theEnv)->PatternArray[i])
#define ObjectAlphaPointer(i)   ((i == ULONG_MAX) ? NULL : (OBJECT_ALPHA_NODE *) &ObjectReteBinaryData(theEnv)->AlphaArray[i])

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if BLOAD_AND_BSAVE
   static void                    BsaveObjectPatternsFind(Environment *);
   static void                    BsaveStorageObjectPatterns(Environment *,FILE *);
   static void                    BsaveObjectPatterns(Environment *,FILE *);
#endif
   static void                    BloadStorageObjectPatterns(Environment *);
   static void                    BloadObjectPatterns(Environment *);
   static void                    UpdateAlpha(Environment *,void *,unsigned long);
   static void                    UpdatePattern(Environment *,void *,unsigned long);
   static void                    ClearBloadObjectPatterns(Environment *);
   static void                    DeallocateObjectReteBinaryData(Environment *);

/* =========================================
   *****************************************
          EXTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

/***********************************************************
  NAME         : SetupObjectPatternsBload
  DESCRIPTION  : Initializes data structures and
                   routines for binary loads of
                   generic function constructs
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Routines defined and structures initialized
  NOTES        : None
 ***********************************************************/
void SetupObjectPatternsBload(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,OBJECTRETEBIN_DATA,sizeof(struct objectReteBinaryData),DeallocateObjectReteBinaryData);

#if BLOAD_AND_BSAVE
   AddBinaryItem(theEnv,"object patterns",0,BsaveObjectPatternsFind,NULL,
                             BsaveStorageObjectPatterns,BsaveObjectPatterns,
                             BloadStorageObjectPatterns,BloadObjectPatterns,
                             ClearBloadObjectPatterns);
#endif
#if BLOAD || BLOAD_ONLY
   AddBinaryItem(theEnv,"object patterns",0,NULL,NULL,NULL,NULL,
                             BloadStorageObjectPatterns,BloadObjectPatterns,
                             ClearBloadObjectPatterns);
#endif
  }

/***********************************************************/
/* DeallocateObjectReteBinaryData: Deallocates environment */
/*    data for object rete binary functionality.           */
/***********************************************************/
static void DeallocateObjectReteBinaryData(
  Environment *theEnv)
  {
#if (BLOAD || BLOAD_ONLY || BLOAD_AND_BSAVE) && (! RUN_TIME)
   size_t space;
   unsigned long i;

   for (i = 0; i < ObjectReteBinaryData(theEnv)->AlphaNodeCount; i++)
     { DestroyAlphaMemory(theEnv,&ObjectReteBinaryData(theEnv)->AlphaArray[i].header,false); }

   space = ObjectReteBinaryData(theEnv)->AlphaNodeCount * sizeof(struct objectAlphaNode);
   if (space != 0) genfree(theEnv,ObjectReteBinaryData(theEnv)->AlphaArray,space);

   space = ObjectReteBinaryData(theEnv)->PatternNodeCount * sizeof(struct objectPatternNode);
   if (space != 0) genfree(theEnv,ObjectReteBinaryData(theEnv)->PatternArray,space);

#endif
  }

/* =========================================
   *****************************************
          INTERNALLY VISIBLE FUNCTIONS
   =========================================
   ***************************************** */

#if BLOAD_AND_BSAVE

/***************************************************
  NAME         : BsaveObjectPatternsFind
  DESCRIPTION  : Sets the Bsave IDs for the object
                 pattern data structures and
                 determines how much space
                 (including padding) is necessary
                 for the alpha node bitmPS
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Counts written
  NOTES        : None
 ***************************************************/
static void BsaveObjectPatternsFind(
  Environment *theEnv)
  {
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *patternPtr;

   SaveBloadCount(theEnv,ObjectReteBinaryData(theEnv)->AlphaNodeCount);
   SaveBloadCount(theEnv,ObjectReteBinaryData(theEnv)->PatternNodeCount);

   ObjectReteBinaryData(theEnv)->AlphaNodeCount = 0L;
   alphaPtr = ObjectNetworkTerminalPointer(theEnv);
   while (alphaPtr != NULL)
     {
      alphaPtr->classbmp->neededBitMap = true;
      if (alphaPtr->slotbmp != NULL)
        alphaPtr->slotbmp->neededBitMap = true;
      alphaPtr->bsaveID = ObjectReteBinaryData(theEnv)->AlphaNodeCount++;
      alphaPtr = alphaPtr->nxtTerminal;
     }

   ObjectReteBinaryData(theEnv)->PatternNodeCount = 0L;
   patternPtr = ObjectNetworkPointer(theEnv);
   while (patternPtr != NULL)
     {
      patternPtr->bsaveID = ObjectReteBinaryData(theEnv)->PatternNodeCount++;
      if (patternPtr->nextLevel == NULL)
        {
         while (patternPtr->rightNode == NULL)
           {
            patternPtr = patternPtr->lastLevel;
            if (patternPtr == NULL)
              return;
           }
         patternPtr = patternPtr->rightNode;
        }
      else
        patternPtr = patternPtr->nextLevel;
     }
  }

/****************************************************
  NAME         : BsaveStorageObjectPatterns
  DESCRIPTION  : Writes out the number of bytes
                 required for object pattern bitmaps,
                 and the number of object pattern
                 alpha an intermediate nodes
  INPUTS       : Bsave file stream pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Counts written
  NOTES        : None
 ****************************************************/
static void BsaveStorageObjectPatterns(
  Environment *theEnv,
  FILE *fp)
  {
   size_t space;

   space = sizeof(long) * 2;
   GenWrite(&space,sizeof(size_t),fp);
   GenWrite(&ObjectReteBinaryData(theEnv)->AlphaNodeCount,sizeof(long),fp);
   GenWrite(&ObjectReteBinaryData(theEnv)->PatternNodeCount,sizeof(long),fp);
  }

/***************************************************
  NAME         : BsaveObjectPatterns
  DESCRIPTION  : Writes ouyt object pattern data
                 structures to binary save file
  INPUTS       : Bsave file stream pointer
  RETURNS      : Nothing useful
  SIDE EFFECTS : Data structures written
  NOTES        : Extra padding written with alpha
                 node bitmaps to ensure correct
                 alignment of structues on bload
 ***************************************************/
static void BsaveObjectPatterns(
  Environment *theEnv,
  FILE *fp)
  {
   size_t space;
   OBJECT_ALPHA_NODE *alphaPtr;
   OBJECT_PATTERN_NODE *patternPtr;
   BSAVE_OBJECT_ALPHA_NODE dummyAlpha;
   BSAVE_OBJECT_PATTERN_NODE dummyPattern;

   space = (sizeof(BSAVE_OBJECT_ALPHA_NODE) * ObjectReteBinaryData(theEnv)->AlphaNodeCount) +
           (sizeof(BSAVE_OBJECT_PATTERN_NODE) * ObjectReteBinaryData(theEnv)->PatternNodeCount);
   GenWrite(&space,sizeof(size_t),fp);

   /* ==========================================
      Write out the alpha terminal pattern nodes
      ========================================== */
   alphaPtr = ObjectNetworkTerminalPointer(theEnv);
   while (alphaPtr != NULL)
     {
      AssignBsavePatternHeaderValues(theEnv,&dummyAlpha.header,&alphaPtr->header);
      dummyAlpha.classbmp = alphaPtr->classbmp->bucket;
      if (alphaPtr->slotbmp != NULL)
        dummyAlpha.slotbmp = alphaPtr->slotbmp->bucket;
      else
        dummyAlpha.slotbmp = ULONG_MAX;
      dummyAlpha.patternNode = BsaveObjectPatternIndex(alphaPtr->patternNode);
      dummyAlpha.nxtInGroup = BsaveObjectAlphaIndex(alphaPtr->nxtInGroup);
      dummyAlpha.nxtTerminal = BsaveObjectAlphaIndex(alphaPtr->nxtTerminal);
      GenWrite(&dummyAlpha,sizeof(BSAVE_OBJECT_ALPHA_NODE),fp);
      alphaPtr = alphaPtr->nxtTerminal;
     }

   /* ========================================
      Write out the intermediate pattern nodes
      ======================================== */
   patternPtr = ObjectNetworkPointer(theEnv);
   while (patternPtr != NULL)
     {
      dummyPattern.multifieldNode = patternPtr->multifieldNode;
      dummyPattern.whichField = patternPtr->whichField;
      dummyPattern.leaveFields = patternPtr->leaveFields;
      dummyPattern.endSlot = patternPtr->endSlot;
      dummyPattern.selector = patternPtr->selector;
      dummyPattern.slotNameID = patternPtr->slotNameID;
      dummyPattern.networkTest = HashedExpressionIndex(theEnv,patternPtr->networkTest);
      dummyPattern.nextLevel = BsaveObjectPatternIndex(patternPtr->nextLevel);
      dummyPattern.lastLevel = BsaveObjectPatternIndex(patternPtr->lastLevel);
      dummyPattern.leftNode = BsaveObjectPatternIndex(patternPtr->leftNode);
      dummyPattern.rightNode = BsaveObjectPatternIndex(patternPtr->rightNode);
      dummyPattern.alphaNode = BsaveObjectAlphaIndex(patternPtr->alphaNode);
      GenWrite(&dummyPattern,sizeof(BSAVE_OBJECT_PATTERN_NODE),fp);

      if (patternPtr->nextLevel == NULL)
        {
         while (patternPtr->rightNode == NULL)
           {
            patternPtr = patternPtr->lastLevel;
            if (patternPtr == NULL)
              {
               RestoreBloadCount(theEnv,&ObjectReteBinaryData(theEnv)->AlphaNodeCount);
               RestoreBloadCount(theEnv,&ObjectReteBinaryData(theEnv)->PatternNodeCount);
               return;
              }
           }
         patternPtr = patternPtr->rightNode;
        }
      else
        patternPtr = patternPtr->nextLevel;
     }

   RestoreBloadCount(theEnv,&ObjectReteBinaryData(theEnv)->AlphaNodeCount);
   RestoreBloadCount(theEnv,&ObjectReteBinaryData(theEnv)->PatternNodeCount);
  }

#endif

/***************************************************
  NAME         : BloadStorageObjectPatterns
  DESCRIPTION  : Reads in the storage requirements
                 for the object patterns in this
                 bload image
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Counts read and arrays allocated
  NOTES        : None
 ***************************************************/
static void BloadStorageObjectPatterns(
  Environment *theEnv)
  {
   size_t space;
   unsigned long counts[2];

   GenReadBinary(theEnv,&space,sizeof(size_t));
   GenReadBinary(theEnv,counts,space);
   ObjectReteBinaryData(theEnv)->AlphaNodeCount = counts[0];
   ObjectReteBinaryData(theEnv)->PatternNodeCount = counts[1];

   if (ObjectReteBinaryData(theEnv)->AlphaNodeCount == 0L)
     ObjectReteBinaryData(theEnv)->AlphaArray = NULL;
   else
     {
      space = (ObjectReteBinaryData(theEnv)->AlphaNodeCount * sizeof(OBJECT_ALPHA_NODE));
      ObjectReteBinaryData(theEnv)->AlphaArray = (OBJECT_ALPHA_NODE *) genalloc(theEnv,space);
     }
   if (ObjectReteBinaryData(theEnv)->PatternNodeCount == 0L)
     ObjectReteBinaryData(theEnv)->PatternArray = NULL;
   else
     {
      space = (ObjectReteBinaryData(theEnv)->PatternNodeCount * sizeof(OBJECT_PATTERN_NODE));
      ObjectReteBinaryData(theEnv)->PatternArray = (OBJECT_PATTERN_NODE *) genalloc(theEnv,space);
     }
  }

/****************************************************
  NAME         : BloadObjectPatterns
  DESCRIPTION  : Reads in all object pattern
                 data structures from binary
                 image and updates pointers
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Binary data structures updated
  NOTES        : Assumes storage allocated previously
 ****************************************************/
static void BloadObjectPatterns(
  Environment *theEnv)
  {
   size_t space;
   unsigned long i;

   GenReadBinary(theEnv,&space,sizeof(size_t));
   if (space == 0L)
     return;

   /* ================================================
      Read in the alpha and intermediate pattern nodes
      ================================================ */
   BloadandRefresh(theEnv,ObjectReteBinaryData(theEnv)->AlphaNodeCount,sizeof(BSAVE_OBJECT_ALPHA_NODE),UpdateAlpha);
   BloadandRefresh(theEnv,ObjectReteBinaryData(theEnv)->PatternNodeCount,sizeof(BSAVE_OBJECT_PATTERN_NODE),UpdatePattern);

   for (i = 0; i < ObjectReteBinaryData(theEnv)->PatternNodeCount; i++)
     {
      if ((ObjectReteBinaryData(theEnv)->PatternArray[i].lastLevel != NULL) &&
          (ObjectReteBinaryData(theEnv)->PatternArray[i].lastLevel->selector))
        {
         AddHashedPatternNode(theEnv,ObjectReteBinaryData(theEnv)->PatternArray[i].lastLevel,
                                     &ObjectReteBinaryData(theEnv)->PatternArray[i],
                                     ObjectReteBinaryData(theEnv)->PatternArray[i].networkTest->type,
                                     ObjectReteBinaryData(theEnv)->PatternArray[i].networkTest->value);
        }
     }

   /* =======================
      Set the global pointers
      ======================= */
   SetObjectNetworkTerminalPointer(theEnv,(OBJECT_ALPHA_NODE *) &ObjectReteBinaryData(theEnv)->AlphaArray[0]);
   SetObjectNetworkPointer(theEnv,(OBJECT_PATTERN_NODE *) &ObjectReteBinaryData(theEnv)->PatternArray[0]);
  }

/***************************************************
  NAME         : UpdateAlpha
  DESCRIPTION  : Updates all the pointers for an
                 alpha node based on the binary
                 image indices
  INPUTS       : 1) A pointer to the binary
                    image alpha node buffer
                 2) The index of the actual
                    alpha node in the array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Alpha node updated
  NOTES        : None
 ***************************************************/
static void UpdateAlpha(
  Environment *theEnv,
  void *buf,
  unsigned long obji)
  {
   BSAVE_OBJECT_ALPHA_NODE *bap;
   OBJECT_ALPHA_NODE *ap;

   bap = (BSAVE_OBJECT_ALPHA_NODE *) buf;
   ap = (OBJECT_ALPHA_NODE *) &ObjectReteBinaryData(theEnv)->AlphaArray[obji];

   UpdatePatternNodeHeader(theEnv,&ap->header,&bap->header);
   ap->matchTimeTag = 0L;
   ap->classbmp = BitMapPointer(bap->classbmp);
   if (bap->slotbmp != ULONG_MAX)
     {
      ap->slotbmp = BitMapPointer(bap->slotbmp);
      IncrementBitMapCount(ap->slotbmp);
     }
   else
     ap->slotbmp = NULL;
   IncrementBitMapCount(ap->classbmp);
   ap->patternNode = ObjectPatternPointer(bap->patternNode);
   ap->nxtInGroup = ObjectAlphaPointer(bap->nxtInGroup);
   ap->nxtTerminal = ObjectAlphaPointer(bap->nxtTerminal);
   ap->bsaveID = 0L;
  }

/***************************************************
  NAME         : UpdatePattern
  DESCRIPTION  : Updates all the pointers for a
                 pattern node based on the binary
                 image indices
  INPUTS       : 1) A pointer to the binary
                    image pattern node buffer
                 2) The index of the actual
                    pattern node in the array
  RETURNS      : Nothing useful
  SIDE EFFECTS : Pattern node updated
  NOTES        : None
 ***************************************************/
static void UpdatePattern(
  Environment *theEnv,
  void *buf,
  unsigned long obji)
  {
   BSAVE_OBJECT_PATTERN_NODE *bop;
   OBJECT_PATTERN_NODE *op;

   bop = (BSAVE_OBJECT_PATTERN_NODE *) buf;
   op = (OBJECT_PATTERN_NODE *) &ObjectReteBinaryData(theEnv)->PatternArray[obji];

   op->blocked = false;
   op->multifieldNode = bop->multifieldNode;
   op->whichField = bop->whichField;
   op->leaveFields = bop->leaveFields;
   op->endSlot = bop->endSlot;
   op->selector = bop->selector;
   op->matchTimeTag = 0L;
   op->slotNameID = bop->slotNameID;
   op->networkTest = HashedExpressionPointer(bop->networkTest);
   op->nextLevel = ObjectPatternPointer(bop->nextLevel);
   op->lastLevel = ObjectPatternPointer(bop->lastLevel);
   op->leftNode = ObjectPatternPointer(bop->leftNode);
   op->rightNode = ObjectPatternPointer(bop->rightNode);
   op->alphaNode = ObjectAlphaPointer(bop->alphaNode);
   op->bsaveID = 0L;
  }

/***************************************************
  NAME         : ClearBloadObjectPatterns
  DESCRIPTION  : Releases all emmory associated
                 with binary image object patterns
  INPUTS       : None
  RETURNS      : Nothing useful
  SIDE EFFECTS : Memory released and global
                 network pointers set to NULL
  NOTES        : None
 ***************************************************/
static void ClearBloadObjectPatterns(
  Environment *theEnv)
  {
   size_t space;
   unsigned long i;

   for (i = 0; i < ObjectReteBinaryData(theEnv)->PatternNodeCount; i++)
     {
      if ((ObjectReteBinaryData(theEnv)->PatternArray[i].lastLevel != NULL) &&
          (ObjectReteBinaryData(theEnv)->PatternArray[i].lastLevel->selector))
        {
         RemoveHashedPatternNode(theEnv,ObjectReteBinaryData(theEnv)->PatternArray[i].lastLevel,
                                        &ObjectReteBinaryData(theEnv)->PatternArray[i],
                                        ObjectReteBinaryData(theEnv)->PatternArray[i].networkTest->type,
                                        ObjectReteBinaryData(theEnv)->PatternArray[i].networkTest->value);
        }
     }

   /* ================================================
      All instances have been deleted by this point
      so we don't need to worry about clearing partial
      matches
      ================================================ */
   for (i = 0L ; i < ObjectReteBinaryData(theEnv)->AlphaNodeCount ; i++)
     {
      DecrementBitMapReferenceCount(theEnv,ObjectReteBinaryData(theEnv)->AlphaArray[i].classbmp);
      if (ObjectReteBinaryData(theEnv)->AlphaArray[i].slotbmp != NULL)
        DecrementBitMapReferenceCount(theEnv,ObjectReteBinaryData(theEnv)->AlphaArray[i].slotbmp);
     }

   if (ObjectReteBinaryData(theEnv)->AlphaNodeCount != 0L)
     {
      space = (ObjectReteBinaryData(theEnv)->AlphaNodeCount * sizeof(OBJECT_ALPHA_NODE));
      genfree(theEnv,ObjectReteBinaryData(theEnv)->AlphaArray,space);
      ObjectReteBinaryData(theEnv)->AlphaArray = NULL;
      ObjectReteBinaryData(theEnv)->AlphaNodeCount = 0;
      space = (ObjectReteBinaryData(theEnv)->PatternNodeCount * sizeof(OBJECT_PATTERN_NODE));
      genfree(theEnv,ObjectReteBinaryData(theEnv)->PatternArray,space);
      ObjectReteBinaryData(theEnv)->PatternArray = NULL;
      ObjectReteBinaryData(theEnv)->PatternNodeCount = 0;
     }

   SetObjectNetworkTerminalPointer(theEnv,NULL);
   SetObjectNetworkPointer(theEnv,NULL);
#if BLOAD_ONLY
   ResetObjectMatchTimeTags(theEnv);
#endif
  }

#endif

