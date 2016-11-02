
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/01/16             */
   /*                                                     */
   /*     FACT PATTERN NETWORK CONSTRUCTS-TO-C MODULE     */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    fact pattern network.                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Added support for path name argument to        */
/*            constructs-to-c.                               */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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

#if DEFRULE_CONSTRUCT && (! RUN_TIME) && DEFTEMPLATE_CONSTRUCT && CONSTRUCT_COMPILER

#define FactPrefix() ArbitraryPrefix(FactData(theEnv)->FactCodeItem,0)

#include <stdio.h>

#include "factbld.h"
#include "factmngr.h"
#include "conscomp.h"
#include "factcmp.h"
#include "tmpltdef.h"
#include "envrnmnt.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static bool                    PatternNetworkToCode(Environment *,const char *,const char *,char *,int,FILE *,int,int);
   static void                    BeforePatternNetworkToCode(Environment *);
   static struct factPatternNode *GetNextPatternNode(struct factPatternNode *);
   static void                    CloseNetworkFiles(Environment *,FILE *,int);
   static void                    PatternNodeToCode(Environment *,FILE *,struct factPatternNode *,int,int);

/**************************************************************/
/* FactPatternsCompilerSetup: Initializes the constructs-to-c */
/*   command for use with the fact pattern network.           */
/**************************************************************/
void FactPatternsCompilerSetup(
  Environment *theEnv)
  {
   FactData(theEnv)->FactCodeItem = AddCodeGeneratorItem(theEnv,"facts",0,BeforePatternNetworkToCode,
                                       NULL,PatternNetworkToCode,1);
  }

/****************************************************************/
/* BeforePatternNetworkToCode: Assigns each pattern node with a */
/*   unique ID which will be used for pointer references when   */
/*   the data structures are written to a file as C code        */
/****************************************************************/
static void BeforePatternNetworkToCode(
  Environment *theEnv)
  {
   int whichPattern = 0;
   int whichDeftemplate = 0;
   Defmodule *theModule;
   Deftemplate *theDeftemplate;
   struct factPatternNode *thePattern;

   /*===========================*/
   /* Loop through each module. */
   /*===========================*/

   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
     {
      /*=========================*/
      /* Set the current module. */
      /*=========================*/

      EnvSetCurrentModule(theEnv,theModule);

      /*======================================================*/
      /* Loop through each deftemplate in the current module. */
      /*======================================================*/

      for (theDeftemplate = EnvGetNextDeftemplate(theEnv,NULL);
           theDeftemplate != NULL;
           theDeftemplate = EnvGetNextDeftemplate(theEnv,theDeftemplate))
        {
         /*=================================================*/
         /* Assign each pattern node in the pattern network */
         /* for the deftemplate a unique integer ID.        */
         /*=================================================*/

         theDeftemplate->header.bsaveID = whichDeftemplate++;
         for (thePattern = theDeftemplate->patternNetwork;
              thePattern != NULL;
              thePattern = GetNextPatternNode(thePattern))
           { thePattern->bsaveID = whichPattern++; }
        }
     }
  }

/********************************************************************/
/* GetNextPatternNode: Returns the next node in a pattern network   */
/*   tree. The next node is computed using a depth first traversal. */
/********************************************************************/
static struct factPatternNode *GetNextPatternNode(
  struct factPatternNode *thePattern)
  {
   /*=========================================*/
   /* If it's possible to go deeper into the  */
   /* tree, then move down to the next level. */
   /*=========================================*/

   if (thePattern->nextLevel != NULL) return(thePattern->nextLevel);

   /*========================================*/
   /* Keep backing up toward the root of the */
   /* tree until a side branch can be taken. */
   /*========================================*/

   while (thePattern->rightNode == NULL)
     {
      /*========================================*/
      /* Back up to check the next side branch. */
      /*========================================*/

      thePattern = thePattern->lastLevel;

      /*======================================*/
      /* If we branched up from the root, the */
      /* entire tree has been traversed.      */
      /*======================================*/

      if (thePattern == NULL) return NULL;
     }

   /*==================================*/
   /* Move on to the next side branch. */
   /*==================================*/

   return(thePattern->rightNode);
  }

/********************************************************************/
/* PatternNetworkToCode: Produces the fact pattern network code for */
/*   a run-time module created using the constructs-to-c function.  */
/********************************************************************/
static bool PatternNetworkToCode(
  Environment *theEnv,
  const char *fileName,
  const char *pathName,
  char *fileNameBuffer,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices)
  {
   int fileCount = 1;
   Defmodule *theModule;
   Deftemplate *theTemplate;
   struct factPatternNode *thePatternNode;
   int networkArrayCount = 0, networkArrayVersion = 1;
   FILE *networkFile = NULL;

   /*===========================================================*/
   /* Include the appropriate fact pattern network header file. */
   /*===========================================================*/

   fprintf(headerFP,"#include \"factbld.h\"\n");

   /*===============================*/
   /* Loop through all the modules. */
   /*===============================*/

   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
     {
      /*=========================*/
      /* Set the current module. */
      /*=========================*/

      EnvSetCurrentModule(theEnv,theModule);

      /*======================================*/
      /* Loop through all of the deftemplates */
      /* in the current module.               */
      /*======================================*/

      for (theTemplate = EnvGetNextDeftemplate(theEnv,NULL);
           theTemplate != NULL;
           theTemplate = EnvGetNextDeftemplate(theEnv,theTemplate))
        {
         /*======================================================*/
         /* Loop through each pattern node in the deftemplate's  */
         /* pattern network writing its C code representation to */
         /* the file as it is traversed.                         */
         /*======================================================*/

         for (thePatternNode = theTemplate->patternNetwork;
              thePatternNode != NULL;
              thePatternNode = GetNextPatternNode(thePatternNode))
           {
            networkFile = OpenFileIfNeeded(theEnv,networkFile,fileName,pathName,fileNameBuffer,fileID,imageID,&fileCount,
                                         networkArrayVersion,headerFP,
                                         "struct factPatternNode",FactPrefix(),false,NULL);
            if (networkFile == NULL)
              {
               CloseNetworkFiles(theEnv,networkFile,maxIndices);
               return false;
              }

            PatternNodeToCode(theEnv,networkFile,thePatternNode,imageID,maxIndices);
            networkArrayCount++;
            networkFile = CloseFileIfNeeded(theEnv,networkFile,&networkArrayCount,
                                            &networkArrayVersion,maxIndices,NULL,NULL);
           }
        }
     }

   /*==============================*/
   /* Close any C files left open. */
   /*==============================*/

   CloseNetworkFiles(theEnv,networkFile,maxIndices);

   /*===============================*/
   /* Return true to indicate the C */
   /* code was successfully saved.  */
   /*===============================*/

   return true;
  }

/****************************************************************/
/* CloseNetworkFiles: Closes all of the C files created for the */
/*   fact pattern network. Called when an error occurs or when  */
/*   the fact pattern network data structures have all been     */
/*   written to the files.                                      */
/****************************************************************/
static void CloseNetworkFiles(
  Environment *theEnv,
  FILE *networkFile,
  int maxIndices)
  {
   int count = maxIndices;
   int arrayVersion = 0;

   if (networkFile != NULL)
     {
      CloseFileIfNeeded(theEnv,networkFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }
  }

/************************************************************/
/* PatternNodeToCode: Writes the C code representation of a */
/*   single fact pattern node slot to the specified file.   */
/************************************************************/
static void PatternNodeToCode(
  Environment *theEnv,
  FILE *theFile,
  struct factPatternNode *thePatternNode,
  int imageID,
  int maxIndices)
  {
   fprintf(theFile,"{");

   /*=====================*/
   /* Pattern Node Header */
   /*=====================*/

   PatternNodeHeaderToCode(theEnv,theFile,&thePatternNode->header,imageID,maxIndices);

   /*========================*/
   /* Field and Slot Indices */
   /*========================*/

   fprintf(theFile,",0,%d,%d,%d,",thePatternNode->whichField,
                             thePatternNode->whichSlot,
                             thePatternNode->leaveFields);

   /*===============*/
   /* Network Tests */
   /*===============*/

   PrintHashedExpressionReference(theEnv,theFile,thePatternNode->networkTest,imageID,maxIndices);

   /*============*/
   /* Next Level */
   /*============*/

   if (thePatternNode->nextLevel == NULL)
     { fprintf(theFile,",NULL,"); }
   else
     {
      fprintf(theFile,",&%s%d_%ld[%ld],",FactPrefix(),
                 imageID,(thePatternNode->nextLevel->bsaveID / maxIndices) + 1,
                         thePatternNode->nextLevel->bsaveID % maxIndices);
     }

   /*============*/
   /* Last Level */
   /*============*/

   if (thePatternNode->lastLevel == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",FactPrefix(),
              imageID,(thePatternNode->lastLevel->bsaveID / maxIndices) + 1,
                   thePatternNode->lastLevel->bsaveID % maxIndices);
     }

   /*===========*/
   /* Left Node */
   /*===========*/

   if (thePatternNode->leftNode == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",FactPrefix(),
             imageID,(thePatternNode->leftNode->bsaveID / maxIndices) + 1,
                  thePatternNode->leftNode->bsaveID % maxIndices);
     }

   /*============*/
   /* Right Node */
   /*============*/

   if (thePatternNode->rightNode == NULL)
     { fprintf(theFile,"NULL}"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld]}",FactPrefix(),
            imageID,(thePatternNode->rightNode->bsaveID / maxIndices) + 1,
                thePatternNode->rightNode->bsaveID % maxIndices);
     }
  }

/**********************************************************/
/* FactPatternNodeReference: Prints C code representation */
/*   of a fact pattern node data structure reference.     */
/**********************************************************/
void FactPatternNodeReference(
  Environment *theEnv,
  void *theVPattern,
  FILE *theFile,
  int imageID,
  int maxIndices)
  {
   struct factPatternNode *thePattern = (struct factPatternNode *) theVPattern;

   if (thePattern == NULL) fprintf(theFile,"NULL");
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld]",FactPrefix(),
                    imageID,(thePattern->bsaveID / maxIndices) + 1,
                            thePattern->bsaveID % maxIndices);
     }
  }

#endif /* DEFRULE_CONSTRUCT && (! RUN_TIME) && DEFTEMPLATE_CONSTRUCT && CONSTRUCT_COMPILER */


