
   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/01/16             */
   /*                                                     */
   /*            DEFRULE CONSTRUCTS-TO-C MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    defrule construct.                                     */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed DYNAMIC_SALIENCE and                   */
/*            LOGICAL_DEPENDENCIES compilation flags.        */
/*                                                           */
/*      6.30: Added support for path name argument to        */
/*            constructs-to-c.                               */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Support for join network changes.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.31: Fixed disjunct bug in defrule iteration.       */
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

#if DEFRULE_CONSTRUCT && (! RUN_TIME) && CONSTRUCT_COMPILER

#include <stdio.h>
#include <string.h>

#include "envrnmnt.h"
#include "factbld.h"
#include "pattern.h"
#include "reteutil.h"

#include "rulecmp.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static bool                    ConstructToCode(Environment *,const char *,const char *,char *,int,FILE *,int,int);
   static void                    JoinToCode(Environment *,FILE *,struct joinNode *,int,int);
   static void                    LinkToCode(Environment *,FILE *,struct joinLink *,int,int);
   static void                    DefruleModuleToCode(Environment *,FILE *,Defmodule *,int,int,int);
   static void                    DefruleToCode(Environment *,FILE *,Defrule *,int,int,int);
   static void                    CloseDefruleFiles(Environment *,FILE *,FILE *,FILE *,FILE*,int);
   static void                    BeforeDefrulesCode(Environment *);
   static void                    InitDefruleCode(Environment *,FILE *,int,int);
   static bool                    RuleCompilerTraverseJoins(Environment *,struct joinNode *,const char *,const char *,char *,int,
                                                            FILE *,int,int,FILE **,FILE **,
                                                            int *,int *,int *,int *,int *);
   static bool                    TraverseJoinLinks(Environment *,struct joinLink *,const char *,const char *,char *,int,FILE *,
                                                    int,int,FILE **,int *,int *, int *);

/***********************************************************/
/* DefruleCompilerSetup: Initializes the defrule construct */
/*   for use with the constructs-to-c command.             */
/***********************************************************/
void DefruleCompilerSetup(
  Environment *theEnv)
  {
   DefruleData(theEnv)->DefruleCodeItem = AddCodeGeneratorItem(theEnv,"defrules",0,BeforeDefrulesCode,
                                          InitDefruleCode,ConstructToCode,4);
  }

/**************************************************************/
/* BeforeDefrulesCode: Assigns each defrule and join with a   */
/*   unique ID which will be used for pointer references when */
/*   the data structures are written to a file as C code      */
/**************************************************************/
static void BeforeDefrulesCode(
  Environment *theEnv)
  {
   long int moduleCount, ruleCount, joinCount, linkCount;

   TagRuleNetwork(theEnv,&moduleCount,&ruleCount,&joinCount,&linkCount);
  }

/*********************************************************/
/* ConstructToCode: Produces defrule code for a run-time */
/*   module created using the constructs-to-c function.  */
/*********************************************************/
static bool ConstructToCode(
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
   Defrule *theDefrule, *theDisjunct;
   int joinArrayCount = 0, joinArrayVersion = 1;
   int linkArrayCount = 0, linkArrayVersion = 1;
   int moduleCount = 0, moduleArrayCount = 0, moduleArrayVersion = 1;
   int defruleArrayCount = 0, defruleArrayVersion = 1;
   FILE *joinFile = NULL, *moduleFile = NULL, *defruleFile = NULL, *linkFile = NULL;

   /*==============================================*/
   /* Include the appropriate defrule header file. */
   /*==============================================*/

   fprintf(headerFP,"#include \"ruledef.h\"\n");

   /*======================================*/
   /* Save the left and right prime links. */
   /*======================================*/

   if (! TraverseJoinLinks(theEnv,DefruleData(theEnv)->LeftPrimeJoins,fileName,pathName,fileNameBuffer,fileID,headerFP,imageID,
                           maxIndices,&linkFile,&fileCount,&linkArrayVersion,&linkArrayCount))
     {
      CloseDefruleFiles(theEnv,moduleFile,defruleFile,joinFile,linkFile,maxIndices);
      return false;
     }

   if (! TraverseJoinLinks(theEnv,DefruleData(theEnv)->RightPrimeJoins,fileName,pathName,fileNameBuffer,fileID,headerFP,imageID,
                           maxIndices,&linkFile,&fileCount,&linkArrayVersion,&linkArrayCount))
     {
      CloseDefruleFiles(theEnv,moduleFile,defruleFile,joinFile,linkFile,maxIndices);
      return false;
     }

   /*=========================================================*/
   /* Loop through all the modules, all the defrules, and all */
   /* the join nodes writing their C code representation to   */
   /* the file as they are traversed.                         */
   /*=========================================================*/

   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
     {
      /*=========================*/
      /* Set the current module. */
      /*=========================*/

      EnvSetCurrentModule(theEnv,theModule);

      /*==========================*/
      /* Save the defrule module. */
      /*==========================*/

      moduleFile = OpenFileIfNeeded(theEnv,moduleFile,fileName,pathName,fileNameBuffer,fileID,imageID,&fileCount,
                                    moduleArrayVersion,headerFP,
                                    "struct defruleModule",ModulePrefix(DefruleData(theEnv)->DefruleCodeItem),
                                    false,NULL);

      if (moduleFile == NULL)
        {
         CloseDefruleFiles(theEnv,moduleFile,defruleFile,joinFile,linkFile,maxIndices);
         return false;
        }

      DefruleModuleToCode(theEnv,moduleFile,theModule,imageID,maxIndices,moduleCount);
      moduleFile = CloseFileIfNeeded(theEnv,moduleFile,&moduleArrayCount,&moduleArrayVersion,
                                     maxIndices,NULL,NULL);

      /*=========================================*/
      /* Loop through all of the defrules (and   */
      /* their disjuncts) in the current module. */
      /*=========================================*/

      for (theDefrule = EnvGetNextDefrule(theEnv,NULL);
           theDefrule != NULL;
           theDefrule = EnvGetNextDefrule(theEnv,theDefrule))
        {
         for (theDisjunct = theDefrule;
              theDisjunct != NULL;
              theDisjunct = theDisjunct->disjunct)
           {
            /*===================================*/
            /* Save the defrule data structures. */
            /*===================================*/

            defruleFile = OpenFileIfNeeded(theEnv,defruleFile,fileName,pathName,fileNameBuffer,fileID,imageID,&fileCount,
                                           defruleArrayVersion,headerFP,
                                           "Defrule",ConstructPrefix(DefruleData(theEnv)->DefruleCodeItem),
                                           false,NULL);
            if (defruleFile == NULL)
              {
               CloseDefruleFiles(theEnv,moduleFile,defruleFile,joinFile,linkFile,maxIndices);
               return false;
              }

            DefruleToCode(theEnv,defruleFile,theDisjunct,imageID,maxIndices,
                           moduleCount);
            defruleArrayCount++;
            defruleFile = CloseFileIfNeeded(theEnv,defruleFile,&defruleArrayCount,&defruleArrayVersion,
                                            maxIndices,NULL,NULL);

            /*================================*/
            /* Save the join data structures. */
            /*================================*/

            if (! RuleCompilerTraverseJoins(theEnv,theDisjunct->lastJoin,fileName,pathName,fileNameBuffer,fileID,headerFP,imageID,
                                            maxIndices,&joinFile,&linkFile,&fileCount,&joinArrayVersion,&joinArrayCount,
                                            &linkArrayVersion,&linkArrayCount))
              {
               CloseDefruleFiles(theEnv,moduleFile,defruleFile,joinFile,linkFile,maxIndices);
               return false;
              }
           }
        }

      moduleCount++;
      moduleArrayCount++;
     }

   CloseDefruleFiles(theEnv,moduleFile,defruleFile,joinFile,linkFile,maxIndices);

   return true;
  }

/*********************************************************************/
/* RuleCompilerTraverseJoins: Traverses the join network for a rule. */
/*********************************************************************/
static bool RuleCompilerTraverseJoins(
  Environment *theEnv,
  struct joinNode *joinPtr,
  const char *fileName,
  const char *pathName,
  char *fileNameBuffer,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices,
  FILE **joinFile,
  FILE **linkFile,
  int *fileCount,
  int *joinArrayVersion,
  int *joinArrayCount,
  int *linkArrayVersion,
  int *linkArrayCount)
  {
   for (;
        joinPtr != NULL;
        joinPtr = joinPtr->lastLevel)
     {
      if (joinPtr->marked)
        {
         *joinFile = OpenFileIfNeeded(theEnv,*joinFile,fileName,pathName,fileNameBuffer,fileID,imageID,fileCount,
                                      *joinArrayVersion,headerFP,
                                      "struct joinNode",JoinPrefix(),false,NULL);
         if (*joinFile == NULL)
           { return false; }

         JoinToCode(theEnv,*joinFile,joinPtr,imageID,maxIndices);
         (*joinArrayCount)++;
         *joinFile = CloseFileIfNeeded(theEnv,*joinFile,joinArrayCount,joinArrayVersion,
                                       maxIndices,NULL,NULL);


         if (! TraverseJoinLinks(theEnv,joinPtr->nextLinks,fileName,pathName,fileNameBuffer,fileID,headerFP,imageID,
                                 maxIndices,linkFile,fileCount,linkArrayVersion,linkArrayCount))
           { return false; }
        }

      if (joinPtr->joinFromTheRight)
        {
         if (RuleCompilerTraverseJoins(theEnv,(struct joinNode *) joinPtr->rightSideEntryStructure,fileName,pathName,
                                       fileNameBuffer,fileID,headerFP,imageID,maxIndices,joinFile,linkFile,fileCount,
                                       joinArrayVersion,joinArrayCount,
                                       linkArrayVersion,linkArrayCount) == false)
           { return false; }
        }
     }

   return true;
  }

/*******************************************************/
/* TraverseJoinLinks: Writes out a list of join links. */
/*******************************************************/
static bool TraverseJoinLinks(
  Environment *theEnv,
  struct joinLink *linkPtr,
  const char *fileName,
  const char *pathName,
  char *fileNameBuffer,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices,
  FILE **linkFile,
  int *fileCount,
  int *linkArrayVersion,
  int *linkArrayCount)
  {
   for (;
        linkPtr != NULL;
        linkPtr = linkPtr->next)
     {
      *linkFile = OpenFileIfNeeded(theEnv,*linkFile,fileName,pathName,fileNameBuffer,fileID,imageID,fileCount,
                                   *linkArrayVersion,headerFP,
                                   "struct joinLink",LinkPrefix(),false,NULL);

      if (*linkFile == NULL)
        { return false; }

      LinkToCode(theEnv,*linkFile,linkPtr,imageID,maxIndices);
      (*linkArrayCount)++;
      *linkFile = CloseFileIfNeeded(theEnv,*linkFile,linkArrayCount,linkArrayVersion,
                                    maxIndices,NULL,NULL);
     }

   return true;
  }

/********************************************************/
/* CloseDefruleFiles: Closes all of the C files created */
/*   for defrule. Called when an error occurs or when   */
/*   the defrules have all been written to the files.   */
/********************************************************/
static void CloseDefruleFiles(
  Environment *theEnv,
  FILE *moduleFile,
  FILE *defruleFile,
  FILE *joinFile,
  FILE *linkFile,
  int maxIndices)
  {
   int count = maxIndices;
   int arrayVersion = 0;

   if (linkFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(theEnv,linkFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (joinFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(theEnv,joinFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (defruleFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(theEnv,defruleFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }

   if (moduleFile != NULL)
     {
      count = maxIndices;
      CloseFileIfNeeded(theEnv,moduleFile,&count,&arrayVersion,maxIndices,NULL,NULL);
     }
  }

/*********************************************************/
/* DefruleModuleToCode: Writes the C code representation */
/*   of a single defrule module to the specified file.   */
/*********************************************************/
static void DefruleModuleToCode(
  Environment *theEnv,
  FILE *theFile,
  Defmodule *theModule,
  int imageID,
  int maxIndices,
  int moduleCount)
  {
#if MAC_XCD
#pragma unused(moduleCount)
#endif

   fprintf(theFile,"{");

   ConstructModuleToCode(theEnv,theFile,theModule,imageID,maxIndices,
                                  DefruleData(theEnv)->DefruleModuleIndex,ConstructPrefix(DefruleData(theEnv)->DefruleCodeItem));

   fprintf(theFile,",NULL}");
  }

/**********************************************************/
/* DefruleToCode: Writes the C code representation of a   */
/*   single defrule data structure to the specified file. */
/**********************************************************/
static void DefruleToCode(
  Environment *theEnv,
  FILE *theFile,
  Defrule *theDefrule,
  int imageID,
  int maxIndices,
  int moduleCount)
  {
   /*==================*/
   /* Construct Header */
   /*==================*/

   fprintf(theFile,"{");

   ConstructHeaderToCode(theEnv,theFile,&theDefrule->header,imageID,maxIndices,
                                  moduleCount,ModulePrefix(DefruleData(theEnv)->DefruleCodeItem),
                                  ConstructPrefix(DefruleData(theEnv)->DefruleCodeItem));

   /*==========================*/
   /* Flags and Integer Values */
   /*==========================*/

   fprintf(theFile,",%d,%d,%d,%d,%d,%d,%d,%d,",
                   theDefrule->salience,theDefrule->localVarCnt,
                   theDefrule->complexity,theDefrule->afterBreakpoint,
                   theDefrule->watchActivation,theDefrule->watchFiring,
                   theDefrule->autoFocus,theDefrule->executing);

   /*==================*/
   /* Dynamic Salience */
   /*==================*/

   ExpressionToCode(theEnv,theFile,theDefrule->dynamicSalience);
   fprintf(theFile,",");

   /*=============*/
   /* RHS Actions */
   /*=============*/

   ExpressionToCode(theEnv,theFile,theDefrule->actions);
   fprintf(theFile,",");

   /*=========================*/
   /* Logical Dependency Join */
   /*=========================*/

   if (theDefrule->logicalJoin != NULL)
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                     imageID,(theDefrule->logicalJoin->bsaveID / maxIndices) + 1,
                             theDefrule->logicalJoin->bsaveID % maxIndices);
     }
   else
     { fprintf(theFile,"NULL,"); }

   /*===========*/
   /* Last Join */
   /*===========*/

   if (theDefrule->lastJoin != NULL)
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                     imageID,(theDefrule->lastJoin->bsaveID / maxIndices) + 1,
                             theDefrule->lastJoin->bsaveID % maxIndices);
     }
   else
     { fprintf(theFile,"NULL,"); }

   /*===============*/
   /* Next Disjunct */
   /*===============*/

   if (theDefrule->disjunct != NULL)
     {
      fprintf(theFile,"&%s%d_%ld[%ld]}",ConstructPrefix(DefruleData(theEnv)->DefruleCodeItem),
                     imageID,(theDefrule->disjunct->header.bsaveID / maxIndices) + 1,
                             theDefrule->disjunct->header.bsaveID % maxIndices);
     }
   else
     { fprintf(theFile,"NULL}"); }
  }

/***************************************************/
/* JoinToCode: Writes the C code representation of */
/*   a single join node to the specified file.     */
/***************************************************/
static void JoinToCode(
  Environment *theEnv,
  FILE *joinFile,
  struct joinNode *theJoin,
  int imageID,
  int maxIndices)
  {
   struct patternParser *theParser;

   /*===========================*/
   /* Mark the join as visited. */
   /*===========================*/

   theJoin->marked = 0;

   /*===========================*/
   /* Flags and Integer Values. */
   /*===========================*/

   fprintf(joinFile,"{%d,%d,%d,%d,%d,0,0,%d,%d,0,0,0,0,0,0,",
                   theJoin->firstJoin,theJoin->logicalJoin,
                   theJoin->joinFromTheRight,theJoin->patternIsNegated,
                   theJoin->patternIsExists,
                   // initialize,
                   // marked
                   theJoin->rhsType,theJoin->depth);
                   // bsaveID
                   // memoryLeftAdds
                   // memoryRightAdds
                   // memoryLeftDeletes
                   // memoryRightDeletes
                   // memoryCompares

   /*==========================*/
   /* Left and right Memories. */
   /*==========================*/

   fprintf(joinFile,"NULL,NULL,");

   /*====================*/
   /* Network Expression */
   /*====================*/

   PrintHashedExpressionReference(theEnv,joinFile,theJoin->networkTest,imageID,maxIndices);
   fprintf(joinFile,",");

   PrintHashedExpressionReference(theEnv,joinFile,theJoin->secondaryNetworkTest,imageID,maxIndices);
   fprintf(joinFile,",");

   PrintHashedExpressionReference(theEnv,joinFile,theJoin->leftHash,imageID,maxIndices);
   fprintf(joinFile,",");

   PrintHashedExpressionReference(theEnv,joinFile,theJoin->rightHash,imageID,maxIndices);
   fprintf(joinFile,",");

   /*============================*/
   /* Right Side Entry Structure */
   /*============================*/

   if (theJoin->rightSideEntryStructure == NULL)
     { fprintf(joinFile,"NULL,"); }
   else if (theJoin->joinFromTheRight == false)
     {
      theParser = GetPatternParser(theEnv,(int) theJoin->rhsType);
      if (theParser->codeReferenceFunction == NULL) fprintf(joinFile,"NULL,");
      else
        {
         fprintf(joinFile,"VS ");
         (*theParser->codeReferenceFunction)(theEnv,theJoin->rightSideEntryStructure,
                                             joinFile,imageID,maxIndices);
         fprintf(joinFile,",");
        }
     }
   else
     {
      fprintf(joinFile,"&%s%d_%ld[%ld],",JoinPrefix(),
              imageID,(((struct joinNode *) theJoin->rightSideEntryStructure)->bsaveID / maxIndices) + 1,
                      ((struct joinNode *) theJoin->rightSideEntryStructure)->bsaveID % maxIndices);
     }

   /*=================*/
   /* Next Join Level */
   /*=================*/

   if (theJoin->nextLinks == NULL)
     { fprintf(joinFile,"NULL,"); }
   else
     {
      fprintf(joinFile,"&%s%d_%ld[%ld],",LinkPrefix(),
                    imageID,(theJoin->nextLinks->bsaveID / maxIndices) + 1,
                            theJoin->nextLinks->bsaveID % maxIndices);
     }

   /*=================*/
   /* Last Join Level */
   /*=================*/

   if (theJoin->lastLevel == NULL)
     { fprintf(joinFile,"NULL,"); }
   else
     {
      fprintf(joinFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                    imageID,(theJoin->lastLevel->bsaveID / maxIndices) + 1,
                            theJoin->lastLevel->bsaveID % maxIndices);
     }

   /*==================*/
   /* Right Match Node */
   /*==================*/

   if (theJoin->rightMatchNode == NULL)
     { fprintf(joinFile,"NULL,"); }
   else
     {
      fprintf(joinFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                    imageID,(theJoin->rightMatchNode->bsaveID / maxIndices) + 1,
                            theJoin->rightMatchNode->bsaveID % maxIndices);
     }

   /*==================*/
   /* Rule to Activate */
   /*==================*/

   if (theJoin->ruleToActivate == NULL)
     { fprintf(joinFile,"NULL}"); }
   else
     {
      fprintf(joinFile,"&%s%d_%ld[%ld]}",ConstructPrefix(DefruleData(theEnv)->DefruleCodeItem),imageID,
                                    (theJoin->ruleToActivate->header.bsaveID / maxIndices) + 1,
                                    theJoin->ruleToActivate->header.bsaveID % maxIndices);
     }
  }

/***************************************************/
/* LinkToCode: Writes the C code representation of */
/*   a single join node to the specified file.     */
/***************************************************/
static void LinkToCode(
  Environment *theEnv,
  FILE *theFile,
  struct joinLink *theLink,
  int imageID,
  int maxIndices)
  {
   /*==================*/
   /* Enter Direction. */
   /*==================*/

   fprintf(theFile,"{%d,",theLink->enterDirection);

   /*======*/
   /* Join */
   /*======*/

   if (theLink->join == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",JoinPrefix(),
                    imageID,(theLink->join->bsaveID / maxIndices) + 1,
                            theLink->join->bsaveID % maxIndices);
     }

   /*======*/
   /* Next */
   /*======*/

   if (theLink->next == NULL)
     { fprintf(theFile,"NULL,"); }
   else
     {
      fprintf(theFile,"&%s%d_%ld[%ld],",LinkPrefix(),
                    imageID,(theLink->next->bsaveID / maxIndices) + 1,
                            theLink->next->bsaveID % maxIndices);
     }

   /*===========*/
   /* Bsave ID. */
   /*===========*/

   fprintf(theFile,"0}");
  }

/*************************************************************/
/* DefruleCModuleReference: Writes the C code representation */
/*   of a reference to a defrule module data structure.      */
/*************************************************************/
void DefruleCModuleReference(
  Environment *theEnv,
  FILE *theFile,
  int count,
  int imageID,
  int maxIndices)
  {
   fprintf(theFile,"MIHS &%s%d_%d[%d]",ModulePrefix(DefruleData(theEnv)->DefruleCodeItem),
                      imageID,
                      (count / maxIndices) + 1,
                      (count % maxIndices));
  }


/*****************************************************************/
/* InitDefruleCode: Writes out initialization code for defrules. */
/*****************************************************************/
static void InitDefruleCode(
  Environment *theEnv,
  FILE *initFP,
  int imageID,
  int maxIndices)
  {
#if MAC_XCD
#pragma unused(maxIndices)
#pragma unused(theEnv)
#pragma unused(imageID)
#endif

   fprintf(initFP,"   DefruleRunTimeInitialize(theEnv,");

   if (DefruleData(theEnv)->RightPrimeJoins == NULL)
     { fprintf(initFP,"NULL,"); }
   else
     {
      fprintf(initFP,"&%s%d_%ld[%ld],",LinkPrefix(),
                    imageID,(DefruleData(theEnv)->RightPrimeJoins->bsaveID / maxIndices) + 1,
                             DefruleData(theEnv)->RightPrimeJoins->bsaveID % maxIndices);
     }

   if (DefruleData(theEnv)->LeftPrimeJoins == NULL)
     { fprintf(initFP,"NULL);\n"); }
   else
     {
      fprintf(initFP,"&%s%d_%ld[%ld]);\n",LinkPrefix(),
                    imageID,(DefruleData(theEnv)->LeftPrimeJoins->bsaveID / maxIndices) + 1,
                             DefruleData(theEnv)->LeftPrimeJoins->bsaveID % maxIndices);
     }
  }

#endif /* DEFRULE_CONSTRUCT && (! RUN_TIME) && CONSTRUCT_COMPILER */


