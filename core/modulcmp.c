   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
   /*                                                     */
   /*           DEFMODULE CONSTRUCTS-TO-C MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Implements the constructs-to-c feature for the   */
/*    defmodule construct.                                   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added support for path name argument to        */
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

#if CONSTRUCT_COMPILER && (! RUN_TIME)

#include <stdio.h>

#include "conscomp.h"
#include "envrnmnt.h"
#include "moduldef.h"
#include "sysdep.h"

#include "modulcmp.h"

/***************/
/* DEFINITIONS */
/***************/

#define ItemPrefix()      ArbitraryPrefix(DefmoduleData(theEnv)->DefmoduleCodeItem,0)
#define DefmodulePrefix() ArbitraryPrefix(DefmoduleData(theEnv)->DefmoduleCodeItem,1)
#define PortPrefix()      ArbitraryPrefix(DefmoduleData(theEnv)->DefmoduleCodeItem,2)

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static bool                    ConstructToCode(Environment *,const char *,const char *,char *,int,FILE *,int,int);
   static void                    InitDefmoduleCode(Environment *,FILE *,int,int);
   static struct portItem        *GetNextPortItem(Environment *,Defmodule **,struct portItem **,
                                                  bool *,bool *);
   static bool                    PortItemsToCode(Environment *,const char *,const char *,char *,int,FILE *,int,int,int *);
   static void                    BeforeDefmodulesToCode(Environment *);

/***************************************************************/
/* DefmoduleCompilerSetup: Initializes the defmodule construct */
/*    for use with the constructs-to-c command.                */
/***************************************************************/
void DefmoduleCompilerSetup(
  Environment *theEnv)
  {
   DefmoduleData(theEnv)->DefmoduleCodeItem =
      AddCodeGeneratorItem(theEnv,"defmodule",200,BeforeDefmodulesToCode,
                           InitDefmoduleCode,ConstructToCode,3);
  }

/***********************************************************/
/* BeforeDefmodulesToCode: Assigns each defmodule a unique */
/*   ID which will be used for pointer references when the */
/*   data structures are written to a file as C code       */
/***********************************************************/
static void BeforeDefmodulesToCode(
  Environment *theEnv)
  {
   int value = 0;
   Defmodule *theModule;

   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
     { theModule->header.bsaveID = value++; }
  }

/*************************************************************/
/* PrintDefmoduleReference: Writes the C code representation */
/*   of a reference to a defmodule data structure.           */
/*************************************************************/
void PrintDefmoduleReference(
  Environment *theEnv,
  FILE *theFile,
  Defmodule *theModule)
  {
   if (theModule == NULL) fprintf(theFile,"NULL");
   else fprintf(theFile,"&%s%d_%ld[%ld]",DefmodulePrefix(),ConstructCompilerData(theEnv)->ImageID,
                                    (long) ((theModule->header.bsaveID / ConstructCompilerData(theEnv)->MaxIndices) + 1),
                                    (long) (theModule->header.bsaveID % ConstructCompilerData(theEnv)->MaxIndices));
  }

/************************************************/
/* InitDefmoduleCode: Writes out initialization */
/*   code for defmodules for a run-time module. */
/************************************************/
static void InitDefmoduleCode(
  Environment *theEnv,
  FILE *initFP,
  int imageID,
  int maxIndices)
  {
#if MAC_XCD
#pragma unused(maxIndices)
#endif

   if (EnvGetNextDefmodule(theEnv,NULL) != NULL)
     { fprintf(initFP,"   SetListOfDefmodules(theEnv,%s%d_1);\n",DefmodulePrefix(),imageID); }
   else
     { fprintf(initFP,"   SetListOfDefmodules(theEnv,NULL);\n"); }
   fprintf(initFP,"   EnvSetCurrentModule(theEnv,EnvGetNextDefmodule(theEnv,NULL));\n");
  }

/***********************************************************/
/* ConstructToCode: Produces defmodule code for a run-time */
/*   module created using the constructs-to-c function.    */
/***********************************************************/
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
   Defmodule *theConstruct;
   FILE *moduleFile = NULL, *itemsFile;
   int portItemCount = 0;
   struct portItem *portItemPtr;
   int mihCount = 0, moduleCount = 0;
   int j;
   struct moduleItem *theItem;
   int moduleArrayVersion = 1;
   int fileCount = 2;

   /*================================================*/
   /* Include the appropriate defmodule header file. */
   /*================================================*/

   fprintf(headerFP,"#include \"moduldef.h\"\n");

   /*============================================*/
   /* Open up the items file for the defmodules. */
   /* Only one file of this type is created so   */
   /* the maximum number of indices is ignored.  */
   /*============================================*/

   if ((itemsFile = NewCFile(theEnv,fileName,pathName,fileNameBuffer,fileID,1,false)) == NULL)
     { return false; }
   fprintf(itemsFile,"struct defmoduleItemHeader *%s%d_%d[] = {\n",ItemPrefix(),imageID,1);
   fprintf(headerFP,"extern struct defmoduleItemHeader *%s%d_%d[];\n",ItemPrefix(),imageID,1);

   /*======================================================*/
   /* Loop through all the defmodules writing their C code */
   /* representation to the file as they are traversed.    */
   /*======================================================*/

   for (theConstruct = EnvGetNextDefmodule(theEnv,NULL);
        theConstruct != NULL;
        theConstruct = EnvGetNextDefmodule(theEnv,theConstruct))
     {
      /*===========================================*/
      /* Open a new file to write to if necessary. */
      /*===========================================*/

      moduleFile = OpenFileIfNeeded(theEnv,moduleFile,fileName,pathName,fileNameBuffer,fileID,imageID,
                                    &fileCount,moduleArrayVersion,headerFP,
                                    "Defmodule",DefmodulePrefix(),
                                    false,NULL);

      if (moduleFile == NULL)
        {
         moduleCount = maxIndices;
         CloseFileIfNeeded(theEnv,moduleFile,&moduleCount,
                           &moduleArrayVersion,maxIndices,NULL,NULL);
         GenClose(theEnv,itemsFile);
         return false;
        }

      /*======================================*/
      /* Write the construct name and ppform. */
      /*======================================*/

      fprintf(moduleFile,"{");
      
      ConstructHeaderToCode(theEnv,moduleFile,&theConstruct->header,imageID,maxIndices,
                         moduleCount,ModulePrefix(DefmoduleData(theEnv)->DefmoduleCodeItem),
                         ConstructPrefix(DefmoduleData(theEnv)->DefmoduleCodeItem));

      fprintf(moduleFile,",");

      /*=====================================================*/
      /* Write the items array pointers to other constructs. */
      /*=====================================================*/

      fprintf(moduleFile,"&%s%d_1[%d],",ItemPrefix(),imageID,mihCount);

      for (j = 0, theItem = GetListOfModuleItems(theEnv);
           (j < GetNumberOfModuleItems(theEnv)) && (theItem != NULL) ;
           j++, theItem = theItem->next)
        {
         mihCount++;
         if (theItem->constructsToCModuleReference == NULL)
           { fprintf(itemsFile,"NULL"); }
         else
           { (*theItem->constructsToCModuleReference)(theEnv,itemsFile,(int) theConstruct->header.bsaveID,imageID,maxIndices); }

         if ((j + 1) < GetNumberOfModuleItems(theEnv)) fprintf(itemsFile,",");
         else if (theConstruct->header.next != NULL) fprintf(itemsFile,",\n");
        }

      /*=================================*/
      /* Write the importList reference. */
      /*=================================*/

      if (theConstruct->importList == NULL)
        { fprintf(moduleFile,"NULL,"); }
      else
        {
         fprintf(moduleFile,"&%s%d_%d[%d],",PortPrefix(),imageID,
                                     (portItemCount / maxIndices) + 1,
                                     portItemCount % maxIndices);
         for (portItemPtr = theConstruct->importList;
              portItemPtr != NULL;
              portItemPtr = portItemPtr->next)
           { portItemCount++; }
        }

      /*=================================*/
      /* Write the exportList reference. */
      /*=================================*/

      if (theConstruct->exportList == NULL)
        { fprintf(moduleFile,"NULL,"); }
      else
        {
         fprintf(moduleFile,"&%s%d_%d[%d],",PortPrefix(),imageID,
                                     (portItemCount / maxIndices) + 1,
                                     portItemCount % maxIndices);
         for (portItemPtr = theConstruct->exportList;
              portItemPtr != NULL;
              portItemPtr = portItemPtr->next)
           { portItemCount++; }
        }

      /*========================*/
      /* Write the visitedFlag. */
      /*========================*/

      fprintf(moduleFile,"0}");

      /*===================================================*/
      /* Increment the number of defmodule data structures */
      /* written and close the output file if necessary.   */
      /*===================================================*/

      moduleCount++;
      moduleFile = CloseFileIfNeeded(theEnv,moduleFile,&moduleCount,&moduleArrayVersion,
                                        maxIndices,NULL,NULL);

     }

   /*=========================*/
   /* Close the output files. */
   /*=========================*/

   moduleCount = maxIndices;
   CloseFileIfNeeded(theEnv,moduleFile,&moduleCount,
                     &moduleArrayVersion,maxIndices,NULL,NULL);
   fprintf(itemsFile,"};\n");
   GenClose(theEnv,itemsFile);

   /*=========================================*/
   /* Write out the portItem data structures. */
   /*=========================================*/

   if (portItemCount == 0) return true;
   return(PortItemsToCode(theEnv,fileName,pathName,fileNameBuffer,fileID,headerFP,imageID,maxIndices,&fileCount));
  }

/************************************************************/
/* PortItemsToCode: Writes the C code representation of all */
/*   portItem data structure nodes the specified file.      */
/************************************************************/
static bool PortItemsToCode(
  Environment *theEnv,
  const char *fileName,
  const char *pathName,
  char *fileNameBuffer,
  int fileID,
  FILE *headerFP,
  int imageID,
  int maxIndices,
  int *fileCount)
  {
   Defmodule *theDefmodule = NULL;
   struct portItem *thePortItem = NULL;
   int portItemCount = 0;
   bool importChecked = false;
   bool exportChecked = false;
   FILE *portItemsFile = NULL;
   int portItemArrayVersion = 1;

   /*=================================================================*/
   /* Loop through each of the portItem data structures writing their */
   /* C code representation to the file as they are traversed.        */
   /*=================================================================*/

   for (thePortItem = GetNextPortItem(theEnv,&theDefmodule,&thePortItem,&importChecked,&exportChecked);
        thePortItem != NULL;
        thePortItem = GetNextPortItem(theEnv,&theDefmodule,&thePortItem,&importChecked,&exportChecked))
     {
      /*===========================================*/
      /* Open a new file to write to if necessary. */
      /*===========================================*/

      portItemsFile = OpenFileIfNeeded(theEnv,portItemsFile,fileName,pathName,fileNameBuffer,fileID,imageID,
                                       fileCount,portItemArrayVersion,headerFP,
                                       "struct portItem",PortPrefix(),
                                       false,NULL);

      if (portItemsFile == NULL)
        {
         portItemCount = maxIndices;
         CloseFileIfNeeded(theEnv,portItemsFile,&portItemCount,
                           &portItemArrayVersion,maxIndices,NULL,NULL);
         return false;
        }

      /*================================================*/
      /* Write the portItem data structure to the file. */
      /*================================================*/

      fprintf(portItemsFile,"{");
      PrintSymbolReference(theEnv,portItemsFile,thePortItem->moduleName);
      fprintf(portItemsFile,",");
      PrintSymbolReference(theEnv,portItemsFile,thePortItem->constructType);
      fprintf(portItemsFile,",");
      PrintSymbolReference(theEnv,portItemsFile,thePortItem->constructName);
      fprintf(portItemsFile,",");

      if (thePortItem->next == NULL)
        { fprintf(portItemsFile,"NULL}"); }
      else
        {
         fprintf(portItemsFile,"&%s%d_%d[%d]}",PortPrefix(),imageID,
                                  ((portItemCount+1) / maxIndices) + 1,
                                   (portItemCount+1) % maxIndices);
        }

      /*==================================================*/
      /* Increment the number of portItem data structures */
      /* written and close the output file if necessary.  */
      /*==================================================*/

      portItemCount++;
      CloseFileIfNeeded(theEnv,portItemsFile,&portItemCount,&portItemArrayVersion,
                        maxIndices,NULL,NULL);
     }

   /*===================================================*/
   /* Close the output file and return true to indicate */
   /* the data structures were successfully written.    */
   /*===================================================*/

   portItemCount = maxIndices;
   CloseFileIfNeeded(theEnv,portItemsFile,&portItemCount,
                     &portItemArrayVersion,maxIndices,NULL,NULL);

   return true;
  }

/*********************************************************************/
/* GetNextPortItem: Given a pointer to a portItem data structure     */
/*   and its defmodule, returns the "next" portItem data structure.  */
/*   If passed a NULL value for both the defmodule and portItem      */
/*   data structure, it returns the "first" portItem data structure. */
/*********************************************************************/
static struct portItem *GetNextPortItem(
  Environment *theEnv,
  Defmodule **theDefmodule,
  struct portItem **thePortItem,
  bool *importChecked,
  bool *exportChecked)
  {
   /*====================================================*/
   /* If the defmodule pointer is NULL, then the "first" */
   /* portItem data structure should be returned. Start  */
   /* the search in the "first" defmodule.               */
   /*====================================================*/

   if (*theDefmodule == NULL)
     {
      *theDefmodule = EnvGetNextDefmodule(theEnv,NULL);
      *thePortItem = NULL;
      *importChecked = false;
      *exportChecked = false;
     }

   /*==============================================*/
   /* Loop through all of the defmodules until the */
   /* "next" portItem data structure is found.     */
   /*==============================================*/

   while (*theDefmodule != NULL)
     {
      /*==========================================*/
      /* Check to see if there's another portItem */
      /* in the import/export list that's being   */
      /* checked in the module being examined.    */
      /*==========================================*/

      if (*thePortItem != NULL) *thePortItem = (*thePortItem)->next;
      if (*thePortItem != NULL) return(*thePortItem);

      /*==================================================*/
      /* If we haven't checked the import list yet, begin */
      /* checking it. If there aren't any items in the    */
      /* import list, then check the export list.         */
      /*==================================================*/

      if (! (*importChecked))
        {
         *thePortItem = (*theDefmodule)->importList;
         *importChecked = true;
         if (*thePortItem == NULL)
           {
            *thePortItem = (*theDefmodule)->exportList;
            *exportChecked = true;
           }
        }

      /*======================================*/
      /* Otherwise, if we haven't checked the */
      /* export list yet, begin checking it.  */
      /*======================================*/

      else if (! (*exportChecked))
        {
         *exportChecked = true;
         *thePortItem = (*theDefmodule)->exportList;
        }

      /*==========================================*/
      /* If the import or export list contained a */
      /* portItem data structure, then return it. */
      /*==========================================*/

      if (*thePortItem != NULL) return(*thePortItem);

      /*==================================*/
      /* Otherwise, check the next module */
      /* for a portItem data structure.   */
      /*==================================*/

      *theDefmodule = EnvGetNextDefmodule(theEnv,*theDefmodule);
      *importChecked = false;
      *exportChecked = false;
     }

   /*=======================================================*/
   /* All the portItem data structures have been traversed. */
   /* Return NULL to indicate that none are left.           */
   /*=======================================================*/

   return NULL;
  }

#endif /* CONSTRUCT_COMPILER && (! RUN_TIME) */


