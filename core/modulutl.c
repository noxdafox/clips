   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
   /*                                                     */
   /*              DEFMODULE UTILITY MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for parsing module/construct   */
/*   names and searching through modules for specific        */
/*   constructs.                                             */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Used genstrncpy instead of strncpy.            */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#include "setup.h"

#include "envrnmnt.h"
#include "memalloc.h"
#include "modulpsr.h"
#include "router.h"
#include "sysdep.h"

#include "modulutl.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static ConstructHeader    *SearchImportedConstructModules(Environment *,CLIPSLexeme *,
                                              Defmodule *,
                                              struct moduleItem *,CLIPSLexeme *,
                                              int *,int,Defmodule *);

/********************************************************************/
/* FindModuleSeparator: Finds the :: separator which delineates the */
/*   boundary between a module name and a construct name. The value */
/*   zero is returned if the separator is not found, otherwise the  */
/*   position of the second colon within the string is returned.    */
/********************************************************************/
unsigned FindModuleSeparator(
  const char *theString)
  {
   unsigned i;
   bool foundColon;

   for (i = 0, foundColon = false; theString[i] != EOS; i++)
     {
      if (theString[i] == ':')
        {
         if (foundColon) return(i);
         foundColon = true;
        }
      else
        { foundColon = false; }
     }

   return 0;
  }

/*******************************************************************/
/* ExtractModuleName: Given the position of the :: separator and a */
/*   module/construct name joined using the separator, returns a   */
/*   symbol reference to the module name (or NULL if a module name */
/*   cannot be extracted).                                         */
/*******************************************************************/
CLIPSLexeme *ExtractModuleName(
  Environment *theEnv,
  unsigned thePosition,
  const char *theString)
  {
   char *newString;
   CLIPSLexeme *returnValue;

   /*=============================================*/
   /* Return NULL if the :: is in a position such */
   /* that a module name can't be extracted.      */
   /*=============================================*/

   if (thePosition <= 1) return NULL;

   /*==========================================*/
   /* Allocate storage for a temporary string. */
   /*==========================================*/

   newString = (char *) gm2(theEnv,thePosition);

   /*======================================================*/
   /* Copy the entire module/construct name to the string. */
   /*======================================================*/

   genstrncpy(newString,theString,
           (STD_SIZE) thePosition - 1);

   /*========================================================*/
   /* Place an end of string marker where the :: is located. */
   /*========================================================*/

   newString[thePosition-1] = EOS;

   /*=====================================================*/
   /* Add the module name (the truncated module/construct */
   /* name) to the symbol table.                          */
   /*=====================================================*/

   returnValue = EnvCreateSymbol(theEnv,newString);

   /*=============================================*/
   /* Return the storage of the temporary string. */
   /*=============================================*/

   rm(theEnv,newString,thePosition);

   /*=============================================*/
   /* Return a pointer to the module name symbol. */
   /*=============================================*/

   return returnValue;
  }

/********************************************************************/
/* ExtractConstructName: Given the position of the :: separator and */
/*   a module/construct name joined using the separator, returns a  */
/*   symbol reference to the construct name (or NULL if a construct */
/*   name cannot be extracted).                                     */
/********************************************************************/
CLIPSLexeme *ExtractConstructName(
  Environment *theEnv,
  unsigned thePosition,
  const char *theString,
  unsigned returnType)
  {
   size_t theLength;
   char *newString;
   CLIPSLexeme *returnValue;

   /*======================================*/
   /* Just return the string if it doesn't */
   /* contain the :: symbol.               */
   /*======================================*/

   if (thePosition == 0) return EnvCreateSymbol(theEnv,theString);

   /*=====================================*/
   /* Determine the length of the string. */
   /*=====================================*/

   theLength = strlen(theString);

   /*=================================================*/
   /* Return NULL if the :: is at the very end of the */
   /* string (and thus there is no construct name).   */
   /*=================================================*/

   if (theLength <= (thePosition + 1)) return NULL;

   /*====================================*/
   /* Allocate a temporary string large  */
   /* enough to hold the construct name. */
   /*====================================*/

   newString = (char *) gm2(theEnv,theLength - thePosition);

   /*================================================*/
   /* Copy the construct name portion of the         */
   /* module/construct name to the temporary string. */
   /*================================================*/

   genstrncpy(newString,&theString[thePosition+1],
           (STD_SIZE) theLength - thePosition);

   /*=============================================*/
   /* Add the construct name to the symbol table. */
   /*=============================================*/

   if (returnType == SYMBOL_TYPE)
     { returnValue = EnvCreateSymbol(theEnv,newString); }
   else if (returnType == INSTANCE_NAME_TYPE)
     { returnValue = EnvCreateInstanceName(theEnv,newString); }
   else
     { returnValue = EnvCreateString(theEnv,newString); }

   /*=============================================*/
   /* Return the storage of the temporary string. */
   /*=============================================*/

   rm(theEnv,newString,theLength - thePosition);

   /*================================================*/
   /* Return a pointer to the construct name symbol. */
   /*================================================*/

   return returnValue;
  }

/****************************************************/
/* ExtractModuleAndConstructName: Extracts both the */
/*   module and construct name from a string. Sets  */
/*   the current module to the specified module.    */
/****************************************************/
const char *ExtractModuleAndConstructName(
  Environment *theEnv,
  const char *theName)
  {
   unsigned separatorPosition;
   CLIPSLexeme *moduleName, *shortName;
   Defmodule *theModule;

   /*========================*/
   /* Find the :: separator. */
   /*========================*/

   separatorPosition = FindModuleSeparator(theName);
   if (! separatorPosition) return(theName);

   /*==========================*/
   /* Extract the module name. */
   /*==========================*/

   moduleName = ExtractModuleName(theEnv,separatorPosition,theName);
   if (moduleName == NULL) return NULL;

   /*====================================*/
   /* Check to see if the module exists. */
   /*====================================*/

   theModule = EnvFindDefmodule(theEnv,moduleName->contents);
   if (theModule == NULL) return NULL;

   /*============================*/
   /* Change the current module. */
   /*============================*/

   EnvSetCurrentModule(theEnv,theModule);

   /*=============================*/
   /* Extract the construct name. */
   /*=============================*/

   shortName = ExtractConstructName(theEnv,separatorPosition,theName,SYMBOL_TYPE);
   if (shortName == NULL) return NULL;
   return shortName->contents;
  }

/************************************************************/
/* FindImportedConstruct: High level routine which searches */
/*   a module and other modules from which it imports       */
/*   constructs for a specified construct.                  */
/************************************************************/
ConstructHeader *FindImportedConstruct(
  Environment *theEnv,
  const char *constructName,
  Defmodule *matchModule,
  const char *findName,
  int *count,
  bool searchCurrent,
  Defmodule *notYetDefinedInModule)
  {
   ConstructHeader *rv;
   struct moduleItem *theModuleItem;

   /*=============================================*/
   /* Set the number of references found to zero. */
   /*=============================================*/

   *count = 0;

   /*===============================*/
   /* The :: should not be included */
   /* in the construct's name.      */
   /*===============================*/

   if (FindModuleSeparator(findName)) return NULL;

   /*=============================================*/
   /* Remember the current module since we'll be  */
   /* changing it during the search and will want */
   /* to restore it once the search is completed. */
   /*=============================================*/

   SaveCurrentModule(theEnv);

   /*==========================================*/
   /* Find the module related access functions */
   /* for the construct type being sought.     */
   /*==========================================*/

   if ((theModuleItem = FindModuleItem(theEnv,constructName)) == NULL)
     {
      RestoreCurrentModule(theEnv);
      return NULL;
     }

   /*===========================================*/
   /* If the construct type doesn't have a find */
   /* function, then we can't look for it.      */
   /*===========================================*/

   if (theModuleItem->findFunction == NULL)
     {
      RestoreCurrentModule(theEnv);
      return NULL;
     }

   /*==================================*/
   /* Initialize the search by marking */
   /* all modules as unvisited.        */
   /*==================================*/

   MarkModulesAsUnvisited(theEnv);

   /*===========================*/
   /* Search for the construct. */
   /*===========================*/

   rv = SearchImportedConstructModules(theEnv,EnvCreateSymbol(theEnv,constructName),
                                       matchModule,theModuleItem,
                                       EnvCreateSymbol(theEnv,findName),count,
                                       searchCurrent,notYetDefinedInModule);

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule(theEnv);

   /*====================================*/
   /* Return a pointer to the construct. */
   /*====================================*/

   return rv;
  }

/*********************************************************/
/* AmbiguousReferenceErrorMessage: Error message printed */
/*   when a reference to a specific construct can be     */
/*   imported from more than one module.                 */
/*********************************************************/
void AmbiguousReferenceErrorMessage(
  Environment *theEnv,
  const char *constructName,
  const char *findName)
  {
   EnvPrintRouter(theEnv,WERROR,"Ambiguous reference to ");
   EnvPrintRouter(theEnv,WERROR,constructName);
   EnvPrintRouter(theEnv,WERROR," ");
   EnvPrintRouter(theEnv,WERROR,findName);
   EnvPrintRouter(theEnv,WERROR,".\nIt is imported from more than one module.\n");
  }

/****************************************************/
/* MarkModulesAsUnvisited: Used for initializing a  */
/*   search through the module heirarchies. Sets    */
/*   the visited flag of each module to false.      */
/****************************************************/
void MarkModulesAsUnvisited(
  Environment *theEnv)
  {
   Defmodule *theModule;

   DefmoduleData(theEnv)->CurrentModule->visitedFlag = false;
   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule))
     { theModule->visitedFlag = false; }
  }

/***********************************************************/
/* SearchImportedConstructModules: Low level routine which */
/*   searches a module and other modules from which it     */
/*   imports constructs for a specified construct.         */
/***********************************************************/
static ConstructHeader *SearchImportedConstructModules(
  Environment *theEnv,
  CLIPSLexeme *constructType,
  Defmodule *matchModule,
  struct moduleItem *theModuleItem,
  CLIPSLexeme *findName,
  int *count,
  int searchCurrent,
  Defmodule *notYetDefinedInModule)
  {
   Defmodule *theModule;
   struct portItem *theImportList, *theExportList;
   ConstructHeader *rv, *arv = NULL;
   bool searchModule, exported;
   Defmodule *currentModule;

   /*=========================================*/
   /* Start the search in the current module. */
   /* If the current module has already been  */
   /* visited, then return.                   */
   /*=========================================*/

   currentModule = EnvGetCurrentModule(theEnv);
   if (currentModule->visitedFlag) return NULL;

   /*=======================================================*/
   /* The searchCurrent flag indicates whether the current  */
   /* module should be included in the search. In addition, */
   /* if matchModule is non-NULL, the current module will   */
   /* only be searched if it is the specific module from    */
   /* which we want the construct imported.                 */
   /*=======================================================*/

   if ((searchCurrent) &&
       ((matchModule == NULL) || (currentModule == matchModule)))
     {
      /*===============================================*/
      /* Look for the construct in the current module. */
      /*===============================================*/

      rv = (*theModuleItem->findFunction)(theEnv,findName->contents);

      /*========================================================*/
      /* If we're in the process of defining the construct in   */
      /* the module we're searching then go ahead and increment */
      /* the count indicating the number of modules in which    */
      /* the construct was found.                               */
      /*========================================================*/

      if (notYetDefinedInModule == currentModule)
        {
         (*count)++;
         arv = rv;
        }

      /*=========================================================*/
      /* Otherwise, if the construct is in the specified module, */
      /* increment the count only if the construct actually      */
      /* belongs to the module. [Some constructs, like the COOL  */
      /* system classes, can be found in any module, but they    */
      /* actually belong to the MAIN module.]                    */
      /*=========================================================*/

      else if (rv != NULL)
        {
         if (rv->whichModule->theModule == currentModule)
           { (*count)++; }
         arv = rv;
        }
     }

   /*=====================================*/
   /* Mark the current module as visited. */
   /*=====================================*/

   currentModule->visitedFlag = true;

   /*===================================*/
   /* Search through all of the modules */
   /* imported by the current module.   */
   /*===================================*/

   theModule = EnvGetCurrentModule(theEnv);
   theImportList = theModule->importList;

   while (theImportList != NULL)
     {
      /*===================================================*/
      /* Determine if the module should be searched (based */
      /* upon whether the entire module, all constructs of */
      /* a specific type, or specifically named constructs */
      /* are imported).                                    */
      /*===================================================*/

      searchModule = false;
      if ((theImportList->constructType == NULL) ||
          (theImportList->constructType == constructType))
        {
         if ((theImportList->constructName == NULL) ||
             (theImportList->constructName == findName))
           { searchModule = true; }
        }

      /*=================================*/
      /* Determine if the module exists. */
      /*=================================*/

      if (searchModule)
        {
         theModule = EnvFindDefmodule(theEnv,theImportList->moduleName->contents);
         if (theModule == NULL) searchModule = false;
        }

      /*=======================================================*/
      /* Determine if the construct is exported by the module. */
      /*=======================================================*/

      if (searchModule)
        {
         exported = false;
         theExportList = theModule->exportList;
         while ((theExportList != NULL) && (! exported))
           {
            if ((theExportList->constructType == NULL) ||
                (theExportList->constructType == constructType))
              {
               if ((theExportList->constructName == NULL) ||
                   (theExportList->constructName == findName))
                 { exported = true; }
               }

            theExportList = theExportList->next;
           }

         if (! exported) searchModule = false;
        }

      /*=================================*/
      /* Search in the specified module. */
      /*=================================*/

      if (searchModule)
        {
         EnvSetCurrentModule(theEnv,theModule);
         if ((rv = SearchImportedConstructModules(theEnv,constructType,matchModule,
                                                  theModuleItem,findName,
                                                  count,true,
                                                  notYetDefinedInModule)) != NULL)
           { arv = rv; }
        }

      /*====================================*/
      /* Move on to the next imported item. */
      /*====================================*/

      theImportList = theImportList->next;
     }

   /*=========================*/
   /* Return a pointer to the */
   /* last construct found.   */
   /*=========================*/

   return arv;
  }

/**************************************************************/
/* ConstructExported: Returns true if the specified construct */
/*   is exported from the specified module.                   */
/**************************************************************/
bool ConstructExported(
  Environment *theEnv,
  const char *constructTypeStr,
  CLIPSLexeme *moduleName,
  CLIPSLexeme *findName)
  {
   CLIPSLexeme *constructType;
   Defmodule *theModule;
   struct portItem *theExportList;

   constructType = FindSymbolHN(theEnv,constructTypeStr,SYMBOL_BIT);
   theModule = EnvFindDefmodule(theEnv,moduleName->contents);

   if ((constructType == NULL) || (theModule == NULL) || (findName == NULL))
     { return false; }

   theExportList = theModule->exportList;
   while (theExportList != NULL)
     {
      if ((theExportList->constructType == NULL) ||
          (theExportList->constructType == constructType))
       {
        if ((theExportList->constructName == NULL) ||
            (theExportList->constructName == findName))
          { return true; }
       }

      theExportList = theExportList->next;
     }

   return false;
  }

/*********************************************************/
/* AllImportedModulesVisited: Returns true if all of the */
/*   imported modules for a module have been visited.    */
/*********************************************************/
bool AllImportedModulesVisited(
  Environment *theEnv,
  Defmodule *theModule)
  {
   struct portItem *theImportList;
   Defmodule *theImportModule;

   theImportList = theModule->importList;
   while (theImportList != NULL)
     {
      theImportModule = EnvFindDefmodule(theEnv,theImportList->moduleName->contents);

      if (! theImportModule->visitedFlag) return false;

      theImportList = theImportList->next;
     }

   return true;
  }

/***************************************/
/* ListItemsDriver: Driver routine for */
/*   listing items in a module.        */
/***************************************/
void ListItemsDriver(
  Environment *theEnv,
  const char *logicalName,
  Defmodule *theModule,
  const char *singleName,
  const char *pluralName,
  GetNextItemFunction *nextFunction,
  const char *(*nameFunction)(void *),
  PrintItemFunction *printFunction,
  bool (*doItFunction)(void *))
  {
   void *constructPtr;
   const char *constructName;
   long count = 0;
   bool allModules = false;
   bool doIt;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule(theEnv);

   /*======================*/
   /* Print out the items. */
   /*======================*/

   if (theModule == NULL)
     {
      theModule = EnvGetNextDefmodule(theEnv,NULL);
      allModules = true;
     }

   while (theModule != NULL)
     {
      if (allModules)
        {
         EnvPrintRouter(theEnv,logicalName,DefmoduleName(theModule));
         EnvPrintRouter(theEnv,logicalName,":\n");
        }

      EnvSetCurrentModule(theEnv,theModule);
      constructPtr = (*nextFunction)(theEnv,NULL);
      while (constructPtr != NULL)
        {
         if (EvaluationData(theEnv)->HaltExecution == true) return;

         if (doItFunction == NULL) doIt = true;
         else doIt = (*doItFunction)(constructPtr);

         if (! doIt) {}
         else if (nameFunction != NULL)
           {
            constructName = (*nameFunction)(constructPtr);
            if (constructName != NULL)
              {
               if (allModules) EnvPrintRouter(theEnv,logicalName,"   ");
               EnvPrintRouter(theEnv,logicalName,constructName);
               EnvPrintRouter(theEnv,logicalName,"\n");
              }
           }
         else if (printFunction != NULL)
           {
            if (allModules) EnvPrintRouter(theEnv,logicalName,"   ");
            (*printFunction)(theEnv,logicalName,constructPtr);
            EnvPrintRouter(theEnv,logicalName,"\n");
           }

         constructPtr = (*nextFunction)(theEnv,constructPtr);
         count++;
        }

      if (allModules) theModule = EnvGetNextDefmodule(theEnv,theModule);
      else theModule = NULL;
     }

   /*=================================================*/
   /* Print the tally and restore the current module. */
   /*=================================================*/

   if (singleName != NULL) PrintTally(theEnv,logicalName,count,singleName,pluralName);

   RestoreCurrentModule(theEnv);
  }

/********************************************************/
/* DoForAllModules: Executes an action for all modules. */
/********************************************************/
long DoForAllModules(
  Environment *theEnv,
  void (*actionFunction)(Defmodule *,void *),
  int interruptable,
  void *userBuffer)
  {
   Defmodule *theModule;
   long moduleCount = 0L;

   /*==========================*/
   /* Save the current module. */
   /*==========================*/

   SaveCurrentModule(theEnv);

   /*==================================*/
   /* Loop through all of the modules. */
   /*==================================*/

   for (theModule = EnvGetNextDefmodule(theEnv,NULL);
        theModule != NULL;
        theModule = EnvGetNextDefmodule(theEnv,theModule), moduleCount++)
     {
      EnvSetCurrentModule(theEnv,theModule);

      if ((interruptable) && EnvGetHaltExecution(theEnv))
        {
         RestoreCurrentModule(theEnv);
         return(-1L);
        }

      (*actionFunction)(theModule,userBuffer);
     }

   /*=============================*/
   /* Restore the current module. */
   /*=============================*/

   RestoreCurrentModule(theEnv);

   /*=========================================*/
   /* Return the number of modules traversed. */
   /*=========================================*/

   return(moduleCount);
  }



