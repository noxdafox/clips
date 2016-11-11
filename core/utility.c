   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/01/16             */
   /*                                                     */
   /*                   UTILITY MODULE                    */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a set of utility functions useful to    */
/*   other modules. Primarily these are the functions for    */
/*   handling periodic garbage collection and appending      */
/*   string data.                                            */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian Dantes                                         */
/*      Jeff Bezanson                                        */
/*         www.cprogramming.com/tutorial/unicode.html        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Added CopyString, DeleteString,                */
/*            InsertInString,and EnlargeString functions.    */
/*                                                           */
/*            Used genstrncpy function instead of strncpy    */
/*            function.                                      */
/*                                                           */
/*            Support for typed EXTERNAL_ADDRESS_TYPE.       */
/*                                                           */
/*            Support for tracked memory (allows memory to   */
/*            be freed if CLIPS is exited while executing).  */
/*                                                           */
/*            Added UTF-8 routines.                          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Added EnvAddPeriodicFunctionWithContext        */
/*            function.                                      */
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
/*            Callbacks must be environment aware.           */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Added CLIPSBlockStart and CLIPSBlockEnd        */
/*            functions for garbage collection blocks.       */
/*                                                           */
/*            Added StringBuilder functions.                 */
/*                                                           */
/*************************************************************/

#include "setup.h"

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "commline.h"
#include "envrnmnt.h"
#include "evaluatn.h"
#include "factmngr.h"
#include "facthsh.h"
#include "memalloc.h"
#include "multifld.h"
#include "prntutil.h"
#include "symbol.h"
#include "sysdep.h"

#include "utility.h"

#define MAX_EPHEMERAL_COUNT 1000L
#define MAX_EPHEMERAL_SIZE 10240L
#define COUNT_INCREMENT 1000L
#define SIZE_INCREMENT 10240L

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static void                    DeallocateUtilityData(Environment *);

/************************************************/
/* InitializeUtilityData: Allocates environment */
/*    data for utility routines.                */
/************************************************/
void InitializeUtilityData(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,UTILITY_DATA,sizeof(struct utilityData),DeallocateUtilityData);

   UtilityData(theEnv)->CurrentGarbageFrame = &UtilityData(theEnv)->MasterGarbageFrame;
   UtilityData(theEnv)->CurrentGarbageFrame->topLevel = true;

   UtilityData(theEnv)->GarbageCollectionLocks = 0;
   UtilityData(theEnv)->PeriodicFunctionsEnabled = true;
   UtilityData(theEnv)->YieldFunctionEnabled = true;
  }

/**************************************************/
/* DeallocateUtilityData: Deallocates environment */
/*    data for utility routines.                  */
/**************************************************/
static void DeallocateUtilityData(
  Environment *theEnv)
  {
   struct trackedMemory *tmpTM, *nextTM;
   struct garbageFrame *theGarbageFrame;
   struct ephemeron *edPtr, *nextEDPtr;
   struct multifield *tmpMFPtr, *nextMFPtr;

   /*======================*/
   /* Free tracked memory. */
   /*======================*/

   tmpTM = UtilityData(theEnv)->trackList;
   while (tmpTM != NULL)
     {
      nextTM = tmpTM->next;
      genfree(theEnv,tmpTM->theMemory,tmpTM->memSize);
      rtn_struct(theEnv,trackedMemory,tmpTM);
      tmpTM = nextTM;
     }

   /*==========================*/
   /* Free callback functions. */
   /*==========================*/

   DeallocateVoidCallList(theEnv,UtilityData(theEnv)->ListOfPeriodicFunctions);
   DeallocateVoidCallList(theEnv,UtilityData(theEnv)->ListOfCleanupFunctions);

   /*=========================================*/
   /* Free the ephemerons tracking data which */
   /* needs to be garbage collected.          */
   /*=========================================*/

   while (UtilityData(theEnv)->CurrentGarbageFrame != NULL)
     {
      theGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame;

      edPtr = theGarbageFrame->ephemeralSymbolList;

      while (edPtr != NULL)
        {
         nextEDPtr = edPtr->next;
         rtn_struct(theEnv,ephemeron,edPtr);
         edPtr = nextEDPtr;
        }

      edPtr = theGarbageFrame->ephemeralFloatList;

      while (edPtr != NULL)
        {
         nextEDPtr = edPtr->next;
         rtn_struct(theEnv,ephemeron,edPtr);
         edPtr = nextEDPtr;
        }

      edPtr = theGarbageFrame->ephemeralIntegerList;

      while (edPtr != NULL)
        {
         nextEDPtr = edPtr->next;
         rtn_struct(theEnv,ephemeron,edPtr);
         edPtr = nextEDPtr;
        }

      edPtr = theGarbageFrame->ephemeralBitMapList;

      while (edPtr != NULL)
        {
         nextEDPtr = edPtr->next;
         rtn_struct(theEnv,ephemeron,edPtr);
         edPtr = nextEDPtr;
        }

      edPtr = theGarbageFrame->ephemeralExternalAddressList;

      while (edPtr != NULL)
        {
         nextEDPtr = edPtr->next;
         rtn_struct(theEnv,ephemeron,edPtr);
         edPtr = nextEDPtr;
        }

      /*==========================*/
      /* Free up multifield data. */
      /*==========================*/

      tmpMFPtr = theGarbageFrame->ListOfMultifields;
      while (tmpMFPtr != NULL)
        {
         nextMFPtr = tmpMFPtr->next;
         ReturnMultifield(theEnv,tmpMFPtr);
         tmpMFPtr = nextMFPtr;
        }

      UtilityData(theEnv)->CurrentGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame->priorFrame;
     }
  }

/*****************************/
/* CleanCurrentGarbageFrame: */
/*****************************/
void CleanCurrentGarbageFrame(
  Environment *theEnv,
  UDFValue *returnValue)
  {
   struct garbageFrame *currentGarbageFrame;

   currentGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame;

   if (! currentGarbageFrame->dirty) return;

   if (returnValue != NULL)
     { ValueInstall(theEnv,returnValue); }

   CallCleanupFunctions(theEnv);
   RemoveEphemeralAtoms(theEnv);
   FlushMultifields(theEnv);

   if (returnValue != NULL)
     { ValueDeinstall(theEnv,returnValue); }

   if ((currentGarbageFrame->ephemeralFloatList == NULL) &&
       (currentGarbageFrame->ephemeralIntegerList == NULL) &&
       (currentGarbageFrame->ephemeralSymbolList == NULL) &&
       (currentGarbageFrame->ephemeralBitMapList == NULL) &&
       (currentGarbageFrame->ephemeralExternalAddressList == NULL) &&
       (currentGarbageFrame->LastMultifield == NULL))
     { currentGarbageFrame->dirty = false; }
  }

/*****************************/
/* RestorePriorGarbageFrame: */
/*****************************/
void RestorePriorGarbageFrame(
  Environment *theEnv,
  struct garbageFrame *newGarbageFrame,
  struct garbageFrame *oldGarbageFrame,
  UDFValue *returnValue)
  {
   if (newGarbageFrame->dirty)
     {
      if (returnValue != NULL) ValueInstall(theEnv,returnValue);
      CallCleanupFunctions(theEnv);
      RemoveEphemeralAtoms(theEnv);
      FlushMultifields(theEnv);
     }

   UtilityData(theEnv)->CurrentGarbageFrame = oldGarbageFrame;

   if (newGarbageFrame->dirty)
     {
      if (newGarbageFrame->ListOfMultifields != NULL)
        {
         if (oldGarbageFrame->ListOfMultifields == NULL)
           { oldGarbageFrame->ListOfMultifields = newGarbageFrame->ListOfMultifields; }
         else
           { oldGarbageFrame->LastMultifield->next = newGarbageFrame->ListOfMultifields; }

         oldGarbageFrame->LastMultifield = newGarbageFrame->LastMultifield;
         oldGarbageFrame->dirty = true;
        }

      if (returnValue != NULL) ValueDeinstall(theEnv,returnValue);
     }

   if (returnValue != NULL)
     { EphemerateValue(theEnv,returnValue->value); }
  }

/********************/
/* CLIPSBlockStart: */
/********************/
void CLIPSBlockStart(
  Environment *theEnv,
  CLIPSBlock *theBlock)
  {
   theBlock->oldGarbageFrame = UtilityData(theEnv)->CurrentGarbageFrame;
   memset(&theBlock->newGarbageFrame,0,sizeof(struct garbageFrame));
   theBlock->newGarbageFrame.priorFrame = theBlock->oldGarbageFrame;
   UtilityData(theEnv)->CurrentGarbageFrame = &theBlock->newGarbageFrame;
  }

/******************/
/* CLIPSBlockEnd: */
/******************/
void CLIPSBlockEnd(
  Environment *theEnv,
  CLIPSBlock *theBlock,
  UDFValue *rv)
  {
   RestorePriorGarbageFrame(theEnv,&theBlock->newGarbageFrame,theBlock->oldGarbageFrame,rv);
  }

/*************************/
/* CallCleanupFunctions: */
/*************************/
void CallCleanupFunctions(
  Environment *theEnv)
  {
   struct voidCallFunctionItem *cleanupPtr;

   for (cleanupPtr = UtilityData(theEnv)->ListOfCleanupFunctions;
        cleanupPtr != NULL;
        cleanupPtr = cleanupPtr->next)
     { (*cleanupPtr->func)(theEnv); }
  }

/**************************************************/
/* CallPeriodicTasks: Calls the list of functions */
/*   for handling periodic tasks.                 */
/**************************************************/
void CallPeriodicTasks(
  Environment *theEnv)
  {
   struct voidCallFunctionItem *periodPtr;

   if (UtilityData(theEnv)->PeriodicFunctionsEnabled)
     {
      for (periodPtr = UtilityData(theEnv)->ListOfPeriodicFunctions;
           periodPtr != NULL;
           periodPtr = periodPtr->next)
        {
         void *oldContext = SetEnvironmentCallbackContext(theEnv,periodPtr->context);

         (*periodPtr->func)(theEnv);

         SetEnvironmentCallbackContext(theEnv,oldContext);
        }
     }
  }

/***************************************************/
/* AddCleanupFunction: Adds a function to the list */
/*   of functions called to perform cleanup such   */
/*   as returning free memory to the memory pool.  */
/***************************************************/
bool AddCleanupFunction(
  Environment *theEnv,
  const char *name,
  void (*theFunction)(Environment *),
  int priority)
  {
   UtilityData(theEnv)->ListOfCleanupFunctions =
     AddVoidFunctionToCallList(theEnv,name,priority,theFunction,
                           UtilityData(theEnv)->ListOfCleanupFunctions);
   return true;
  }

/******************************************************/
/* EnvGetPeriodicFunctionContext: Returns the context */
/*   associated with a periodic function.             */
/******************************************************/
void *EnvGetPeriodicFunctionContext(
  Environment *theEnv,
  const char *name)
  {
   struct voidCallFunctionItem *theItem;

   theItem = GetVoidFunctionFromCallList(theEnv,name,
                                     UtilityData(theEnv)->ListOfPeriodicFunctions);

   if (theItem == NULL) return NULL;

   return theItem->context;
  }

/*************************************************************/
/* EnvAddPeriodicFunctionWithContext: Adds a function to the */
/*   list of functions called to handle periodic tasks.      */
/*************************************************************/
bool EnvAddPeriodicFunctionWithContext(
  Environment *theEnv,
  const char *name,
  void (*theFunction)(Environment *),
  int priority,
  void *context)
  {
   UtilityData(theEnv)->ListOfPeriodicFunctions =
     AddVoidFunctionToCallListWithContext(theEnv,name,priority,theFunction,
                           UtilityData(theEnv)->ListOfPeriodicFunctions,context);
   return true;
  }

/*******************************************************/
/* EnvAddPeriodicFunction: Adds a function to the list */
/*   of functions called to handle periodic tasks.     */
/*******************************************************/
bool EnvAddPeriodicFunction(
  Environment *theEnv,
  const char *name,
  void (*theFunction)(Environment *),
  int priority)
  {
   return EnvAddPeriodicFunctionWithContext(theEnv,name,theFunction,priority,NULL);
  }

/*******************************************************/
/* RemoveCleanupFunction: Removes a function from the  */
/*   list of functions called to perform cleanup such  */
/*   as returning free memory to the memory pool.      */
/*******************************************************/
bool RemoveCleanupFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   UtilityData(theEnv)->ListOfCleanupFunctions =
      RemoveVoidFunctionFromCallList(theEnv,name,UtilityData(theEnv)->ListOfCleanupFunctions,&found);

   return found;
  }

/**********************************************************/
/* EnvRemovePeriodicFunction: Removes a function from the */
/*   list of functions called to handle periodic tasks.   */
/**********************************************************/
bool EnvRemovePeriodicFunction(
  Environment *theEnv,
  const char *name)
  {
   bool found;

   UtilityData(theEnv)->ListOfPeriodicFunctions =
      RemoveVoidFunctionFromCallList(theEnv,name,UtilityData(theEnv)->ListOfPeriodicFunctions,&found);

   return found;
  }

/*****************************************************/
/* StringPrintForm: Generates printed representation */
/*   of a string. Replaces / with // and " with /".  */
/*****************************************************/
const char *StringPrintForm(
  Environment *theEnv,
  const char *str)
  {
   int i = 0;
   size_t pos = 0;
   size_t max = 0;
   char *theString = NULL;
   CLIPSLexeme *thePtr;

   theString = ExpandStringWithChar(theEnv,'"',theString,&pos,&max,max+80);
   while (str[i] != EOS)
     {
      if ((str[i] == '"') || (str[i] == '\\'))
        {
         theString = ExpandStringWithChar(theEnv,'\\',theString,&pos,&max,max+80);
         theString = ExpandStringWithChar(theEnv,str[i],theString,&pos,&max,max+80);
        }
      else
        { theString = ExpandStringWithChar(theEnv,str[i],theString,&pos,&max,max+80); }
      i++;
     }

   theString = ExpandStringWithChar(theEnv,'"',theString,&pos,&max,max+80);

   thePtr = EnvCreateString(theEnv,theString);
   rm(theEnv,theString,max);
   
   return thePtr->contents;
  }

/**************************************************************/
/* CopyString: Copies a string using CLIPS memory management. */
/**************************************************************/
char *CopyString(
  Environment *theEnv,
  const char *theString)
  {
   char *stringCopy = NULL;

   if (theString != NULL)
     {
      stringCopy = (char *) genalloc(theEnv,strlen(theString) + 1);
      genstrcpy(stringCopy,theString);
     }

   return stringCopy;
  }

/*****************************************************************/
/* DeleteString: Deletes a string using CLIPS memory management. */
/*****************************************************************/
void DeleteString(
  Environment *theEnv,
  char *theString)
  {
   if (theString != NULL)
     { genfree(theEnv,theString,strlen(theString) + 1); }
  }

/***********************************************************/
/* AppendStrings: Appends two strings together. The string */
/*   created is added to the SymbolTable, so it is not     */
/*   necessary to deallocate the string returned.          */
/***********************************************************/
const char *AppendStrings(
  Environment *theEnv,
  const char *str1,
  const char *str2)
  {
   size_t pos = 0;
   size_t max = 0;
   char *theString = NULL;
   CLIPSLexeme *thePtr;

   theString = AppendToString(theEnv,str1,theString,&pos,&max);
   theString = AppendToString(theEnv,str2,theString,&pos,&max);

   thePtr = EnvCreateString(theEnv,theString);
   rm(theEnv,theString,max);
   return thePtr->contents;
  }

/******************************************************/
/* AppendToString: Appends a string to another string */
/*   (expanding the other string if necessary).       */
/******************************************************/
char *AppendToString(
  Environment *theEnv,
  const char *appendStr,
  char *oldStr,
  size_t *oldPos,
  size_t *oldMax)
  {
   size_t length;

   /*=========================================*/
   /* Expand the old string so it can contain */
   /* the new string (if necessary).          */
   /*=========================================*/

   length = strlen(appendStr);

   /*==============================================================*/
   /* Return NULL if the old string was not successfully expanded. */
   /*==============================================================*/

   if ((oldStr = EnlargeString(theEnv,length,oldStr,oldPos,oldMax)) == NULL) { return NULL; }

   /*===============================================*/
   /* Append the new string to the expanded string. */
   /*===============================================*/

   genstrcpy(&oldStr[*oldPos],appendStr);
   *oldPos += (int) length;

   /*============================================================*/
   /* Return the expanded string containing the appended string. */
   /*============================================================*/

   return(oldStr);
  }

/**********************************************************/
/* InsertInString: Inserts a string within another string */
/*   (expanding the other string if necessary).           */
/**********************************************************/
char *InsertInString(
  Environment *theEnv,
  const char *insertStr,
  size_t position,
  char *oldStr,
  size_t *oldPos,
  size_t *oldMax)
  {
   size_t length;

   /*=========================================*/
   /* Expand the old string so it can contain */
   /* the new string (if necessary).          */
   /*=========================================*/

   length = strlen(insertStr);

   /*==============================================================*/
   /* Return NULL if the old string was not successfully expanded. */
   /*==============================================================*/

   if ((oldStr = EnlargeString(theEnv,length,oldStr,oldPos,oldMax)) == NULL) { return NULL; }

   /*================================================================*/
   /* Shift the contents to the right of insertion point so that the */
   /* new text does not overwrite what is currently in the string.   */
   /*================================================================*/

   memmove(&oldStr[position],&oldStr[position+length],*oldPos - position);

   /*===============================================*/
   /* Insert the new string in the expanded string. */
   /*===============================================*/

   genstrncpy(&oldStr[*oldPos],insertStr,length);
   *oldPos += (int) length;

   /*============================================================*/
   /* Return the expanded string containing the appended string. */
   /*============================================================*/

   return(oldStr);
  }

/*******************************************************************/
/* EnlargeString: Enlarges a string by the specified amount.       */
/*******************************************************************/
char *EnlargeString(
  Environment *theEnv,
  size_t length,
  char *oldStr,
  size_t *oldPos,
  size_t *oldMax)
  {
   /*=========================================*/
   /* Expand the old string so it can contain */
   /* the new string (if necessary).          */
   /*=========================================*/

   if (length + *oldPos + 1 > *oldMax)
     {
      oldStr = (char *) genrealloc(theEnv,oldStr,*oldMax,length + *oldPos + 1);
      *oldMax = length + *oldPos + 1;
     }

   /*==============================================================*/
   /* Return NULL if the old string was not successfully expanded. */
   /*==============================================================*/

   if (oldStr == NULL) { return NULL; }

   return(oldStr);
  }

/*******************************************************/
/* AppendNToString: Appends a string to another string */
/*   (expanding the other string if necessary). Only a */
/*   specified number of characters are appended from  */
/*   the string.                                       */
/*******************************************************/
char *AppendNToString(
  Environment *theEnv,
  const char *appendStr,
  char *oldStr,
  size_t length,
  size_t *oldPos,
  size_t *oldMax)
  {
   size_t lengthWithEOS;

   /*====================================*/
   /* Determine the number of characters */
   /* to be appended from the string.    */
   /*====================================*/

   if (appendStr[length-1] != '\0') lengthWithEOS = length + 1;
   else lengthWithEOS = length;

   /*=========================================*/
   /* Expand the old string so it can contain */
   /* the new string (if necessary).          */
   /*=========================================*/

   if (lengthWithEOS + *oldPos > *oldMax)
     {
      oldStr = (char *) genrealloc(theEnv,oldStr,*oldMax,*oldPos + lengthWithEOS);
      *oldMax = *oldPos + lengthWithEOS;
     }

   /*==============================================================*/
   /* Return NULL if the old string was not successfully expanded. */
   /*==============================================================*/

   if (oldStr == NULL) { return NULL; }

   /*==================================*/
   /* Append N characters from the new */
   /* string to the expanded string.   */
   /*==================================*/

   genstrncpy(&oldStr[*oldPos],appendStr,length);
   *oldPos += (lengthWithEOS - 1);
   oldStr[*oldPos] = '\0';

   /*============================================================*/
   /* Return the expanded string containing the appended string. */
   /*============================================================*/

   return(oldStr);
  }

/*******************************************************/
/* ExpandStringWithChar: Adds a character to a string, */
/*   reallocating space for the string if it needs to  */
/*   be enlarged. The backspace character causes the   */
/*   size of the string to reduced if it is "added" to */
/*   the string.                                       */
/*******************************************************/
char *ExpandStringWithChar(
  Environment *theEnv,
  int inchar,
  char *str,
  size_t *pos,
  size_t *max,
  size_t newSize)
  {
   if ((*pos + 1) >= *max)
     {
      str = (char *) genrealloc(theEnv,str,*max,newSize);
      *max = newSize;
     }

  if (inchar != '\b')
    {
     str[*pos] = (char) inchar;
     (*pos)++;
     str[*pos] = '\0';
    }
  else
    {
     /*===========================================================*/
     /* First delete any UTF-8 multibyte continuation characters. */
     /*===========================================================*/

     while ((*pos > 1) && IsUTF8MultiByteContinuation(str[*pos - 1]))
       { (*pos)--; }

     /*===================================================*/
     /* Now delete the first byte of the UTF-8 character. */
     /*===================================================*/

     if (*pos > 0) (*pos)--;
     str[*pos] = '\0';
    }

   return(str);
  }

/**************************************************************/
/* AddVoidFunctionToCallList: Adds a function to a list of    */
/*   functions which are called to perform certain operations */
/*   (e.g. clear, reset, and bload functions).                */
/**************************************************************/
struct voidCallFunctionItem *AddVoidFunctionToCallList(
  Environment *theEnv,
  const char *name,
  int priority,
  void (*func)(Environment *),
  struct voidCallFunctionItem *head)
  {
   return AddVoidFunctionToCallListWithContext(theEnv,name,priority,func,head,NULL);
  }

/**************************************************************/
/* AddBoolFunctionToCallList: Adds a function to a list of    */
/*   functions which are called to perform certain operations */
/*   (e.g. clear, reset, and bload functions).                */
/**************************************************************/
struct boolCallFunctionItem *AddBoolFunctionToCallList(
  Environment *theEnv,
  const char *name,
  int priority,
  bool (*func)(Environment *),
  struct boolCallFunctionItem *head)
  {
   return AddBoolFunctionToCallListWithContext(theEnv,name,priority,func,head,NULL);
  }

/**************************************************************/
/* AddSaveFunctionToCallList: Adds a function to a list of    */
/*   functions which are called to perform certain operations */
/*   (e.g. clear, reset, and bload functions).                */
/**************************************************************/
struct saveCallFunctionItem *AddSaveFunctionToCallList(
  Environment *theEnv,
  const char *name,
  int priority,
  void (*func)(Environment *,Defmodule *,const char *),
  struct saveCallFunctionItem *head)
  {
   return AddSaveFunctionToCallListWithContext(theEnv,name,priority,func,head,NULL);
  }

/*************************************************************/
/* AddVoidFunctionToCallListWithContext: Adds a function to  */
/*   a list of functions which are called to perform certain */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
struct voidCallFunctionItem *AddVoidFunctionToCallListWithContext(
  Environment *theEnv,
  const char *name,
  int priority,
  void (*func)(Environment *),
  struct voidCallFunctionItem *head,
  void *context)
  {
   struct voidCallFunctionItem *newPtr, *currentPtr, *lastPtr = NULL;
   char  *nameCopy;

   newPtr = get_struct(theEnv,voidCallFunctionItem);

   nameCopy = (char *) genalloc(theEnv,strlen(name) + 1);
   genstrcpy(nameCopy,name);
   newPtr->name = nameCopy;

   newPtr->func = func;
   newPtr->priority = priority;
   newPtr->context = context;

   if (head == NULL)
     {
      newPtr->next = NULL;
      return(newPtr);
     }

   currentPtr = head;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : false)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = head;
      head = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(head);
  }

/*************************************************************/
/* AddBoolFunctionToCallListWithContext: Adds a function to  */
/*   a list of functions which are called to perform certain */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
struct boolCallFunctionItem *AddBoolFunctionToCallListWithContext(
  Environment *theEnv,
  const char *name,
  int priority,
  bool (*func)(Environment *),
  struct boolCallFunctionItem *head,
  void *context)
  {
   struct boolCallFunctionItem *newPtr, *currentPtr, *lastPtr = NULL;
   char  *nameCopy;

   newPtr = get_struct(theEnv,boolCallFunctionItem);

   nameCopy = (char *) genalloc(theEnv,strlen(name) + 1);
   genstrcpy(nameCopy,name);
   newPtr->name = nameCopy;

   newPtr->func = func;
   newPtr->priority = priority;
   newPtr->context = context;

   if (head == NULL)
     {
      newPtr->next = NULL;
      return(newPtr);
     }

   currentPtr = head;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : false)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = head;
      head = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(head);
  }

/*************************************************************/
/* AddSaveFunctionToCallListWithContext: Adds a function to  */
/*   a list of functions which are called to perform certain */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
struct saveCallFunctionItem *AddSaveFunctionToCallListWithContext(
  Environment *theEnv,
  const char *name,
  int priority,
  void (*func)(Environment *,Defmodule *,const char *),
  struct saveCallFunctionItem *head,
  void *context)
  {
   struct saveCallFunctionItem *newPtr, *currentPtr, *lastPtr = NULL;
   char  *nameCopy;

   newPtr = get_struct(theEnv,saveCallFunctionItem);

   nameCopy = (char *) genalloc(theEnv,strlen(name) + 1);
   genstrcpy(nameCopy,name);
   newPtr->name = nameCopy;

   newPtr->func = func;
   newPtr->priority = priority;
   newPtr->context = context;

   if (head == NULL)
     {
      newPtr->next = NULL;
      return(newPtr);
     }

   currentPtr = head;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : false)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = head;
      head = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(head);
  }

/****************************************************************/
/* GetVoidFunctionFromCallList: Retrieves a function from a list of */
/*   functions which are called to perform certain operations   */
/*   (e.g. clear, reset, and bload functions).                  */
/****************************************************************/
struct voidCallFunctionItem *GetVoidFunctionFromCallList(
  Environment *theEnv,
  const char *name,
  struct voidCallFunctionItem *head)
  {
   struct voidCallFunctionItem *currentPtr;

   for (currentPtr = head; currentPtr != NULL; currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->name) == 0)
        { return currentPtr; }
     }

   return NULL;
  }

/****************************************************************/
/* GetBoolFunctionFromCallList: Retrieves a function from a list of */
/*   functions which are called to perform certain operations   */
/*   (e.g. clear, reset, and bload functions).                  */
/****************************************************************/
struct boolCallFunctionItem *GetBoolFunctionFromCallList(
  Environment *theEnv,
  const char *name,
  struct boolCallFunctionItem *head)
  {
   struct boolCallFunctionItem *currentPtr;

   for (currentPtr = head; currentPtr != NULL; currentPtr = currentPtr->next)
     {
      if (strcmp(name,currentPtr->name) == 0)
        { return currentPtr; }
     }

   return NULL;
  }

/******************************************************************/
/* RemoveVoidFunctionFromCallList: Removes a function from a list */
/*   of functions which are called to perform certain operations  */
/*   (e.g. clear, reset, and bload functions).                    */
/******************************************************************/
struct voidCallFunctionItem *RemoveVoidFunctionFromCallList(
  Environment *theEnv,
  const char *name,
  struct voidCallFunctionItem *head,
  bool *found)
  {
   struct voidCallFunctionItem *currentPtr, *lastPtr;

   *found = false;
   lastPtr = NULL;
   currentPtr = head;

   while (currentPtr != NULL)
     {
      if (strcmp(name,currentPtr->name) == 0)
        {
         *found = true;
         if (lastPtr == NULL)
           { head = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }

         genfree(theEnv,(void *) currentPtr->name,strlen(currentPtr->name) + 1);
         rtn_struct(theEnv,voidCallFunctionItem,currentPtr);
         return head;
        }

      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return head;
  }

/******************************************************************/
/* RemoveBoolFunctionFromCallList: Removes a function from a list */
/*   of functions which are called to perform certain operations  */
/*   (e.g. clear, reset, and bload functions).                    */
/******************************************************************/
struct boolCallFunctionItem *RemoveBoolFunctionFromCallList(
  Environment *theEnv,
  const char *name,
  struct boolCallFunctionItem *head,
  bool *found)
  {
   struct boolCallFunctionItem *currentPtr, *lastPtr;

   *found = false;
   lastPtr = NULL;
   currentPtr = head;

   while (currentPtr != NULL)
     {
      if (strcmp(name,currentPtr->name) == 0)
        {
         *found = true;
         if (lastPtr == NULL)
           { head = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }

         genfree(theEnv,(void *) currentPtr->name,strlen(currentPtr->name) + 1);
         rtn_struct(theEnv,boolCallFunctionItem,currentPtr);
         return head;
        }

      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return head;
  }

/******************************************************************/
/* RemoveSaveFunctionFromCallList: Removes a function from a list */
/*   of functions which are called to perform certain operations  */
/*   (e.g. clear, reset, and bload functions).                    */
/******************************************************************/
struct saveCallFunctionItem *RemoveSaveFunctionFromCallList(
  Environment *theEnv,
  const char *name,
  struct saveCallFunctionItem *head,
  bool *found)
  {
   struct saveCallFunctionItem *currentPtr, *lastPtr;

   *found = false;
   lastPtr = NULL;
   currentPtr = head;

   while (currentPtr != NULL)
     {
      if (strcmp(name,currentPtr->name) == 0)
        {
         *found = true;
         if (lastPtr == NULL)
           { head = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }

         genfree(theEnv,(void *) currentPtr->name,strlen(currentPtr->name) + 1);
         rtn_struct(theEnv,saveCallFunctionItem,currentPtr);
         return head;
        }

      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return head;
  }

/*************************************************************/
/* DeallocateVoidCallList: Removes all functions from a list */
/*   of functions which are called to perform certain        */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
void DeallocateVoidCallList(
  Environment *theEnv,
  struct voidCallFunctionItem *theList)
  {
   struct voidCallFunctionItem *tmpPtr, *nextPtr;

   tmpPtr = theList;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      genfree(theEnv,(void *) tmpPtr->name,strlen(tmpPtr->name) + 1);
      rtn_struct(theEnv,voidCallFunctionItem,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/*************************************************************/
/* DeallocateBoolCallList: Removes all functions from a list */
/*   of functions which are called to perform certain        */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
void DeallocateBoolCallList(
  Environment *theEnv,
  struct boolCallFunctionItem *theList)
  {
   struct boolCallFunctionItem *tmpPtr, *nextPtr;

   tmpPtr = theList;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      genfree(theEnv,(void *) tmpPtr->name,strlen(tmpPtr->name) + 1);
      rtn_struct(theEnv,boolCallFunctionItem,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/*************************************************************/
/* DeallocateSaveCallList: Removes all functions from a list */
/*   of functions which are called to perform certain        */
/*   operations (e.g. clear, reset, and bload functions).    */
/*************************************************************/
void DeallocateSaveCallList(
  Environment *theEnv,
  struct saveCallFunctionItem *theList)
  {
   struct saveCallFunctionItem *tmpPtr, *nextPtr;

   tmpPtr = theList;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      genfree(theEnv,(void *) tmpPtr->name,strlen(tmpPtr->name) + 1);
      rtn_struct(theEnv,boolCallFunctionItem,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/***************************************************************/
/* AddFunctionToCallListWithArg: Adds a function to a list of  */
/*   functions which are called to perform certain operations  */
/*   (e.g. clear,reset, and bload functions).                  */
/***************************************************************/
struct callFunctionItemWithArg *AddFunctionToCallListWithArg(
  Environment *theEnv,
  const char *name,
  int priority,
  void (*func)(Environment *, void *),
  struct callFunctionItemWithArg *head)
  {
   return AddFunctionToCallListWithArgWithContext(theEnv,name,priority,func,head,NULL);
  }

/***************************************************************/
/* AddFunctionToCallListWithArgWithContext: Adds a function to */
/*   a list of functions which are called to perform certain   */
/*   operations (e.g. clear, reset, and bload functions).      */
/***************************************************************/
struct callFunctionItemWithArg *AddFunctionToCallListWithArgWithContext(
  Environment *theEnv,
  const char *name,
  int priority,
  void (*func)(Environment *,void *),
  struct callFunctionItemWithArg *head,
  void *context)
  {
   struct callFunctionItemWithArg *newPtr, *currentPtr, *lastPtr = NULL;

   newPtr = get_struct(theEnv,callFunctionItemWithArg);

   newPtr->name = name;
   newPtr->func = func;
   newPtr->priority = priority;
   newPtr->context = context;

   if (head == NULL)
     {
      newPtr->next = NULL;
      return(newPtr);
     }

   currentPtr = head;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : false)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = head;
      head = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return(head);
  }

/**************************************************************/
/* RemoveFunctionFromCallListWithArg: Removes a function from */
/*   a list of functions which are called to perform certain  */
/*   operations (e.g. clear, reset, and bload functions).     */
/**************************************************************/
struct callFunctionItemWithArg *RemoveFunctionFromCallListWithArg(
  Environment *theEnv,
  const char *name,
  struct callFunctionItemWithArg *head,
  bool *found)
  {
   struct callFunctionItemWithArg *currentPtr, *lastPtr;

   *found = false;
   lastPtr = NULL;
   currentPtr = head;

   while (currentPtr != NULL)
     {
      if (strcmp(name,currentPtr->name) == 0)
        {
         *found = true;
         if (lastPtr == NULL)
           { head = currentPtr->next; }
         else
           { lastPtr->next = currentPtr->next; }

         rtn_struct(theEnv,callFunctionItemWithArg,currentPtr);
         return(head);
        }

      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return(head);
  }

/**************************************************************/
/* DeallocateCallListWithArg: Removes all functions from a list of   */
/*   functions which are called to perform certain operations */
/*   (e.g. clear, reset, and bload functions).                */
/**************************************************************/
void DeallocateCallListWithArg(
  Environment *theEnv,
  struct callFunctionItemWithArg *theList)
  {
   struct callFunctionItemWithArg *tmpPtr, *nextPtr;

   tmpPtr = theList;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      rtn_struct(theEnv,callFunctionItemWithArg,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/*****************************************/
/* ItemHashValue: Returns the hash value */
/*   for the specified value.            */
/*****************************************/
unsigned long ItemHashValue(
  Environment *theEnv,
  unsigned short theType,
  void *theValue,
  unsigned long theRange)
  {
   union
     {
      void *vv;
      unsigned uv;
     } fis;

   switch(theType)
     {
      case FLOAT_TYPE:
        return(HashFloat(((CLIPSFloat *) theValue)->contents,theRange));

      case INTEGER_TYPE:
        return(HashInteger(((CLIPSInteger *) theValue)->contents,theRange));

      case SYMBOL_TYPE:
      case STRING_TYPE:
#if OBJECT_SYSTEM
      case INSTANCE_NAME_TYPE:
#endif
        return HashSymbol(((CLIPSLexeme *) theValue)->contents,theRange);

      case MULTIFIELD_TYPE:
        return(HashMultifield((Multifield *) theValue,theRange));

#if DEFTEMPLATE_CONSTRUCT
      case FACT_ADDRESS_TYPE:
        return(((Fact *) theValue)->hashValue % theRange);
#endif

      case EXTERNAL_ADDRESS_TYPE:
        return HashExternalAddress(((CLIPSExternalAddress *) theValue)->contents,theRange);

#if OBJECT_SYSTEM
      case INSTANCE_ADDRESS_TYPE:
#endif
        fis.uv = 0;
        fis.vv = theValue;
        return(fis.uv % theRange);
     }

   SystemError(theEnv,"UTILITY",1);
   return 0;
  }

/********************************************/
/* YieldTime: Yields time to a user-defined */
/*   function. Intended to allow foreground */
/*   application responsiveness when CLIPS  */
/*   is running in the background.          */
/********************************************/
void YieldTime(
  Environment *theEnv)
  {
   if ((UtilityData(theEnv)->YieldTimeFunction != NULL) && UtilityData(theEnv)->YieldFunctionEnabled)
     { (*UtilityData(theEnv)->YieldTimeFunction)(); }
  }

/**********************************************/
/* EnvIncrementGCLocks: Increments the number */
/*   of garbage collection locks.             */
/**********************************************/
void EnvIncrementGCLocks(
  Environment *theEnv)
  {
   UtilityData(theEnv)->GarbageCollectionLocks++;
  }

/**********************************************/
/* EnvDecrementGCLocks: Decrements the number */
/*   of garbage collection locks.             */
/**********************************************/
void EnvDecrementGCLocks(
  Environment *theEnv)
  {
   if (UtilityData(theEnv)->GarbageCollectionLocks > 0)
     { UtilityData(theEnv)->GarbageCollectionLocks--; }

   if ((UtilityData(theEnv)->CurrentGarbageFrame->topLevel) && (! CommandLineData(theEnv)->EvaluatingTopLevelCommand) &&
       (EvaluationData(theEnv)->CurrentExpression == NULL) && (UtilityData(theEnv)->GarbageCollectionLocks == 0))
     {
      CleanCurrentGarbageFrame(theEnv,NULL);
      CallPeriodicTasks(theEnv);
     }
  }

/****************************/
/* EnablePeriodicFunctions: */
/****************************/
bool EnablePeriodicFunctions(
  Environment *theEnv,
  bool value)
  {
   bool oldValue;

   oldValue = UtilityData(theEnv)->PeriodicFunctionsEnabled;

   UtilityData(theEnv)->PeriodicFunctionsEnabled = value;

   return(oldValue);
  }

/************************/
/* EnableYieldFunction: */
/************************/
short EnableYieldFunction(
  Environment *theEnv,
  short value)
  {
   short oldValue;

   oldValue = UtilityData(theEnv)->YieldFunctionEnabled;

   UtilityData(theEnv)->YieldFunctionEnabled = value;

   return(oldValue);
  }

/*************************************************************************/
/* AddTrackedMemory: Tracked memory is memory allocated by CLIPS that's  */
/*   referenced by a variable on the stack, but not by any environment   */
/*   data structure. An example would be the storage for local variables */
/*   allocated when a deffunction is executed. Tracking this memory      */
/*   allows it to be removed later when using longjmp as the code that   */
/*   would normally deallocate the memory would be bypassed.             */
/*************************************************************************/
struct trackedMemory *AddTrackedMemory(
  Environment *theEnv,
  void *theMemory,
  size_t theSize)
  {
   struct trackedMemory *newPtr;

   newPtr = get_struct(theEnv,trackedMemory);

   newPtr->prev = NULL;
   newPtr->theMemory = theMemory;
   newPtr->memSize = theSize;
   newPtr->next = UtilityData(theEnv)->trackList;
   UtilityData(theEnv)->trackList = newPtr;

   return newPtr;
  }

/************************/
/* RemoveTrackedMemory: */
/************************/
void RemoveTrackedMemory(
  Environment *theEnv,
  struct trackedMemory *theTracker)
  {
   if (theTracker->prev == NULL)
     { UtilityData(theEnv)->trackList = theTracker->next; }
   else
     { theTracker->prev->next = theTracker->next; }

   if (theTracker->next != NULL)
     { theTracker->next->prev = theTracker->prev; }

   rtn_struct(theEnv,trackedMemory,theTracker);
  }

/******************************************/
/* UTF8Length: Returns the logical number */
/*   of characters in a UTF8 string.      */
/******************************************/
size_t UTF8Length(
  const char *s)
  {
   size_t i = 0, length = 0;

   while (s[i] != '\0')
     {
      UTF8Increment(s,&i);
      length++;
     }

   return(length);
  }

/*********************************************/
/* UTF8Increment: Finds the beginning of the */
/*   next character in a UTF8 string.        */
/*********************************************/
void UTF8Increment(
  const char *s,
  size_t *i)
  {
   (void) (IsUTF8Start(s[++(*i)]) ||
           IsUTF8Start(s[++(*i)]) ||
           IsUTF8Start(s[++(*i)]) ||
           ++(*i));
  }

/****************************************************/
/* UTF8Offset: Converts the logical character index */
/*   in a UTF8 string to the actual byte offset.    */
/****************************************************/
size_t UTF8Offset(
  const char *str,
  size_t charnum)
  {
   size_t offs = 0;

   while ((charnum > 0) && (str[offs]))
     {
      (void) (IsUTF8Start(str[++offs]) ||
              IsUTF8Start(str[++offs]) ||
              IsUTF8Start(str[++offs]) ||
              ++offs);

      charnum--;
     }

   return offs;
  }

/*************************************************/
/* UTF8CharNum: Converts the UTF8 character byte */
/*   offset to the logical character index.      */
/*************************************************/
size_t UTF8CharNum(
  const char *s,
  size_t offset)
  {
   size_t charnum = 0, offs=0;

   while ((offs < offset) && (s[offs]))
     {
      (void) (IsUTF8Start(s[++offs]) ||
              IsUTF8Start(s[++offs]) ||
              IsUTF8Start(s[++offs]) ||
              ++offs);

      charnum++;
     }

   return charnum;
  }

/***************************/
/* EnvCreateStringBuilder: */
/***************************/
StringBuilder *EnvCreateStringBuilder(
  Environment *theEnv,
  size_t theSize)
  {
   StringBuilder *theSB;
   
   theSB = get_struct(theEnv,stringBuilder);
   if (theSB == NULL) return NULL;
   
   theSize++;
   theSB->sbEnv = theEnv;
   theSB->bufferReset = theSize;
   theSB->bufferMaximum = theSize;
   theSB->bufferPosition = 0;
   theSB->stringBuffer = (char *) genalloc(theEnv,theSize);
   theSB->stringBuffer[0] = EOS;
   
   return theSB;
  }

/************************/
/* StringBuilderAppend: */
/************************/
void StringBuilderAppend(
  StringBuilder *theSB,
  const char *appendString)
  {
   theSB->stringBuffer = AppendToString(theSB->sbEnv,appendString,
                                        theSB->stringBuffer,&theSB->bufferPosition,&theSB->bufferMaximum);
  }

/**********************/
/* StringBuilderReset */
/**********************/
void StringBuilderReset(
  StringBuilder *theSB)
  {
   if (theSB->bufferReset != theSB->bufferMaximum)
     {
      rm(theSB->sbEnv,theSB->stringBuffer,theSB->bufferMaximum);
      theSB->stringBuffer = (char *) genalloc(theSB->sbEnv,theSB->bufferReset);
     }
     
   theSB->bufferPosition = 0;
   theSB->stringBuffer[0] = EOS;
  }

/*********************/
/* StringBuilderCopy */
/*********************/
char *StringBuilderCopy(
  StringBuilder *theSB)
  {
   char *stringCopy;

   stringCopy = (char *) malloc(strlen(theSB->stringBuffer) + 1);
   strcpy(stringCopy,theSB->stringBuffer);

   return stringCopy;
  }

/*************************/
/* StringBuilderDispose: */
/*************************/
void StringBuilderDispose(
  StringBuilder *theSB)
  {
   Environment *theEnv = theSB->sbEnv;
   rm(theEnv,theSB->stringBuffer,theSB->bufferMaximum);
   rtn_struct(theEnv,stringBuilder,theSB);
  }

