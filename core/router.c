   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  11/01/16             */
   /*                                                     */
   /*                  I/O ROUTER MODULE                  */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides a centralized mechanism for handling    */
/*   input and output requests.                              */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Removed conversion of '\r' to '\n' from the    */
/*            EnvGetcRouter function.                        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added support for passing context information  */
/*            to the router functions.                       */
/*                                                           */
/*      6.30: Fixed issues with passing context to routers.  */
/*                                                           */
/*            Added AwaitingInput flag.                      */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Added EnvInputBufferCount function.            */
/*                                                           */
/*            Added check for reuse of existing router name. */
/*                                                           */
/*            Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Changed return values for router functions.    */
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
/*************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "setup.h"

#include "argacces.h"
#include "constant.h"
#include "envrnmnt.h"
#include "extnfunc.h"
#include "filertr.h"
#include "memalloc.h"
#include "prntutil.h"
#include "scanner.h"
#include "strngrtr.h"
#include "sysdep.h"

#include "router.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static bool                    QueryRouter(Environment *,const char *,struct router *);
   static void                    DeallocateRouterData(Environment *);

/*********************************************************/
/* InitializeDefaultRouters: Initializes output streams. */
/*********************************************************/
void InitializeDefaultRouters(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,ROUTER_DATA,sizeof(struct routerData),DeallocateRouterData);

   RouterData(theEnv)->CommandBufferInputCount = 0;
   RouterData(theEnv)->AwaitingInput = true;

   InitializeFileRouter(theEnv);
   InitializeStringRouter(theEnv);
  }

/*************************************************/
/* DeallocateRouterData: Deallocates environment */
/*    data for I/O routers.                      */
/*************************************************/
static void DeallocateRouterData(
  Environment *theEnv)
  {
   struct router *tmpPtr, *nextPtr;

   tmpPtr = RouterData(theEnv)->ListOfRouters;
   while (tmpPtr != NULL)
     {
      nextPtr = tmpPtr->next;
      genfree(theEnv,(void *) tmpPtr->name,strlen(tmpPtr->name) + 1);
      rtn_struct(theEnv,router,tmpPtr);
      tmpPtr = nextPtr;
     }
  }

/*******************************************/
/* EnvPrintRouter: Generic print function. */
/*******************************************/
void EnvPrintRouter(
  Environment *theEnv,
  const char *logicalName,
  const char *str)
  {
   struct router *currentPtr;

   /*===================================================*/
   /* If the "fast save" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* fprintf can be called directly to bypass querying */
   /* all of the routers.                               */
   /*===================================================*/

   if (((char *) RouterData(theEnv)->FastSaveFilePtr) == logicalName)
     {
      fprintf(RouterData(theEnv)->FastSaveFilePtr,"%s",str);
      return;
     }

   /*==============================================*/
   /* Search through the list of routers until one */
   /* is found that will handle the print request. */
   /*==============================================*/

   currentPtr = RouterData(theEnv)->ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->printCallback != NULL) ? QueryRouter(theEnv,logicalName,currentPtr) : false)
        {
         SetEnvironmentRouterContext(theEnv,currentPtr->context);
         (*currentPtr->printCallback)(theEnv,logicalName,str);
         return;
        }
      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/

   if (strcmp(WERROR,logicalName) != 0)
     { UnrecognizedRouterMessage(theEnv,logicalName); }
  }

/**************************************************/
/* EnvGetcRouter: Generic get character function. */
/**************************************************/
int EnvGetcRouter(
  Environment *theEnv,
  const char *logicalName)
  {
   struct router *currentPtr;
   int inchar;

   /*===================================================*/
   /* If the "fast load" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* getc can be called directly to bypass querying    */
   /* all of the routers.                               */
   /*===================================================*/

   if (((char *) RouterData(theEnv)->FastLoadFilePtr) == logicalName)
     {
      inchar = getc(RouterData(theEnv)->FastLoadFilePtr);

      if ((inchar == '\r') || (inchar == '\n'))
        {
         if (((char *) RouterData(theEnv)->FastLoadFilePtr) == RouterData(theEnv)->LineCountRouter)
           { IncrementLineCount(theEnv); }
        }

      /* if (inchar == '\r') return('\n'); */

      return(inchar);
     }

   /*===============================================*/
   /* If the "fast string get" option is being used */
   /* for the specified logical name, then bypass   */
   /* the router system and extract the character   */
   /* directly from the fast get string.            */
   /*===============================================*/

   if (RouterData(theEnv)->FastCharGetRouter == logicalName)
     {
      inchar = (unsigned char) RouterData(theEnv)->FastCharGetString[RouterData(theEnv)->FastCharGetIndex];

      RouterData(theEnv)->FastCharGetIndex++;

      if (inchar == '\0') return(EOF);

      if ((inchar == '\r') || (inchar == '\n'))
        {
         if (RouterData(theEnv)->FastCharGetRouter == RouterData(theEnv)->LineCountRouter)
           { IncrementLineCount(theEnv); }
        }

      return(inchar);
     }

   /*==============================================*/
   /* Search through the list of routers until one */
   /* is found that will handle the getc request.  */
   /*==============================================*/

   currentPtr = RouterData(theEnv)->ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->getcCallback != NULL) ? QueryRouter(theEnv,logicalName,currentPtr) : false)
        {
         SetEnvironmentRouterContext(theEnv,currentPtr->context);
         inchar = (*currentPtr->getcCallback)(theEnv,logicalName);

         if ((inchar == '\r') || (inchar == '\n'))
           {
            if ((RouterData(theEnv)->LineCountRouter != NULL) &&
                (strcmp(logicalName,RouterData(theEnv)->LineCountRouter) == 0))
              { IncrementLineCount(theEnv); }
           }

         return(inchar);
        }
      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/

   UnrecognizedRouterMessage(theEnv,logicalName);
   return(-1);
  }

/******************************************************/
/* EnvUngetcRouter: Generic unget character function. */
/******************************************************/
int EnvUngetcRouter(
  Environment *theEnv,
  int ch,
  const char *logicalName)
  {
   struct router *currentPtr;

   /*===================================================*/
   /* If the "fast load" option is being used, then the */
   /* logical name is actually a pointer to a file and  */
   /* ungetc can be called directly to bypass querying  */
   /* all of the routers.                               */
   /*===================================================*/

   if (((char *) RouterData(theEnv)->FastLoadFilePtr) == logicalName)
     {
      if ((ch == '\r') || (ch == '\n'))
        {
         if (((char *) RouterData(theEnv)->FastLoadFilePtr) == RouterData(theEnv)->LineCountRouter)
           { DecrementLineCount(theEnv); }
        }

      return ungetc(ch,RouterData(theEnv)->FastLoadFilePtr);
     }

   /*===============================================*/
   /* If the "fast string get" option is being used */
   /* for the specified logical name, then bypass   */
   /* the router system and unget the character     */
   /* directly from the fast get string.            */
   /*===============================================*/

   if (RouterData(theEnv)->FastCharGetRouter == logicalName)
     {
      if ((ch == '\r') || (ch == '\n'))
        {
         if (RouterData(theEnv)->FastCharGetRouter == RouterData(theEnv)->LineCountRouter)
           { DecrementLineCount(theEnv); }
        }

      if (RouterData(theEnv)->FastCharGetIndex > 0) RouterData(theEnv)->FastCharGetIndex--;
      return ch;
     }

   /*===============================================*/
   /* Search through the list of routers until one  */
   /* is found that will handle the ungetc request. */
   /*===============================================*/

   currentPtr = RouterData(theEnv)->ListOfRouters;
   while (currentPtr != NULL)
     {
      if ((currentPtr->ungetcCallback != NULL) ? QueryRouter(theEnv,logicalName,currentPtr) : false)
        {
         if ((ch == '\r') || (ch == '\n'))
           {
            if ((RouterData(theEnv)->LineCountRouter != NULL) &&
                (strcmp(logicalName,RouterData(theEnv)->LineCountRouter) == 0))
              { DecrementLineCount(theEnv); }
           }

         SetEnvironmentRouterContext(theEnv,currentPtr->context);
         return (*currentPtr->ungetcCallback)(theEnv,ch,logicalName);
        }

      currentPtr = currentPtr->next;
     }

   /*=====================================================*/
   /* The logical name was not recognized by any routers. */
   /*=====================================================*/

   UnrecognizedRouterMessage(theEnv,logicalName);
   return -1;
  }

/***********************************************/
/* EnvExitRouter: Generic exit function. Calls */
/*   all of the router exit functions.         */
/***********************************************/
void EnvExitRouter(
  Environment *theEnv,
  int num)
  {
   struct router *currentPtr, *nextPtr;

   RouterData(theEnv)->Abort = false;
   currentPtr = RouterData(theEnv)->ListOfRouters;
   while (currentPtr != NULL)
     {
      nextPtr = currentPtr->next;
      if (currentPtr->active == true)
        {
         if (currentPtr->exitCallback != NULL)
           {
            SetEnvironmentRouterContext(theEnv,currentPtr->context);
            (*currentPtr->exitCallback)(theEnv,num);
           }
        }
      currentPtr = nextPtr;
     }

   if (RouterData(theEnv)->Abort) return;
   genexit(theEnv,num);
  }

/********************************************/
/* AbortExit: Forces ExitRouter to terminate */
/*   after calling all closing routers.     */
/********************************************/
void AbortExit(
  Environment *theEnv)
  {
   RouterData(theEnv)->Abort = true;
  }

/************************************************************/
/* EnvAddRouter: Adds an I/O router to the list of routers. */
/************************************************************/
bool EnvAddRouter(
  Environment *theEnv,
  const char *routerName,
  int priority,
  RouterQueryFunction *queryFunction,
  RouterPrintFunction *printFunction,
  RouterGetcFunction *getcFunction,
  RouterUngetcFunction *ungetcFunction,
  RouterExitFunction *exitFunction)
  {
   return EnvAddRouterWithContext(theEnv,routerName,priority,
                                  queryFunction,printFunction,getcFunction,
                                  ungetcFunction,exitFunction,NULL);
  }

/***********************************************************************/
/* EnvAddRouterWithContext: Adds an I/O router to the list of routers. */
/***********************************************************************/
bool EnvAddRouterWithContext(
  Environment *theEnv,
  const char *routerName,
  int priority,
  RouterQueryFunction *queryFunction,
  RouterPrintFunction *printFunction,
  RouterGetcFunction *getcFunction,
  RouterUngetcFunction *ungetcFunction,
  RouterExitFunction *exitFunction,
  void *context)
  {
   struct router *newPtr, *lastPtr, *currentPtr;
   char  *nameCopy;

   /*==================================================*/
   /* Reject the router if the name is already in use. */
   /*==================================================*/

   for (currentPtr = RouterData(theEnv)->ListOfRouters;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        { return false; }
     }

   newPtr = get_struct(theEnv,router);

   nameCopy = (char *) genalloc(theEnv,strlen(routerName) + 1);
   genstrcpy(nameCopy,routerName);
   newPtr->name = nameCopy;

   newPtr->active = true;
   newPtr->context = context;
   newPtr->priority = priority;
   newPtr->queryCallback = queryFunction;
   newPtr->printCallback = printFunction;
   newPtr->exitCallback = exitFunction;
   newPtr->getcCallback = getcFunction;
   newPtr->ungetcCallback = ungetcFunction;
   newPtr->next = NULL;

   if (RouterData(theEnv)->ListOfRouters == NULL)
     {
      RouterData(theEnv)->ListOfRouters = newPtr;
      return true;
     }

   lastPtr = NULL;
   currentPtr = RouterData(theEnv)->ListOfRouters;
   while ((currentPtr != NULL) ? (priority < currentPtr->priority) : false)
     {
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   if (lastPtr == NULL)
     {
      newPtr->next = RouterData(theEnv)->ListOfRouters;
      RouterData(theEnv)->ListOfRouters = newPtr;
     }
   else
     {
      newPtr->next = currentPtr;
      lastPtr->next = newPtr;
     }

   return true;
  }

/********************************************************************/
/* EnvDeleteRouter: Removes an I/O router from the list of routers. */
/********************************************************************/
bool EnvDeleteRouter(
  Environment *theEnv,
  const char *routerName)
  {
   struct router *currentPtr, *lastPtr;

   currentPtr = RouterData(theEnv)->ListOfRouters;
   lastPtr = NULL;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         genfree(theEnv,(void *) currentPtr->name,strlen(currentPtr->name) + 1);
         if (lastPtr == NULL)
           {
            RouterData(theEnv)->ListOfRouters = currentPtr->next;
            rm(theEnv,currentPtr,(int) sizeof(struct router));
            return true;
           }
         lastPtr->next = currentPtr->next;
         rm(theEnv,currentPtr,(int) sizeof(struct router));
         return true;
        }
      lastPtr = currentPtr;
      currentPtr = currentPtr->next;
     }

   return false;
  }

/*********************************************************************/
/* QueryRouters: Determines if any router recognizes a logical name. */
/*********************************************************************/
bool QueryRouters(
  Environment *theEnv,
  const char *logicalName)
  {
   struct router *currentPtr;

   currentPtr = RouterData(theEnv)->ListOfRouters;
   while (currentPtr != NULL)
     {
      if (QueryRouter(theEnv,logicalName,currentPtr) == true) return true;
      currentPtr = currentPtr->next;
     }

   return false;
  }

/************************************************/
/* QueryRouter: Determines if a specific router */
/*    recognizes a logical name.                */
/************************************************/
static bool QueryRouter(
  Environment *theEnv,
  const char *logicalName,
  struct router *currentPtr)
  {
   /*===================================================*/
   /* If the router is inactive, then it can't respond. */
   /*===================================================*/

   if (currentPtr->active == false)
     { return false; }

   /*=============================================================*/
   /* If the router has no query function, then it can't respond. */
   /*=============================================================*/

   if (currentPtr->queryCallback == NULL) return false;

   /*=========================================*/
   /* Call the router's query function to see */
   /* if it recognizes the logical name.      */
   /*=========================================*/

   SetEnvironmentRouterContext(theEnv,currentPtr->context);
   if ((*currentPtr->queryCallback)(theEnv,logicalName) == true)
     { return true; }

   return false;
  }

/*******************************************************/
/* EnvDeactivateRouter: Deactivates a specific router. */
/*******************************************************/
bool EnvDeactivateRouter(
  Environment *theEnv,
  const char *routerName)
  {
   struct router *currentPtr;

   currentPtr = RouterData(theEnv)->ListOfRouters;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         currentPtr->active = false;
         return true;
        }
      currentPtr = currentPtr->next;
     }

   return false;
  }

/***************************************************/
/* EnvActivateRouter: Activates a specific router. */
/***************************************************/
bool EnvActivateRouter(
  Environment *theEnv,
  const char *routerName)
  {
   struct router *currentPtr;

   currentPtr = RouterData(theEnv)->ListOfRouters;

   while (currentPtr != NULL)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        {
         currentPtr->active = true;
         return true;
        }
      currentPtr = currentPtr->next;
     }

   return false;
  }

/********************************************/
/* EnvFindRouter: Locates the named router. */
/********************************************/
struct router *EnvFindRouter(
  Environment *theEnv,
  const char *routerName)
  {
   struct router *currentPtr;

   for (currentPtr = RouterData(theEnv)->ListOfRouters;
        currentPtr != NULL;
        currentPtr = currentPtr->next)
     {
      if (strcmp(currentPtr->name,routerName) == 0)
        { return(currentPtr); }
     }

   return NULL;
  }

/********************************************************/
/* SetFastLoad: Used to bypass router system for loads. */
/********************************************************/
void SetFastLoad(
  Environment *theEnv,
  FILE *filePtr)
  {
   RouterData(theEnv)->FastLoadFilePtr = filePtr;
  }

/********************************************************/
/* SetFastSave: Used to bypass router system for saves. */
/********************************************************/
void SetFastSave(
  Environment *theEnv,
  FILE *filePtr)
  {
   RouterData(theEnv)->FastSaveFilePtr = filePtr;
  }

/******************************************************/
/* GetFastLoad: Returns the "fast load" file pointer. */
/******************************************************/
FILE *GetFastLoad(
  Environment *theEnv)
  {
   return(RouterData(theEnv)->FastLoadFilePtr);
  }

/******************************************************/
/* GetFastSave: Returns the "fast save" file pointer. */
/******************************************************/
FILE *GetFastSave(
  Environment *theEnv)
  {
   return(RouterData(theEnv)->FastSaveFilePtr);
  }

/*****************************************************/
/* UnrecognizedRouterMessage: Standard error message */
/*   for an unrecognized router name.                */
/*****************************************************/
void UnrecognizedRouterMessage(
  Environment *theEnv,
  const char *logicalName)
  {
   PrintErrorID(theEnv,"ROUTER",1,false);
   EnvPrintRouter(theEnv,WERROR,"Logical name ");
   EnvPrintRouter(theEnv,WERROR,logicalName);
   EnvPrintRouter(theEnv,WERROR," was not recognized by any routers\n");
  }

/*****************************************/
/* PrintNRouter: Generic print function. */
/*****************************************/
void PrintNRouter(
  Environment *theEnv,
  const char *logicalName,
  const char *str,
  unsigned long length)
  {
   char *tempStr;

   tempStr = (char *) genalloc(theEnv,length+1);
   genstrncpy(tempStr,str,length);
   tempStr[length] = 0;
   EnvPrintRouter(theEnv,logicalName,tempStr);
   genfree(theEnv,tempStr,length+1);
  }

/************************/
/* EnvInputBufferCount: */
/************************/
size_t EnvInputBufferCount(
   Environment *theEnv)
   {
    return RouterData(theEnv)->CommandBufferInputCount;
   }
