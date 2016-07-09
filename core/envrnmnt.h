   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                ENVRNMNT HEADER FILE                 */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for supporting multiple environments.   */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added code to CreateEnvironment to free        */
/*            already allocated data if one of the malloc    */
/*            calls fail.                                    */
/*                                                           */
/*            Modified AllocateEnvironmentData to print a    */
/*            message if it was unable to allocate memory.   */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added CreateRuntimeEnvironment function.       */
/*                                                           */
/*            Added support for context information when an  */
/*            environment is created (i.e a pointer from the */
/*            CLIPS environment to its parent environment).  */
/*                                                           */
/*      6.30: Added support for passing context information  */ 
/*            to user defined functions and callback         */
/*            functions.                                     */
/*                                                           */
/*            Support for hashing EXTERNAL_ADDRESS data      */
/*            type.                                          */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*************************************************************/

#ifndef _H_envrnmnt

#pragma once

#define _H_envrnmnt

#include <stdbool.h>

#include "symbol.h"

#define USER_ENVIRONMENT_DATA 70
#define MAXIMUM_ENVIRONMENT_POSITIONS 100

struct environmentCleanupFunction
  {
   const char *name;
   void (*func)(void *);
   int priority;
   struct environmentCleanupFunction *next;
  };

struct environmentData
  {   
   unsigned int initialized : 1;
   unsigned long environmentIndex;
   void *context;
   void *routerContext;
   void *functionContext;
   void *callbackContext;
   void **theData;
   void (**cleanupFunctions)(void *);
   struct environmentCleanupFunction *listOfCleanupEnvironmentFunctions;
   struct environmentData *next;
  };

typedef struct environmentData ENVIRONMENT_DATA;
typedef struct environmentData * ENVIRONMENT_DATA_PTR;

#define GetEnvironmentData(theEnv,position) (((struct environmentData *) theEnv)->theData[position])
#define SetEnvironmentData(theEnv,position,value) (((struct environmentData *) theEnv)->theData[position] = value)

   bool                           AllocateEnvironmentData(void *,unsigned int,unsigned long,void (*)(void *));
   bool                           DeallocateEnvironmentData(void);
#if ALLOW_ENVIRONMENT_GLOBALS
   void                           SetCurrentEnvironment(void *);
   bool                           SetCurrentEnvironmentByIndex(unsigned long);
   void                          *GetEnvironmentByIndex(unsigned long);
   void                          *GetCurrentEnvironment(void);
   unsigned long                  GetEnvironmentIndex(void *);
#endif
   void                          *CreateEnvironment(void);
   void                          *CreateRuntimeEnvironment(struct symbolHashNode **,struct floatHashNode **,
                                                                  struct integerHashNode **,struct bitMapHashNode **);
   bool                           DestroyEnvironment(void *);
   bool                           AddEnvironmentCleanupFunction(void *,const char *,void (*)(void *),int);
   void                          *GetEnvironmentContext(void *);
   void                          *SetEnvironmentContext(void *,void *);
   void                          *GetEnvironmentRouterContext(void *);
   void                          *SetEnvironmentRouterContext(void *,void *);
   void                          *GetEnvironmentFunctionContext(void *);
   void                          *SetEnvironmentFunctionContext(void *,void *);
   void                          *GetEnvironmentCallbackContext(void *);
   void                          *SetEnvironmentCallbackContext(void *,void *);

#endif /* _H_envrnmnt */

