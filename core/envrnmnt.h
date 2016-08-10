   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/06/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*************************************************************/

#ifndef _H_envrnmnt

#pragma once

#define _H_envrnmnt

#include <stdbool.h>

typedef struct environmentData Environment;

#include "symbol.h"

#define USER_ENVIRONMENT_DATA 70
#define MAXIMUM_ENVIRONMENT_POSITIONS 100

struct environmentCleanupFunction
  {
   const char *name;
   void (*func)(Environment *);
   int priority;
   struct environmentCleanupFunction *next;
  };

struct environmentData
  {   
   unsigned int initialized : 1;
   void *context;
   void *routerContext;
   void *functionContext;
   void *callbackContext;
   void **theData;
   void (**cleanupFunctions)(Environment *);
   struct environmentCleanupFunction *listOfCleanupEnvironmentFunctions;
   struct environmentData *next;
  };

#define GetEnvironmentData(theEnv,position) (((struct environmentData *) theEnv)->theData[position])
#define SetEnvironmentData(theEnv,position,value) (((struct environmentData *) theEnv)->theData[position] = value)

   bool                           AllocateEnvironmentData(Environment *,unsigned int,unsigned long,void (*)(Environment *));
   void                          *CreateEnvironment(void);
   void                          *CreateRuntimeEnvironment(struct symbolHashNode **,struct floatHashNode **,
                                                                  struct integerHashNode **,struct bitMapHashNode **);
   bool                           DestroyEnvironment(Environment *);
   bool                           AddEnvironmentCleanupFunction(Environment *,const char *,void (*)(Environment *),int);
   void                          *GetEnvironmentContext(Environment *);
   void                          *SetEnvironmentContext(Environment *,void *);
   void                          *GetEnvironmentRouterContext(Environment *);
   void                          *SetEnvironmentRouterContext(Environment *,void *);
   void                          *GetEnvironmentFunctionContext(Environment *);
   void                          *SetEnvironmentFunctionContext(Environment *,void *);
   void                          *GetEnvironmentCallbackContext(Environment *);
   void                          *SetEnvironmentCallbackContext(Environment *,void *);

#endif /* _H_envrnmnt */

