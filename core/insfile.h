   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Added environment parameter to GenClose.       */
/*            Added environment parameter to GenOpen.        */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_insfile
#define _H_insfile

#ifndef _H_expressn
#include "expressn.h"
#endif

#define INSTANCE_FILE_DATA 30

#if BLOAD_INSTANCES || BSAVE_INSTANCES
struct instanceFileData
  { 
   const char *InstanceBinaryPrefixID;
   const char *InstanceBinaryVersionID;
   unsigned long BinaryInstanceFileSize;

#if BLOAD_INSTANCES
   unsigned long BinaryInstanceFileOffset;
   char *CurrentReadBuffer;
   unsigned long CurrentReadBufferSize;
   unsigned long CurrentReadBufferOffset;
#endif
  };

#define InstanceFileData(theEnv) ((struct instanceFileData *) GetEnvironmentData(theEnv,INSTANCE_FILE_DATA))

#endif /* BLOAD_INSTANCES || BSAVE_INSTANCES */

   void                           SetupInstanceFileCommands(void *);
   long                           SaveInstancesCommand(void *);
   long                           LoadInstancesCommand(void *);
   long                           RestoreInstancesCommand(void *);
   long                           EnvSaveInstancesDriver(void *,const char *,int,EXPRESSION *,intBool);
   long                           EnvSaveInstances(void *,const char *,int);
#if BSAVE_INSTANCES
   long                           BinarySaveInstancesCommand(void *);
   long                           EnvBinarySaveInstancesDriver(void *,const char *,int,EXPRESSION *,intBool);
   long                           EnvBinarySaveInstances(void *,const char *,int);
#endif
#if BLOAD_INSTANCES
   long                           BinaryLoadInstancesCommand(void *);
   long                           EnvBinaryLoadInstances(void *,const char *);
#endif
   long                           EnvLoadInstances(void *,const char *);
   long                           EnvLoadInstancesFromString(void *,const char *,int);
   long                           EnvRestoreInstances(void *,const char *);
   long                           EnvRestoreInstancesFromString(void *,const char *,int);

#if ALLOW_ENVIRONMENT_GLOBALS

#if BLOAD_INSTANCES
   long                           BinaryLoadInstances(const char *);
#endif
#if BSAVE_INSTANCES
   long                           BinarySaveInstances(const char *,int);
#endif
   long                           LoadInstances(const char *);
   long                           LoadInstancesFromString(const char *,int);
   long                           RestoreInstances(const char *);
   long                           RestoreInstancesFromString(const char *,int);
   long                           SaveInstances(const char *,int);
   
#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_insfile */



