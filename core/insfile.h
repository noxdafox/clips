   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
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
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_insfile

#pragma once

#define _H_insfile

#include "expressn.h"

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

   void                           SetupInstanceFileCommands(Environment *);
   void                           SaveInstancesCommand(Environment *,UDFContext *,UDFValue *);
   void                           LoadInstancesCommand(Environment *,UDFContext *,UDFValue *);
   void                           RestoreInstancesCommand(Environment *,UDFContext *,UDFValue *);
   long                           EnvSaveInstancesDriver(Environment *,const char *,int,Expression *,bool);
   long                           EnvSaveInstances(Environment *,const char *,int);
#if BSAVE_INSTANCES
   void                           BinarySaveInstancesCommand(Environment *,UDFContext *,UDFValue *);
   long                           EnvBinarySaveInstancesDriver(Environment *,const char *,int,Expression *,bool);
   long                           EnvBinarySaveInstances(Environment *,const char *,int);
#endif
#if BLOAD_INSTANCES
   void                           BinaryLoadInstancesCommand(Environment *,UDFContext *,UDFValue *);
   long                           EnvBinaryLoadInstances(Environment *,const char *);
#endif
   long                           EnvLoadInstances(Environment *,const char *);
   long                           EnvLoadInstancesFromString(Environment *,const char *,size_t);
   long                           EnvRestoreInstances(Environment *,const char *);
   long                           EnvRestoreInstancesFromString(Environment *,const char *,size_t);

#endif /* _H_insfile */



