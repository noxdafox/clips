   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*      CONSTRUCT PROFILING FUNCTIONS HEADER FILE      */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Modified OutputProfileInfo to allow a before   */
/*            and after prefix so that a string buffer does  */
/*            not need to be created to contain the entire   */
/*            prefix. This allows a buffer overflow problem  */
/*            to be corrected. DR0857.                       */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added pragmas to remove compilation warnings.  */
/*                                                           */
/*            Corrected code to remove run-time program      */
/*            compiler warnings.                             */
/*                                                           */
/*      6.30: Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_TBC).         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_proflfun

#pragma once

#define _H_proflfun

#include "userdata.h"

struct constructProfileInfo
  {
   struct userData usrData;
   long numberOfEntries;
   unsigned int childCall : 1;
   double startTime;
   double totalSelfTime;
   double totalWithChildrenTime;
  };

struct profileFrameInfo
  {
   unsigned int parentCall : 1;
   unsigned int profileOnExit : 1;
   double parentStartTime;
   struct constructProfileInfo *oldProfileFrame;
  };
  
#define PROFLFUN_DATA 15

struct profileFunctionData
  { 
   double ProfileStartTime;
   double ProfileEndTime;
   double ProfileTotalTime;
   int LastProfileInfo;
   double PercentThreshold;
   struct userDataRecord ProfileDataInfo;
   unsigned char ProfileDataID;
   bool ProfileUserFunctions;
   bool ProfileConstructs;
   struct constructProfileInfo *ActiveProfileFrame;
   const char *OutputString;
  };

#define ProfileFunctionData(theEnv) ((struct profileFunctionData *) GetEnvironmentData(theEnv,PROFLFUN_DATA))

   void                           ConstructProfilingFunctionDefinitions(Environment *);
   void                           ProfileCommand(Environment *,UDFContext *,CLIPSValue *);
   void                           ProfileInfoCommand(Environment *,UDFContext *,CLIPSValue *);
   void                           StartProfile(Environment *,struct profileFrameInfo *,
                                               struct userData **,bool);
   void                           EndProfile(Environment *,struct profileFrameInfo *);
   void                           ProfileResetCommand(Environment *,UDFContext *,CLIPSValue *);
   void                           ResetProfileInfo(struct constructProfileInfo *);

   void                           SetProfilePercentThresholdCommand(Environment *,UDFContext *,CLIPSValue *);
   double                         SetProfilePercentThreshold(Environment *,double);
   void                           GetProfilePercentThresholdCommand(Environment *,UDFContext *,CLIPSValue *);
   double                         GetProfilePercentThreshold(Environment *);
   bool                           Profile(Environment *,const char *);
   void                           DeleteProfileData(Environment *,void *);
   void                          *CreateProfileData(Environment *);
   const char                    *SetProfileOutputString(Environment *,const char *);

#endif /* _H_proflfun */


