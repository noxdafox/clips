   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                USER DATA HEADER FILE                */
   /*******************************************************/

/*************************************************************/
/* Purpose: Routines for attaching user data to constructs,  */
/*   facts, instances, user functions, etc.                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_userdata

#pragma once

#define _H_userdata

struct userData
  {
   unsigned char dataID;
   struct userData *next;
  };

typedef struct userData USER_DATA;
typedef struct userData * USER_DATA_PTR;
  
struct userDataRecord
  {
   unsigned char dataID;
   void *(*createUserData)(void *);
   void (*deleteUserData)(void *,void *);
  };
  
typedef struct userDataRecord USER_DATA_RECORD;
typedef struct userDataRecord * USER_DATA_RECORD_PTR;

#define MAXIMUM_USER_DATA_RECORDS 100

#define USER_DATA_DATA 56

struct userDataData
  { 
   struct userDataRecord *UserDataRecordArray[MAXIMUM_USER_DATA_RECORDS];
   unsigned char UserDataRecordCount;
  };

#define UserDataData(theEnv) ((struct userDataData *) GetEnvironmentData(theEnv,USER_DATA_DATA))

   void                           InitializeUserDataData(void *);
   unsigned char                  InstallUserDataRecord(void *,struct userDataRecord *);
   struct userData               *FetchUserData(void *,unsigned char,struct userData **);
   struct userData               *TestUserData(unsigned char,struct userData *);
   void                           ClearUserDataList(void *,struct userData *);
   struct userData               *DeleteUserData(void *,unsigned char,struct userData *);

#endif

