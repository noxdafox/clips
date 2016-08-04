   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
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
   void *(*createUserData)(Environment *);
   void (*deleteUserData)(Environment *,void *);
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

   void                           InitializeUserDataData(Environment *);
   unsigned char                  InstallUserDataRecord(Environment *,struct userDataRecord *);
   struct userData               *FetchUserData(Environment *,unsigned char,struct userData **);
   struct userData               *TestUserData(unsigned char,struct userData *);
   void                           ClearUserDataList(Environment *,struct userData *);
   struct userData               *DeleteUserData(Environment *,unsigned char,struct userData *);

#endif

