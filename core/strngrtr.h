   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*            STRING_TYPE I/O ROUTER HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: I/O Router routines which allow strings to be    */
/*   used as input and output sources.                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Used genstrcpy instead of strcpy.              */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Changed integer type/precision.                */
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
/*************************************************************/

#ifndef _H_strngrtr

#pragma once

#define _H_strngrtr

#include <stdio.h>

#define STRING_ROUTER_DATA 48

struct stringRouter
  {
   const char *name;
   const char *readString;
   char *writeString;
   size_t currentPosition;
   size_t maximumPosition;
   int readWriteType;
   struct stringRouter *next;
  };

struct stringRouterData
  {
   struct stringRouter *ListOfStringRouters;
  };

#define StringRouterData(theEnv) ((struct stringRouterData *) GetEnvironmentData(theEnv,STRING_ROUTER_DATA))

/**************************/
/* I/O ROUTER DEFINITIONS */
/**************************/

   void                           InitializeStringRouter(Environment *);
   bool                           OpenStringSource(Environment *,const char *,const char *,size_t);
   bool                           OpenTextSource(Environment *,const char *,const char *,size_t,size_t);
   bool                           CloseStringSource(Environment *,const char *);
   bool                           OpenStringDestination(Environment *,const char *,char *,size_t);
   bool                           CloseStringDestination(Environment *,const char *);

#endif /* _H_strngrtr */


