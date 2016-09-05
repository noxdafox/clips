   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*           DEFFACTS BSAVE/BLOAD HEADER FILE          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_dffctbin

#pragma once

#define _H_dffctbin

#if (! RUN_TIME)

#include "constrct.h"
#include "cstrcbin.h"
#include "dffctdef.h"
#include "modulbin.h"

struct bsaveDeffacts
  {
   struct bsaveConstructHeader header;
   long assertList;
  };

struct bsaveDeffactsModule
  {
   struct bsaveDefmoduleItemHeader header;
  };

#define DFFCTBIN_DATA 26

struct deffactsBinaryData
  {
   Deffacts *DeffactsArray;
   long NumberOfDeffacts;
   struct deffactsModule *ModuleArray;
   long NumberOfDeffactsModules;
  };

#define DeffactsBinaryData(theEnv) ((struct deffactsBinaryData *) GetEnvironmentData(theEnv,DFFCTBIN_DATA))

   void                           DeffactsBinarySetup(Environment *);
   void                          *BloadDeffactsModuleReference(Environment *,int);

#endif /* (! RUN_TIME) */

#endif /* _H_dffctbin */




