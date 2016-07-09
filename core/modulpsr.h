   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*             DEFMODULE PARSER HEADER FILE            */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: GetConstructNameAndComment API change.         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when DEFMODULE_CONSTRUCT   */
/*            compiler flag is set to 0.                     */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_modulpsr

#pragma once

#define _H_modulpsr

struct portConstructItem
  {
   const char *constructName;
   int typeExpected;
   struct portConstructItem *next;
  };

#include "evaluatn.h"
#include "moduldef.h"
#include "symbol.h"

   long                           GetNumberOfDefmodules(void *);
   void                           SetNumberOfDefmodules(void *,long);
   void                           AddAfterModuleDefinedFunction(void *,const char *,void (*)(void *),int);
   bool                           ParseDefmodule(void *,const char *);
   void                           AddPortConstructItem(void *,const char *,int);
   struct portConstructItem      *ValidPortConstructItem(void *,const char *);
   bool                           FindImportExportConflict(void *,const char *,struct defmodule *,const char *);

#endif /* _H_modulpsr */


