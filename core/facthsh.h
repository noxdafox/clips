   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*                 FACT HASHING MODULE                 */
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
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Fact hash table is resizable.                  */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*            Added FactWillBeAsserted.                      */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*************************************************************/

#ifndef _H_facthsh

#pragma once

#define _H_facthsh

struct factHashEntry;

#include "factmngr.h"

struct factHashEntry
  {
   struct fact *theFact;
   struct factHashEntry *next;
  };

#define SIZE_FACT_HASH 16231

   void                           AddHashedFact(void *,struct fact *,unsigned long);
   bool                           RemoveHashedFact(void *,struct fact *);
   unsigned long                  HandleFactDuplication(void *,void *,bool *);
   bool                           EnvGetFactDuplication(void *);
   bool                           EnvSetFactDuplication(void *,bool);
   void                           InitializeFactHashTable(void *);
   void                           ShowFactHashTable(void *);
   unsigned long                  HashFact(struct fact *);
   bool                           FactWillBeAsserted(void *,void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           GetFactDuplication(void);
   bool                           SetFactDuplication(bool);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_facthsh */


