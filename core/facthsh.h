   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
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

   void                           AddHashedFact(Environment *,Fact *,unsigned long);
   bool                           RemoveHashedFact(Environment *,Fact *);
   unsigned long                  HandleFactDuplication(Environment *,Fact *,bool *);
   bool                           EnvGetFactDuplication(Environment *);
   bool                           EnvSetFactDuplication(Environment *,bool);
   void                           InitializeFactHashTable(Environment *);
   void                           ShowFactHashTable(Environment *);
   unsigned long                  HashFact(Fact *);
   bool                           FactWillBeAsserted(Environment *,Fact *);

#if ALLOW_ENVIRONMENT_GLOBALS

   bool                           GetFactDuplication(void);
   bool                           SetFactDuplication(bool);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_facthsh */


