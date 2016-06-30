   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
   intBool                        RemoveHashedFact(void *,struct fact *);
   unsigned long                  HandleFactDuplication(void *,void *,intBool *);
   intBool                        EnvGetFactDuplication(void *);
   intBool                        EnvSetFactDuplication(void *,int);
   void                           InitializeFactHashTable(void *);
   void                           ShowFactHashTable(void *);
   unsigned long                  HashFact(struct fact *);
   intBool                        FactWillBeAsserted(void *,void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                        GetFactDuplication(void);
   intBool                        SetFactDuplication(int);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_facthsh */


