   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*            DEFAULT ATTRIBUTE HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides functions for parsing the default       */
/*   attribute and determining default values based on       */
/*   slot constraints.                                       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Support for deftemplate-slot-default-value     */
/*            function.                                      */ 
/*                                                           */
/*      6.30: Support for long long integers.                */
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

#ifndef _H_default

#pragma once

#define _H_default

#include "constrnt.h"
#include "evaluatn.h"

   void                           DeriveDefaultFromConstraints(Environment *,CONSTRAINT_RECORD *,CLIPSValue *,bool,bool);
   struct expr                   *ParseDefault(Environment *,const char *,bool,bool,bool,bool *,bool *,bool *);

#endif /* _H_default */



