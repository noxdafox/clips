   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*       PROCEDURAL FUNCTIONS PARSER HEADER FILE       */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
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

#ifndef _H_prcdrpsr

#pragma once

#define _H_prcdrpsr

#include "constrnt.h"

struct BindInfo
  {
   CLIPSLexeme *name;
   CONSTRAINT_RECORD *constraints;
   struct BindInfo *next;
  };

#if (! RUN_TIME)
   void                           ProceduralFunctionParsers(Environment *);
   struct BindInfo               *GetParsedBindNames(Environment *);
   void                           SetParsedBindNames(Environment *,struct BindInfo *);
   void                           ClearParsedBindNames(Environment *);
   bool                           ParsedBindNamesEmpty(Environment *);
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
   int                            SearchParsedBindNames(Environment *,CLIPSLexeme *);
   int                            CountParsedBindNames(Environment *);
   void                           RemoveParsedBindName(Environment *,CLIPSLexeme *);
   struct constraintRecord       *FindBindConstraints(Environment *,CLIPSLexeme *);
#endif

#endif /* _H_prcdrpsr */




