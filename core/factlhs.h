   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                FACT BUILD HEADER FILE               */
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
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Initialize the exists member.                  */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_factlhs

#pragma once

#define _H_factlhs

#include "scanner.h"
#include "symbol.h"

   int                            FactPatternParserFind(SYMBOL_HN *);
   struct lhsParseNode           *FactPatternParse(void *,const char *,struct token *);
   struct lhsParseNode           *SequenceRestrictionParse(void *,const char *,struct token *);
   struct lhsParseNode           *CreateInitialFactPattern(void *);

#endif /* _H_factlhs */
