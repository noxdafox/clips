   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.50  07/30/16             */
   /*                                                     */
   /*           FACT LHS PATTERN PARSING MODULE           */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains routines for integration of ordered and */
/*   deftemplate fact patterns with the defrule LHS pattern  */
/*   parser including routines for recognizing fact          */
/*   patterns, parsing ordered fact patterns, and initiating */
/*   the parsing of deftemplate fact patterns.               */
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
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*      6.50: Removed initial-fact support.                  */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>

#include "cstrcpsr.h"
#include "envrnmnt.h"
#include "modulpsr.h"
#include "modulutl.h"
#include "pattern.h"
#include "pprint.h"
#include "prntutil.h"
#include "reorder.h"
#include "router.h"
#include "tmpltdef.h"
#include "tmpltlhs.h"
#include "tmpltpsr.h"
#include "tmpltutl.h"

#include "factlhs.h"

/***********************************************/
/* SequenceRestrictionParse: Parses an ordered */
/*   fact pattern conditional element.         */
/*                                             */
/*   <ordered-fact-pattern-CE>                 */
/*             ::= (<symbol> <constraint>+)    */
/***********************************************/
struct lhsParseNode *SequenceRestrictionParse(
  Environment *theEnv,
  const char *readSource,
  struct token *theToken)
  {
   struct lhsParseNode *topNode;
   struct lhsParseNode *nextField;

   /*================================================*/
   /* Create the pattern node for the relation name. */
   /*================================================*/

   topNode = GetLHSParseNode(theEnv);
   topNode->pnType = SF_WILDCARD_NODE;
   topNode->negated = false;
   topNode->exists = false;
   topNode->index = -1;
   topNode->slotNumber = 1;
   topNode->bottom = GetLHSParseNode(theEnv);
   topNode->bottom->pnType = SYMBOL_NODE;
   topNode->bottom->negated = false;
   topNode->bottom->exists = false;
   topNode->bottom->value = theToken->value;

   /*======================================================*/
   /* Connective constraints cannot be used in conjunction */
   /* with the first field of a pattern.                   */
   /*======================================================*/

   SavePPBuffer(theEnv," ");
   GetToken(theEnv,readSource,theToken);
   if ((theToken->tknType == OR_CONSTRAINT_TOKEN) ||
       (theToken->tknType == AND_CONSTRAINT_TOKEN))
     {
      ReturnLHSParseNodes(theEnv,topNode);
      SyntaxErrorMessage(theEnv,"the first field of a pattern");
      return NULL;
     }

   /*============================================================*/
   /* Treat the remaining constraints of an ordered fact pattern */
   /* as if they were contained in a multifield slot.            */
   /*============================================================*/

   nextField = RestrictionParse(theEnv,readSource,theToken,true,NULL,1,NULL,1);
   if (nextField == NULL)
     {
      ReturnLHSParseNodes(theEnv,topNode);
      return NULL;
     }
   topNode->right = nextField;

   /*================================================*/
   /* The pattern must end with a right parenthesis. */
   /*================================================*/

   if (theToken->tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      PPBackup(theEnv);
      SavePPBuffer(theEnv," ");
      SavePPBuffer(theEnv,theToken->printForm);
      SyntaxErrorMessage(theEnv,"fact patterns");
      ReturnLHSParseNodes(theEnv,topNode);
      return NULL;
     }

   /*====================================*/
   /* Fix the pretty print output if the */
   /* slot contained no restrictions.    */
   /*====================================*/

   if (nextField->bottom == NULL)
     {
      PPBackup(theEnv);
      PPBackup(theEnv);
      SavePPBuffer(theEnv,")");
     }

   /*===================================*/
   /* If no errors, return the pattern. */
   /*===================================*/

   return(topNode);
  }

/**********************************************************************/
/* FactPatternParserFind: This function is the pattern find function  */
/*   for facts. It tells the pattern parsing code that the specified  */
/*   pattern can be parsed as a fact pattern. By default, any pattern */
/*   beginning with a symbol can be parsed as a fact pattern. Since   */
/*   all patterns begin with a symbol, it follows that all patterns   */
/*   can be parsed as a fact pattern.                                 */
/**********************************************************************/
bool FactPatternParserFind(
  CLIPSLexeme *theRelation)
  {
#if MAC_XCD
#pragma unused(theRelation)
#endif
   return true;
  }

/******************************************************/
/* FactPatternParse: This function is called to parse */
/*  both deftemplate and ordered fact patterns.       */
/******************************************************/
struct lhsParseNode *FactPatternParse(
  Environment *theEnv,
  const char *readSource,
  struct token *theToken)
  {
   Deftemplate *theDeftemplate;
   int count;

   /*=========================================*/
   /* A module separator can not be included  */
   /* as part of the pattern's relation name. */
   /*=========================================*/

   if (FindModuleSeparator(theToken->lexemeValue->contents))
     {
      IllegalModuleSpecifierMessage(theEnv);
      return NULL;
     }

   /*=========================================================*/
   /* Find the deftemplate associated with the relation name. */
   /*=========================================================*/

   theDeftemplate = (Deftemplate *)
                    FindImportedConstruct(theEnv,"deftemplate",NULL,theToken->lexemeValue->contents,
                                          &count,true,NULL);

   if (count > 1)
     {
      AmbiguousReferenceErrorMessage(theEnv,"deftemplate",theToken->lexemeValue->contents);
      return NULL;
     }

   /*======================================================*/
   /* If no deftemplate exists with the specified relation */
   /* name, then create an implied deftemplate.            */
   /*======================================================*/

   if (theDeftemplate == NULL)
     {
#if DEFMODULE_CONSTRUCT
      if (FindImportExportConflict(theEnv,"deftemplate",EnvGetCurrentModule(theEnv),theToken->lexemeValue->contents))
        {
         ImportExportConflictMessage(theEnv,"implied deftemplate",theToken->lexemeValue->contents,NULL,NULL);
         return NULL;
        }
#endif /* DEFMODULE_CONSTRUCT */

      if (! ConstructData(theEnv)->CheckSyntaxMode)
        { theDeftemplate = CreateImpliedDeftemplate(theEnv,theToken->lexemeValue,true); }
      else
        { theDeftemplate = NULL; }
     }

   /*===============================================*/
   /* If an explicit deftemplate exists, then parse */
   /* the pattern as a deftemplate pattern.         */
   /*===============================================*/

   if ((theDeftemplate != NULL) && (theDeftemplate->implied == false))
     { return(DeftemplateLHSParse(theEnv,readSource,theDeftemplate)); }

   /*================================*/
   /* Parse an ordered fact pattern. */
   /*================================*/

   return(SequenceRestrictionParse(theEnv,readSource,theToken));
  }

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY) */



