   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  07/30/16             */
   /*                                                     */
   /*                DEFTEMPLATE LHS MODULE               */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses LHS deftemplate patterns.                 */
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
/*      6.30: Support for rete network exists node.          */
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
/*************************************************************/

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY)

#include <stdio.h>
#include <string.h>

#include "constant.h"
#include "constrct.h"
#include "constrnt.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "factrhs.h"
#include "memalloc.h"
#include "modulutl.h"
#include "pattern.h"
#include "pprint.h"
#include "prntutil.h"
#include "reorder.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltdef.h"
#include "tmpltutl.h"

#include "tmpltlhs.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

   static struct lhsParseNode    *GetLHSSlots(Environment *,const char *,struct token *,Deftemplate *,bool *);
   static struct lhsParseNode    *GetSingleLHSSlot(Environment *,const char *,struct token *,
                                                   struct templateSlot *,bool *,short);
   static bool                    MultiplyDefinedLHSSlots(Environment *,struct lhsParseNode *,CLIPSLexeme *);

/*********************************************/
/* DeftemplateLHSParse: Parses a LHS pattern */
/*   that uses the deftemplate format.       */
/*********************************************/
struct lhsParseNode *DeftemplateLHSParse(
  Environment *theEnv,
  const char *readSource,
  Deftemplate *theDeftemplate)
  {
   struct lhsParseNode *head, *firstSlot;
   struct token theToken;
   bool error;

   /*===============================================================*/
   /* Make sure the deftemplate name is not connected to subfields. */
   /*===============================================================*/

   GetToken(theEnv,readSource,&theToken);
   if ((theToken.tknType == OR_CONSTRAINT_TOKEN) ||
       (theToken.tknType == AND_CONSTRAINT_TOKEN))
     {
      SyntaxErrorMessage(theEnv,"deftemplate patterns");
      return NULL;
     }

   /*===================================================*/
   /* Create the pattern node for the deftemplate name. */
   /*===================================================*/

   head = GetLHSParseNode(theEnv);
   head->pnType = SF_WILDCARD_NODE;
   head->negated = false;
   head->exists = false;
   head->index = 0;
   head->slotNumber = 1;
   head->bottom = GetLHSParseNode(theEnv);
   head->bottom->pnType = SYMBOL_NODE;
   head->bottom->negated = false;
   head->bottom->exists = false;
   head->bottom->value = theDeftemplate->header.name;

   /*==========================================*/
   /* Get the other fields in the deftemplate. */
   /*==========================================*/

   error = false;
   firstSlot = GetLHSSlots(theEnv,readSource,&theToken,theDeftemplate,&error);
   if (error)
     {
      ReturnLHSParseNodes(theEnv,firstSlot);
      ReturnLHSParseNodes(theEnv,head);
      return NULL;
     }

   /*=========================*/
   /* Return the LHS pattern. */
   /*=========================*/

   head->right = firstSlot;
   return(head);
  }

/******************************************/
/* GetLHSSlots: Retrieves all of the slot */
/*   values used in a LHS pattern.        */
/******************************************/
static struct lhsParseNode *GetLHSSlots(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  Deftemplate *theDeftemplate,
  bool *error)
  {
   struct lhsParseNode *firstSlot = NULL, *nextSlot, *lastSlot = NULL;
   struct templateSlot *slotPtr;
   short position;

   /*=======================================================*/
   /* Continue parsing slot definitions until the pattern's */
   /* closing right parenthesis is encountered.             */
   /*=======================================================*/

   while (tempToken->tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      PPBackup(theEnv);
      SavePPBuffer(theEnv," ");
      SavePPBuffer(theEnv,tempToken->printForm);

      /*=================================================*/
      /* Slot definitions begin with a left parenthesis. */
      /*=================================================*/

      if (tempToken->tknType != LEFT_PARENTHESIS_TOKEN)
        {
         *error = true;
         SyntaxErrorMessage(theEnv,"deftemplate patterns");
         ReturnLHSParseNodes(theEnv,firstSlot);
         return NULL;
        }

      /*====================*/
      /* Get the slot name. */
      /*====================*/

      GetToken(theEnv,readSource,tempToken);
      if (tempToken->tknType != SYMBOL_TOKEN)
        {
         *error = true;
         SyntaxErrorMessage(theEnv,"deftemplate patterns");
         ReturnLHSParseNodes(theEnv,firstSlot);
         return NULL;
        }

      /*==========================================================*/
      /* Determine if the slot name is valid for the deftemplate. */
      /*==========================================================*/

      if ((slotPtr = FindSlot(theDeftemplate,tempToken->lexemeValue,&position)) == NULL)
        {
         *error = true;
         InvalidDeftemplateSlotMessage(theEnv,tempToken->lexemeValue->contents,
                                       theDeftemplate->header.name->contents,true);
         ReturnLHSParseNodes(theEnv,firstSlot);
         return NULL;
        }

      /*============================================*/
      /* Determine if the slot is multiply defined. */
      /*============================================*/

      if (MultiplyDefinedLHSSlots(theEnv,firstSlot,tempToken->lexemeValue) == true)
        {
         *error = true;
         ReturnLHSParseNodes(theEnv,firstSlot);
         return NULL;
        }

      /*==============================================================*/
      /* Get the pattern matching values used in the slot definition. */
      /*==============================================================*/

      nextSlot = GetSingleLHSSlot(theEnv,readSource,tempToken,slotPtr,error,(short) (position+1));
      if (*error)
        {
         ReturnLHSParseNodes(theEnv,firstSlot);
         ReturnLHSParseNodes(theEnv,nextSlot);
         return NULL;
        }

      /*=====================================*/
      /* Add the slot definition to the list */
      /* of slot definitions already parsed. */
      /*=====================================*/

      if (lastSlot == NULL)
        { firstSlot = nextSlot; }
      else
        { lastSlot->right = nextSlot; }

      while (nextSlot->right != NULL) nextSlot = nextSlot->right;
      lastSlot = nextSlot;

      /*==============================*/
      /* Begin parsing the next slot. */
      /*==============================*/

      GetToken(theEnv,readSource,tempToken);
     }

   /*===========================================================*/
   /* Return all the slot definitions found in the lhs pattern. */
   /*===========================================================*/

   return(firstSlot);
  }

/*****************************************************/
/* GetSingleLHSSlot: Get the pattern matching values */
/*   to be associated with a slot name.              */
/*****************************************************/
static struct lhsParseNode *GetSingleLHSSlot(
  Environment *theEnv,
  const char *readSource,
  struct token *tempToken,
  struct templateSlot *slotPtr,
  bool *error,
  short position)
  {
   struct lhsParseNode *nextSlot;
   CLIPSLexeme *slotName;

   /*================================================*/
   /* Get the slot name and read in the first token. */
   /*================================================*/

   slotName = tempToken->lexemeValue;
   SavePPBuffer(theEnv," ");
   GetToken(theEnv,readSource,tempToken);

   /*====================================*/
   /* Get value for a single field slot. */
   /*====================================*/

   if (slotPtr->multislot == false)
     {
      /*=======================*/
      /* Get the single value. */
      /*=======================*/

      nextSlot = RestrictionParse(theEnv,readSource,tempToken,false,
                                  slotPtr->slotName,(short) (position - 1),
                                  slotPtr->constraints,0);
      if (nextSlot == NULL)
        {
         *error = true;
         return NULL;
        }

      /*======================================*/
      /* Multi field wildcards and variables  */
      /* not allowed in a single field slot.  */
      /*======================================*/

      if ((nextSlot->pnType == MF_VARIABLE_NODE) ||
          (nextSlot->pnType == MF_WILDCARD_NODE))
        {
         SingleFieldSlotCardinalityError(theEnv,slotPtr->slotName->contents);
         *error = true;
         ReturnLHSParseNodes(theEnv,nextSlot);
         return NULL;
        }
     }

   /*===================================*/
   /* Get values for a multifield slot. */
   /*===================================*/

   else
     {
      nextSlot = RestrictionParse(theEnv,readSource,tempToken,true,slotName,(short) (position - 1),
                                  slotPtr->constraints,1);
      if (nextSlot == NULL)
        {
         *error = true;
         return NULL;
        }
     }

   /*========================================================*/
   /* The slot definition must end with a right parenthesis. */
   /*========================================================*/

   if (tempToken->tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      PPBackup(theEnv);
      SavePPBuffer(theEnv," ");
      SavePPBuffer(theEnv,tempToken->printForm);
      SyntaxErrorMessage(theEnv,"deftemplate patterns");
      *error = true;
      ReturnLHSParseNodes(theEnv,nextSlot);
      return NULL;
     }

   /*===============================================*/
   /* Fix the pretty print output if the multifield */
   /* slot contained no restrictions.               */
   /*===============================================*/

   if ((nextSlot->bottom == NULL) && slotPtr->multislot)
     {
      PPBackup(theEnv);
      PPBackup(theEnv);
      SavePPBuffer(theEnv,")");
     }

   /*=================================*/
   /* Add the slot values to the slot */
   /* structure and return it.        */
   /*=================================*/

   return(nextSlot);
  }

/******************************************************/
/* MultiplyDefinedLHSSlots: Determines if a slot name */
/*   was used more than once in a LHS pattern.        */
/******************************************************/
static bool MultiplyDefinedLHSSlots(
  Environment *theEnv,
  struct lhsParseNode *theSlots,
  CLIPSLexeme *slotName)
  {
   for (;
        theSlots != NULL;
        theSlots = theSlots->right)
     {
      if (theSlots->slot == slotName)
        {
         AlreadyParsedErrorMessage(theEnv,"slot ",slotName->contents);
         return true;
        }
     }

   return false;
  }

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT && (! RUN_TIME) && (! BLOAD_ONLY) */


