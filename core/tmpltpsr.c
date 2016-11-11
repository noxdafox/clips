   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/11/16             */
   /*                                                     */
   /*              DEFTEMPLATE PARSER MODULE              */
   /*******************************************************/

/*************************************************************/
/* Purpose: Parses the deftemplate construct.                */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW and       */
/*            MAC_MCW).                                      */
/*                                                           */
/*            GetConstructNameAndComment API change.         */
/*                                                           */
/*            Support for deftemplate slot facets.           */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Static constraint checking is always enabled.  */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT

#include <stdio.h>
#include <string.h>

#if BLOAD || BLOAD_AND_BSAVE
#include "bload.h"
#endif
#include "constant.h"
#include "constrct.h"
#include "cstrcpsr.h"
#include "cstrnchk.h"
#include "cstrnpsr.h"
#include "cstrnutl.h"
#include "default.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "factmngr.h"
#include "memalloc.h"
#include "modulutl.h"
#include "pattern.h"
#include "pprint.h"
#include "prntutil.h"
#include "router.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltbsc.h"
#include "tmpltdef.h"
#include "watch.h"

#include "tmpltpsr.h"

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if (! RUN_TIME) && (! BLOAD_ONLY)
   static struct templateSlot    *SlotDeclarations(Environment *,const char *,struct token *);
   static struct templateSlot    *ParseSlot(Environment *,const char *,struct token *,struct templateSlot *);
   static struct templateSlot    *DefinedSlots(Environment *,const char *,CLIPSLexeme *,bool,struct token *);
   static bool                    ParseFacetAttribute(Environment *,const char *,struct templateSlot *,bool);
#endif

/*******************************************************/
/* ParseDeftemplate: Parses the deftemplate construct. */
/*******************************************************/
bool ParseDeftemplate(
  Environment *theEnv,
  const char *readSource)
  {
#if (! RUN_TIME) && (! BLOAD_ONLY)
   CLIPSLexeme *deftemplateName;
   Deftemplate *newDeftemplate;
   struct templateSlot *slots;
   struct token inputToken;

   /*================================================*/
   /* Initialize pretty print and error information. */
   /*================================================*/

   DeftemplateData(theEnv)->DeftemplateError = false;
   SetPPBufferStatus(theEnv,true);
   FlushPPBuffer(theEnv);
   SavePPBuffer(theEnv,"(deftemplate ");

   /*==============================================================*/
   /* Deftemplates can not be added when a binary image is loaded. */
   /*==============================================================*/

#if BLOAD || BLOAD_AND_BSAVE
   if ((Bloaded(theEnv) == true) && (! ConstructData(theEnv)->CheckSyntaxMode))
     {
      CannotLoadWithBloadMessage(theEnv,"deftemplate");
      return true;
     }
#endif

   /*=======================================================*/
   /* Parse the name and comment fields of the deftemplate. */
   /*=======================================================*/

#if DEBUGGING_FUNCTIONS
   DeftemplateData(theEnv)->DeletedTemplateDebugFlags = 0;
#endif

   deftemplateName = GetConstructNameAndComment(theEnv,readSource,&inputToken,"deftemplate",
                                                (FindConstructFunction *) EnvFindDeftemplateInModule,
                                                (DeleteConstructFunction *) Undeftemplate,"%",
                                                true,true,true,false);

   if (deftemplateName == NULL) return true;

   if (ReservedPatternSymbol(theEnv,deftemplateName->contents,"deftemplate"))
     {
      ReservedPatternSymbolErrorMsg(theEnv,deftemplateName->contents,"a deftemplate name");
      return true;
     }

   /*===========================================*/
   /* Parse the slot fields of the deftemplate. */
   /*===========================================*/

   slots = SlotDeclarations(theEnv,readSource,&inputToken);
   if (DeftemplateData(theEnv)->DeftemplateError == true) return true;

   /*==============================================*/
   /* If we're only checking syntax, don't add the */
   /* successfully parsed deftemplate to the KB.   */
   /*==============================================*/

   if (ConstructData(theEnv)->CheckSyntaxMode)
     {
      ReturnSlots(theEnv,slots);
      return false;
     }

   /*=====================================*/
   /* Create a new deftemplate structure. */
   /*=====================================*/

   newDeftemplate = get_struct(theEnv,deftemplate);
   newDeftemplate->header.name =  deftemplateName;
   newDeftemplate->header.next = NULL;
   newDeftemplate->header.usrData = NULL;
   newDeftemplate->header.constructType = DEFTEMPLATE;
   newDeftemplate->header.env = theEnv;
   newDeftemplate->slotList = slots;
   newDeftemplate->implied = false;
   newDeftemplate->numberOfSlots = 0;
   newDeftemplate->busyCount = 0;
   newDeftemplate->watch = 0;
   newDeftemplate->inScope = true;
   newDeftemplate->patternNetwork = NULL;
   newDeftemplate->factList = NULL;
   newDeftemplate->lastFact = NULL;
   newDeftemplate->header.whichModule = (struct defmoduleItemHeader *)
                                        GetModuleItem(theEnv,NULL,DeftemplateData(theEnv)->DeftemplateModuleIndex);

   /*================================*/
   /* Determine the number of slots. */
   /*================================*/

   while (slots != NULL)
     {
      newDeftemplate->numberOfSlots++;
      slots = slots->next;
     }

   /*====================================*/
   /* Store pretty print representation. */
   /*====================================*/

   if (EnvGetConserveMemory(theEnv) == true)
     { newDeftemplate->header.ppForm = NULL; }
   else
     { newDeftemplate->header.ppForm = CopyPPBuffer(theEnv); }

   /*=======================================================================*/
   /* If a template is redefined, then we want to restore its watch status. */
   /*=======================================================================*/

#if DEBUGGING_FUNCTIONS
   if ((BitwiseTest(DeftemplateData(theEnv)->DeletedTemplateDebugFlags,0)) ||
       (EnvGetWatchItem(theEnv,"facts") == 1))
     { DeftemplateSetWatch(newDeftemplate,true); }
#endif

   /*==============================================*/
   /* Add deftemplate to the list of deftemplates. */
   /*==============================================*/

   AddConstructToModule(&newDeftemplate->header);

   InstallDeftemplate(theEnv,newDeftemplate);

#else
#if MAC_XCD
#pragma unused(theEnv)
#endif
#endif

   return false;
  }

#if (! RUN_TIME) && (! BLOAD_ONLY)

/**************************************************************/
/* InstallDeftemplate: Increments all occurrences in the hash */
/*   table of symbols found in an deftemplate and adds it to  */
/*   the hash table.                                          */
/**************************************************************/
void InstallDeftemplate(
  Environment *theEnv,
  Deftemplate *theDeftemplate)
  {
   struct templateSlot *slotPtr;
   struct expr *tempExpr;

   IncrementSymbolCount(theDeftemplate->header.name);

   for (slotPtr = theDeftemplate->slotList;
        slotPtr != NULL;
        slotPtr = slotPtr->next)
     {
      IncrementSymbolCount(slotPtr->slotName);
      tempExpr = AddHashedExpression(theEnv,slotPtr->defaultList);
      ReturnExpression(theEnv,slotPtr->defaultList);
      slotPtr->defaultList = tempExpr;
      tempExpr = AddHashedExpression(theEnv,slotPtr->facetList);
      ReturnExpression(theEnv,slotPtr->facetList);
      slotPtr->facetList = tempExpr;
      slotPtr->constraints = AddConstraint(theEnv,slotPtr->constraints);
     }
  }

/********************************************************************/
/* SlotDeclarations: Parses the slot declarations of a deftemplate. */
/********************************************************************/
static struct templateSlot *SlotDeclarations(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken)
  {
   struct templateSlot *newSlot, *slotList = NULL, *lastSlot = NULL;
   struct templateSlot *multiSlot = NULL;

   while (inputToken->tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      /*====================================================*/
      /* Slots begin with a '(' followed by a slot keyword. */
      /*====================================================*/

      if (inputToken->tknType != LEFT_PARENTHESIS_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"deftemplate");
         ReturnSlots(theEnv,slotList);
         ReturnSlots(theEnv,multiSlot);
         DeftemplateData(theEnv)->DeftemplateError = true;
         return NULL;
        }

      GetToken(theEnv,readSource,inputToken);
      if (inputToken->tknType != SYMBOL_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"deftemplate");
         ReturnSlots(theEnv,slotList);
         ReturnSlots(theEnv,multiSlot);
         DeftemplateData(theEnv)->DeftemplateError = true;
         return NULL;
        }

      /*=================*/
      /* Parse the slot. */
      /*=================*/

      newSlot = ParseSlot(theEnv,readSource,inputToken,slotList);
      if (DeftemplateData(theEnv)->DeftemplateError == true)
        {
         ReturnSlots(theEnv,newSlot);
         ReturnSlots(theEnv,slotList);
         ReturnSlots(theEnv,multiSlot);
         return NULL;
        }

      /*===========================================*/
      /* Attach the new slot to the list of slots. */
      /*===========================================*/

      if (newSlot != NULL)
        {
         if (lastSlot == NULL)
           { slotList = newSlot; }
         else
           { lastSlot->next = newSlot; }
         lastSlot = newSlot;
        }

      /*================================*/
      /* Check for closing parenthesis. */
      /*================================*/

      GetToken(theEnv,readSource,inputToken);
      if (inputToken->tknType != RIGHT_PARENTHESIS_TOKEN)
        {
         PPBackup(theEnv);
         SavePPBuffer(theEnv,"\n   ");
         SavePPBuffer(theEnv,inputToken->printForm);
        }
     }

  SavePPBuffer(theEnv,"\n");

  /*=======================*/
  /* Return the slot list. */
  /*=======================*/

  return(slotList);
 }

/*****************************************************/
/* ParseSlot: Parses a single slot of a deftemplate. */
/*****************************************************/
static struct templateSlot *ParseSlot(
  Environment *theEnv,
  const char *readSource,
  struct token *inputToken,
  struct templateSlot *slotList)
  {
   bool parsingMultislot;
   CLIPSLexeme *slotName;
   struct templateSlot *newSlot;
   int rv;

   /*=====================================================*/
   /* Slots must  begin with keyword field or multifield. */
   /*=====================================================*/

   if ((strcmp(inputToken->lexemeValue->contents,"field") != 0) &&
       (strcmp(inputToken->lexemeValue->contents,"multifield") != 0) &&
       (strcmp(inputToken->lexemeValue->contents,"slot") != 0) &&
       (strcmp(inputToken->lexemeValue->contents,"multislot") != 0))
     {
      SyntaxErrorMessage(theEnv,"deftemplate");
      DeftemplateData(theEnv)->DeftemplateError = true;
      return NULL;
     }

   /*===============================================*/
   /* Determine if multifield slot is being parsed. */
   /*===============================================*/

   if ((strcmp(inputToken->lexemeValue->contents,"multifield") == 0) ||
       (strcmp(inputToken->lexemeValue->contents,"multislot") == 0))
     { parsingMultislot = true; }
   else
     { parsingMultislot = false; }

   /*========================================*/
   /* The name of the slot must be a symbol. */
   /*========================================*/

   SavePPBuffer(theEnv," ");
   GetToken(theEnv,readSource,inputToken);
   if (inputToken->tknType != SYMBOL_TOKEN)
     {
      SyntaxErrorMessage(theEnv,"deftemplate");
      DeftemplateData(theEnv)->DeftemplateError = true;
      return NULL;
     }

   slotName = inputToken->lexemeValue;

   /*================================================*/
   /* Determine if the slot has already been parsed. */
   /*================================================*/

   while (slotList != NULL)
     {
      if (slotList->slotName == slotName)
        {
         AlreadyParsedErrorMessage(theEnv,"slot ",slotList->slotName->contents);
         DeftemplateData(theEnv)->DeftemplateError = true;
         return NULL;
        }

      slotList = slotList->next;
     }

   /*===================================*/
   /* Parse the attributes of the slot. */
   /*===================================*/

   newSlot = DefinedSlots(theEnv,readSource,slotName,parsingMultislot,inputToken);
   if (newSlot == NULL)
     {
      DeftemplateData(theEnv)->DeftemplateError = true;
      return NULL;
     }

   /*=================================*/
   /* Check for slot conflict errors. */
   /*=================================*/

   if (CheckConstraintParseConflicts(theEnv,newSlot->constraints) == false)
     {
      ReturnSlots(theEnv,newSlot);
      DeftemplateData(theEnv)->DeftemplateError = true;
      return NULL;
     }

   if ((newSlot->defaultPresent) || (newSlot->defaultDynamic))
     { rv = ConstraintCheckExpressionChain(theEnv,newSlot->defaultList,newSlot->constraints); }
   else
     { rv = NO_VIOLATION; }

   if (rv != NO_VIOLATION)
     {
      const char *temp;
      if (newSlot->defaultDynamic) temp = "the default-dynamic attribute";
      else temp = "the default attribute";
      ConstraintViolationErrorMessage(theEnv,"An expression",temp,false,0,
                                      newSlot->slotName,0,rv,newSlot->constraints,true);
      ReturnSlots(theEnv,newSlot);
      DeftemplateData(theEnv)->DeftemplateError = true;
      return NULL;
     }

   /*==================*/
   /* Return the slot. */
   /*==================*/

   return(newSlot);
  }

/**************************************************************/
/* DefinedSlots: Parses a field or multifield slot attribute. */
/**************************************************************/
static struct templateSlot *DefinedSlots(
  Environment *theEnv,
  const char *readSource,
  CLIPSLexeme *slotName,
  bool multifieldSlot,
  struct token *inputToken)
  {
   struct templateSlot *newSlot;
   struct expr *defaultList;
   bool defaultFound = false;
   bool noneSpecified, deriveSpecified;
   CONSTRAINT_PARSE_RECORD parsedConstraints;

   /*===========================*/
   /* Build the slot container. */
   /*===========================*/

   newSlot = get_struct(theEnv,templateSlot);
   newSlot->slotName = slotName;
   newSlot->defaultList = NULL;
   newSlot->facetList = NULL;
   newSlot->constraints = GetConstraintRecord(theEnv);
   if (multifieldSlot)
     { newSlot->constraints->multifieldsAllowed = true; }
   newSlot->multislot = multifieldSlot;
   newSlot->noDefault = false;
   newSlot->defaultPresent = false;
   newSlot->defaultDynamic = false;
   newSlot->next = NULL;

   /*========================================*/
   /* Parse the primitive slot if it exists. */
   /*========================================*/

   InitializeConstraintParseRecord(&parsedConstraints);
   GetToken(theEnv,readSource,inputToken);

   while (inputToken->tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      PPBackup(theEnv);
      SavePPBuffer(theEnv," ");
      SavePPBuffer(theEnv,inputToken->printForm);

      /*================================================*/
      /* Slot attributes begin with a left parenthesis. */
      /*================================================*/

      if (inputToken->tknType != LEFT_PARENTHESIS_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"deftemplate");
         ReturnSlots(theEnv,newSlot);
         DeftemplateData(theEnv)->DeftemplateError = true;
         return NULL;
        }

      /*=============================================*/
      /* The name of the attribute must be a symbol. */
      /*=============================================*/

      GetToken(theEnv,readSource,inputToken);
      if (inputToken->tknType != SYMBOL_TOKEN)
        {
         SyntaxErrorMessage(theEnv,"deftemplate");
         ReturnSlots(theEnv,newSlot);
         DeftemplateData(theEnv)->DeftemplateError = true;
         return NULL;
        }

      /*================================================================*/
      /* Determine if the attribute is one of the standard constraints. */
      /*================================================================*/

      if (StandardConstraint(inputToken->lexemeValue->contents))
        {
         if (ParseStandardConstraint(theEnv,readSource,(inputToken->lexemeValue->contents),
                                     newSlot->constraints,&parsedConstraints,
                                     multifieldSlot) == false)
           {
            DeftemplateData(theEnv)->DeftemplateError = true;
            ReturnSlots(theEnv,newSlot);
            return NULL;
           }
        }

      /*=================================================*/
      /* else if the attribute is the default attribute, */
      /* then get the default list for this slot.        */
      /*=================================================*/

      else if ((strcmp(inputToken->lexemeValue->contents,"default") == 0) ||
               (strcmp(inputToken->lexemeValue->contents,"default-dynamic") == 0))
        {
         /*======================================================*/
         /* Check to see if the default has already been parsed. */
         /*======================================================*/

         if (defaultFound)
           {
            AlreadyParsedErrorMessage(theEnv,"default attribute",NULL);
            DeftemplateData(theEnv)->DeftemplateError = true;
            ReturnSlots(theEnv,newSlot);
            return NULL;
           }

         newSlot->noDefault = false;

         /*=====================================================*/
         /* Determine whether the default is dynamic or static. */
         /*=====================================================*/

         if (strcmp(inputToken->lexemeValue->contents,"default") == 0)
           {
            newSlot->defaultPresent = true;
            newSlot->defaultDynamic = false;
           }
         else
           {
            newSlot->defaultPresent = false;
            newSlot->defaultDynamic = true;
           }

         /*===================================*/
         /* Parse the list of default values. */
         /*===================================*/

         defaultList = ParseDefault(theEnv,readSource,multifieldSlot,newSlot->defaultDynamic,
                                  true,&noneSpecified,&deriveSpecified,&DeftemplateData(theEnv)->DeftemplateError);
         if (DeftemplateData(theEnv)->DeftemplateError == true)
           {
            ReturnSlots(theEnv,newSlot);
            return NULL;
           }

         /*==================================*/
         /* Store the default with the slot. */
         /*==================================*/

         defaultFound = true;
         if (deriveSpecified) newSlot->defaultPresent = false;
         else if (noneSpecified)
           {
            newSlot->noDefault = true;
            newSlot->defaultPresent = false;
           }
         newSlot->defaultList = defaultList;
        }

      /*===============================================*/
      /* else if the attribute is the facet attribute. */
      /*===============================================*/

      else if (strcmp(inputToken->lexemeValue->contents,"facet") == 0)
        {
         if (! ParseFacetAttribute(theEnv,readSource,newSlot,false))
           {
            ReturnSlots(theEnv,newSlot);
            DeftemplateData(theEnv)->DeftemplateError = true;
            return NULL;
           }
        }

      else if (strcmp(inputToken->lexemeValue->contents,"multifacet") == 0)
        {
         if (! ParseFacetAttribute(theEnv,readSource,newSlot,true))
           {
            ReturnSlots(theEnv,newSlot);
            DeftemplateData(theEnv)->DeftemplateError = true;
            return NULL;
           }
        }

      /*============================================*/
      /* Otherwise the attribute is an invalid one. */
      /*============================================*/

      else
        {
         SyntaxErrorMessage(theEnv,"slot attributes");
         ReturnSlots(theEnv,newSlot);
         DeftemplateData(theEnv)->DeftemplateError = true;
         return NULL;
        }

      /*===================================*/
      /* Begin parsing the next attribute. */
      /*===================================*/

      GetToken(theEnv,readSource,inputToken);
     }

   /*============================*/
   /* Return the attribute list. */
   /*============================*/

   return(newSlot);
  }

/***************************************************/
/* ParseFacetAttribute: Parses the type attribute. */
/***************************************************/
static bool ParseFacetAttribute(
  Environment *theEnv,
  const char *readSource,
  struct templateSlot *theSlot,
  bool multifacet)
  {
   struct token inputToken;
   CLIPSLexeme *facetName;
   struct expr *facetPair, *tempFacet, *facetValue = NULL, *lastValue = NULL;

   /*==============================*/
   /* Parse the name of the facet. */
   /*==============================*/

   SavePPBuffer(theEnv," ");
   GetToken(theEnv,readSource,&inputToken);

   /*==================================*/
   /* The facet name must be a symbol. */
   /*==================================*/

   if (inputToken.tknType != SYMBOL_TOKEN)
     {
      if (multifacet) SyntaxErrorMessage(theEnv,"multifacet attribute");
      else SyntaxErrorMessage(theEnv,"facet attribute");
      return false;
     }

   facetName = inputToken.lexemeValue;

   /*===================================*/
   /* Don't allow facets with the same  */
   /* name as a predefined CLIPS facet. */
   /*===================================*/

   /*====================================*/
   /* Has the facet already been parsed? */
   /*====================================*/

   for (tempFacet = theSlot->facetList;
        tempFacet != NULL;
        tempFacet = tempFacet->nextArg)
     {
      if (tempFacet->value == facetName)
        {
         if (multifacet) AlreadyParsedErrorMessage(theEnv,"multifacet ",facetName->contents);
         else AlreadyParsedErrorMessage(theEnv,"facet ",facetName->contents);
         return false;
        }
     }

   /*===============================*/
   /* Parse the value of the facet. */
   /*===============================*/

   SavePPBuffer(theEnv," ");
   GetToken(theEnv,readSource,&inputToken);

   while (inputToken.tknType != RIGHT_PARENTHESIS_TOKEN)
     {
      /*=====================================*/
      /* The facet value must be a constant. */
      /*=====================================*/

      if (! ConstantType(TokenTypeToType(inputToken.tknType)))
        {
         if (multifacet) SyntaxErrorMessage(theEnv,"multifacet attribute");
         else SyntaxErrorMessage(theEnv,"facet attribute");
         ReturnExpression(theEnv,facetValue);
         return false;
        }

      /*======================================*/
      /* Add the value to the list of values. */
      /*======================================*/

      if (lastValue == NULL)
        {
         facetValue = GenConstant(theEnv,TokenTypeToType(inputToken.tknType),inputToken.value);
         lastValue = facetValue;
        }
      else
        {
         lastValue->nextArg = GenConstant(theEnv,TokenTypeToType(inputToken.tknType),inputToken.value);
         lastValue = lastValue->nextArg;
        }

      /*=====================*/
      /* Get the next token. */
      /*=====================*/

      SavePPBuffer(theEnv," ");
      GetToken(theEnv,readSource,&inputToken);

      /*===============================================*/
      /* A facet can't contain more than one constant. */
      /*===============================================*/

      if ((! multifacet) && (inputToken.tknType != RIGHT_PARENTHESIS_TOKEN))
        {
         SyntaxErrorMessage(theEnv,"facet attribute");
         ReturnExpression(theEnv,facetValue);
         return false;
        }
     }

   /*========================================================*/
   /* Remove the space before the closing right parenthesis. */
   /*========================================================*/

   PPBackup(theEnv);
   PPBackup(theEnv);
   SavePPBuffer(theEnv,")");

   /*====================================*/
   /* A facet must contain one constant. */
   /*====================================*/

   if ((! multifacet) && (facetValue == NULL))
     {
      SyntaxErrorMessage(theEnv,"facet attribute");
      return false;
     }

   /*=================================================*/
   /* Add the facet to the list of the slot's facets. */
   /*=================================================*/

   facetPair = GenConstant(theEnv,SYMBOL_TYPE,facetName);

   if (multifacet)
     {
      facetPair->argList = GenConstant(theEnv,FCALL,FindFunction(theEnv,"create$"));
      facetPair->argList->argList = facetValue;
     }
   else
     { facetPair->argList = facetValue; }

   facetPair->nextArg = theSlot->facetList;
   theSlot->facetList = facetPair;

   /*===============================================*/
   /* The facet/multifacet was successfully parsed. */
   /*===============================================*/

   return true;
  }

#endif /* (! RUN_TIME) && (! BLOAD_ONLY) */

#endif /* DEFTEMPLATE_CONSTRUCT */


