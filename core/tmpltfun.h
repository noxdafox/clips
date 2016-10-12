   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*          DEFTEMPLATE FUNCTION HEADER FILE           */
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
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Added deftemplate-slot-names,                  */
/*            deftemplate-slot-default-value,                */
/*            deftemplate-slot-cardinality,                  */
/*            deftemplate-slot-allowed-values,               */
/*            deftemplate-slot-range,                        */
/*            deftemplate-slot-types,                        */
/*            deftemplate-slot-multip,                       */
/*            deftemplate-slot-singlep,                      */
/*            deftemplate-slot-existp, and                   */
/*            deftemplate-slot-defaultp functions.           */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Support for deftemplate slot facets.           */
/*                                                           */
/*            Added deftemplate-slot-facet-existp and        */
/*            deftemplate-slot-facet-value functions.        */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Support for modify callback function.          */
/*                                                           */
/*            Added additional argument to function          */
/*            CheckDeftemplateAndSlotArguments to specify    */
/*            the expected number of arguments.              */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
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
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_tmpltfun

#pragma once

#define _H_tmpltfun

#include "expressn.h"
#include "factmngr.h"
#include "scanner.h"
#include "symbol.h"
#include "tmpltdef.h"

   bool                           UpdateModifyDuplicate(Environment *,struct expr *,const char *,void *);
   struct expr                   *ModifyParse(Environment *,struct expr *,const char *);
   struct expr                   *DuplicateParse(Environment *,struct expr *,const char *);
   void                           DeftemplateFunctions(Environment *);
   void                           ModifyCommand(Environment *,UDFContext *,UDFValue *);
   void                           DuplicateCommand(Environment *,UDFContext *,UDFValue *);
   void                           DeftemplateSlotNamesFunction(Environment *,UDFContext *,UDFValue *);
   void                           EnvDeftemplateSlotNames(Environment *,Deftemplate *,UDFValue *);
   void                           DeftemplateSlotDefaultValueFunction(Environment *,UDFContext *,UDFValue *);
   bool                           EnvDeftemplateSlotDefaultValue(Environment *,Deftemplate *,const char *,UDFValue *);
   void                           DeftemplateSlotCardinalityFunction(Environment *,UDFContext *,UDFValue *);
   void                           EnvDeftemplateSlotCardinality(Environment *,Deftemplate *,const char *,UDFValue *);
   void                           DeftemplateSlotAllowedValuesFunction(Environment *,UDFContext *,UDFValue *);
   void                           EnvDeftemplateSlotAllowedValues(Environment *,Deftemplate *,const char *,UDFValue *);
   void                           DeftemplateSlotRangeFunction(Environment *,UDFContext *,UDFValue *);
   void                           EnvDeftemplateSlotRange(Environment *,Deftemplate *,const char *,UDFValue *);
   void                           DeftemplateSlotTypesFunction(Environment *,UDFContext *,UDFValue *);
   void                           EnvDeftemplateSlotTypes(Environment *,Deftemplate *,const char *,UDFValue *);
   void                           DeftemplateSlotMultiPFunction(Environment *,UDFContext *,UDFValue *);
   bool                           EnvDeftemplateSlotMultiP(Environment *,Deftemplate *,const char *);
   void                           DeftemplateSlotSinglePFunction(Environment *,UDFContext *,UDFValue *);
   bool                           EnvDeftemplateSlotSingleP(Environment *,Deftemplate *,const char *);
   void                           DeftemplateSlotExistPFunction(Environment *,UDFContext *,UDFValue *);
   bool                           EnvDeftemplateSlotExistP(Environment *,Deftemplate *,const char *);
   void                           DeftemplateSlotDefaultPFunction(Environment *,UDFContext *,UDFValue *);
   int                            EnvDeftemplateSlotDefaultP(Environment *,Deftemplate *,const char *);
   void                           DeftemplateSlotFacetExistPFunction(Environment *,UDFContext *,UDFValue *);
   bool                           EnvDeftemplateSlotFacetExistP(Environment *,Deftemplate *,const char *,const char *);
   void                           DeftemplateSlotFacetValueFunction(Environment *,UDFContext *,UDFValue *);
   bool                           EnvDeftemplateSlotFacetValue(Environment *,Deftemplate *,const char *,const char *,UDFValue *);
   CLIPSLexeme                   *FindTemplateForFactAddress(CLIPSLexeme *,struct lhsParseNode *);

#endif /* _H_tmpltfun */




