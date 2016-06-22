   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_tmpltfun

#define _H_tmpltfun

#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_factmngr
#include "factmngr.h"
#endif
#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

   intBool                        UpdateModifyDuplicate(void *,struct expr *,const char *,void *);
   struct expr                   *ModifyParse(void *,struct expr *,const char *);
   struct expr                   *DuplicateParse(void *,struct expr *,const char *);
   void                           DeftemplateFunctions( void *);
   void                           ModifyCommand(void *,DATA_OBJECT_PTR);
   void                           DuplicateCommand(void *,DATA_OBJECT_PTR);
   void                           DeftemplateSlotNamesFunction(void *,DATA_OBJECT *);
   void                           EnvDeftemplateSlotNames(void *,void *,DATA_OBJECT *);
   void                           DeftemplateSlotDefaultValueFunction(void *,DATA_OBJECT *);
   intBool                        EnvDeftemplateSlotDefaultValue(void *,void *,const char *,DATA_OBJECT *);
   void                           DeftemplateSlotCardinalityFunction(void *,DATA_OBJECT *);
   void                           EnvDeftemplateSlotCardinality(void *,void *,const char *,DATA_OBJECT *);
   void                           DeftemplateSlotAllowedValuesFunction(void *,DATA_OBJECT *);
   void                           EnvDeftemplateSlotAllowedValues(void *,void *,const char *,DATA_OBJECT *);
   void                           DeftemplateSlotRangeFunction(void *,DATA_OBJECT *);
   void                           EnvDeftemplateSlotRange(void *,void *,const char *,DATA_OBJECT *);
   void                           DeftemplateSlotTypesFunction(void *,DATA_OBJECT *);
   void                           EnvDeftemplateSlotTypes(void *,void *,const char *,DATA_OBJECT *);
   int                            DeftemplateSlotMultiPFunction(void *);
   int                            EnvDeftemplateSlotMultiP(void *,void *,const char *);
   int                            DeftemplateSlotSinglePFunction(void *);
   int                            EnvDeftemplateSlotSingleP(void *,void *,const char *);
   int                            DeftemplateSlotExistPFunction(void *);
   int                            EnvDeftemplateSlotExistP(void *,void *,const char *);
   void                          *DeftemplateSlotDefaultPFunction(void *);
   int                            EnvDeftemplateSlotDefaultP(void *,void *,const char *);
   int                            DeftemplateSlotFacetExistPFunction(void *);
   int                            EnvDeftemplateSlotFacetExistP(void *,void *,const char *,const char *);
   void                           DeftemplateSlotFacetValueFunction(void *,DATA_OBJECT *);
   int                            EnvDeftemplateSlotFacetValue(void *,void *,const char *,const char *,DATA_OBJECT *);

#if ALLOW_ENVIRONMENT_GLOBALS

   void                           DeftemplateSlotNames(void *,DATA_OBJECT *);
   intBool                        DeftemplateSlotDefaultValue(void *,const char *,DATA_OBJECT_PTR);
   void                           DeftemplateSlotCardinality(void *,const char *,DATA_OBJECT *);
   void                           DeftemplateSlotAllowedValues(void *,const char *,DATA_OBJECT *);
   void                           DeftemplateSlotRange(void *,const char *,DATA_OBJECT *);
   void                           DeftemplateSlotTypes(void *,const char *,DATA_OBJECT *);
   int                            DeftemplateSlotMultiP(void *,const char *);
   int                            DeftemplateSlotSingleP(void *,const char *);
   int                            DeftemplateSlotExistP(void *,const char *);
   int                            DeftemplateSlotDefaultP(void *,const char *);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_tmpltfun */




