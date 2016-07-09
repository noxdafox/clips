   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/05/16            */
   /*                                                     */
   /*          DEFTEMPLATE UTILITIES HEADER FILE          */
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
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Added additional arguments to                  */
/*            InvalidDeftemplateSlotMessage function.        */
/*                                                           */
/*            Added additional arguments to                  */
/*            PrintTemplateFact function.                    */
/*                                                           */
/*      6.30: Support for long long integers.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
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
/*************************************************************/

#ifndef _H_tmpltutl

#pragma once

#define _H_tmpltutl

#include "constrnt.h"
#include "evaluatn.h"
#include "expressn.h"
#include "factmngr.h"
#include "symbol.h"

   void                           InvalidDeftemplateSlotMessage(void *,const char *,const char *,bool);
   void                           SingleFieldSlotCardinalityError(void *,const char *);
   void                           MultiIntoSingleFieldSlotError(void *,struct templateSlot *,struct deftemplate *);
   void                           CheckTemplateFact(void *,struct fact *);
   bool                           CheckRHSSlotTypes(void *,struct expr *,struct templateSlot *,const char *);
   struct templateSlot           *GetNthSlot(struct deftemplate *,int);
   int                            FindSlotPosition(struct deftemplate *,struct symbolHashNode *);
   void                           PrintTemplateFact(void *,const char *,struct fact *,bool,bool);
   void                           UpdateDeftemplateScope(void *);
   struct templateSlot           *FindSlot(struct deftemplate *,struct symbolHashNode *,short *);
   struct deftemplate            *CreateImpliedDeftemplate(void *,SYMBOL_HN *,bool);

#endif /* _H_tmpltutl */



