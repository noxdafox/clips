   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_tmpltutl

#define _H_tmpltutl

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_factmngr
#include "factmngr.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

   void                           InvalidDeftemplateSlotMessage(void *,const char *,const char *,int);
   void                           SingleFieldSlotCardinalityError(void *,const char *);
   void                           MultiIntoSingleFieldSlotError(void *,struct templateSlot *,struct deftemplate *);
   void                           CheckTemplateFact(void *,struct fact *);
   intBool                        CheckRHSSlotTypes(void *,struct expr *,struct templateSlot *,const char *);
   struct templateSlot           *GetNthSlot(struct deftemplate *,int);
   int                            FindSlotPosition(struct deftemplate *,struct symbolHashNode *);
   void                           PrintTemplateFact(void *,const char *,struct fact *,int,int);
   void                           UpdateDeftemplateScope(void *);
   struct templateSlot           *FindSlot(struct deftemplate *,struct symbolHashNode *,short *);
   struct deftemplate            *CreateImpliedDeftemplate(void *,SYMBOL_HN *,int);

#endif /* _H_tmpltutl */



