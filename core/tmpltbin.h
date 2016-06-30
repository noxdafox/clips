   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*         DEFTEMPLATE BSAVE/BLOAD HEADER FILE         */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Added support for templates maintaining their  */
/*            own list of facts.                             */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for deftemplate slot facets.           */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_tmpltbin

#pragma once

#define _H_tmpltbin

#if (! RUN_TIME)

struct bsaveTemplateSlot
  {
   unsigned long slotName;
   unsigned int multislot : 1;
   unsigned int noDefault : 1;
   unsigned int defaultPresent : 1;
   unsigned int defaultDynamic : 1;
   long constraints;
   long defaultList;
   long facetList;
   long next;
  };

struct bsaveDeftemplate;
struct bsaveDeftemplateModule;

#include "cstrcbin.h"

struct bsaveDeftemplate
  {
   struct bsaveConstructHeader header;
   long slotList;
   unsigned int implied : 1;
   unsigned int numberOfSlots : 15;
   long patternNetwork;
  };

#include "modulbin.h"

struct bsaveDeftemplateModule
  {
   struct bsaveDefmoduleItemHeader header;
  };

#define TMPLTBIN_DATA 61

struct deftemplateBinaryData
  { 
   struct deftemplate *DeftemplateArray;
   long NumberOfDeftemplates;
   long NumberOfTemplateSlots;
   long NumberOfTemplateModules;
   struct templateSlot *SlotArray;
   struct deftemplateModule *ModuleArray;
  };
  
#define DeftemplateBinaryData(theEnv) ((struct deftemplateBinaryData *) GetEnvironmentData(theEnv,TMPLTBIN_DATA))

#define DeftemplatePointer(i) ((struct deftemplate *) (&DeftemplateBinaryData(theEnv)->DeftemplateArray[i]))

#ifndef _H_tmpltdef
#include "tmpltdef.h"
#endif

   void                           DeftemplateBinarySetup(void *);
   void                          *BloadDeftemplateModuleReference(void *,int);

#endif /* (! RUN_TIME) */

#endif /* _H_tmpltbin */



