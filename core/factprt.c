   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  10/01/16             */
   /*                                                     */
   /*           FACT RETE PRINT FUNCTIONS MODULE          */
   /*******************************************************/

/*************************************************************/
/* Purpose: Print routines for the fact rete primitives.     */
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
/*            Changed integer type/precision.                */
/*                                                           */
/*            Updates to support new struct members.         */
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

#include <stdio.h>

#include "setup.h"

#if DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT

#include "envrnmnt.h"
#include "factgen.h"
#include "prntutil.h"
#include "router.h"
#include "symbol.h"

#include "factprt.h"

/***************************************/
/* PrintFactJNCompVars1: Print routine */
/*   for the FactJNCompVars1 function. */
/***************************************/
void PrintFactJNCompVars1(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCompVarsJN1Call *hack;

   hack = (struct factCompVarsJN1Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-jn-cmp-vars1 ");
   if (hack->pass) EnvPrintRouter(theEnv,logicalName,"= ");
   else EnvPrintRouter(theEnv,logicalName,"<> ");

   EnvPrintRouter(theEnv,logicalName,"p");
   PrintLongInteger(theEnv,logicalName,(long long) hack->pattern1 + 1);

   if (hack->p1lhs)
     { EnvPrintRouter(theEnv,logicalName," L"); }
   else if (hack->p1rhs)
     { EnvPrintRouter(theEnv,logicalName," R"); }

   EnvPrintRouter(theEnv,logicalName," s");
   PrintLongInteger(theEnv,logicalName,(long long) hack->slot1);

   EnvPrintRouter(theEnv,logicalName," p");
   PrintLongInteger(theEnv,logicalName,(long long) hack->pattern2 + 1);

   if (hack->p2lhs)
     { EnvPrintRouter(theEnv,logicalName," L"); }
   else if (hack->p2rhs)
     { EnvPrintRouter(theEnv,logicalName," R"); }

   EnvPrintRouter(theEnv,logicalName," s");
   PrintLongInteger(theEnv,logicalName,(long long) hack->slot2);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactJNCompVars2: Print routine */
/*   for the FactJNCompVars2 function. */
/***************************************/
void PrintFactJNCompVars2(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCompVarsJN2Call *hack;

   hack = (struct factCompVarsJN2Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-jn-cmp-vars2 ");
   if (hack->pass) EnvPrintRouter(theEnv,logicalName,"= ");
   else EnvPrintRouter(theEnv,logicalName,"<> ");

   EnvPrintRouter(theEnv,logicalName,"p");
   PrintLongInteger(theEnv,logicalName,(long long) hack->pattern1 + 1);

   if (hack->p1lhs)
     { EnvPrintRouter(theEnv,logicalName," L"); }
   else if (hack->p1rhs)
     { EnvPrintRouter(theEnv,logicalName," R"); }

   EnvPrintRouter(theEnv,logicalName," s");
   PrintLongInteger(theEnv,logicalName,(long long) hack->slot1);

   if (hack->fromBeginning1) EnvPrintRouter(theEnv,logicalName, " b");
   else EnvPrintRouter(theEnv,logicalName," e");

   EnvPrintRouter(theEnv,logicalName," f");
   PrintLongInteger(theEnv,logicalName,(long long) hack->offset1);

   EnvPrintRouter(theEnv,logicalName," p");
   PrintLongInteger(theEnv,logicalName,(long long) hack->pattern2 + 1);

   if (hack->p2lhs)
     { EnvPrintRouter(theEnv,logicalName," L"); }
   else if (hack->p2rhs)
     { EnvPrintRouter(theEnv,logicalName," R"); }

   EnvPrintRouter(theEnv,logicalName," s");
   PrintLongInteger(theEnv,logicalName,(long long) hack->slot2);

   if (hack->fromBeginning2) EnvPrintRouter(theEnv,logicalName," b");
   else EnvPrintRouter(theEnv,logicalName," e");

   EnvPrintRouter(theEnv,logicalName," f");
   PrintLongInteger(theEnv,logicalName,(long long) hack->offset2);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactPNCompVars1: Print routine */
/*   for the FactPNCompVars1 function. */
/***************************************/
void PrintFactPNCompVars1(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCompVarsPN1Call *hack;

   hack = (struct factCompVarsPN1Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-pn-cmp-vars ");
   if (hack->pass) EnvPrintRouter(theEnv,logicalName,"p ");
   else EnvPrintRouter(theEnv,logicalName,"n ");
   PrintLongInteger(theEnv,logicalName,(long long) hack->field1);
   EnvPrintRouter(theEnv,logicalName," ");
   PrintLongInteger(theEnv,logicalName,(long long) hack->field2);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/**************************************/
/* PrintFactSlotLength: Print routine */
/*   for the FactSlotLength function. */
/**************************************/
void PrintFactSlotLength(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factCheckLengthPNCall *hack;

   hack = (struct factCheckLengthPNCall *) ((CLIPSBitMap *) theValue)->contents;

   EnvPrintRouter(theEnv,logicalName,"(slot-length ");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);
   EnvPrintRouter(theEnv,logicalName," ");
   if (hack->exactly) EnvPrintRouter(theEnv,logicalName,"= ");
   else EnvPrintRouter(theEnv,logicalName,">= ");
   PrintLongInteger(theEnv,logicalName,(long long) hack->minLength);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactJNGetVar1: Print routine */
/*   for the FactJNGetvar1 function. */
/*************************************/
void PrintFactJNGetVar1(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarJN1Call *hack;

   hack = (struct factGetVarJN1Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-jn-getvar-1 ");
   if (hack->factAddress) EnvPrintRouter(theEnv,logicalName,"t ");
   else EnvPrintRouter(theEnv,logicalName,"f ");
   if (hack->allFields) EnvPrintRouter(theEnv,logicalName,"t ");
   else EnvPrintRouter(theEnv,logicalName,"f ");

   EnvPrintRouter(theEnv,logicalName,"p");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichPattern + 1);
   EnvPrintRouter(theEnv,logicalName," ");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichField);
   EnvPrintRouter(theEnv,logicalName," s");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);

   if (hack->lhs)
     { EnvPrintRouter(theEnv,logicalName," L"); }
   else if (hack->rhs)
     { EnvPrintRouter(theEnv,logicalName," R"); }
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactJNGetVar2: Print routine */
/*   for the FactJNGetvar2 function. */
/*************************************/
void PrintFactJNGetVar2(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarJN2Call *hack;

   hack = (struct factGetVarJN2Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-jn-getvar-2");

   EnvPrintRouter(theEnv,logicalName," p");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichPattern + 1);
   EnvPrintRouter(theEnv,logicalName," s");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);
   if (hack->lhs)
     { EnvPrintRouter(theEnv,logicalName," L"); }
   else if (hack->rhs)
     { EnvPrintRouter(theEnv,logicalName," R"); }
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactJNGetVar3: Print routine */
/*   for the FactJNGetVar3 function. */
/*************************************/
void PrintFactJNGetVar3(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarJN3Call *hack;

   hack = (struct factGetVarJN3Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-jn-getvar-3 ");
   if (hack->fromBeginning) EnvPrintRouter(theEnv,logicalName,"t ");
   else EnvPrintRouter(theEnv,logicalName,"f ");
   if (hack->fromEnd) EnvPrintRouter(theEnv,logicalName,"t ");
   else EnvPrintRouter(theEnv,logicalName,"f ");

   PrintLongInteger(theEnv,logicalName,(long long) hack->beginOffset);
   EnvPrintRouter(theEnv,logicalName," ");
   PrintLongInteger(theEnv,logicalName,(long long) hack->endOffset);
   EnvPrintRouter(theEnv,logicalName," ");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);

   EnvPrintRouter(theEnv,logicalName," p");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichPattern + 1);

   if (hack->lhs)
     { EnvPrintRouter(theEnv,logicalName," L"); }
   else if (hack->rhs)
     { EnvPrintRouter(theEnv,logicalName," R"); }

   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactPNGetVar1: Print routine */
/*   for the FactPNGetvar1 function. */
/*************************************/
void PrintFactPNGetVar1(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarPN1Call *hack;

   hack = (struct factGetVarPN1Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-pn-getvar-1 ");
   if (hack->factAddress) EnvPrintRouter(theEnv,logicalName,"t ");
   else EnvPrintRouter(theEnv,logicalName,"f ");
   if (hack->allFields) EnvPrintRouter(theEnv,logicalName,"t F");
   else EnvPrintRouter(theEnv,logicalName,"f F");

   PrintLongInteger(theEnv,logicalName,(long long) hack->whichField);
   EnvPrintRouter(theEnv,logicalName," S");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactPNGetVar2: Print routine */
/*   for the FactPNGetvar2 function. */
/*************************************/
void PrintFactPNGetVar2(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarPN2Call *hack;

   hack = (struct factGetVarPN2Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-pn-getvar-2 S");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/*************************************/
/* PrintFactPNGetVar3: Print routine */
/*   for the FactPNGetvar3 function. */
/*************************************/
void PrintFactPNGetVar3(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factGetVarPN3Call *hack;

   hack = (struct factGetVarPN3Call *) ((CLIPSBitMap *) theValue)->contents;
   EnvPrintRouter(theEnv,logicalName,"(fact-pn-getvar-3 ");

   if (hack->fromBeginning) EnvPrintRouter(theEnv,logicalName,"t ");
   else EnvPrintRouter(theEnv,logicalName,"f ");
   if (hack->fromEnd) EnvPrintRouter(theEnv,logicalName,"t B");
   else EnvPrintRouter(theEnv,logicalName,"f B");

   PrintLongInteger(theEnv,logicalName,(long long) hack->beginOffset);
   EnvPrintRouter(theEnv,logicalName," E");
   PrintLongInteger(theEnv,logicalName,(long long) hack->endOffset);
   EnvPrintRouter(theEnv,logicalName," S");
   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactPNConstant1: Print routine */
/*   for the FactPNConstant1 function. */
/***************************************/
void PrintFactPNConstant1(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factConstantPN1Call *hack;

   hack = (struct factConstantPN1Call *) ((CLIPSBitMap *) theValue)->contents;

   EnvPrintRouter(theEnv,logicalName,"(fact-pn-constant1 ");

   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);

   if (hack->testForEquality) EnvPrintRouter(theEnv,logicalName," = ");
   else EnvPrintRouter(theEnv,logicalName," != ");

   PrintAtom(theEnv,logicalName,GetFirstArgument()->type,GetFirstArgument()->value);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

/***************************************/
/* PrintFactPNConstant2: Print routine */
/*   for the FactPNConstant2 function. */
/***************************************/
void PrintFactPNConstant2(
  Environment *theEnv,
  const char *logicalName,
  void *theValue)
  {
#if DEVELOPER
   struct factConstantPN2Call *hack;

   hack = (struct factConstantPN2Call *) ((CLIPSBitMap *) theValue)->contents;

   EnvPrintRouter(theEnv,logicalName,"(fact-pn-constant2 ");

   PrintLongInteger(theEnv,logicalName,(long long) hack->whichSlot);

   EnvPrintRouter(theEnv,logicalName," ");

   PrintLongInteger(theEnv,logicalName,(long long) hack->offset);

   if (hack->testForEquality) EnvPrintRouter(theEnv,logicalName," = ");
   else EnvPrintRouter(theEnv,logicalName," != ");

   PrintAtom(theEnv,logicalName,GetFirstArgument()->type,GetFirstArgument()->value);
   EnvPrintRouter(theEnv,logicalName,")");
#else
#if MAC_XCD
#pragma unused(theEnv)
#pragma unused(logicalName)
#pragma unused(theValue)
#endif
#endif
  }

#endif /* DEFTEMPLATE_CONSTRUCT && DEFRULE_CONSTRUCT */


