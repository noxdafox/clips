   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*                                                     */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Changed integer type/precision.                */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_insmult

#pragma once

#define _H_insmult

#include "evaluatn.h"

#if (! RUN_TIME)
   void                           SetupInstanceMultifieldCommands(Environment *);
#endif

   void                           MVSlotReplaceCommand(Environment *,UDFContext *,UDFValue *);
   void                           MVSlotInsertCommand(Environment *,UDFContext *,UDFValue *);
   void                           MVSlotDeleteCommand(Environment *,UDFContext *,UDFValue *);
   void                           DirectMVReplaceCommand(Environment *,UDFContext *,UDFValue *);
   void                           DirectMVInsertCommand(Environment *,UDFContext *,UDFValue *);
   void                           DirectMVDeleteCommand(Environment *,UDFContext *,UDFValue *);

#endif /* _H_insmult */



