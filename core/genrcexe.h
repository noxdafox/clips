   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
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
/*      6.24: Removed IMPERATIVE_METHODS compilation flag.   */
/*                                                           */
/*      6.30: Changed garbage collection algorithm.          */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Changed integer type/precision.                */
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
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_genrcexe

#pragma once

#define _H_genrcexe

#if DEFGENERIC_CONSTRUCT

#include "evaluatn.h"
#include "expressn.h"
#include "genrcfun.h"

   void                           GenericDispatch(Environment *,Defgeneric *,Defmethod *,Defmethod *,EXPRESSION *,DATA_OBJECT *);
   void                           UnboundMethodErr(Environment *);
   bool                           IsMethodApplicable(Environment *,Defmethod *);

   bool                           NextMethodP(Environment *);
   void                           CallNextMethod(Environment *,DATA_OBJECT *);
   void                           CallSpecificMethod(Environment *,DATA_OBJECT *);
   void                           OverrideNextMethod(Environment *,DATA_OBJECT *);

   void                           GetGenericCurrentArgument(Environment *,DATA_OBJECT *);

#endif /* DEFGENERIC_CONSTRUCT */

#endif /* _H_genrcexe */




