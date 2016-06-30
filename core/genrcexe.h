   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
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
/*************************************************************/

#ifndef _H_genrcexe

#pragma once

#define _H_genrcexe

#if DEFGENERIC_CONSTRUCT

#include "evaluatn.h"
#include "expressn.h"
#include "genrcfun.h"

   void                           GenericDispatch(void *,DEFGENERIC *,DEFMETHOD *,DEFMETHOD *,EXPRESSION *,DATA_OBJECT *);
   void                           UnboundMethodErr(void *);
   intBool                        IsMethodApplicable(void *,DEFMETHOD *);

   int                            NextMethodP(void *);
   void                           CallNextMethod(void *,DATA_OBJECT *);
   void                           CallSpecificMethod(void *,DATA_OBJECT *);
   void                           OverrideNextMethod(void *,DATA_OBJECT *);

   void                           GetGenericCurrentArgument(void *,DATA_OBJECT *);

#endif /* DEFGENERIC_CONSTRUCT */

#endif /* _H_genrcexe */




