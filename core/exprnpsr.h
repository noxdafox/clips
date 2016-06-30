   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*            EXPRESSION PARSER HEADER FILE            */
   /*******************************************************/

/*************************************************************/
/* Purpose: Provides routines for parsing expressions.       */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Module specifier can be used within an         */
/*            expression to refer to a deffunction or        */
/*            defgeneric exported by the specified module,   */
/*            but not necessarily imported by the current    */
/*            module.                                        */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_exprnpsr

#pragma once

#define _H_exprnpsr

#if (! RUN_TIME)

typedef struct saved_contexts
  {
   int rtn;
   int brk;
   struct saved_contexts *nxt;
  } SAVED_CONTEXTS;

#endif

#include "extnfunc.h"
#include "scanner.h"

   struct expr                   *Function0Parse(void *,const char *);
   struct expr                   *Function1Parse(void *,const char *);
   struct expr                   *Function2Parse(void *,const char *,const char *);
   void                           PushRtnBrkContexts(void *);
   void                           PopRtnBrkContexts(void *);
   intBool                        ReplaceSequenceExpansionOps(void *,struct expr *,struct expr *,
                                                                     void *,void *);
   struct expr                   *CollectArguments(void *,struct expr *,const char *);
   struct expr                   *ArgumentParse(void *,const char *,int *);
   struct expr                   *ParseAtomOrExpression(void *,const char *,struct token *);
   EXPRESSION                    *ParseConstantArguments(void *,const char *,int *);
   intBool                        EnvSetSequenceOperatorRecognition(void *,int);
   intBool                        EnvGetSequenceOperatorRecognition(void *);
   struct expr                   *GroupActions(void *,const char *,struct token *,
                                                      int,const char *,int);
   struct expr                   *RemoveUnneededProgn(void *,struct expr *);

#if (! RUN_TIME)

   int                            CheckExpressionAgainstRestrictions(void *,struct expr *,
                                                                            const char *,const char *);
#endif

#if ALLOW_ENVIRONMENT_GLOBALS

   intBool                        SetSequenceOperatorRecognition(int);
   intBool                        GetSequenceOperatorRecognition(void);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_exprnpsr */




