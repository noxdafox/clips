   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*        FACT RHS PATTERN PARSER HEADER MODULE        */
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
/*      6.30: Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Added code to prevent a clear command from     */
/*            being executed during fact assertions via      */
/*            Increment/DecrementClearReadyLocks API.        */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_factrhs
#define _H_factrhs

#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_scanner
#include "scanner.h"
#endif
#ifndef _H_factmngr
#include "factmngr.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

   struct expr                   *BuildRHSAssert(void *,const char *,struct token *,int *,int,int,const char *);
   struct expr                   *GetAssertArgument(void *,const char *,struct token *,int *,int,int,int *);
   struct expr                   *GetRHSPattern(void *,const char *,struct token *,int *,int,
                                                       int,int,int);
   struct fact                   *StringToFact(void *,const char *);

#endif /* _H_factrhs */

