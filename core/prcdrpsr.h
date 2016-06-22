   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*       PROCEDURAL FUNCTIONS PARSER HEADER FILE       */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian L. Dantes                                      */
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
/*      6.30: Local variables set with the bind function     */
/*            persist until a reset/clear command is issued. */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when BLOAD_ONLY compiler   */
/*            flag is set to 1.                              */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_prcdrpsr

#define _H_prcdrpsr

#ifndef _H_constrnt
#include "constrnt.h"
#endif

struct BindInfo
  {
   struct symbolHashNode *name;
   CONSTRAINT_RECORD *constraints;
   struct BindInfo *next;
  };

#if (! RUN_TIME)
   void                           ProceduralFunctionParsers(void *);
   struct BindInfo               *GetParsedBindNames(void *);
   void                           SetParsedBindNames(void *,struct BindInfo *);
   void                           ClearParsedBindNames(void *);
   intBool                        ParsedBindNamesEmpty(void *);
#endif
#if (! BLOAD_ONLY) && (! RUN_TIME)
   int                            SearchParsedBindNames(void *,struct symbolHashNode *);
   int                            CountParsedBindNames(void *);
   void                           RemoveParsedBindName(void *,struct symbolHashNode *);
   struct constraintRecord       *FindBindConstraints(void *,struct symbolHashNode *);
#endif

#endif /* _H_prcdrpsr */




