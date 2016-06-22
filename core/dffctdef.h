   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                DEFFACTS HEADER FILE                 */
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
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Changed find construct functionality so that   */
/*            imported modules are search when locating a    */
/*            named construct.                               */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*************************************************************/

#ifndef _H_dffctdef
#define _H_dffctdef

#ifndef _H_conscomp
#include "conscomp.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_cstrccom
#include "cstrccom.h"
#endif

#define DEFFACTS_DATA 0

struct deffactsData
  { 
   struct construct *DeffactsConstruct;
   int DeffactsModuleIndex;  
#if CONSTRUCT_COMPILER && (! RUN_TIME)
   struct CodeGeneratorItem *DeffactsCodeItem;
#endif
  };
  
struct deffacts
  {
   struct constructHeader header;
   struct expr *assertList;
  };

struct deffactsModule
  {
   struct defmoduleItemHeader header;
  };

#define DeffactsData(theEnv) ((struct deffactsData *) GetEnvironmentData(theEnv,DEFFACTS_DATA))

   void                           InitializeDeffacts(void *);
   void                          *EnvFindDeffacts(void *,const char *);
   void                          *EnvFindDeffactsInModule(void *,const char *);
   void                          *EnvGetNextDeffacts(void *,void *);
   void                           CreateInitialFactDeffacts(void);
   intBool                        EnvIsDeffactsDeletable(void *,void *);
   struct deffactsModule         *GetDeffactsModuleItem(void *,struct defmodule *);
   const char                    *EnvDeffactsModule(void *,void *);
   const char                    *EnvGetDeffactsName(void *,void *);
   const char                    *EnvGetDeffactsPPForm(void *,void *);

#if ALLOW_ENVIRONMENT_GLOBALS

   void                          *FindDeffacts(const char *);
   void                          *GetNextDeffacts(void *);
   intBool                        IsDeffactsDeletable(void *);
   const char                    *DeffactsModule(void *);
   const char                    *GetDeffactsName(void *);
   const char                    *GetDeffactsPPForm(void *);
   
#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_dffctdef */


