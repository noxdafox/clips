   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*                 DEVELOPER HEADER FILE               */
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
/*      6.24: Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*      6.30: Added support for hashed alpha memories.       */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*            Functions enable-gc-heuristics and             */
/*            disable-gc-heuristics are no longer supported. */
/*                                                           */
/*            Changed integer type/precision.                */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_developr

#pragma once

#define _H_developr

   void                           DeveloperCommands(void *);
   void                           PrimitiveTablesInfo(void *);
   void                           PrimitiveTablesUsage(void *);

#if DEFRULE_CONSTRUCT && DEFTEMPLATE_CONSTRUCT
   void                           ShowFactPatternNetwork(void *);
   intBool                        ValidateFactIntegrity(void *);
#endif
#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           PrintObjectPatternNetwork(void *);
#endif
#if OBJECT_SYSTEM
   void                           InstanceTableUsage(void *);
#endif
#if DEFRULE_CONSTRUCT
   void                           ValidateBetaMemories(void *);
#endif

#endif /* _H_developr */


