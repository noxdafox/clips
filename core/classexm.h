   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  07/30/16            */
   /*                                                     */
   /*             CLASS EXAMINATION HEADER FILE           */
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
/*      6.23: Corrected compilation errors for files         */
/*            generated by constructs-to-c. DR0861           */
/*                                                           */                                      
/*      6.24: The DescribeClass macros were incorrectly      */
/*            defined. DR0862                                */
/*                                                           */
/*            Added allowed-classes slot facet.              */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Added EnvSlotDefaultP function.                */
/*                                                           */
/*            Borland C (IBM_TBC) and Metrowerks CodeWarrior */
/*            (MAC_MCW, IBM_MCW) are no longer supported.    */
/*                                                           */
/*            Used gensprintf and genstrcat instead of       */
/*            sprintf and strcat.                            */
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
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_classexm

#pragma once

#define _H_classexm

#if DEBUGGING_FUNCTIONS

   void                           BrowseClassesCommand(Environment *);
   void                           EnvBrowseClasses(Environment *,const char *,Defclass *);
   void                           DescribeClassCommand(Environment *);
   void                           EnvDescribeClass(Environment *,const char *,Defclass *);

#endif /* DEBUGGING_FUNCTIONS */

   const char                    *GetCreateAccessorString(SlotDescriptor *);
   void                          *GetDefclassModuleCommand(Environment *);
   bool                           SuperclassPCommand(Environment *);
   bool                           EnvSuperclassP(Environment *,Defclass *,Defclass *);
   bool                           SubclassPCommand(Environment *);
   bool                           EnvSubclassP(Environment *,Defclass *,Defclass *);
   bool                           SlotExistPCommand(Environment *);
   bool                           EnvSlotExistP(Environment *,Defclass *,const char *,bool);
   bool                           MessageHandlerExistPCommand(Environment *);
   bool                           SlotWritablePCommand(Environment *);
   bool                           EnvSlotWritableP(Environment *,Defclass *,const char *);
   bool                           SlotInitablePCommand(Environment *);
   bool                           EnvSlotInitableP(Environment *,Defclass *,const char *);
   bool                           SlotPublicPCommand(Environment *);
   bool                           EnvSlotPublicP(Environment *,Defclass *,const char *);
   bool                           SlotDirectAccessPCommand(Environment *);
   bool                           EnvSlotDirectAccessP(Environment *,Defclass *,const char *);
   void                           SlotDefaultValueCommand(Environment *,DATA_OBJECT_PTR);
   bool                           EnvSlotDefaultValue(Environment *,Defclass *,const char *,DATA_OBJECT_PTR);
   bool                           ClassExistPCommand(Environment *);
   int                            EnvSlotDefaultP(Environment *,Defclass *,const char *);
  
#if ALLOW_ENVIRONMENT_GLOBALS

#if DEBUGGING_FUNCTIONS
   void                           BrowseClasses(const char *,Defclass *);
   void                           DescribeClass(const char *,Defclass *);
#endif
   bool                           SlotDirectAccessP(Defclass *,const char *);
   bool                           SlotExistP(Defclass *,const char *,bool);
   bool                           SlotInitableP(Defclass *,const char *);
   bool                           SlotPublicP(Defclass *,const char *);
   int                            SlotDefaultP(Defclass *,const char *);
   bool                           SlotWritableP(Defclass *,const char *);
   bool                           SubclassP(Defclass *,Defclass *);
   bool                           SuperclassP(Defclass *,Defclass *);
   bool                           SlotDefaultValue(Defclass *,const char *,DATA_OBJECT_PTR);

#endif /* ALLOW_ENVIRONMENT_GLOBALS */

#endif /* _H_classexm */
