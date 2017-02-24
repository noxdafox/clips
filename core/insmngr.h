   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  08/25/16            */
   /*                                                     */
   /*          INSTANCE PRIMITIVE SUPPORT MODULE          */
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
/*      6.24: Removed LOGICAL_DEPENDENCIES compilation flag. */
/*                                                           */
/*            Converted INSTANCE_PATTERN_MATCHING to         */
/*            DEFRULE_CONSTRUCT.                             */
/*                                                           */
/*            Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Used gensprintf instead of sprintf.            */
/*                                                           */
/*            Changed garbage collection algorithm.          */
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
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#ifndef _H_insmngr

#pragma once

#define _H_insmngr

#include "object.h"

   void                           InitializeInstanceCommand(Environment *,UDFContext *,UDFValue *);
   void                           MakeInstanceCommand(Environment *,UDFContext *,UDFValue *);
   CLIPSLexeme                   *GetFullInstanceName(Environment *,Instance *);
   Instance                      *BuildInstance(Environment *,CLIPSLexeme *,Defclass *,bool);
   void                           InitSlotsCommand(Environment *,UDFContext *,UDFValue *);
   bool                           QuashInstance(Environment *,Instance *);

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           InactiveInitializeInstance(Environment *,UDFContext *,UDFValue *);
   void                           InactiveMakeInstance(Environment *,UDFContext *,UDFValue *);
#endif

   InstanceBuilder               *CreateInstanceBuilder(Environment *,const char *);
   bool                           IBPutSlot(InstanceBuilder *,const char *,CLIPSValue *);

   Instance                      *IBMake(InstanceBuilder *,const char *);

   void                           IBDispose(InstanceBuilder *);
   void                           IBAbort(InstanceBuilder *);
   bool                           IBSetDefclass(InstanceBuilder *,const char *);

   bool                           IBPutSlotCLIPSInteger(InstanceBuilder *,const char *,CLIPSInteger *);
   bool                           IBPutSlotInt(InstanceBuilder *,const char *,int);
   bool                           IBPutSlotLong(InstanceBuilder *,const char *,long);
   bool                           IBPutSlotLongLong(InstanceBuilder *,const char *,long long);

   bool                           IBPutSlotCLIPSFloat(InstanceBuilder *,const char *,CLIPSFloat *);
   bool                           IBPutSlotFloat(InstanceBuilder *,const char *,float);
   bool                           IBPutSlotDouble(InstanceBuilder *,const char *,double);

   bool                           IBPutSlotCLIPSLexeme(InstanceBuilder *,const char *,CLIPSLexeme *);
   bool                           IBPutSlotSymbol(InstanceBuilder *,const char *,const char *);
   bool                           IBPutSlotString(InstanceBuilder *,const char *,const char *);
   bool                           IBPutSlotInstanceName(InstanceBuilder *,const char *,const char *);

   bool                           IBPutSlotFact(InstanceBuilder *,const char *,Fact *);
   bool                           IBPutSlotInstance(InstanceBuilder *,const char *,Instance *);
   bool                           IBPutSlotExternalAddress(InstanceBuilder *,const char *,CLIPSExternalAddress *);
   bool                           IBPutSlotMultifield(InstanceBuilder *,const char *,Multifield *);

   InstanceModifier              *CreateInstanceModifier(Environment *,Instance *);
   bool                           IMPutSlot(InstanceModifier *,const char *,CLIPSValue *);
   void                           IMDispose(InstanceModifier *);
   void                           IMAbort(InstanceModifier *);
   bool                           IMSetInstance(InstanceModifier *,Instance *);
   Instance                      *IMModify(InstanceModifier *);

   bool                           IMPutSlotCLIPSInteger(InstanceModifier *,const char *,CLIPSInteger *);
   bool                           IMPutSlotInt(InstanceModifier *,const char *,int);
   bool                           IMPutSlotLong(InstanceModifier *,const char *,long);
   bool                           IMPutSlotLongLong(InstanceModifier *,const char *,long long);

   bool                           IMPutSlotCLIPSFloat(InstanceModifier *,const char *,CLIPSFloat *);
   bool                           IMPutSlotFloat(InstanceModifier *,const char *,float);
   bool                           IMPutSlotDouble(InstanceModifier *,const char *,double);

   bool                           IMPutSlotCLIPSLexeme(InstanceModifier *,const char *,CLIPSLexeme *);
   bool                           IMPutSlotSymbol(InstanceModifier *,const char *,const char *);
   bool                           IMPutSlotString(InstanceModifier *,const char *,const char *);
   bool                           IMPutSlotInstanceName(InstanceModifier *,const char *,const char *);

   bool                           IMPutSlotFact(InstanceModifier *,const char *,Fact *);
   bool                           IMPutSlotInstance(InstanceModifier *,const char *,Instance *);
   bool                           IMPutSlotExternalAddress(InstanceModifier *,const char *,CLIPSExternalAddress *);
   bool                           IMPutSlotMultifield(InstanceModifier *,const char *,Multifield *);

#endif /* _H_insmngr */







