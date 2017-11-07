   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  10/24/17            */
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
#include "inscom.h"

   void                           InitializeInstanceCommand(Environment *,UDFContext *,UDFValue *);
   void                           MakeInstanceCommand(Environment *,UDFContext *,UDFValue *);
   CLIPSLexeme                   *GetFullInstanceName(Environment *,Instance *);
   Instance                      *BuildInstance(Environment *,CLIPSLexeme *,Defclass *,bool);
   void                           InitSlotsCommand(Environment *,UDFContext *,UDFValue *);
   UnmakeInstanceError            QuashInstance(Environment *,Instance *);

#if DEFRULE_CONSTRUCT && OBJECT_SYSTEM
   void                           InactiveInitializeInstance(Environment *,UDFContext *,UDFValue *);
   void                           InactiveMakeInstance(Environment *,UDFContext *,UDFValue *);
#endif

   InstanceBuilder               *CreateInstanceBuilder(Environment *,const char *);
   PutSlotError                   IBPutSlot(InstanceBuilder *,const char *,CLIPSValue *);

   Instance                      *IBMake(InstanceBuilder *,const char *);

   void                           IBDispose(InstanceBuilder *);
   void                           IBAbort(InstanceBuilder *);
   InstanceBuilderError           IBSetDefclass(InstanceBuilder *,const char *);

   PutSlotError                   IBPutSlotCLIPSInteger(InstanceBuilder *,const char *,CLIPSInteger *);
   PutSlotError                   IBPutSlotInt(InstanceBuilder *,const char *,int);
   PutSlotError                   IBPutSlotLong(InstanceBuilder *,const char *,long);
   PutSlotError                   IBPutSlotLongLong(InstanceBuilder *,const char *,long long);

   PutSlotError                   IBPutSlotCLIPSFloat(InstanceBuilder *,const char *,CLIPSFloat *);
   PutSlotError                   IBPutSlotFloat(InstanceBuilder *,const char *,float);
   PutSlotError                   IBPutSlotDouble(InstanceBuilder *,const char *,double);

   PutSlotError                   IBPutSlotCLIPSLexeme(InstanceBuilder *,const char *,CLIPSLexeme *);
   PutSlotError                   IBPutSlotSymbol(InstanceBuilder *,const char *,const char *);
   PutSlotError                   IBPutSlotString(InstanceBuilder *,const char *,const char *);
   PutSlotError                   IBPutSlotInstanceName(InstanceBuilder *,const char *,const char *);

   PutSlotError                   IBPutSlotFact(InstanceBuilder *,const char *,Fact *);
   PutSlotError                   IBPutSlotInstance(InstanceBuilder *,const char *,Instance *);
   PutSlotError                   IBPutSlotExternalAddress(InstanceBuilder *,const char *,CLIPSExternalAddress *);
   PutSlotError                   IBPutSlotMultifield(InstanceBuilder *,const char *,Multifield *);
   InstanceBuilderError           IBError(Environment *);

   InstanceModifier              *CreateInstanceModifier(Environment *,Instance *);
   PutSlotError                   IMPutSlot(InstanceModifier *,const char *,CLIPSValue *);
   void                           IMDispose(InstanceModifier *);
   void                           IMAbort(InstanceModifier *);
   InstanceModifierError          IMSetInstance(InstanceModifier *,Instance *);
   Instance                      *IMModify(InstanceModifier *);

   PutSlotError                   IMPutSlotCLIPSInteger(InstanceModifier *,const char *,CLIPSInteger *);
   PutSlotError                   IMPutSlotInt(InstanceModifier *,const char *,int);
   PutSlotError                   IMPutSlotLong(InstanceModifier *,const char *,long);
   PutSlotError                   IMPutSlotLongLong(InstanceModifier *,const char *,long long);

   PutSlotError                   IMPutSlotCLIPSFloat(InstanceModifier *,const char *,CLIPSFloat *);
   PutSlotError                   IMPutSlotFloat(InstanceModifier *,const char *,float);
   PutSlotError                   IMPutSlotDouble(InstanceModifier *,const char *,double);

   PutSlotError                   IMPutSlotCLIPSLexeme(InstanceModifier *,const char *,CLIPSLexeme *);
   PutSlotError                   IMPutSlotSymbol(InstanceModifier *,const char *,const char *);
   PutSlotError                   IMPutSlotString(InstanceModifier *,const char *,const char *);
   PutSlotError                   IMPutSlotInstanceName(InstanceModifier *,const char *,const char *);

   PutSlotError                   IMPutSlotFact(InstanceModifier *,const char *,Fact *);
   PutSlotError                   IMPutSlotInstance(InstanceModifier *,const char *,Instance *);
   PutSlotError                   IMPutSlotExternalAddress(InstanceModifier *,const char *,CLIPSExternalAddress *);
   PutSlotError                   IMPutSlotMultifield(InstanceModifier *,const char *,Multifield *);
   InstanceModifierError          IMError(Environment *);

#endif /* _H_insmngr */







