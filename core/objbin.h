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
/*      6.24: Removed IMPERATIVE_MESSAGE_HANDLERS and        */
/*            AUXILIARY_MESSAGE_HANDLERS compilation flags.  */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_objbin

#pragma once

#define _H_objbin

#include "object.h"

#define OBJECTBIN_DATA 33

struct objectBinaryData
  {
   Defclass *DefclassArray;
   long ModuleCount;
   long ClassCount;
   long LinkCount;
   long SlotCount;
   long SlotNameCount;
   long TemplateSlotCount;
   long SlotNameMapCount;
   long HandlerCount;
   DEFCLASS_MODULE *ModuleArray;
   Defclass **LinkArray;
   SlotDescriptor *SlotArray;
   SlotDescriptor **TmpslotArray;
   SLOT_NAME *SlotNameArray;
   unsigned *MapslotArray;
   DefmessageHandler *HandlerArray;
   unsigned *MaphandlerArray;
  };

#define ObjectBinaryData(theEnv) ((struct objectBinaryData *) GetEnvironmentData(theEnv,OBJECTBIN_DATA))

#define DefclassPointer(i) (((i) == -1L) ? NULL : &ObjectBinaryData(theEnv)->DefclassArray[i])
#define DefclassIndex(cls) (((cls) == NULL) ? -1 : ((ConstructHeader *) cls)->bsaveID)

   void                    SetupObjectsBload(Environment *);
   void                   *BloadDefclassModuleReference(Environment *,int);

#endif /* _H_objbin */



