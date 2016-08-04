   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.40  07/30/16          */
   /*                                                     */
   /*                OBJECT SYSTEM DEFINITIONS            */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Brian L. Dantes                                      */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*      6.40: Pragma once and other inclusion changes.       */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*************************************************************/

#ifndef _H_object

#pragma once

#define _H_object

typedef struct defclassModule DEFCLASS_MODULE;
typedef struct defclass Defclass;
typedef struct packedClassLinks PACKED_CLASS_LINKS;
typedef struct classLink CLASS_LINK;
typedef struct slotName SLOT_NAME;
typedef struct slotDescriptor SlotDescriptor;
typedef struct defmessageHandler DefmessageHandler;
typedef struct instance Instance;

typedef struct instanceSlot INSTANCE_SLOT;

/* Maximum # of simultaneous class hierarchy traversals
   should be a multiple of BITS_PER_BYTE and less than MAX_INT      */

#define MAX_TRAVERSALS  256
#define TRAVERSAL_BYTES 32       /* (MAX_TRAVERSALS / BITS_PER_BYTE) */

#define VALUE_REQUIRED     0
#define VALUE_PROHIBITED   1
#define VALUE_NOT_REQUIRED 2

#ifndef _H_constrct
#include "constrct.h"
#endif
#ifndef _H_constrnt
#include "constrnt.h"
#endif
#ifndef _H_moduldef
#include "moduldef.h"
#endif
#ifndef _H_evaluatn
#include "evaluatn.h"
#endif
#ifndef _H_expressn
#include "expressn.h"
#endif
#ifndef _H_multifld
#include "multifld.h"
#endif
#ifndef _H_symbol
#include "symbol.h"
#endif

#ifndef _H_match
#include "match.h"
#endif
#ifndef _H_pattern
#include "pattern.h"
#endif

#define GetInstanceSlotLength(sp) GetMFLength(sp->value)

struct packedClassLinks
  {
   long classCount;
   Defclass **classArray;
  };

struct defclassModule
  {
   struct defmoduleItemHeader header;
  };

struct defclass
  {
   struct constructHeader header;
   unsigned installed      : 1;
   unsigned system         : 1;
   unsigned abstract       : 1;
   unsigned reactive       : 1;
   unsigned traceInstances : 1;
   unsigned traceSlots     : 1;
   unsigned id;
   unsigned busy,
            hashTableIndex;
   PACKED_CLASS_LINKS directSuperclasses,
                      directSubclasses,
                      allSuperclasses;
   SlotDescriptor *slots,
             **instanceTemplate;
   unsigned *slotNameMap;
   short slotCount;
   short localInstanceSlotCount;
   short instanceSlotCount;
   short maxSlotNameID;
   Instance *instanceList,
                 *instanceListBottom;
   DefmessageHandler *handlers;
   unsigned *handlerOrderMap;
   short handlerCount;
   Defclass *nxtHash;
   BITMAP_HN *scopeMap;
   char traversalRecord[TRAVERSAL_BYTES];
  };

struct classLink
  {
   Defclass *cls;
   struct classLink *nxt;
  };

struct slotName
  {
   unsigned hashTableIndex,
            use;
   short id;
   SYMBOL_HN *name,
             *putHandlerName;
   struct slotName *nxt;
   long bsaveIndex;
  };

struct instanceSlot
  {
   SlotDescriptor *desc;
   unsigned valueRequired : 1;
   unsigned override      : 1;
   unsigned short type;
   void *value;
  };

struct slotDescriptor
  {
   unsigned shared                   : 1;
   unsigned multiple                 : 1;
   unsigned composite                : 1;
   unsigned noInherit                : 1;
   unsigned noWrite                  : 1;
   unsigned initializeOnly           : 1;
   unsigned dynamicDefault           : 1;
   unsigned defaultSpecified         : 1;
   unsigned noDefault                : 1;
   unsigned reactive                 : 1;
   unsigned publicVisibility         : 1;
   unsigned createReadAccessor       : 1;
   unsigned createWriteAccessor      : 1;
   unsigned overrideMessageSpecified : 1;
   Defclass *cls;
   SLOT_NAME *slotName;
   SYMBOL_HN *overrideMessage;
   void *defaultValue;
   CONSTRAINT_RECORD *constraint;
   unsigned sharedCount;
   long bsaveIndex;
   INSTANCE_SLOT sharedValue;
  };

struct instance
  {
   struct patternEntity header;
   void *partialMatchList;
   INSTANCE_SLOT *basisSlots;
   unsigned installed            : 1;
   unsigned garbage              : 1;
   unsigned initSlotsCalled      : 1;
   unsigned initializeInProgress : 1;
   unsigned reteSynchronized     : 1;
   SYMBOL_HN *name;
   unsigned hashTableIndex;
   unsigned busy;
   Defclass *cls;
   Instance *prvClass,*nxtClass,
                 *prvHash,*nxtHash,
                 *prvList,*nxtList;
   INSTANCE_SLOT **slotAddresses,
                 *slots;
  };

struct defmessageHandler
  {
   unsigned system         : 1;
   unsigned type           : 2;
   unsigned mark           : 1;
   unsigned trace          : 1;
   unsigned busy;
   SYMBOL_HN *name;
   Defclass *cls;
   short minParams;
   short maxParams;
   short localVarCount;
   EXPRESSION *actions;
   char *ppForm;
   struct userData *usrData;
  };

#endif /* _H_object */





