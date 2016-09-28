   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.40  08/25/16             */
   /*                                                     */
   /*                  MULTIFIELD MODULE                  */
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
/*            Corrected code to remove compiler warnings.    */
/*                                                           */
/*            Moved ImplodeMultifield from multifun.c.       */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Used DataObjectToString instead of             */
/*            ValueToString in implode$ to handle            */
/*            print representation of external addresses.    */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Converted API macros to function calls.        */
/*                                                           */
/*            Fixed issue with StoreInMultifield when        */
/*            asserting void values in implied deftemplate   */
/*            facts.                                         */
/*                                                           */
/*      6.40: Refactored code to reduce header dependencies  */
/*            in sysdep.c.                                   */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            ALLOW_ENVIRONMENT_GLOBALS no longer supported. */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*************************************************************/

#include <stdio.h>

#include "setup.h"

#include "constant.h"
#include "envrnmnt.h"
#include "evaluatn.h"
#include "memalloc.h"
#if OBJECT_SYSTEM
#include "object.h"
#endif
#include "scanner.h"
#include "router.h"
#include "strngrtr.h"
#include "utility.h"

#include "multifld.h"

/**********************/
/* CreateMultifield2: */
/**********************/
Multifield *CreateMultifield2(
  Environment *theEnv,
  long size)
  {
   Multifield *theSegment;
   long newSize = size;

   if (size <= 0) newSize = 1;

   theSegment = get_var_struct(theEnv,multifield,(long) sizeof(struct field) * (newSize - 1L));

   theSegment->th.type = MULTIFIELD;
   theSegment->multifieldLength = size;
   theSegment->busyCount = 0;
   theSegment->next = NULL;

   return theSegment;
  }

/*********************/
/* ReturnMultifield: */
/*********************/
void ReturnMultifield(
  Environment *theEnv,
  Multifield *theSegment)
  {
   unsigned long newSize;

   if (theSegment == NULL) return;

   if (theSegment->multifieldLength == 0) newSize = 1;
   else newSize = theSegment->multifieldLength;

   rtn_var_struct(theEnv,multifield,sizeof(struct field) * (newSize - 1),theSegment);
  }

/**********************/
/* MultifieldInstall: */
/**********************/
void MultifieldInstall(
  Environment *theEnv,
  struct multifield *theSegment)
  {
   unsigned long length, i;
   struct field *theFields;

   if (theSegment == NULL) return;

   length = theSegment->multifieldLength;

   theSegment->busyCount++;
   theFields = theSegment->theFields;

   for (i = 0 ; i < length ; i++)
     { AtomInstall(theEnv,theFields[i].header->type,theFields[i].value); }
  }

/************************/
/* MultifieldDeinstall: */
/************************/
void MultifieldDeinstall(
  Environment *theEnv,
  struct multifield *theSegment)
  {
   unsigned long length, i;
   struct field *theFields;

   if (theSegment == NULL) return;

   length = theSegment->multifieldLength;
   theSegment->busyCount--;
   theFields = theSegment->theFields;

   for (i = 0 ; i < length ; i++)
     { AtomDeinstall(theEnv,theFields[i].header->type,theFields[i].value); }
  }

/************************/
/* CVMultifieldInstall: */
/************************/
void CVMultifieldInstall(
  Environment *theEnv,
  struct multifield *theSegment)
  {
   unsigned long length, i;
   struct field *theFields;

   if (theSegment == NULL) return;

   length = theSegment->multifieldLength;

   theSegment->busyCount++;
   theFields = theSegment->theFields;

   for (i = 0 ; i < length ; i++)
     { CVAtomInstall(theEnv,theFields[i].value); }
  }

/**************************/
/* CVMultifieldDeinstall: */
/**************************/
void CVMultifieldDeinstall(
  Environment *theEnv,
  struct multifield *theSegment)
  {
   unsigned long length, i;
   struct field *theFields;

   if (theSegment == NULL) return;

   length = theSegment->multifieldLength;
   theSegment->busyCount--;
   theFields = theSegment->theFields;

   for (i = 0 ; i < length ; i++)
     { CVAtomDeinstall(theEnv,theFields[i].value); }
  }

/*******************************************************/
/* StringToMultifield: Returns a multifield structure  */
/*    that represents the string sent as the argument. */
/*******************************************************/
struct multifield *StringToMultifield(
  Environment *theEnv,
  const char *theString)
  {
   struct token theToken;
   Multifield *theSegment;
   struct field *theFields;
   unsigned long numberOfFields = 0;
   struct expr *topAtom = NULL, *lastAtom = NULL, *theAtom;

   /*====================================================*/
   /* Open the string as an input source and read in the */
   /* list of values to be stored in the multifield.     */
   /*====================================================*/

   OpenStringSource(theEnv,"multifield-str",theString,0);

   GetToken(theEnv,"multifield-str",&theToken);
   while (theToken.tknType != STOP_TOKEN)
     {
      if ((theToken.tknType == SYMBOL_TOKEN) || (theToken.tknType == STRING_TOKEN) ||
          (theToken.tknType == FLOAT_TOKEN) || (theToken.tknType == INTEGER_TOKEN) ||
          (theToken.tknType == INSTANCE_NAME_TOKEN))
        { theAtom = GenConstant(theEnv,TokenTypeToType(theToken.tknType),theToken.value); }
      else
        { theAtom = GenConstant(theEnv,STRING,EnvCreateString(theEnv,theToken.printForm)); }

      numberOfFields++;
      if (topAtom == NULL) topAtom = theAtom;
      else lastAtom->nextArg = theAtom;

      lastAtom = theAtom;
      GetToken(theEnv,"multifield-str",&theToken);
     }

   CloseStringSource(theEnv,"multifield-str");

   /*====================================================================*/
   /* Create a multifield of the appropriate size for the values parsed. */
   /*====================================================================*/

   theSegment = EnvCreateMultifield(theEnv,numberOfFields);
   theFields = theSegment->theFields;

   /*====================================*/
   /* Copy the values to the multifield. */
   /*====================================*/

   theAtom = topAtom;
   numberOfFields = 0;
   while (theAtom != NULL)
     {
      theFields[numberOfFields].value = theAtom->value;
      numberOfFields++;
      theAtom = theAtom->nextArg;
     }

   /*===========================*/
   /* Return the parsed values. */
   /*===========================*/

   ReturnExpression(theEnv,topAtom);

   /*============================*/
   /* Return the new multifield. */
   /*============================*/

   return(theSegment);
  }

/**************************************************************/
/* EnvCreateMultifield: Creates a multifield of the specified */
/*   size and adds it to the list of segments.                */
/**************************************************************/
Multifield *EnvCreateMultifield(
  Environment *theEnv,
  long size)
  {
   Multifield *theSegment;
   long newSize;

   if (size <= 0) newSize = 1;
   else newSize = size;

   theSegment = get_var_struct(theEnv,multifield,(long) sizeof(struct field) * (newSize - 1L));

   theSegment->th.type = MULTIFIELD;
   theSegment->multifieldLength = size;
   theSegment->busyCount = 0;
   theSegment->next = NULL;

   theSegment->next = UtilityData(theEnv)->CurrentGarbageFrame->ListOfMultifields;
   UtilityData(theEnv)->CurrentGarbageFrame->ListOfMultifields = theSegment;
   UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
   if (UtilityData(theEnv)->CurrentGarbageFrame->LastMultifield == NULL)
     { UtilityData(theEnv)->CurrentGarbageFrame->LastMultifield = theSegment; }

   return theSegment;
  }

/*******************/
/* DOToMultifield: */
/*******************/
Multifield *DOToMultifield(
  Environment *theEnv,
  CLIPSValue *theValue)
  {
   Multifield *dst, *src;

   if (theValue->header->type != MULTIFIELD) return NULL;

   dst = CreateMultifield2(theEnv,(unsigned long) GetpDOLength(theValue));

   src = (Multifield *) theValue->value;
   GenCopyMemory(struct field,dst->multifieldLength,
              &(dst->theFields[0]),&(src->theFields[theValue->begin]));

   return dst;
  }

/************************/
/* AddToMultifieldList: */
/************************/
void AddToMultifieldList(
  Environment *theEnv,
  struct multifield *theSegment)
  {
   theSegment->next = UtilityData(theEnv)->CurrentGarbageFrame->ListOfMultifields;
   UtilityData(theEnv)->CurrentGarbageFrame->ListOfMultifields = theSegment;
   UtilityData(theEnv)->CurrentGarbageFrame->dirty = true;
   if (UtilityData(theEnv)->CurrentGarbageFrame->LastMultifield == NULL)
     { UtilityData(theEnv)->CurrentGarbageFrame->LastMultifield = theSegment; }
  }

/*********************/
/* FlushMultifields: */
/*********************/
void FlushMultifields(
  Environment *theEnv)
  {
   struct multifield *theSegment, *nextPtr, *lastPtr = NULL;
   unsigned long newSize;

   theSegment = UtilityData(theEnv)->CurrentGarbageFrame->ListOfMultifields;
   while (theSegment != NULL)
     {
      nextPtr = theSegment->next;
      if (theSegment->busyCount == 0)
        {
         if (theSegment->multifieldLength == 0) newSize = 1;
         else newSize = theSegment->multifieldLength;
         rtn_var_struct(theEnv,multifield,sizeof(struct field) * (newSize - 1),theSegment);
         if (lastPtr == NULL) UtilityData(theEnv)->CurrentGarbageFrame->ListOfMultifields = nextPtr;
         else lastPtr->next = nextPtr;

         /*=================================================*/
         /* If the multifield deleted was the last in the   */
         /* list, update the pointer to the last multifield */
         /* to the prior multifield.                        */
         /*=================================================*/

         if (nextPtr == NULL)
           { UtilityData(theEnv)->CurrentGarbageFrame->LastMultifield = lastPtr; }
        }
      else
        { lastPtr = theSegment; }

      theSegment = nextPtr;
     }
  }

/************************************************************************/
/* DuplicateMultifield: Allocates a new segment and copies results from */
/*   old value to new. This value is not put on the ListOfMultifields.  */
/************************************************************************/
void DuplicateMultifield(
  Environment *theEnv,
  CLIPSValue *dst,
  CLIPSValue *src)
  {
   dst->begin = 0;
   dst->end = src->end - src->begin;
   dst->value = CreateMultifield2(theEnv,(unsigned long) dst->end + 1);
   GenCopyMemory(struct field,dst->end + 1,&((Multifield *) dst->value)->theFields[0],
                                        &((Multifield *) src->value)->theFields[src->begin]);
  }

/*******************/
/* CopyMultifield: */
/*******************/
Multifield *CopyMultifield(
  Environment *theEnv,
  Multifield *src)
  {
   Multifield *dst;

   dst = CreateMultifield2(theEnv,src->multifieldLength);
   GenCopyMemory(struct field,src->multifieldLength,&(dst->theFields[0]),&(src->theFields[0]));
   return dst;
  }

/**********************************************************/
/* EphemerateMultifield: Marks the values of a multifield */
/*   as ephemeral if they have not already been marker.   */
/**********************************************************/
void EphemerateMultifield(
  Environment *theEnv,
  struct multifield *theSegment)
  {
   unsigned long length, i;
   struct field *theFields;

   if (theSegment == NULL) return;

   length = theSegment->multifieldLength;

   theFields = theSegment->theFields;

   for (i = 0 ; i < length ; i++)
     { EphemerateValue(theEnv,theFields[i].value); }
  }

/*********************************************/
/* PrintMultifield: Prints out a multifield. */
/*********************************************/
void PrintMultifield(
  Environment *theEnv,
  const char *fileid,
  struct multifield *segment,
  long begin,
  long end,
  bool printParens)
  {
   struct field *theMultifield;
   int i;

   theMultifield = segment->theFields;
   if (printParens)
     EnvPrintRouter(theEnv,fileid,"(");
   i = begin;
   while (i <= end)
     {
      PrintAtom(theEnv,fileid,theMultifield[i].header->type,theMultifield[i].value);
      i++;
      if (i <= end) EnvPrintRouter(theEnv,fileid," ");
     }
   if (printParens)
     EnvPrintRouter(theEnv,fileid,")");
  }

/****************************************************/
/* StoreInMultifield: Append function for segments. */
/****************************************************/
void StoreInMultifield(
  Environment *theEnv,
  CLIPSValue *returnValue,
  EXPRESSION *expptr,
  bool garbageSegment)
  {
   CLIPSValue val_ptr;
   CLIPSValue *val_arr;
   Multifield *theMultifield;
   Multifield *orig_ptr;
   long start, end, i,j, k, argCount;
   unsigned long seg_size;

   argCount = CountArguments(expptr);

   /*=========================================*/
   /* If no arguments are given return a NULL */
   /* multifield of length zero.              */
   /*=========================================*/

   if (argCount == 0)
     {
      returnValue->begin = 0;
      returnValue->end = -1;
      if (garbageSegment) theMultifield = EnvCreateMultifield(theEnv,0L);
      else theMultifield = CreateMultifield2(theEnv,0L);
      returnValue->value = theMultifield;
      return;
     }
   else
     {
      /*========================================*/
      /* Get a new segment with length equal to */
      /* the total length of all the arguments. */
      /*========================================*/

      val_arr = (CLIPSValue *) gm3(theEnv,(long) sizeof(CLIPSValue) * argCount);
      seg_size = 0;

      for (i = 1; i <= argCount; i++, expptr = expptr->nextArg)
        {
         EvaluateExpression(theEnv,expptr,&val_ptr);
         if (EvaluationData(theEnv)->EvaluationError)
           {
            returnValue->begin = 0;
            returnValue->end = -1;
            if (garbageSegment)
              { theMultifield = EnvCreateMultifield(theEnv,0L); }
            else theMultifield = CreateMultifield2(theEnv,0L);
            returnValue->value = theMultifield;
            rm3(theEnv,val_arr,(long) sizeof(CLIPSValue) * argCount);
            return;
           }
         if (val_ptr.header->type == MULTIFIELD)
           {
            (val_arr+i-1)->value = val_ptr.value;
            start = val_ptr.begin;
            end = val_ptr.end;
           }
         else if (val_ptr.header->type == RVOID)
           {
            (val_arr+i-1)->value = val_ptr.value;
            start = 0;
            end = -1;
           }
         else
           {
            (val_arr+i-1)->value = val_ptr.value;
            start = end = -1;
           }

         seg_size += (unsigned long) (end - start + 1);
         (val_arr+i-1)->begin = start;
         (val_arr+i-1)->end = end;
        }

      if (garbageSegment)
        { theMultifield = EnvCreateMultifield(theEnv,seg_size); }
      else theMultifield = CreateMultifield2(theEnv,seg_size);

      /*========================================*/
      /* Copy each argument into new segment.  */
      /*========================================*/

      for (k = 0, j = 0; k < argCount; k++)
        {
         if ((val_arr+k)->header->type == MULTIFIELD)
           {
            start = (val_arr+k)->begin;
            end = (val_arr+k)->end;
            orig_ptr = (Multifield *) (val_arr+k)->value;
            for (i = start; i < end + 1; i++, j++)
              {
               theMultifield->theFields[j].value = orig_ptr->theFields[i].value;
              }
           }
         else if ((val_arr+k)->header->type != RVOID)
           {
            theMultifield->theFields[j].value = (val_arr+k)->value;
            j++;
           }
        }

      /*=========================*/
      /* Return the new segment. */
      /*=========================*/

      returnValue->begin = 0;
      returnValue->end = ((long) seg_size) - 1;
      returnValue->value = theMultifield;
      rm3(theEnv,val_arr,(long) sizeof(CLIPSValue) * argCount);
      return;
     }
  }

/*************************************************************/
/* MultifieldDOsEqual: determines if two segments are equal. */
/*************************************************************/
bool MultifieldDOsEqual(
  CLIPSValue *dobj1,
  CLIPSValue *dobj2)
  {
   long extent1,extent2; /* 6.04 Bug Fix */
   FIELD_PTR e1,e2;

   extent1 = GetpDOLength(dobj1);
   extent2 = GetpDOLength(dobj2);
   if (extent1 != extent2)
     { return false; }

   e1 = &((Multifield *) dobj1->value)->theFields[dobj1->begin];
   e2 = &((Multifield *) dobj2->value)->theFields[dobj2->begin];

   while (extent1 != 0)
     {
      if (e1->value != e2->value)
        { return false; }

      extent1--;

      if (extent1 > 0)
        {
         e1++;
         e2++;
        }
     }
   return true;
  }

/******************************************************************/
/* MultifieldsEqual: Determines if two multifields are identical. */
/******************************************************************/
bool MultifieldsEqual(
  struct multifield *segment1,
  struct multifield *segment2)
  {
   struct field *elem1;
   struct field *elem2;
   long length, i = 0;

   length = segment1->multifieldLength;
   if (length != segment2->multifieldLength)
     { return false; }

   elem1 = segment1->theFields;
   elem2 = segment2->theFields;

   /*==================================================*/
   /* Compare each field of both facts until the facts */
   /* match completely or the facts mismatch.          */
   /*==================================================*/

   while (i < length)
     {
      if (elem1[i].header->type == MULTIFIELD)
        {
         if (MultifieldsEqual((Multifield *) elem1[i].value,
                              (Multifield *) elem2[i].value) == false)
          { return false; }
        }
      else if (elem1[i].value != elem2[i].value)
        { return false; }

      i++;
     }
   return true;
  }

/************************************************************/
/* HashMultifield: Returns the hash value for a multifield. */
/************************************************************/
unsigned long HashMultifield(
  struct multifield *theSegment,
  unsigned long theRange)
  {
   unsigned long length, i;
   unsigned long tvalue;
   unsigned long count;
   struct field *fieldPtr;
   union
     {
      double fv;
      void *vv;
      unsigned long liv;
     } fis;

   /*================================================*/
   /* Initialize variables for computing hash value. */
   /*================================================*/

   count = 0;
   length = theSegment->multifieldLength;
   fieldPtr = theSegment->theFields;

   /*====================================================*/
   /* Loop through each value in the multifield, compute */
   /* its hash value, and add it to the running total.   */
   /*====================================================*/

   for (i = 0;
        i < length;
        i++)
     {
      switch(fieldPtr[i].header->type)
         {
          case MULTIFIELD:
            count += HashMultifield((Multifield *) fieldPtr[i].value,theRange);
            break;

          case FLOAT:
            fis.liv = 0;
            fis.fv = fieldPtr[i].floatValue->contents;
            count += (fis.liv * (i + 29))  +
                     (unsigned long) fieldPtr[i].floatValue->contents;
            break;

          case INTEGER:
            count += (((unsigned long) fieldPtr[i].integerValue->contents) * (i + 29)) +
                      ((unsigned long) fieldPtr[i].integerValue->contents);
            break;

          case FACT_ADDRESS:
#if OBJECT_SYSTEM
          case INSTANCE_ADDRESS:
#endif
            fis.liv = 0;
            fis.vv = fieldPtr[i].value;
            count += (unsigned long) (fis.liv * (i + 29));
            break;

          case EXTERNAL_ADDRESS:
            fis.liv = 0;
            fis.vv = ValueToExternalAddress(fieldPtr[i].value);
            count += (unsigned long) (fis.liv * (i + 29));
            break;

          case SYMBOL:
          case STRING:
#if OBJECT_SYSTEM
          case INSTANCE_NAME:
#endif
            tvalue = (unsigned long) HashSymbol(fieldPtr[i].lexemeValue->contents,theRange);
            count += (unsigned long) (tvalue * (i + 29));
            break;
         }
     }

   /*========================*/
   /* Return the hash value. */
   /*========================*/

   return(count);
  }

/**********************/
/* GetMultifieldList: */
/**********************/
Multifield *GetMultifieldList(
  Environment *theEnv)
  {
   return(UtilityData(theEnv)->CurrentGarbageFrame->ListOfMultifields);
  }

/***************************************/
/* ImplodeMultifield: C access routine */
/*   for the implode$ function.        */
/***************************************/
CLIPSLexeme *ImplodeMultifield(
  Environment *theEnv,
  CLIPSValue *value)
  {
   size_t strsize = 0;
   long i, j;
   const char *tmp_str;
   char *ret_str;
   CLIPSLexeme *rv;
   struct multifield *theMultifield;
   CLIPSValue tempDO;

   /*===================================================*/
   /* Determine the size of the string to be allocated. */
   /*===================================================*/

   theMultifield = (Multifield *) value->value;
   for (i = value->begin ; i <= value->end ; i++)
     {
      if (theMultifield->theFields[i].header->type == FLOAT)
        {
         tmp_str = FloatToString(theEnv,theMultifield->theFields[i].floatValue->contents);
         strsize += strlen(tmp_str) + 1;
        }
      else if (theMultifield->theFields[i].header->type == INTEGER)
        {
         tmp_str = LongIntegerToString(theEnv,theMultifield->theFields[i].integerValue->contents);
         strsize += strlen(tmp_str) + 1;
        }
      else if (theMultifield->theFields[i].header->type == STRING)
        {
         strsize += strlen(theMultifield->theFields[i].lexemeValue->contents) + 3;
         tmp_str = theMultifield->theFields[i].lexemeValue->contents;
         while(*tmp_str)
           {
            if (*tmp_str == '"')
              { strsize++; }
            else if (*tmp_str == '\\') /* GDR 111599 #835 */
              { strsize++; }           /* GDR 111599 #835 */
            tmp_str++;
           }
        }
#if OBJECT_SYSTEM
      else if (theMultifield->theFields[i].header->type == INSTANCE_NAME)
        { strsize += strlen(theMultifield->theFields[i].lexemeValue->contents) + 3; }
      else if (theMultifield->theFields[i].header->type == INSTANCE_ADDRESS)
        {
         strsize += strlen(theMultifield->theFields[i].instanceValue->name->contents) + 3;
        }
#endif

      else
        {
         tempDO.value = theMultifield->theFields[i].value;
         strsize += strlen(DataObjectToString(theEnv,&tempDO)) + 1;
        }
     }

   /*=============================================*/
   /* Allocate the string and copy all components */
   /* of the MULTIFIELD variable to it.           */
   /*=============================================*/

   if (strsize == 0) return(EnvCreateString(theEnv,""));
   ret_str = (char *) gm2(theEnv,strsize);
   for(j = 0, i = value->begin ; i <= value->end ; i++)
     {
      /*============================*/
      /* Convert numbers to strings */
      /*============================*/

      if (theMultifield->theFields[i].header->type == FLOAT)
        {
         tmp_str = FloatToString(theEnv,theMultifield->theFields[i].floatValue->contents);
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
        }
      else if (theMultifield->theFields[i].header->type == INTEGER)
        {
         tmp_str = LongIntegerToString(theEnv,theMultifield->theFields[i].integerValue->contents);
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
        }

      /*=======================================*/
      /* Enclose strings in quotes and preceed */
      /* imbedded quotes with a backslash      */
      /*=======================================*/

      else if (theMultifield->theFields[i].header->type == STRING)
        {
         tmp_str = theMultifield->theFields[i].lexemeValue->contents;
         *(ret_str+j) = '"';
         j++;
         while(*tmp_str)
           {
            if (*tmp_str == '"')
              {
               *(ret_str+j) = '\\';
               j++;
              }
            else if (*tmp_str == '\\') /* GDR 111599 #835 */
              {                        /* GDR 111599 #835 */
               *(ret_str+j) = '\\';    /* GDR 111599 #835 */
               j++;                    /* GDR 111599 #835 */
              }                        /* GDR 111599 #835 */

            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str+j) = '"';
         j++;
        }
#if OBJECT_SYSTEM
      else if (theMultifield->theFields[i].header->type == INSTANCE_NAME)
        {
         tmp_str = theMultifield->theFields[i].lexemeValue->contents;
         *(ret_str + j++) = '[';
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str + j++) = ']';
        }
      else if (theMultifield->theFields[i].header->type == INSTANCE_ADDRESS)
        {
         tmp_str = theMultifield->theFields[i].instanceValue->name->contents;
         *(ret_str + j++) = '[';
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         *(ret_str + j++) = ']';
        }
#endif
      else
        {
         tempDO.value = theMultifield->theFields[i].value;
         tmp_str = DataObjectToString(theEnv,&tempDO);
         while(*tmp_str)
           {
            *(ret_str+j) = *tmp_str;
            j++, tmp_str++;
           }
         }
      *(ret_str+j) = ' ';
      j++;
     }
   *(ret_str+j-1) = '\0';

   /*====================*/
   /* Return the string. */
   /*====================*/

   rv = EnvCreateString(theEnv,ret_str);
   rm(theEnv,ret_str,strsize);
   return rv;
  }

