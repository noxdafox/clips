   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*            CLIPS Version 6.50  10/18/16             */
   /*                                                     */
   /*             MULTIFIELD_TYPE FUNCTIONS MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for several multifield         */
/*   functions including first$, rest$, subseq$, delete$,    */
/*   delete-member$, replace-member$, replace$, insert$,     */
/*   explode$, implode$, nth$, member$, subsetp and progn$.  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*      Brian Dantes                                         */
/*      Barry Cameron                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*            Changed name of variable exp to theExp         */
/*            because of Unix compiler warnings of shadowed  */
/*            definitions.                                   */
/*                                                           */
/*      6.24: Renamed BOOLEAN macro type to intBool.         */
/*                                                           */
/*            Moved ImplodeMultifield to multifld.c.         */
/*                                                           */
/*      6.30: Changed integer type/precision.                */
/*                                                           */
/*            Support for long long integers.                */
/*                                                           */
/*            Changed garbage collection algorithm.          */
/*                                                           */
/*            Fixed memory leaks when error occurred.        */
/*                                                           */
/*            Added const qualifiers to remove C++           */
/*            deprecation warnings.                          */
/*                                                           */
/*            Fixed linkage issue when DEFMODULE_CONSTRUCT   */
/*            compiler flag is set to 0.                     */
/*                                                           */
/*      6.40: Added Env prefix to GetEvaluationError and     */
/*            SetEvaluationError functions.                  */
/*                                                           */
/*            Added Env prefix to GetHaltExecution and       */
/*            SetHaltExecution functions.                    */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*            Added support for booleans with <stdbool.h>.   */
/*                                                           */
/*            Removed use of void pointers for specific      */
/*            data structures.                               */
/*                                                           */
/*            Removed member, mv-replace, mv-subseq,         */
/*            mv-delete, str-implode, str-explode, subset,   */
/*            and nth functions.                             */
/*                                                           */
/*            UDF redesign.                                  */
/*                                                           */
/*            Added CLIPSBlockStart and CLIPSBlockEnd        */
/*            functions for garbage collection blocks.       */
/*                                                           */
/*            Eval support for run time and bload only.      */
/*                                                           */
/*      6.50: Fact ?var:slot references in progn$/foreach.   */
/*                                                           */
/*************************************************************/

#include "setup.h"

#if MULTIFIELD_FUNCTIONS || OBJECT_SYSTEM

#include <stdio.h>
#include <string.h>

#include "argacces.h"
#include "envrnmnt.h"
#include "exprnpsr.h"
#include "memalloc.h"
#include "multifld.h"
#include "multifun.h"
#if OBJECT_SYSTEM
#include "object.h"
#endif
#include "pprint.h"
#include "prcdrpsr.h"
#include "prcdrfun.h"
#include "prntutil.h"
#include "router.h"
#if (! BLOAD_ONLY) && (! RUN_TIME)
#include "scanner.h"
#endif
#include "utility.h"

/**************/
/* STRUCTURES */
/**************/

typedef struct fieldVarStack
  {
   unsigned short type;
   void *value;
   long index;
   struct fieldVarStack *nxt;
  } FIELD_VAR_STACK;

/***************************************/
/* LOCAL INTERNAL FUNCTION DEFINITIONS */
/***************************************/

#if MULTIFIELD_FUNCTIONS
   static bool                    MVRangeCheck(long,long,long *,int);
   static void                    MultifieldPrognDriver(UDFContext *,UDFValue *,const char *);
#if (! BLOAD_ONLY)
   static struct expr            *MultifieldPrognParser(Environment *,struct expr *,const char *);
   static struct expr            *ForeachParser(Environment *,struct expr *,const char *);
   static void                    ReplaceMvPrognFieldVars(Environment *,CLIPSLexeme *,struct expr *,int);
#endif /* (! BLOAD_ONLY) && (! RUN_TIME) */
#endif /* MULTIFIELD_FUNCTIONS */
   static void                    MVRangeError(Environment *,long,long,long,const char *);
#endif /* MULTIFIELD_FUNCTIONS || OBJECT_SYSTEM */

/***************************************/
/* LOCAL INTERNAL VARIABLE DEFINITIONS */
/***************************************/

#if MULTIFIELD_FUNCTIONS

#define MULTIFUN_DATA 10

struct multiFunctionData
  {
   FIELD_VAR_STACK *FieldVarStack;
  };

#define MultiFunctionData(theEnv) ((struct multiFunctionData *) GetEnvironmentData(theEnv,MULTIFUN_DATA))

/**********************************************/
/* MultifieldFunctionDefinitions: Initializes */
/*   the multifield functions.                */
/**********************************************/
void MultifieldFunctionDefinitions(
  Environment *theEnv)
  {
   AllocateEnvironmentData(theEnv,MULTIFUN_DATA,sizeof(struct multiFunctionData),NULL);

#if ! RUN_TIME
   EnvAddUDF(theEnv,"first$","m",1,1,"m",FirstFunction,"FirstFunction",NULL);
   EnvAddUDF(theEnv,"rest$","m",1,1,"m",RestFunction,"RestFunction",NULL);
   EnvAddUDF(theEnv,"subseq$","m",3,3,"l;m",SubseqFunction,"SubseqFunction",NULL);
   EnvAddUDF(theEnv,"delete-member$","m",2,UNBOUNDED,"*;m",DeleteMemberFunction,"DeleteMemberFunction",NULL);
   EnvAddUDF(theEnv,"replace-member$","m",3,UNBOUNDED,"*;m",ReplaceMemberFunction,"ReplaceMemberFunction",NULL);
   EnvAddUDF(theEnv,"delete$","m",3,3,"l;m",DeleteFunction,"DeleteFunction",NULL);
   EnvAddUDF(theEnv,"replace$","m",4,UNBOUNDED,"*;m;l;l",ReplaceFunction,"ReplaceFunction",NULL);
   EnvAddUDF(theEnv,"insert$","m",3,UNBOUNDED,"*;m;l",InsertFunction,"InsertFunction",NULL);
   EnvAddUDF(theEnv,"explode$","m",1,1,"s",ExplodeFunction,"ExplodeFunction",NULL);
   EnvAddUDF(theEnv,"implode$","s",1,1,"m",ImplodeFunction,"ImplodeFunction",NULL);
   EnvAddUDF(theEnv,"nth$","synldife",2,2,";l;m",NthFunction,"NthFunction",NULL);
   EnvAddUDF(theEnv,"member$","blm",2,2,";*;m",MemberFunction,"MemberFunction",NULL);
   EnvAddUDF(theEnv,"subsetp","b",2,2,";m;m",SubsetpFunction,"SubsetpFunction",NULL);
   EnvAddUDF(theEnv,"progn$","*",0,UNBOUNDED,NULL,MultifieldPrognFunction,"MultifieldPrognFunction",NULL);
   EnvAddUDF(theEnv,"foreach","*",0,UNBOUNDED,NULL,ForeachFunction,"ForeachFunction",NULL);
   FuncSeqOvlFlags(theEnv,"progn$",false,false);
   FuncSeqOvlFlags(theEnv,"foreach",false,false);
   EnvAddUDF(theEnv,"(get-progn$-field)","*",0,0,NULL,GetMvPrognField,"GetMvPrognField",NULL);
   EnvAddUDF(theEnv,"(get-progn$-index)","l",0,0,NULL,GetMvPrognIndex,"GetMvPrognIndex",NULL);
#endif

#if ! BLOAD_ONLY
   AddFunctionParser(theEnv,"progn$",MultifieldPrognParser);
   AddFunctionParser(theEnv,"foreach",ForeachParser);
#endif
  }

/****************************************/
/* DeleteFunction: H/L access routine   */
/*   for the delete$ function.          */
/****************************************/
void DeleteFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue value1, value2, value3;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((! UDFFirstArgument(context,MULTIFIELD_BIT,&value1)) ||
       (! UDFNextArgument(context,INTEGER_BIT,&value2)) ||
       (! UDFNextArgument(context,INTEGER_BIT,&value3)))
     { return; }

   /*=================================================*/
   /* Delete the section out of the multifield value. */
   /*=================================================*/

   if (DeleteMultiValueField(theEnv,returnValue,&value1,
                             (long) value2.integerValue->contents,
                             (long) value3.integerValue->contents,"delete$") == false)/* TBD */
     {
      EnvSetEvaluationError(theEnv,true);
      EnvSetMultifieldErrorValue(theEnv,returnValue);
     }
  }

/*****************************************/
/* ReplaceFunction: H/L access routine   */
/*   for the replace$ function.          */
/*****************************************/
void ReplaceFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue value1, value2, value3, value4;
   Expression *fieldarg;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((! UDFFirstArgument(context,MULTIFIELD_BIT,&value1)) ||
       (! UDFNextArgument(context,INTEGER_BIT,&value2)) ||
       (! UDFNextArgument(context,INTEGER_BIT,&value3)))
     { return; }

   /*===============================*/
   /* Create the replacement value. */
   /*===============================*/

   fieldarg = GetFirstArgument()->nextArg->nextArg->nextArg;
   if (fieldarg->nextArg != NULL)
     { StoreInMultifield(theEnv,&value4,fieldarg,true); }
   else
     { EvaluateExpression(theEnv,fieldarg,&value4); }

   /*==============================================*/
   /* Replace the section in the multifield value. */
   /*==============================================*/

   if (ReplaceMultiValueField(theEnv,returnValue,&value1,(long) value2.integerValue->contents,
                   (long) value3.integerValue->contents,&value4,"replace$") == false) /* TBD */
     {
      EnvSetEvaluationError(theEnv,true);
      EnvSetMultifieldErrorValue(theEnv,returnValue);
     }
  }

/**********************************************/
/* DeleteMemberFunction: H/L access routine   */
/*   for the delete-member$ function.         */
/**********************************************/
void DeleteMemberFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue resultValue, *delVals, tmpVal;
   int i, argCnt;
   unsigned delSize;
   long j,k;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   argCnt = UDFArgumentCount(context);

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&resultValue))
     { return; }

   /*===================================================*/
   /* For every value specified, delete all occurrences */
   /* of those values from the multifield.              */
   /*===================================================*/

   delSize = (sizeof(UDFValue) * (argCnt-1));
   delVals = (UDFValue *) gm2(theEnv,delSize);
   for (i = 2 ; i <= argCnt ; i++)
     {
      if (! UDFNextArgument(context,ANY_TYPE_BITS,&delVals[i-2]))
        {
         rm(theEnv,delVals,delSize);
         return;
        }
     }

   while (FindDOsInSegment(delVals,argCnt-1,&resultValue,&j,&k,NULL,0))
     {
      if (DeleteMultiValueField(theEnv,&tmpVal,&resultValue,
                                j,k,"delete-member$") == false)
        {
         rm(theEnv,delVals,delSize);
         EnvSetEvaluationError(theEnv,true);
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         return;
        }
      GenCopyMemory(UDFValue,1,&resultValue,&tmpVal);
     }
   rm(theEnv,delVals,delSize);
   GenCopyMemory(UDFValue,1,returnValue,&resultValue);
  }

/***********************************************/
/* ReplaceMemberFunction: H/L access routine   */
/*   for the replace-member$ function.         */
/***********************************************/
void ReplaceMemberFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue resultValue,replVal,*delVals,tmpVal;
   int i,argCnt;
   unsigned delSize;
   long j,k,mink[2],*minkp;
   long replLen = 1L;

   /*============================================*/
   /* Check for the correct number of arguments. */
   /*============================================*/

   argCnt = UDFArgumentCount(context);

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&resultValue))
     { return; }

   if (! UDFNextArgument(context,ANY_TYPE_BITS,&replVal))
     { return; }
   if (replVal.header->type == MULTIFIELD_TYPE)
     replLen = replVal.range;

   /*======================================================*/
   /* For the value (or values from multifield) specified, */
   /* replace all occurrences of those values with all     */
   /* values specified.                                    */
   /*======================================================*/

   delSize = (sizeof(UDFValue) * (argCnt-2));
   delVals = (UDFValue *) gm2(theEnv,delSize);

   for (i = 3 ; i <= argCnt ; i++)
     {
      if (! UDFNthArgument(context,i,ANY_TYPE_BITS,&delVals[i-3]))
        {
         rm(theEnv,delVals,delSize);
         return;
        }
     }
   minkp = NULL;
   while (FindDOsInSegment(delVals,argCnt-2,&resultValue,&j,&k,minkp,minkp ? 1 : 0))
     {
      if (ReplaceMultiValueField(theEnv,&tmpVal,&resultValue,j,k,
                                 &replVal,"replace-member$") == false)
        {
         rm(theEnv,delVals,delSize);
         EnvSetEvaluationError(theEnv,true);
         EnvSetMultifieldErrorValue(theEnv,returnValue);
         return;
        }
      GenCopyMemory(UDFValue,1,&resultValue,&tmpVal);
      mink[0] = 1L;
      mink[1] = j + replLen - 1L;
      minkp = mink;
     }
   rm(theEnv,delVals,delSize);
   GenCopyMemory(UDFValue,1,returnValue,&resultValue);
  }

/****************************************/
/* InsertFunction: H/L access routine   */
/*   for the insert$ function.          */
/****************************************/
void InsertFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue value1, value2, value3;
   Expression *fieldarg;

   /*=======================================*/
   /* Check for the correct argument types. */
   /*=======================================*/

   if ((! UDFFirstArgument(context,MULTIFIELD_BIT,&value1)) ||
       (! UDFNextArgument(context,INTEGER_BIT,&value2)))
     { return; }

   /*=============================*/
   /* Create the insertion value. */
   /*=============================*/

   fieldarg = GetFirstArgument()->nextArg->nextArg;
   if (fieldarg->nextArg != NULL)
     StoreInMultifield(theEnv,&value3,fieldarg,true);
   else
     EvaluateExpression(theEnv,fieldarg,&value3);

   /*===========================================*/
   /* Insert the value in the multifield value. */
   /*===========================================*/

   if (InsertMultiValueField(theEnv,returnValue,&value1,(long) value2.integerValue->contents, /* TBD */
                             &value3,"insert$") == false)
     {
      EnvSetEvaluationError(theEnv,true);
      EnvSetMultifieldErrorValue(theEnv,returnValue);
     }
  }

/*****************************************/
/* ExplodeFunction: H/L access routine   */
/*   for the explode$ function.          */
/*****************************************/
void ExplodeFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue value;
   Multifield *theMultifield;
   unsigned long end;

   /*==================================*/
   /* The argument should be a string. */
   /*==================================*/

   if (! UDFFirstArgument(context,STRING_BIT,&value))
     { return; }

   /*=====================================*/
   /* Convert the string to a multifield. */
   /*=====================================*/

   theMultifield = StringToMultifield(theEnv,value.lexemeValue->contents);
   if (theMultifield == NULL)
     {
      theMultifield = EnvCreateMultifield(theEnv,0L);
      end = 0;
     }
   else
     { end = theMultifield->length; }

   /*========================*/
   /* Return the multifield. */
   /*========================*/

   returnValue->begin = 0;
   returnValue->range = end;
   returnValue->value = theMultifield;
  }

/*****************************************/
/* ImplodeFunction: H/L access routine   */
/*   for the implode$ function.          */
/*****************************************/
void ImplodeFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue theArg;

   /*======================================*/
   /* The argument should be a multifield. */
   /*======================================*/

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&theArg))
     { return; }

   /*====================*/
   /* Return the string. */
   /*====================*/

   returnValue->value = ImplodeMultifield(theEnv,&theArg);
  }

/****************************************/
/* SubseqFunction: H/L access routine   */
/*   for the subseq$ function.          */
/****************************************/
void SubseqFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue theArg;
   Multifield *theList;
   long long offset, start, end, length; /* 6.04 Bug Fix */

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&theArg))
     { return; }

   theList = theArg.multifieldValue;
   offset = theArg.begin;
   length = theArg.range;

   /*=============================================*/
   /* Get range arguments. If they are not within */
   /* appropriate ranges, return a null segment.  */
   /*=============================================*/

   if (! UDFNextArgument(context,INTEGER_BIT,&theArg))
     { return; }

   start = theArg.integerValue->contents;

   if (! UDFNextArgument(context,INTEGER_BIT,&theArg))
     { return; }
   end = theArg.integerValue->contents;

   if ((end < 1) || (end < start))
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }

   /*===================================================*/
   /* Adjust lengths  to conform to segment boundaries. */
   /*===================================================*/

   if (start > length)
     {
      EnvSetMultifieldErrorValue(theEnv,returnValue);
      return;
     }
   if (end > length) end = length;
   if (start < 1) start = 1;

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   returnValue->value = theList;
   returnValue->range = (long) ((end - start) + 1);
   returnValue->begin = (long) (offset + start - 1);
  }

/***************************************/
/* FirstFunction: H/L access routine   */
/*   for the first$ function.          */
/***************************************/
void FirstFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue theArg;
   Multifield *theList;

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&theArg)) return;

   theList = theArg.multifieldValue;

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   returnValue->value = theList;
   if (theArg.range >= 1)
     { returnValue->range = 1; }
   else
     { returnValue->range = 0; }
   returnValue->begin = theArg.begin;
  }

/**************************************/
/* RestFunction: H/L access routine   */
/*   for the rest$ function.          */
/**************************************/
void RestFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue theArg;
   Multifield *theList;

   /*===================================*/
   /* Get the segment to be subdivided. */
   /*===================================*/

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&theArg)) return;

   theList = theArg.multifieldValue;

   /*=========================*/
   /* Return the new segment. */
   /*=========================*/

   returnValue->value = theList;
   
   if (theArg.range > 0)
     {
      returnValue->begin = theArg.begin + 1;
      returnValue->range = theArg.range - 1;
     }
   else
     {
      returnValue->begin = theArg.begin;
      returnValue->range = theArg.range;
     }
  }

/*************************************/
/* NthFunction: H/L access routine   */
/*   for the nth$ function.          */
/*************************************/
void NthFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue value1, value2;
   Multifield *elm_ptr;
   long long n; /* 6.04 Bug Fix */

   if ((! UDFFirstArgument(context,INTEGER_BIT,&value1)) ||
	   (! UDFNextArgument(context,MULTIFIELD_BIT,&value2)))
     { return; }

   n = value1.integerValue->contents; /* 6.04 Bug Fix */
   if ((n > value2.range) || (n < 1))
	 {
      returnValue->lexemeValue = EnvCreateSymbol(theEnv,"nil");
	  return;
	 }

   elm_ptr = value2.multifieldValue;
   returnValue->value = elm_ptr->theFields[((long) n - 1) + value2.begin].value;
  }

/* ------------------------------------------------------------------
 *    SubsetFunction:
 *               This function compares two multi-field variables
 *               to see if the first is a subset of the second. It
 *               does not consider order.
 *
 *    INPUTS:    Two arguments via argument stack. First is the sublist
 *               multi-field variable, the second is the list to be
 *               compared to. Both should be of type MULTIFIELD_TYPE.
 *
 *    OUTPUTS:   True if the first list is a subset of the
 *               second, else false
 *
 *    NOTES:     This function is called from H/L with the subset
 *               command. Repeated values in the sublist must also
 *               be repeated in the main list.
 * ------------------------------------------------------------------
 */

void SubsetpFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue item1, item2, tmpItem;
   long i,j,k;

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&item1))
     { return; }

   if (! UDFNextArgument(context,MULTIFIELD_BIT,&item2))
     { return; }

   if (item1.range == 0)
     {
      returnValue->lexemeValue = TrueSymbol(theEnv);
      return;
     }

   if (item2.range == 0)
     {
      returnValue->lexemeValue = FalseSymbol(theEnv);
      return;
     }

   for (i = item1.begin ; i < (item1.begin + item1.range) ; i++)
     {
      tmpItem.value = item1.multifieldValue->theFields[i].value;
      tmpItem.begin = 0;
      tmpItem.range = 1;

      if (! FindDOsInSegment(&tmpItem,1,&item2,&j,&k,NULL,0))
        {
         returnValue->lexemeValue = FalseSymbol(theEnv);
         return;
        }
     }

   returnValue->lexemeValue = TrueSymbol(theEnv);
  }

/****************************************/
/* MemberFunction: H/L access routine   */
/*   for the member$ function.          */
/****************************************/
void MemberFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   UDFValue item1, item2;
   long j, k;

   returnValue->lexemeValue = FalseSymbol(theEnv);

   if (! UDFFirstArgument(context,ANY_TYPE_BITS,&item1)) return;

   if (! UDFNextArgument(context,MULTIFIELD_BIT,&item2)) return;

   if (FindDOsInSegment(&item1,1,&item2,&j,&k,NULL,0))
     {
      if (j == k)
        {
         returnValue->integerValue = EnvCreateInteger(theEnv,j);
        }
      else
        {
         returnValue->value = EnvCreateMultifield(theEnv,2);
         returnValue->multifieldValue->theFields[0].integerValue = EnvCreateInteger(theEnv,j);
         returnValue->multifieldValue->theFields[1].integerValue = EnvCreateInteger(theEnv,k);
         returnValue->begin = 0;
         returnValue->range = 2;
        }
     }
  }

/*********************/
/* FindDOsInSegment: */
/*********************/
/* 6.05 Bug Fix */
bool FindDOsInSegment(
  UDFValue *searchDOs,
  int scnt,
  UDFValue *value,
  long *si,
  long *ei,
  long *excludes,
  int epaircnt)
  {
   long mul_length,slen,i,k; /* 6.04 Bug Fix */
   int j;

   mul_length = value->range;
   for (i = 0 ; i < mul_length ; i++)
     {
      for (j = 0 ; j < scnt ; j++)
        {
         if (searchDOs[j].header->type == MULTIFIELD_TYPE)
           {
            slen = searchDOs[j].range;
            if (MVRangeCheck(i+1L,i+slen,excludes,epaircnt))
              {
               for (k = 0L ; (k < slen) && ((k + i) < mul_length) ; k++)
                 if (searchDOs[j].multifieldValue->theFields[k+searchDOs[j].begin].value !=
                     value->multifieldValue->theFields[k+i+value->begin].value)
                   break;
               if (k >= slen)
                 {
                  *si = i + 1L;
                  *ei = i + slen;
                  return true;
                 }
              }
           }
         else if ((searchDOs[j].value == value->multifieldValue->theFields[i + value->begin].value) &&
                  MVRangeCheck(i+1L,i+1L,excludes,epaircnt))
           {
            *si = *ei = i+1L;
            return true;
           }
        }
     }

   return false;
  }

/*****************/
/* MVRangeCheck: */
/*****************/
static bool MVRangeCheck(
  long si,
  long ei,
  long *elist,
  int epaircnt)
{
  int i;

  if (!elist || !epaircnt)
    return true;
  for (i = 0 ; i < epaircnt ; i++)
    if (((si >= elist[i*2]) && (si <= elist[i*2+1])) ||
        ((ei >= elist[i*2]) && (ei <= elist[i*2+1])))
    return false;

  return true;
}

#if (! BLOAD_ONLY)

/******************************************************/
/* MultifieldPrognParser: Parses the progn$ function. */
/******************************************************/
static struct expr *MultifieldPrognParser(
  Environment *theEnv,
  struct expr *top,
  const char *infile)
  {
   struct BindInfo *oldBindList, *newBindList, *prev;
   struct token tkn;
   struct expr *tmp;
   CLIPSLexeme *fieldVar = NULL;

   SavePPBuffer(theEnv," ");
   GetToken(theEnv,infile,&tkn);

   /* ================================
      Simple form: progn$ <mf-exp> ...
      ================================ */
   if (tkn.tknType != LEFT_PARENTHESIS_TOKEN)
     {
      top->argList = ParseAtomOrExpression(theEnv,infile,&tkn);
      if (top->argList == NULL)
        {
         ReturnExpression(theEnv,top);
         return NULL;
        }
     }
   else
     {
      GetToken(theEnv,infile,&tkn);
      if (tkn.tknType != SF_VARIABLE_TOKEN)
        {
         if (tkn.tknType != SYMBOL_TOKEN)
           goto MvPrognParseError;
         top->argList = Function2Parse(theEnv,infile,tkn.lexemeValue->contents);
         if (top->argList == NULL)
           {
            ReturnExpression(theEnv,top);
            return NULL;
           }
        }

      /* =========================================
         Complex form: progn$ (<var> <mf-exp>) ...
         ========================================= */
      else
        {
         fieldVar = tkn.lexemeValue;
         SavePPBuffer(theEnv," ");
         top->argList = ParseAtomOrExpression(theEnv,infile,NULL);
         if (top->argList == NULL)
           {
            ReturnExpression(theEnv,top);
            return NULL;
           }
         GetToken(theEnv,infile,&tkn);
         if (tkn.tknType != RIGHT_PARENTHESIS_TOKEN)
           goto MvPrognParseError;
         PPBackup(theEnv);
         /* PPBackup(theEnv); */
         SavePPBuffer(theEnv,tkn.printForm);
         SavePPBuffer(theEnv," ");
        }
     }

   if (CheckArgumentAgainstRestriction(theEnv,top->argList,MULTIFIELD_BIT))
     goto MvPrognParseError;

   oldBindList = GetParsedBindNames(theEnv);
   SetParsedBindNames(theEnv,NULL);
   IncrementIndentDepth(theEnv,3);
   ExpressionData(theEnv)->BreakContext = true;
   ExpressionData(theEnv)->ReturnContext = ExpressionData(theEnv)->svContexts->rtn;
   PPCRAndIndent(theEnv);
   top->argList->nextArg = GroupActions(theEnv,infile,&tkn,true,NULL,false);
   DecrementIndentDepth(theEnv,3);
   PPBackup(theEnv);
   PPBackup(theEnv);
   SavePPBuffer(theEnv,tkn.printForm);
   if (top->argList->nextArg == NULL)
     {
      ClearParsedBindNames(theEnv);
      SetParsedBindNames(theEnv,oldBindList);
      ReturnExpression(theEnv,top);
      return NULL;
     }
   tmp = top->argList->nextArg;
   top->argList->nextArg = tmp->argList;
   tmp->argList = NULL;
   ReturnExpression(theEnv,tmp);
   newBindList = GetParsedBindNames(theEnv);
   prev = NULL;
   while (newBindList != NULL)
     {
      if ((fieldVar == NULL) ? false :
          (strcmp(newBindList->name->contents,fieldVar->contents) == 0))
        {
         ClearParsedBindNames(theEnv);
         SetParsedBindNames(theEnv,oldBindList);
         PrintErrorID(theEnv,"MULTIFUN",2,false);
         EnvPrintRouter(theEnv,WERROR,"Cannot rebind field variable in function progn$.\n");
         ReturnExpression(theEnv,top);
         return NULL;
        }
      prev = newBindList;
      newBindList = newBindList->next;
     }
   if (prev == NULL)
     SetParsedBindNames(theEnv,oldBindList);
   else
     prev->next = oldBindList;
   if (fieldVar != NULL)
     ReplaceMvPrognFieldVars(theEnv,fieldVar,top->argList->nextArg,0);
   return(top);

MvPrognParseError:
   SyntaxErrorMessage(theEnv,"progn$");
   ReturnExpression(theEnv,top);
   return NULL;
  }

/***********************************************/
/* ForeachParser: Parses the foreach function. */
/***********************************************/
static struct expr *ForeachParser(
  Environment *theEnv,
  struct expr *top,
  const char *infile)
  {
   struct BindInfo *oldBindList,*newBindList,*prev;
   struct token tkn;
   struct expr *tmp;
   CLIPSLexeme *fieldVar;

   SavePPBuffer(theEnv," ");
   GetToken(theEnv,infile,&tkn);

   if (tkn.tknType != SF_VARIABLE_TOKEN)
     { goto ForeachParseError; }

   fieldVar = tkn.lexemeValue;
   SavePPBuffer(theEnv," ");
   top->argList = ParseAtomOrExpression(theEnv,infile,NULL);
   if (top->argList == NULL)
     {
      ReturnExpression(theEnv,top);
      return NULL;
     }

   if (CheckArgumentAgainstRestriction(theEnv,top->argList,MULTIFIELD_BIT))
     goto ForeachParseError;

   oldBindList = GetParsedBindNames(theEnv);
   SetParsedBindNames(theEnv,NULL);
   IncrementIndentDepth(theEnv,3);
   ExpressionData(theEnv)->BreakContext = true;
   ExpressionData(theEnv)->ReturnContext = ExpressionData(theEnv)->svContexts->rtn;
   PPCRAndIndent(theEnv);
   top->argList->nextArg = GroupActions(theEnv,infile,&tkn,true,NULL,false);
   DecrementIndentDepth(theEnv,3);
   PPBackup(theEnv);
   PPBackup(theEnv);
   SavePPBuffer(theEnv,tkn.printForm);
   if (top->argList->nextArg == NULL)
     {
      ClearParsedBindNames(theEnv);
      SetParsedBindNames(theEnv,oldBindList);
      ReturnExpression(theEnv,top);
      return NULL;
     }
   tmp = top->argList->nextArg;
   top->argList->nextArg = tmp->argList;
   tmp->argList = NULL;
   ReturnExpression(theEnv,tmp);
   newBindList = GetParsedBindNames(theEnv);
   prev = NULL;
   while (newBindList != NULL)
     {
      if ((fieldVar == NULL) ? false :
          (strcmp(newBindList->name->contents,fieldVar->contents) == 0))
        {
         ClearParsedBindNames(theEnv);
         SetParsedBindNames(theEnv,oldBindList);
         PrintErrorID(theEnv,"MULTIFUN",2,false);
         EnvPrintRouter(theEnv,WERROR,"Cannot rebind field variable in function foreach.\n");
         ReturnExpression(theEnv,top);
         return NULL;
        }
      prev = newBindList;
      newBindList = newBindList->next;
     }
   if (prev == NULL)
     SetParsedBindNames(theEnv,oldBindList);
   else
     prev->next = oldBindList;
   if (fieldVar != NULL)
     ReplaceMvPrognFieldVars(theEnv,fieldVar,top->argList->nextArg,0);
   return(top);

ForeachParseError:
   SyntaxErrorMessage(theEnv,"foreach");
   ReturnExpression(theEnv,top);
   return NULL;
  }

/**********************************************/
/* ReplaceMvPrognFieldVars: Replaces variable */
/*   references found in the progn$ function. */
/**********************************************/
static void ReplaceMvPrognFieldVars(
  Environment *theEnv,
  CLIPSLexeme *fieldVar,
  struct expr *theExp,
  int depth)
  {
   size_t flen;

   flen = strlen(fieldVar->contents);
   while (theExp != NULL)
     {
      if ((theExp->type != SF_VARIABLE) ? false :
          (strncmp(theExp->lexemeValue->contents,fieldVar->contents,
                   (STD_SIZE) flen) == 0))
        {
         if (theExp->lexemeValue->contents[flen] == '\0')
           {
            theExp->type = FCALL;
            theExp->value = FindFunction(theEnv,"(get-progn$-field)");
            theExp->argList = GenConstant(theEnv,INTEGER_TYPE,EnvCreateInteger(theEnv,(long long) depth));
           }
#if DEFTEMPLATE_CONSTRUCT
         else if (theExp->lexemeValue->contents[flen] == ':')
           {
            size_t svlen = strlen(theExp->lexemeValue->contents);
            if (svlen > (flen + 1))
              {
               const char *slotName = &theExp->lexemeValue->contents[flen+1];

               theExp->argList = GenConstant(theEnv,FCALL,(void *) FindFunction(theEnv,"(get-progn$-field)"));
               theExp->argList->argList = GenConstant(theEnv,INTEGER_TYPE,EnvCreateInteger(theEnv,(long long) depth));

               theExp->argList->nextArg = GenConstant(theEnv,SYMBOL_TYPE,EnvCreateSymbol(theEnv,slotName));
               theExp->argList->nextArg->nextArg = GenConstant(theEnv,SYMBOL_TYPE,theExp->value);

               theExp->type = FCALL;
               theExp->value = FindFunction(theEnv,"(slot-value)");
              }
           }
#endif
         else if (strcmp(theExp->lexemeValue->contents + flen,"-index") == 0)
           {
            theExp->type = FCALL;
            theExp->value = FindFunction(theEnv,"(get-progn$-index)");
            theExp->argList = GenConstant(theEnv,INTEGER_TYPE,EnvCreateInteger(theEnv,(long long) depth));
           }
        }
      else if (theExp->argList != NULL)
        {
         if ((theExp->type == FCALL) && ((theExp->value == (void *) FindFunction(theEnv,"progn$")) ||
                                        (theExp->value == (void *) FindFunction(theEnv,"foreach")) ))
           ReplaceMvPrognFieldVars(theEnv,fieldVar,theExp->argList,depth+1);
         else
           ReplaceMvPrognFieldVars(theEnv,fieldVar,theExp->argList,depth);
        }
      theExp = theExp->nextArg;
     }
  }

#endif /* (! BLOAD_ONLY) */

/*****************************************/
/* MultifieldPrognFunction: H/L access   */
/*   routine for the progn$ function.    */
/*****************************************/
void MultifieldPrognFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   MultifieldPrognDriver(context,returnValue,"progn$");
  }

/***************************************/
/* ForeachFunction: H/L access routine */
/*   for the foreach function.         */
/***************************************/
void ForeachFunction(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   MultifieldPrognDriver(context,returnValue,"foreach");
  }

/*******************************************/
/* MultifieldPrognDriver: Driver routine   */
/*   for the progn$ and foreach functions. */
/******************************************/
static void MultifieldPrognDriver(
  UDFContext *context,
  UDFValue *returnValue,
  const char *functionName)
  {
   Expression *theExp;
   UDFValue argval;
   long i, end; /* 6.04 Bug Fix */
   FIELD_VAR_STACK *tmpField;
   CLIPSBlock gcBlock;
   Environment *theEnv = context->environment;

   tmpField = get_struct(theEnv,fieldVarStack);
   tmpField->type = SYMBOL_TYPE;
   tmpField->value = FalseSymbol(theEnv);
   tmpField->nxt = MultiFunctionData(theEnv)->FieldVarStack;
   MultiFunctionData(theEnv)->FieldVarStack = tmpField;
   returnValue->value = FalseSymbol(theEnv);

   if (! UDFFirstArgument(context,MULTIFIELD_BIT,&argval))
     {
      MultiFunctionData(theEnv)->FieldVarStack = tmpField->nxt;
      rtn_struct(theEnv,fieldVarStack,tmpField);
      returnValue->value = FalseSymbol(theEnv);
      return;
     }

   CLIPSBlockStart(theEnv,&gcBlock);

   end = (argval.begin + argval.range) - 1;
   for (i = argval.begin ; i <= end ; i++)
     {
      tmpField->type = argval.multifieldValue->theFields[i].header->type;
      tmpField->value = argval.multifieldValue->theFields[i].value;
      tmpField->index = (i - argval.begin) + 1;
      for (theExp = GetFirstArgument()->nextArg ; theExp != NULL ; theExp = theExp->nextArg)
        {
         EvaluateExpression(theEnv,theExp,returnValue);

         if (EvaluationData(theEnv)->HaltExecution || ProcedureFunctionData(theEnv)->BreakFlag || ProcedureFunctionData(theEnv)->ReturnFlag)
           {
            ProcedureFunctionData(theEnv)->BreakFlag = false;
            if (EvaluationData(theEnv)->HaltExecution)
              {
               returnValue->value = FalseSymbol(theEnv);
              }
            MultiFunctionData(theEnv)->FieldVarStack = tmpField->nxt;
            rtn_struct(theEnv,fieldVarStack,tmpField);
            CLIPSBlockEnd(theEnv,&gcBlock,returnValue);
            return;
           }

         /*===================================*/
         /* Garbage collect if this isn't the */
         /* last evaluation of the progn$.    */
         /*===================================*/

         if ((i < end) || (theExp->nextArg != NULL))
           {
            CleanCurrentGarbageFrame(theEnv,NULL);
            CallPeriodicTasks(theEnv);
           }
        }
     }

   ProcedureFunctionData(theEnv)->BreakFlag = false;
   MultiFunctionData(theEnv)->FieldVarStack = tmpField->nxt;
   rtn_struct(theEnv,fieldVarStack,tmpField);

   CLIPSBlockEnd(theEnv,&gcBlock,returnValue);
   CallPeriodicTasks(theEnv);
  }

/*******************/
/* GetMvPrognField */
/*******************/
void GetMvPrognField(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   int depth;
   FIELD_VAR_STACK *tmpField;

   depth = (int) GetFirstArgument()->integerValue->contents;
   tmpField = MultiFunctionData(theEnv)->FieldVarStack;
   while (depth > 0)
     {
      tmpField = tmpField->nxt;
      depth--;
     }
   returnValue->value = tmpField->value;
  }

/*******************/
/* GetMvPrognIndex */
/*******************/
void GetMvPrognIndex(
  Environment *theEnv,
  UDFContext *context,
  UDFValue *returnValue)
  {
   int depth;
   FIELD_VAR_STACK *tmpField;

   depth = (int) GetFirstArgument()->integerValue->contents;
   tmpField = MultiFunctionData(theEnv)->FieldVarStack;
   while (depth > 0)
     {
      tmpField = tmpField->nxt;
      depth--;
     }
   returnValue->integerValue = EnvCreateInteger(theEnv,tmpField->index);
  }

#endif /* MULTIFIELD_FUNCTIONS */

#if OBJECT_SYSTEM || MULTIFIELD_FUNCTIONS

/**************************************************************************
  NAME         : ReplaceMultiValueField
  DESCRIPTION  : Performs a replace on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) Beginning of index range
                 4) End of range
                 5) The new field value
  RETURNS      : True if successful, false otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
bool ReplaceMultiValueField(
  Environment *theEnv,
  UDFValue *dst,
  UDFValue *src,
  long rb,
  long re,
  UDFValue *field,
  const char *funcName)
  {
   long i,j,k;
   struct field *deptr;
   struct field *septr;
   long srclen,dstlen;

   srclen = ((src != NULL) ? src->range : 0);
   if ((re < rb) ||
	   (rb < 1) || (re < 1) ||
	   (rb > srclen) || (re > srclen))
	 {
	  MVRangeError(theEnv,rb,re,srclen,funcName);
	  return false;
	 }
   rb = src->begin + rb - 1;
   re = src->begin + re - 1;
   if (field->header->type == MULTIFIELD_TYPE)
	 dstlen = srclen + field->range - (re-rb+1);
   else
	 dstlen = srclen + 1 - (re-rb+1);
   dst->begin = 0;
   dst->value = EnvCreateMultifield(theEnv,dstlen);
   dst->range = dstlen;
   for (i = 0 , j = src->begin ; j < rb ; i++ , j++)
	 {
	  deptr = &dst->multifieldValue->theFields[i];
	  septr = &src->multifieldValue->theFields[j];
	  deptr->value = septr->value;
	 }
   if (field->header->type != MULTIFIELD_TYPE)
	 {
	  deptr = &dst->multifieldValue->theFields[i++];
	  deptr->value = field->value;
	 }
   else
	 {
	  for (k = field->begin ; k < (field->begin + field->range) ; k++ , i++)
		{
		 deptr = &dst->multifieldValue->theFields[i];
		 septr = &field->multifieldValue->theFields[k];
		 deptr->value = septr->value;
		}
	 }
   while (j < re)
	 j++;
   for (j++ ; i < dstlen ; i++ , j++)
	 {
	  deptr = &dst->multifieldValue->theFields[i];
	  septr = &src->multifieldValue->theFields[j];
	  deptr->value = septr->value;
	 }
   return true;
  }

/**************************************************************************
  NAME         : InsertMultiValueField
  DESCRIPTION  : Performs an insert on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) The index for the change
                 4) The new field value
  RETURNS      : True if successful, false otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
bool InsertMultiValueField(
  Environment *theEnv,
  UDFValue *dst,
  UDFValue *src,
  long theIndex,
  UDFValue *field,
  const char *funcName)
  {
   long i,j,k;
   Field *deptr, *septr;
   long srclen,dstlen;

   srclen = (long) ((src != NULL) ? src->range : 0);
   if (theIndex < 1)
     {
      MVRangeError(theEnv,theIndex,theIndex,srclen+1,funcName);
      return false;
     }
   if (theIndex > (srclen + 1))
     theIndex = (srclen + 1);
   dst->begin = 0;
   if (src == NULL)
     {
      if (field->header->type == MULTIFIELD_TYPE)
        {
         DuplicateMultifield(theEnv,dst,field);
         AddToMultifieldList(theEnv,dst->multifieldValue);
        }
      else
        {
         dst->value = EnvCreateMultifield(theEnv,0L);
         dst->range = 1;
         deptr = &dst->multifieldValue->theFields[0];
         deptr->value = field->value;
        }
      return true;
     }
   dstlen = (field->header->type == MULTIFIELD_TYPE) ? field->range + srclen : srclen + 1;
   dst->value = EnvCreateMultifield(theEnv,dstlen);
   dst->range = dstlen;
   theIndex--;
   for (i = 0 , j = src->begin ; i < theIndex ; i++ , j++)
     {
      deptr = &dst->multifieldValue->theFields[i];
      septr = &src->multifieldValue->theFields[j];
      deptr->value = septr->value;
     }
   if (field->header->type != MULTIFIELD_TYPE)
     {
      deptr = &dst->multifieldValue->theFields[theIndex];
      deptr->value = field->value;
      i++;
     }
   else
     {
      for (k = field->begin ; k < (field->begin + field->range) ; k++ , i++)
        {
         deptr = &dst->multifieldValue->theFields[i];
         septr = &field->multifieldValue->theFields[k];
         deptr->value = septr->value;
        }
     }
   for ( ; j < (src->begin + src->range) ; i++ , j++)
     {
      deptr = &dst->multifieldValue->theFields[i];
      septr = &src->multifieldValue->theFields[j];
      deptr->value = septr->value;
     }
   return true;
  }

/*******************************************************
  NAME         : MVRangeError
  DESCRIPTION  : Prints out an error messages for index
                   out-of-range errors in multi-field
                   access functions
  INPUTS       : 1) The bad range start
                 2) The bad range end
                 3) The max end of the range (min is
                     assumed to be 1)
  RETURNS      : Nothing useful
  SIDE EFFECTS : None
  NOTES        : None
 ******************************************************/
static void MVRangeError(
  Environment *theEnv,
  long brb,
  long bre,
  long max,
  const char *funcName)
  {
   PrintErrorID(theEnv,"MULTIFUN",1,false);
   EnvPrintRouter(theEnv,WERROR,"Multifield index ");
   if (brb == bre)
     PrintLongInteger(theEnv,WERROR,(long long) brb);
   else
     {
      EnvPrintRouter(theEnv,WERROR,"range ");
      PrintLongInteger(theEnv,WERROR,(long long) brb);
      EnvPrintRouter(theEnv,WERROR,"..");
      PrintLongInteger(theEnv,WERROR,(long long) bre);
     }
   EnvPrintRouter(theEnv,WERROR," out of range 1..");
   PrintLongInteger(theEnv,WERROR,(long long) max);
   if (funcName != NULL)
     {
      EnvPrintRouter(theEnv,WERROR," in function ");
      EnvPrintRouter(theEnv,WERROR,funcName);
     }
   EnvPrintRouter(theEnv,WERROR,".\n");
  }

/**************************************************************************
  NAME         : DeleteMultiValueField
  DESCRIPTION  : Performs a modify on the src multi-field value
                   storing the results in the dst multi-field value
  INPUTS       : 1) The destination value buffer
                 2) The source value (can be NULL)
                 3) The beginning index for deletion
                 4) The ending index for deletion
  RETURNS      : True if successful, false otherwise
  SIDE EFFECTS : Allocates and sets a ephemeral segment (even if new
                   number of fields is 0)
                 Src value segment is not changed
  NOTES        : index is NOT guaranteed to be valid
                 src is guaranteed to be a multi-field variable or NULL
 **************************************************************************/
bool DeleteMultiValueField(
  Environment *theEnv,
  UDFValue *dst,
  UDFValue *src,
  long rb,
  long re,
  const char *funcName)
  {
   long i,j;
   Field *deptr, *septr;
   long srclen, dstlen;

   srclen = (long) ((src != NULL) ? src->range : 0);
   if ((re < rb) ||
       (rb < 1) || (re < 1) ||
       (rb > srclen) || (re > srclen))
     {
      MVRangeError(theEnv,rb,re,srclen,funcName);
      return false;
     }
   dst->begin = 0;
   if (srclen == 0)
    {
     dst->value = EnvCreateMultifield(theEnv,0L);
     dst->range = 0;
     return true;
    }
   rb = src->begin + rb -1;
   re = src->begin + re -1;
   dstlen = srclen-(re-rb+1);
   dst->range = dstlen;
   dst->value = EnvCreateMultifield(theEnv,dstlen);
   for (i = 0 , j = src->begin ; j < rb ; i++ , j++)
     {
      deptr = &dst->multifieldValue->theFields[i];
      septr = &src->multifieldValue->theFields[j];
      deptr->value = septr->value;
     }
   while (j < re)
     j++;
   for (j++ ; i < (dst->begin + dst->range) ; j++ , i++)
     {
      deptr = &dst->multifieldValue->theFields[i];
      septr = &src->multifieldValue->theFields[j];
      deptr->value = septr->value;
     }
   return true;
  }

#endif /* OBJECT_SYSTEM || MULTIFIELD_FUNCTIONS */
