   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.40  06/20/16            */
   /*                                                     */
   /*            SORT FUNCTIONS HEADER MODULE             */
   /*******************************************************/

/*************************************************************/
/* Purpose: Contains the code for sorting functions.         */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.23: Correction for FalseSymbol/TrueSymbol. DR0859  */
/*                                                           */
/*      6.24: The sort function leaks memory when called     */
/*            with a multifield value of length zero.        */
/*            DR0864                                         */
/*                                                           */
/*      6.30: Added environment cleanup call function        */
/*            DeallocateSortFunctionData.                    */
/*                                                           */
/*      6.40: Removed LOCALE definition.                     */
/*                                                           */
/*            Pragma once and other inclusion changes.       */
/*                                                           */
/*************************************************************/

#ifndef _H_sortfun

#pragma once

#define _H_sortfun

   void                           SortFunctionDefinitions(void *);
   void                           MergeSort(void *,unsigned long,DATA_OBJECT *,
                                                   int (*)(void *,DATA_OBJECT *,DATA_OBJECT *));
   void                           SortFunction(void *,DATA_OBJECT *);

#endif /* _H_sortfun */



