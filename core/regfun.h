   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  01/12/15            */
   /*                                                     */
   /*          Regular Expression Functions Header        */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Matteo Cafasso                                       */
/*                                                           */
/* TODO: Regular expression pattern caching.                 */
/*                                                           */
/*************************************************************/

#include "clips.h"

extern int StringRegexMatch(void *);
extern int StringRegexCaseMatch(void *);
extern void StringRegexSearch(void *, DATA_OBJECT_PTR);
extern void StringRegexCaseSearch(void *, DATA_OBJECT_PTR);

extern void StringRegexClear(void *);
