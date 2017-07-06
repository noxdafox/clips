   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  27/06/17            */
   /*                                                     */
   /*             Regular Expression Functions            */
   /*******************************************************/

/*************************************************************/
/* Purpose: provides regular expression functionality.       */
/*    Regular expressions are compiled and cached for better */
/*    performance.                                           */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Matteo Cafasso                                       */
/*                                                           */
/*************************************************************/


#include "clips.h"


#define REGEX_JIT_COMPILE 1
#define REGEXP_CACHE_SIZE 512
#define REGEX_DATA_INDEX USER_ENVIRONMENT_DATA + 0


#if REGEXP_CACHE_SIZE
struct regex_data
{
    void *cache;
};
typedef struct regex_data regex_data_t;


#define REGEX_DATA(environment)                                         \
    ((regex_data_t *) GetEnvironmentData(environment, REGEX_DATA_INDEX))

extern void RegexCacheClear(void *);
#endif

extern void RegexMatch(void *, DATA_OBJECT_PTR);
extern void RegexCaseMatch(void *, DATA_OBJECT_PTR);
