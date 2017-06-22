   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  01/12/15            */
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
/* Notes: the implementation sacrifices memory for           */
/*    achieving speed.                                       */
/*                                                           */
/*************************************************************/

struct regexp;
typedef struct regexp regexp_t;


#define REGEXP_CACHE_SIZE 512


#define MATCH_OFFSET(matches, index)            \
    (matches[2 * index])
#define MATCH_LIMIT(matches, index)             \
    (matches[2 * index + 1])
#define MATCH_SIZE(matches, index)                      \
    (matches[2 * index + 1] - matches[2 * index])


regexp_t *regexp_compile(const char *, int);
int regexp_matches_size(regexp_t *);
int regexp_match(regexp_t *, const char *, int, int, int *, int);
void regexp_cleanup();
