   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*             CLIPS Version 6.30  01/12/15            */
   /*                                                     */
   /*          Regular Expression User Functions          */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Matteo Cafasso                                       */
/*                                                           */
/* Notes: the implementation sacrifices memory for           */
/*    achieving speed.                                       */
/*                                                           */
/*************************************************************/

#include <string.h>

#include "clips.h"
#include "regfun.h"
#include "regexp.h"


static inline void regex_search(void *, DATA_OBJECT_PTR,
                                const char *, const char *, int);
static inline int regex_count(const char *, const char *, int);
static inline int string_arguments(void *, char *,
                                   DATA_OBJECT *, DATA_OBJECT *);
static inline void *match_multifield(void *, const char *, int *, int);


/****************************************************/
/* StringRegexMatch: H/L access routine             */
/*   for the str-regexmatch function.               */
/****************************************************/
int StringRegexMatch(void *environment)
{
    DATA_OBJECT pattern, string;

    if (string_arguments(environment, "str-regexmatch", &pattern, &string) < 0)
        return 0;

    return regex_count(DOToString(pattern), DOToString(string), FALSE);
}


/*****************************************************/
/* StringRegexCaseMatch: H/L access routine          */
/*   for the str-regeximatch function.               */
/*****************************************************/
int StringRegexCaseMatch(void *environment)
{
    DATA_OBJECT pattern, string;

    if (string_arguments(environment, "str-regeximatch", &pattern, &string) < 0)
        return 0;

    return regex_count(DOToString(pattern), DOToString(string), TRUE);
}


/*****************************************************/
/* StringRegexSearch: H/L access routine             */
/*   for the str-regexsearch function.               */
/*****************************************************/
void StringRegexSearch(void *environment, DATA_OBJECT_PTR returnValue)
{
    DATA_OBJECT pattern, string;

    if (string_arguments(environment, "str-regexsearch",
                         &pattern, &string) == 0)
        regex_search(environment, returnValue,
                     DOToString(pattern), DOToString(string), FALSE);
}


/*****************************************************/
/* StringRegexCaseSearch: H/L access routine         */
/*   for the str-regexisearch function.              */
/*****************************************************/
void StringRegexCaseSearch(void *environment, DATA_OBJECT_PTR returnValue)
{
    DATA_OBJECT pattern, string;

    if (string_arguments(environment, "str-regexisearch",
                         &pattern, &string) == 0)
        regex_search(environment, returnValue,
                     DOToString(pattern), DOToString(string), TRUE);
}


/***************************************************/
/* StringRegexCaseSearchGlobal: H/L access routine */
/*   for Clear function callback.                  */
/***************************************************/
void StringRegexClear(void *environment)
{
    regexp_cleanup();
}


/*************************************************************/
/* regex_count: counts the occurrences of pattern in string. */
/*************************************************************/
static inline int regex_count(const char *pattern, const char *string,
                              int caseless)
{
    regexp_t *regex = NULL;
    int nummatch = 0, string_offset = 0, string_size = strlen(string);

    regex = regexp_compile(pattern, caseless);

    if (regex != NULL) {
        int matches_size = regexp_matches_size(regex);
        int matches[matches_size];

        while (regexp_match(regex, string, string_offset,
                            string_size, matches, matches_size) > 0 &&
               string_offset != MATCH_LIMIT(matches, 0)) {
            nummatch++;
            string_offset = MATCH_LIMIT(matches, 0);
        }
    }

    return nummatch;
}


/*************************************************/
/* regex_search: searches for pattern in string. */
/*    Found results are stored in returnValue.   */
/*************************************************/
static inline void regex_search(void *environment, DATA_OBJECT_PTR returnValue,
                                const char *pattern, const char *string,
                                int caseless)
{
    regexp_t *regex = NULL;
    void *multifield = NULL;
    int string_size = strlen(string), nummatch = 0;

    regex = regexp_compile(pattern, caseless);

    if (regex != NULL) {
        int matches_size = regexp_matches_size(regex);
        int matches[matches_size];

        nummatch = regexp_match(regex, string, 0, string_size,
                                matches, matches_size);

        if (nummatch > 0)
            multifield = match_multifield(environment, string,
                                          matches, nummatch);
    }

    SetpType(returnValue, MULTIFIELD);
    SetpValue(returnValue, multifield);
    SetpDOBegin(returnValue, 1);
    SetpDOEnd(returnValue, nummatch);
}


/****************************************************************************/
/* match_multifields: builds a Multifield data type from the regex matches. */
/****************************************************************************/
static inline void *match_multifield(void *environment, const char *string,
                                     int *matches, int nummatch)
{
    int index;
    void *multifield = EnvCreateMultifield(environment, nummatch + 1);

    for (index = 0; index < nummatch; index++) {
        int size = MATCH_SIZE(matches, index);
        int offset = MATCH_OFFSET(matches, index);
        char token[size + 1];

        memset(token, 0, (size + 1)*sizeof(char));
        strncpy(token, string + offset, size);

        SetMFType(multifield, index + 1, STRING);
        SetMFValue(multifield, index + 1, EnvAddSymbol(environment, token));
    }

    return multifield;
}


/**************************************************************/
/* string_arguments: gets the arguments from the Environment. */
/*    On success 0 is returned, -1 otherwise.                 */
/**************************************************************/
static inline int string_arguments(void *environment, char *function_name,
                                   DATA_OBJECT *argument1,
                                   DATA_OBJECT *argument2)
{
    if (EnvArgCountCheck(environment, function_name, EXACTLY, 2) == -1)
        return -1;

    if (EnvArgTypeCheck(environment, function_name, 1,
                        SYMBOL_OR_STRING, argument1) == FALSE)
        return -1;

    if (EnvArgTypeCheck(environment, function_name, 2,
                        SYMBOL_OR_STRING, argument2) == FALSE)
        return -1;

    return 0;
}
